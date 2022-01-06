#pragma once
#include<vector>
#include<array>
#include<unordered_map>
#include<optional>
#include<algorithm>
#include"debug.h"
#include"scanner.h"
#include"precedence.h"

namespace Lox {
	struct Local {
		Token name;
		int depth;
		Local(const Token& _name, int _depth) :name(_name), depth(_depth) {}
	};

	struct Compiler {
		std::vector<Local> locals;
		int localCount;
		int scopeDepth;
		Compiler() :localCount(0), scopeDepth(0) {}
	};

	struct VM {
		enum struct interpretResult {
			OK, COMPILE_ERROR, RUNTIME_ERROR
		};

		using rule_t = decltype (ParseRuleHelp<VM>(allTokenType{}));
		Chunk& chunk;
		Chunk compilingChunk;
		std::size_t ip, stackTop;
		int innermostLoopStart = -1, innermostLoopScopeDepth = 0, innermostLoopSelect = 0;
		std::vector<value_t> stack;
		Parser parser;
		Scanner scanner{ nullptr };
		std::unordered_map<std::string_view, value_t> globals;
		Compiler compiler;
		rule_t rules;
		std::optional<size_t> previousOp;
		int startInc = -1, endInc = -1;
		VM(Chunk& _chunk) :chunk(_chunk), ip(0), stackTop(0), rules(ParseRuleHelp<VM>(allTokenType{})) {}

		auto& getRule(TokenType type) {
			return rules[static_cast<std::size_t>(type)];
		}

		void errorAtCurrent(const char* message) {
			errorAt(parser.current, message);
		}

		void error(const char* message) {
			errorAt(parser.previous, message);
		}

		void errorAt(const Token& token, const char* message) {
			if (parser.panicMode) return;
			parser.panicMode = true;
			std::cerr << string_format("[line %d] Error", token.line);
			if (token.type == TokenType::eof) {
				std::cerr << string_format(" at end");
			}
			else if (token.type == TokenType::error) {	 // Nothing.
			}
			else {
				std::cerr << string_format(" at '%.*s'", token.lexem.size(), token.lexem.data());
			}
			std::cerr << string_format(": %s\n", message, token.lexem.size(), token.lexem.data());
			parser.hadError = true;
		}

		void parsePrecedence(Precedence precedence) {
			advance();
			auto& prefixRule = getRule(parser.previous.type).prefix;
			if (prefixRule == nullptr) {
				error("Expect expression.");
				return;
			}

			bool canAssign = precedence <= Precedence::ASSIGNMENT;
			(this->*prefixRule)(canAssign);
			while (precedence <= getRule(parser.current.type).precedence) {
				advance();
				auto& infixRule = getRule(parser.previous.type).infix;
				(this->*infixRule)(canAssign);
			}


			if (canAssign &&
				match(TokenType::equal, TokenType::plus_equal, TokenType::minus_equal, TokenType::star_equal, TokenType::slash_equal)) {
				error("Invalid assignment target.");
			}
		}

		void advance() {
			parser.previous = parser.current;
			while (true) {
				parser.current = scanner.scanToken();
				if (parser.current.type != TokenType::error) break;
				errorAtCurrent(parser.current.lexem.data());
			}
		}

		void consume(TokenType type, const char* message) {
			if (parser.current.type == type) {
				advance();
				return;
			}
			errorAtCurrent(message);
		}

		void literal(bool canAssign) {
			switch (parser.previous.type) {
			case TokenType::kw_false: emitByte(OpCode::FALSE); break;
			case TokenType::nil: emitByte(OpCode::NIL); break;
			case TokenType::kw_true: emitByte(OpCode::TRUE); break;
			default: return; // Unreachable.
			}
		}

		void string(bool canAssign) {
			emitConstant(std::string{ parser.previous.lexem.substr(1,parser.previous.lexem.size() - 2) });
		}

		void number(bool canAssign) {
			double value = std::atof(parser.previous.lexem.data());
			emitConstant(value);
		}

		void grouping(bool canAssign) {
			expression();
			consume(TokenType::right_paren, "Expect ')' after expression.");
		}

		void binary(bool canAssign) {
			TokenType operatorType = parser.previous.type;
			auto& rule = getRule(operatorType);

			if (operatorType == TokenType::inc) {
				if (previousOp) {
					auto get = std::get<OpCode>(compilingChunk.code[*previousOp]);
					bool isVariable = true;
					OpCode set;
					switch (get) {
					case OpCode::GET_GLOBAL: set = OpCode::SET_GLOBAL; break;
					case OpCode::GET_LOCAL: set = OpCode::SET_LOCAL; break;
					default: isVariable = false;
					}
					if (isVariable) {
						auto arg = compilingChunk.code[*previousOp + 1];
						startInc = compilingChunk.code.size();
						emitConstant(1.);
						emitBytes(OpCode::ADD, set, arg);
						emitConstant(1.);
						emitByte(OpCode::SUBTRACT);
						endInc = compilingChunk.code.size();
					}
					else error("Invalid assignment target.");
				}
			}

			else parsePrecedence((Precedence)(static_cast<int>(rule.precedence) + 1));

			switch (operatorType) {
			case TokenType::plus:emitByte(OpCode::ADD); break;
			case TokenType::minus: emitByte(OpCode::SUBTRACT); break;
			case TokenType::star:emitByte(OpCode::MULTIPLY); break;
			case TokenType::slash:emitByte(OpCode::DIVIDE); break;
			case TokenType::bang_equal:emitBytes(OpCode::EQUAL, OpCode::NOT); break;
			case TokenType::equal_equal:emitByte(OpCode::EQUAL); break;
			case TokenType::greater:emitByte(OpCode::GREATER); break;
			case TokenType::greater_equal:emitBytes(OpCode::LESS, OpCode::NOT); break;
			case TokenType::less:emitByte(OpCode::LESS); break;
			case TokenType::less_equal:emitBytes(OpCode::GREATER, OpCode::NOT); break;
			default: return; // Unreachable.
			}

			if (startInc != -1 && endInc != -1) {
				//std::rotate(compilingChunk.code.begin() + startInc, compilingChunk.code.begin() + endInc, compilingChunk.code.end());
				startInc = endInc = -1;
			}
		}

		void unary(bool canAssign) {
			TokenType operatorType = parser.previous.type;


			//expression;	// Compile the operand.
			parsePrecedence(Precedence::UNARY);

			switch (operatorType) { // Emit the operator instruction.
			case TokenType::bang: emitByte(OpCode::NOT); break;
			case TokenType::minus: emitByte(OpCode::NEGATE); break;
			case TokenType::inc:
				if (previousOp) {
					auto get = std::get<OpCode>(compilingChunk.code[*previousOp]);
					bool isVariable = true;
					OpCode set;
					switch (get) {
					case OpCode::GET_GLOBAL: set = OpCode::SET_GLOBAL; break;
					case OpCode::GET_LOCAL: set = OpCode::SET_LOCAL; break;
					default: isVariable = false;
					}
					if (isVariable) {
						auto arg = compilingChunk.code[*previousOp + 1];
						emitConstant(1.);
						emitBytes(OpCode::ADD, set, arg);
					}
					else error("Invalid assignment target.");
				}
				break;
			default: return; // Unreachable.
			}
		}

		code_t makeConstant(const value_t& value) {
			auto constant = compilingChunk.addConstant(value);
			if (constant > UINT8_MAX) {
				error("Too many constants in one chunk.");
				return 0;
			}
			return constant;
		}

		void emitConstant(const value_t& value) {
			emitBytes(OpCode::CONSTANT, makeConstant(value));
		}

		void expression() {
			parsePrecedence(Precedence::ASSIGNMENT);
		}

		void declaration() {
			if (match(TokenType::var)) {
				varDeclaration();
			}
			else {
				statement();
			}
			if (parser.panicMode) synchronize();
		}

		code_t parseVariable(const char* errorMessage) {
			consume(TokenType::identifier, errorMessage);
			declareVariable();
			if (compiler.scopeDepth > 0) return 0;
			return identifierConstant(parser.previous);
		}

		void declareVariable() {
			if (compiler.scopeDepth == 0) return;
			auto& name = parser.previous;
			for (int i = compiler.localCount - 1; i >= 0; i--) {
				auto& local = compiler.locals[i];
				if (local.depth != -1 && local.depth < compiler.scopeDepth) {
					break;
				}

				if (identifiersEqual(name, local.name)) {
					error("Already a variable with this name in this scope.");
				}
			}
			addLocal(name);
		}

		bool identifiersEqual(const Token& a, const Token& b) {
			return a.lexem == b.lexem;
		}

		void addLocal(const Token& name) {
			/*if (compiler.localCount == 50) {
				error("Too many local variables in function.");
				return;
			}*/
			//auto &local = compiler.locals[compiler.localCount++];
			compiler.locals.emplace_back(name, -1);
			compiler.localCount++;
			/*local.name = name;
			local.depth = -1;
			local.depth = compiler.scopeDepth;*/
		}

		code_t identifierConstant(const Token& name) {
			return makeConstant(std::string{ name.lexem });
		}

		void varDeclaration() {
			code_t global = parseVariable("Expect variable name.");

			if (match(TokenType::equal)) {
				expression();
			}
			else {
				emitByte(OpCode::NIL);
			}
			//consume(TokenType::semicolon, "Expect ';' after variable declaration.");
			defineVariable(global);
			if (match(TokenType::comma)) {
				varDeclaration();
				return;
			}
			consume(TokenType::semicolon, "Expect ';' after variable declaration.");
		}

		void defineVariable(const code_t& global) {
			if (compiler.scopeDepth > 0) {
				markInitialized();
				return;
			}
			emitBytes(OpCode::DEFINE_GLOBAL, global);
		}

		void markInitialized() {
			compiler.locals[compiler.localCount - 1].depth = compiler.scopeDepth;
		}

		void variable(bool canAssign) {
			namedVariable(parser.previous, canAssign);
		}

		int resolveLocal(const Token& name) {
			for (int i = compiler.localCount - 1; i >= 0; i--) {
				auto& local = compiler.locals[i];
				if (name.lexem == local.name.lexem) {
					if (local.depth == -1) {
						error("Can't read local variable in its own initializer.");
					}
					return i;
				}
			}
			return -1;
		}

		void namedVariable(const Token& name, bool canAssign) {
			code_t arg; // = identifierConstant(name);
			code_t getOp, setOp;
			if (auto localIndex = resolveLocal(name); localIndex != -1) {
				arg = static_cast<std::size_t>(localIndex);
				getOp = OpCode::GET_LOCAL;
				setOp = OpCode::SET_LOCAL;
			}
			else {
				arg = identifierConstant(name);
				getOp = OpCode::GET_GLOBAL;
				setOp = OpCode::SET_GLOBAL;
			}

			if (canAssign && match(TokenType::equal)) {
				expression();
				emitBytes(setOp, arg);
			}
			else if (canAssign &&
				match(TokenType::plus_equal, TokenType::minus_equal, TokenType::star_equal, TokenType::slash_equal)) {
				auto type = parser.previous.type;
				emitBytes(getOp, arg);
				expression();
				switch (type) {
				case TokenType::plus_equal:emitByte(OpCode::ADD); break;
				case TokenType::minus_equal:emitByte(OpCode::SUBTRACT); break;
				case TokenType::star_equal:emitByte(OpCode::MULTIPLY); break;
				case TokenType::slash_equal:emitByte(OpCode::DIVIDE); break;
				}
				emitBytes(setOp, arg);
			}
			/*else if (match(TokenType::plus)) {
				//advance();
				expression();
				emitBytes(getOp, arg);
			}*/
			else {
				// expression();
				 /*advance();
				 auto& infixRule = getRule(parser.previous.type).infix;
				 (this->*infixRule)(canAssign);*/
				emitBytes(getOp, arg);
			}
			//emitBytes(OpCode::GET_GLOBAL, arg);
		}

		void synchronize() {
			parser.panicMode = false;

			while (parser.current.type != TokenType::eof) {
				if (parser.previous.type == TokenType::semicolon) return;
				switch (parser.current.type) {
				case TokenType::kw_class:
				case TokenType::fun:
				case TokenType::var:
				case TokenType::kw_for:
				case TokenType::kw_if:
				case TokenType::kw_while:
				case TokenType::print:
				case TokenType::kw_return: return;
				default:; // Do nothing.
				}

				advance();
			}
		}

		void statement() {
			if (match(TokenType::print)) {
				printStatement();
			}
			else if (match(TokenType::kw_for)) {
				forStatement();
			}
			else if (match(TokenType::kw_if)) {
				ifStatement();
			}
			else if (match(TokenType::kw_while)) {
				whileStatement();
			}
			else if (match(TokenType::left_brace)) {
				beginScope();
				block();
				endScope();
			}
			else if (match(TokenType::kw_continue)) {
				continueStatement();
			}
			else if (match(TokenType::kw_break)) {
				breakStatement();
			}
			else {
				expressionStatement();
			}
		}

		void and_(bool canAssign) {
			int endJump = emitJump(OpCode::JUMP_IF_FALSE);

			emitByte(OpCode::POP);
			parsePrecedence(Precedence::AND);

			patchJump(endJump);
		}

		void or_(bool canAssign) {
			int elseJump = emitJump(OpCode::JUMP_IF_FALSE);
			int endJump = emitJump(OpCode::JUMP);

			patchJump(elseJump);
			emitByte(OpCode::POP);

			parsePrecedence(Precedence::OR);
			patchJump(endJump);
		}

		void forStatement() {
			beginScope();
			consume(TokenType::left_paren, "Expect '(' after 'for'.");

			if (match(TokenType::semicolon)) { // consume(TokenType::semicolon, "Expect ';'.");
				// No initializer.
			}
			else if (match(TokenType::var)) {
				varDeclaration();
			}
			else {
				expressionStatement();
			}

			auto surroundingLoopStart = innermostLoopStart;
			auto surroundingLoopScopeDepth = innermostLoopScopeDepth;
			auto surroundingLoopSelect = innermostLoopSelect;
			innermostLoopStart = compilingChunk.code.size();
			innermostLoopScopeDepth = compiler.scopeDepth;

			int exitJump = -1;
			if (!match(TokenType::semicolon)) {
				expression();
				consume(TokenType::semicolon, "Expect ';' after loop condition.");
				exitJump = emitJump(OpCode::JUMP_IF_FALSE); // Jump out of the loop if the condition is false.
				innermostLoopSelect = exitJump - 1;
				emitByte(OpCode::POP); // Condition.
			}

			if (!match(TokenType::right_paren)) {
				int bodyJump = emitJump(OpCode::JUMP);
				std::size_t incrementStart = compilingChunk.code.size();
				expression();
				emitByte(OpCode::POP);
				consume(TokenType::right_paren, "Expect ')' after for clauses.");

				emitLoop(innermostLoopStart);
				innermostLoopStart = incrementStart;
				patchJump(bodyJump);
			}

			statement();

			emitLoop(innermostLoopStart);

			if (exitJump != -1) {
				patchJump(exitJump);
				emitByte(OpCode::POP);	// Condition.
			}

			innermostLoopStart = surroundingLoopStart;
			innermostLoopScopeDepth = surroundingLoopScopeDepth;
			innermostLoopSelect = surroundingLoopStart;
			endScope();
		}

		void continueStatement() {
			if (innermostLoopStart == -1) {
				error("Can't use 'continue' outside of a loop");
			}
			consume(TokenType::semicolon, "Expect ';' after 'continue'.");
			for (auto i = compiler.localCount - 1;
				i >= 0 && compiler.locals[i].depth > innermostLoopScopeDepth;
				--i) {
				emitByte(OpCode::POP);
			}
			emitLoop(innermostLoopStart); // Jump to top of current innermost loop.
		}

		void breakStatement() {
			if (innermostLoopStart == -1) {
				error("Can't use 'break' outside of a loop");
			}
			consume(TokenType::semicolon, "Expect ';' after 'break'.");
			for (auto i = compiler.localCount - 1;
				i >= 0 && compiler.locals[i].depth > innermostLoopScopeDepth;
				--i) {
				emitByte(OpCode::POP);
			}
			if (innermostLoopSelect > 0) {
				emitByte(OpCode::FALSE);
				emitLoop(innermostLoopSelect);
			}
		}

		void whileStatement() {
			auto surroundingLoopStart = innermostLoopStart;
			auto surroundingLoopScopeDepth = innermostLoopScopeDepth;
			auto surroundingLoopSelect = innermostLoopSelect;
			innermostLoopStart = compilingChunk.code.size();
			innermostLoopScopeDepth = compiler.scopeDepth;

			consume(TokenType::left_paren, "Expect '(' after 'while'.");
			expression();
			consume(TokenType::right_paren, "Expect ')' after condition.");

			int exitJump = emitJump(OpCode::JUMP_IF_FALSE);
			innermostLoopSelect = exitJump - 1;
			emitByte(OpCode::POP);
			statement();

			emitLoop(innermostLoopStart);

			patchJump(exitJump);
			emitByte(OpCode::POP);

			innermostLoopStart = surroundingLoopStart;
			innermostLoopScopeDepth = surroundingLoopScopeDepth;
			innermostLoopSelect = surroundingLoopStart;
		}

		void emitLoop(std::size_t loopStart) {
			emitByte(OpCode::LOOP);

			std::size_t offset = compilingChunk.code.size() - loopStart + 1;
			if (offset > std::numeric_limits<std::size_t>::max()) error("Loop body too large.");

			emitByte(offset);
		}

		void ifStatement() {
			consume(TokenType::left_paren, "Expect '(' after 'if'.");
			expression();
			consume(TokenType::right_paren, "Expect ')' after condition.");
			int thenJump = emitJump(OpCode::JUMP_IF_FALSE);
			emitByte(OpCode::POP);
			statement();
			int elseJump = emitJump(OpCode::JUMP);
			patchJump(thenJump);
			emitByte(OpCode::POP);
			if (match(TokenType::elif)) ifStatement();
			if (match(TokenType::kw_else)) statement();
			patchJump(elseJump);
		}

		int emitJump(const code_t& instruction) {
			emitByte(instruction);
			emitByte(std::numeric_limits<std::size_t>::max());
			//emitByte(0xff);
			return compilingChunk.code.size() - 1;
		}

		void patchJump(int offset) {
			// -2 to adjust for the bytecode for the jump offset itself.
			int jump = compilingChunk.code.size() - offset - 1;

			/*/if (jump > UINT16_MAX) {
				error("Too much code to jump over.");
			}*/

			compilingChunk.code[offset] = jump;
		}

		void endScope() {
			compiler.scopeDepth--;

			while (compiler.localCount > 0 &&
				compiler.locals[compiler.localCount - 1].depth >
				compiler.scopeDepth) {
				emitByte(OpCode::POP);
				compiler.localCount--;
			}

		}

		void beginScope() {
			compiler.scopeDepth++;
		}

		void block() {
			while (!check(TokenType::right_brace) && !check(TokenType::eof)) {
				declaration();
			}

			consume(TokenType::right_brace, "Expect '}' after block.");
		}

		void expressionStatement() {
			expression();
			consume(TokenType::semicolon, "Expect ';' after value.");
			emitByte(OpCode::POP);
		}

		template<class ...Ts, std::enable_if_t<(... && std::is_convertible_v<Ts, TokenType>), int> = 0>
		bool match(const Ts...ts) {
			if ((check(ts) || ...)) {
				advance();
				return true;
			}
			return false;
		}

		bool check(TokenType type) {
			return parser.current.type == type;
		}

		void printStatement() {
			expression();
			consume(TokenType::semicolon, "Expect ';' after value.");
			emitByte(OpCode::PRINT);
		}

		bool compile(const char* source) {
			scanner = Scanner{ source };
			advance();
			while (!match(TokenType::eof)) {
				declaration();
			}
			/*expression();
			consume(TokenType::eof, "Expect end of expression.");*/
			endCompiler();
			return !parser.hadError;
		}

		void endCompiler() {
			emitReturn();
		}

		void emitReturn() {
			emitByte(OpCode::RETURN);
		}

		template<class ...Ts, std::enable_if_t<(... && std::is_convertible_v<Ts, code_t>), int> = 0>
		void emitBytes(const Ts & ...ts) {
			(emitByte(ts), ...);
			///emitByte(byte2);
		}

		void emitByte(const code_t& byte) {
			if (std::holds_alternative<OpCode>(byte)) previousOp = compilingChunk.code.size();
			compilingChunk.write(byte, parser.previous.line);
		}

		void push(const value_t& value) {
			++stackTop;
			if (stackTop > stack.size())
				stack.emplace_back(value);
			else
				stack[stackTop - 1] = value;
		}

		value_t pop() {
			return stack[--stackTop];
		}

		auto interpret() {
			return run(compilingChunk);
		}

		value_t peek(int distance) {
			return stack[stackTop - 1 - distance];
		}

		template<class ... Ts, std::enable_if_t<(... && std::is_convertible_v<Ts, const char*>), int> = 0>
		void runtimeError(const char* format, Ts ... args) {
			std::cerr << string_format(format, args...);
			std::cerr << "\n";
			size_t instruction = ip - 1;
			int line = compilingChunk.lines[instruction];
			std::cerr << string_format("[line %d] in script\n", line);
			resetStack();
		}

		void resetStack() {
			stackTop = 0;
		}

		bool isFalsey(const value_t& value) {
			return std::holds_alternative<std::nullptr_t>(value) || (std::holds_alternative<bool>(value) && !std::get<bool>(value));
		}

		bool valuesEqual(const value_t& a, const value_t& b) {
			if (a.index() != b.index()) return false;
			if (std::holds_alternative<bool>(a)) return std::get<bool>(a) == std::get<bool>(b);
			else if (std::holds_alternative<std::nullptr_t>(a)) return true;
			else if (std::holds_alternative<double>(a)) return std::get<double>(a) == std::get<double>(b);
			else return false;		// Unreachable.
		}

		interpretResult run(Chunk& chunk) {
#define DEBUG_TRACE_EXECUTION 1
#define ONLYPRINTCODE 0
#define BINARY_OP(op) \
do { \
if(!std::holds_alternative<double>(peek(0)) || !std::holds_alternative<double>(peek(1))){\
runtimeError("Operands must be numbers");\
return interpretResult::RUNTIME_ERROR;\
}\
double b = std::get<double>(pop()); \
double a = std::get<double>(pop()); \
push(a op b); \
}while (false)

			for (;;) {
#if ONLYPRINTCODE
				ip = Debug::disassembleInstruction(chunk, ip);
				if (ip + 1 >= compilingChunk.code.size()) {
					return interpretResult::OK;
				}
				continue;
#else

#if DEBUG_TRACE_EXECUTION
				std::cout << "          ";
				for (std::size_t index = 0; index < stackTop; ++index) {
					std::cout << "[" << stack[index] << "]";
				}
				std::cout << "\n";
				Debug::disassembleInstruction(chunk, ip);
#endif
				auto instruction = chunk.code[ip++];
				if (std::holds_alternative<OpCode>(instruction))
					switch (std::get<OpCode>(instruction)) {
					case OpCode::CONSTANT: {
						auto constant = chunk.values[std::get<std::size_t>(chunk.code[ip++])];
						push(constant);
						break;
					}
					case OpCode::NIL: push(nullptr); break;
					case OpCode::TRUE: push(true); break;
					case OpCode::FALSE: push(false); break;
					case OpCode::DEFINE_GLOBAL: {
						auto name = std::string_view{
							std::get<std::string>(chunk.values[std::get<std::size_t>(chunk.code[ip++])])
						};
						globals[name] = peek(0);
						pop();
						break;
					}
					case OpCode::POP: pop(); break;
					case OpCode::GET_LOCAL: {
						auto slot = std::get<std::size_t>(chunk.code[ip++]);
						push(stack[slot]);
						break;
					}
					case OpCode::SET_LOCAL: {
						auto slot = std::get<std::size_t>(chunk.code[ip++]);
						stack[slot] = peek(0);
						break;
					}
					case OpCode::GET_GLOBAL: {
						auto name = std::string_view{
							std::get<std::string>(chunk.values[std::get<std::size_t>(chunk.code[ip++])])
						};
						auto value = globals.find(name);
						if (value == globals.end()) {
							runtimeError("Undefined variable '%s'.", name.data());
							return interpretResult::RUNTIME_ERROR;
						}
						push(value->second);
						break;
					}
					case OpCode::SET_GLOBAL: {
						auto name = std::string_view{
						std::get<std::string>(chunk.values[std::get<std::size_t>(chunk.code[ip++])])
						};
						auto value = globals.find(name);
						if (value == globals.end()) {
							runtimeError("Undefined variable '%s'.", name.data());
							return interpretResult::RUNTIME_ERROR;
						}
						else {
							globals[name] = peek(0);
						}
						break;
					}
					case OpCode::EQUAL: {
						auto b = pop();
						auto a = pop();
						push(valuesEqual(a, b));
						break;
					}
					case OpCode::GREATER:  BINARY_OP(> ); break;
					case OpCode::LESS:     BINARY_OP(< ); break;
					case OpCode::ADD: {
						if (std::holds_alternative<std::string>(peek(0)) || std::holds_alternative<std::string>(peek(1))) {
							push(std::string{ std::to_string(pop()) + std::to_string(pop()) });
						}
						else { BINARY_OP(+); }
						break;
					}
					case OpCode::SUBTRACT: BINARY_OP(-); break;
					case OpCode::MULTIPLY: BINARY_OP(*); break;
					case OpCode::DIVIDE:   BINARY_OP(/ ); break;
					case OpCode::NOT: push(isFalsey(pop())); break;
					case OpCode::NEGATE:
						if (!std::holds_alternative<double>(peek(0))) {
							runtimeError("Operand must be a number.");
							return interpretResult::RUNTIME_ERROR;
						}
						push(-std::get<double>(pop())); break;
					case OpCode::PRINT: {
						std::cout << pop() << "\n";
						break;
					}
					case OpCode::JUMP: {
						auto offset = std::get<std::size_t>(chunk.code[ip++]);
						ip += offset;
						break;
					}
					case OpCode::JUMP_IF_FALSE: {
						auto offset = std::get<std::size_t>(chunk.code[ip++]);
						if (isFalsey(peek(0))) ip += offset;
						break;
					}
					case OpCode::LOOP: {
						auto offset = std::get<std::size_t>(chunk.code[ip++]);
						ip -= offset;
						break;
					}
					case OpCode::RETURN: {
						//	std::cout << pop() << "\n";
						return interpretResult::OK;
					}
					}
#endif
			}
#undef BINARY_OP
#undef DEBUG_TRACE_EXECUTION
#undef ONLYPRINTCODE
		}
	};
}
