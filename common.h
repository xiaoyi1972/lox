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
		bool isCaptured;
		Local(const Token& _name, int _depth, bool _isCaptured = false) :
			name(_name), depth(_depth), isCaptured(_isCaptured) {}
	};

	struct CallFrame {
		Closure* closure = nullptr;
		std::size_t  ip = 0;
		std::size_t  slots = 0;
	};


	struct Compiler {
		Compiler* enclosing;
		std::shared_ptr<Function> func;
		FunctionType type;
		std::vector<Local> locals;
		int localCount;
		static constexpr std::size_t COUNT_MAX = 100;
		std::array<Upvalue, COUNT_MAX > upvalues;
		int scopeDepth;
		Compiler(Compiler* previous = nullptr) :enclosing(previous),
			func(new Function), type(FunctionType::SCRIPT), localCount(0), scopeDepth(0) {}
	};

	struct VM {
		enum struct interpretResult {
			OK, COMPILE_ERROR, RUNTIME_ERROR
		};

		struct { int start = -1; int scopeDepth = 0; int select = 0; } innermostLoop;
		static constexpr std::size_t FRAMES_MAX = 64;
		std::array<CallFrame, FRAMES_MAX> frames{};
		int frameCount = 0;
		Stack stack;
		Parser parser;
		Scanner scanner{ nullptr };
		std::unordered_map<std::string_view, value_t> globals;
		Compiler rootCompiler;
		Compiler* current = &rootCompiler;
		rule_t<VM>  rules;
		std::optional<size_t> previousOp;
		std::shared_ptr<ObjUpvalue> openUpvalues = nullptr;

		VM() :rules(ParseRuleHelp<VM>(allTokenType{})) {}

		Chunk& currentChunk() {
			return current->func->chunk;
		}

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
				match(TokenType::equal, TokenType::plus_equal, TokenType::minus_equal,
					TokenType::star_equal, TokenType::slash_equal)) {
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
			emitConstant(std::atof(parser.previous.lexem.data()));
		}

		void grouping(bool canAssign) {
			expression();
			consume(TokenType::right_paren, "Expect ')' after expression.");
		}

		void binary(bool canAssign) {
			TokenType operatorType = parser.previous.type;
			auto& rule = getRule(operatorType);

			if (operatorType == TokenType::inc || operatorType == TokenType::dec) {
				if (previousOp) {
					auto get = std::get<OpCode>(currentChunk().code[*previousOp]);
					bool isVariable = (get == OpCode::GET_GLOBAL || get == OpCode::GET_LOCAL);
					if (isVariable) {
						emitBytes(OpCode::INC, 0 + (operatorType == TokenType::dec ? 1 : 0));
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

		}

		void unary(bool canAssign) {
			TokenType operatorType = parser.previous.type;

			//expression;	// Compile the operand.
			parsePrecedence(Precedence::UNARY);

			switch (operatorType) { // Emit the operator instruction.
			case TokenType::bang: emitByte(OpCode::NOT); break;
			case TokenType::minus: emitByte(OpCode::NEGATE); break;
			case TokenType::inc:
			case TokenType::dec:
				if (previousOp) {
					auto get = std::get<OpCode>(currentChunk().code[*previousOp]);
					bool isVariable = (get == OpCode::GET_GLOBAL || get == OpCode::GET_LOCAL);
					if (isVariable) {
						emitBytes(OpCode::INC, 2 + (operatorType == TokenType::dec ? 1 : 0));
					}
					else error("Invalid assignment target.");
				}
				break;
			default: return; // Unreachable.
			}
		}

		code_t makeConstant(const value_t& value) {
			auto constant = currentChunk().addConstant(value);
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

		void funDeclaration() {
			auto global = parseVariable("Expect function name.");
			markInitialized();
			function(FunctionType::FUNCTION);
			defineVariable(global);
		}

		void call(bool canAssign) {
			auto argCount = argumentList();
			emitBytes(OpCode::CALL, argCount);
		}

		bool call(Closure* closure, int argCount) {
			auto func = closure->function;
			if (argCount != func->arity) {
				runtimeError("Expected %d arguments but got %d.", func->arity, argCount);
				return false;
			}

			if (frameCount == FRAMES_MAX) {
				runtimeError("Stack overflow.");
				return false;
			}

			CallFrame* frame = &frames[frameCount++];
			frame->closure = closure;
			frame->ip = 0;
			frame->slots = stack.top - argCount;
			return true;
		}

		bool callValue(value_t& callee, int argCount) {
			if (std::holds_alternative<Object>(callee)) {
				auto& obj = std::get<Object>(callee);
				if (std::holds_alternative<std::shared_ptr<Closure>>(obj)) {
					return call(std::get<std::shared_ptr<Closure>>(obj).get(), argCount);
				}
				else if (std::holds_alternative<std::shared_ptr<NativeFn>>(obj)) {
				auto nativeFunc = std::get<std::shared_ptr<NativeFn>>(obj).get();
				if (argCount != nativeFunc->arity) {
					runtimeError("Expected %d arguments but got %d.", nativeFunc->arity, argCount);
					return false;
				}
				auto result = nativeFunc->func(stack, stack.top - nativeFunc->arity);
				stack.top -= argCount + 1;
				if (result.has_value()) stack.push(*result);
				return true;
			}
			}
			runtimeError("Can only call functions and classes.");
			return false;
		}

		template<class ...Ts, std::enable_if_t<(... && std::is_convertible_v<Ts, const NativeFn&>), int> = 0>
		void defineNative(const Ts & ...function) {
			auto callee = [this](auto&& arg) {globals[arg.name] = std::make_shared<NativeFn>(arg); };
			(callee(function), ...);
		}

		std::size_t argumentList() {
			std::size_t argCount = 0;
			if (!check(TokenType::right_paren)) {
				do {
					expression();
					if (argCount == 255) {
						error("Can't have more than 255 arguments.");
					}
					argCount++;
				} while (match(TokenType::comma));
			}
			consume(TokenType::right_paren, "Expect ')' after arguments.");
			return argCount;
		}

		void function(FunctionType type) {
			Compiler compiler(current);
			current = &compiler;
			compiler.type = type;
			if (type != FunctionType::SCRIPT) {
				current->func->name = parser.previous.lexem;
			}

			beginScope();

			consume(TokenType::left_paren, "Expect '(' after function name.");
			if (!check(TokenType::right_paren)) {
				do {
					current->func->arity++;
					if (current->func->arity > 255) {
						errorAtCurrent("Can't have more than 255 parameters.");
					}
					auto constant = parseVariable("Expect parameter name.");
					defineVariable(constant);
				} while (match(TokenType::comma));
			}
			consume(TokenType::right_paren, "Expect ')' after parameters.");
			consume(TokenType::left_brace, "Expect '{' before function body.");
			block();

			endCompiler();

			emitBytes(OpCode::CLOSURE, makeConstant(compiler.func));

			for (int i = 0; i < compiler.func->upvalueCount; i++) {
				emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
				emitByte(compiler.upvalues[i].index);
			}
		}

		void declaration() {
			if (match(TokenType::fun)) {
				funDeclaration();
			}
			else	if (match(TokenType::var)) {
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
			if (current->scopeDepth > 0) return 0;
			return identifierConstant(parser.previous);
		}

		void declareVariable() {
			if (current->scopeDepth == 0) return;
			auto& name = parser.previous;
			for (int i = current->localCount - 1; i >= 0; i--) {
				auto& local = current->locals[i];
				if (local.depth != -1 && local.depth < current->scopeDepth) {
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
#ifdef LIMIT_LOCAL_VARIABLE_COUNT
			if (current->localCount == 50) {
				error("Too many local variables in function.");
				return;
			}
			auto& local = current->locals[current->localCount++];
#endif

			current->locals.emplace_back(name, -1);
			current->localCount++;

#ifdef LIMIT_LOCAL_VARIABLE_COUNT
			local.name = name;
			local.depth = -1;
			local.depth = current->scopeDepth;
#endif
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
			if (current->scopeDepth > 0) {
				markInitialized();
				return;
			}
			emitBytes(OpCode::DEFINE_GLOBAL, global);
		}

		void markInitialized() {
			if (current->scopeDepth == 0) return;
			current->locals[current->localCount - 1].depth = current->scopeDepth;
		}

		void variable(bool canAssign) {
			namedVariable(parser.previous, canAssign);
		}

		int resolveLocal(Compiler* compiler, const Token& name) {
			for (int i = compiler->localCount - 1; i >= 0; i--) {
				auto& local = compiler->locals[i];
				if (name.lexem == local.name.lexem) {
					if (local.depth == -1) {
						error("Can't read local variable in its own initializer.");
					}
					return i;
				}
			}
			return -1;
		}

		int resolveUpvalue(Compiler* compiler, const Token& name) {
			if (compiler->enclosing == nullptr) 
				return -1;
			int local = resolveLocal(compiler->enclosing, name);
			if (local != -1) {
				compiler->enclosing->locals[local].isCaptured = true;
				return addUpvalue(compiler, local, true);
			}
			int upvalue = resolveUpvalue(compiler->enclosing, name);
			if (upvalue != -1) {
				return addUpvalue(compiler, upvalue, false);
			}
			return -1;
		}

		int addUpvalue(Compiler* compiler, std::size_t index, bool isLocal) {
			int upvalueCount = compiler->func->upvalueCount;
			for (int i = 0; i < upvalueCount; i++) {
				Upvalue* upvalue = &compiler->upvalues[i];
				if (upvalue->index == index && upvalue->isLocal == isLocal) {
					return i;
				}
			}

			if (upvalueCount == Compiler::COUNT_MAX) {
				error("Too many closure variables in function.");
				return 0;
			}

			compiler->upvalues[upvalueCount].isLocal = isLocal;
			compiler->upvalues[upvalueCount].index = index;
			return compiler->func->upvalueCount++;
		}

		std::shared_ptr<ObjUpvalue> captureUpvalue(std::size_t local) {
			std::shared_ptr<ObjUpvalue> prevUpvalue = nullptr;
			auto upvalue = openUpvalues;
			while (upvalue != nullptr && upvalue->location > local) {
				prevUpvalue = upvalue;
				upvalue = upvalue->next;
			}

			if (upvalue != nullptr && upvalue->location == local) {
				return upvalue;
			}

			auto createdUpvalue = std::make_shared<ObjUpvalue>(local);
			createdUpvalue->next = upvalue;

			if (prevUpvalue == nullptr) {
			 openUpvalues = createdUpvalue;
			}
			else {
				prevUpvalue->next = createdUpvalue;
			}

			return createdUpvalue;
		}

		void closeUpvalues(std::size_t  last) {
			while (openUpvalues != nullptr &&
				openUpvalues->location >= last) {
				auto upvalue = openUpvalues;
				upvalue->closed = stack[upvalue->location];
				openUpvalues = upvalue->next;
			}
		}

		void namedVariable(const Token& name, bool canAssign) {
			code_t arg; // = identifierConstant(name);
			code_t getOp, setOp;
			auto localIndex = resolveLocal(current,name);
			if (localIndex != -1) {
				arg = static_cast<std::size_t>(localIndex);
				getOp = OpCode::GET_LOCAL, setOp = OpCode::SET_LOCAL;
			}
			else if ((localIndex = resolveUpvalue(current, name)) != -1) {
				arg = static_cast<std::size_t>(localIndex);
				getOp = OpCode::GET_UPVALUE, setOp = OpCode::SET_UPVALUE;
			}
			else {
				arg = identifierConstant(name);
				getOp = OpCode::GET_GLOBAL, setOp = OpCode::SET_GLOBAL;
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
			else {
				emitBytes(getOp, arg);
			}
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
			else if (match(TokenType::kw_return)) {
				returnStatement();
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

		void returnStatement() {
			if (current->type == FunctionType::SCRIPT) {
				error("Can't return from top-level code.");
			}
			if (match(TokenType::semicolon)) {
				emitReturn();
			}
			else {
				expression();
				consume(TokenType::semicolon, "Expect ';' after return value.");
				emitByte(OpCode::RETURN);
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

			if (match(TokenType::semicolon)) {
				// consume(TokenType::semicolon, "Expect ';'.");
				// No initializer.
			}
			else if (match(TokenType::var)) { varDeclaration(); }
			else { expressionStatement(); }

			decltype (innermostLoop) surroundingLoop;
			std::memcpy(&surroundingLoop, &innermostLoop, sizeof innermostLoop);
			innermostLoop.start = currentChunk().code.size();
			innermostLoop.scopeDepth = current->scopeDepth;

			int exitJump = -1;
			if (!match(TokenType::semicolon)) {
				expression();
				consume(TokenType::semicolon, "Expect ';' after loop condition.");
				exitJump = emitJump(OpCode::JUMP_IF_FALSE); // Jump out of the loop if the condition is false.
				innermostLoop.select = exitJump - 1;
				emitByte(OpCode::POP); // Condition.
			}

			if (!match(TokenType::right_paren)) {
				int bodyJump = emitJump(OpCode::JUMP);
				std::size_t incrementStart = currentChunk().code.size();
				expression();
				emitByte(OpCode::POP);
				consume(TokenType::right_paren, "Expect ')' after for clauses.");

				emitLoop(innermostLoop.start);
				innermostLoop.start = incrementStart;
				patchJump(bodyJump);
			}

			statement();

			emitLoop(innermostLoop.start);

			if (exitJump != -1) {
				patchJump(exitJump);
				emitByte(OpCode::POP); // Condition.
			}

			std::memcpy(&innermostLoop, &surroundingLoop, sizeof innermostLoop);
			endScope();
		}

		void continueStatement() {
			if (innermostLoop.start == -1) {
				error("Can't use 'continue' outside of a loop");
			}
			consume(TokenType::semicolon, "Expect ';' after 'continue'.");
			for (auto i = current->localCount - 1;
				i >= 0 && current->locals[i].depth > innermostLoop.scopeDepth;
				--i) {
				emitByte(OpCode::POP);
			}
			emitLoop(innermostLoop.start); // Jump to top of current innermost loop.
		}

		void breakStatement() {
			if (innermostLoop.start == -1) {
				error("Can't use 'break' outside of a loop");
			}
			consume(TokenType::semicolon, "Expect ';' after 'break'.");
			for (auto i = current->localCount - 1;
				i >= 0 && current->locals[i].depth > innermostLoop.scopeDepth;
				--i) {
				emitByte(OpCode::POP);
			}

			if (innermostLoop.select > 0) {
				emitByte(OpCode::FALSE);
				emitLoop(innermostLoop.select);
			}
		}

		void whileStatement() {

			decltype (innermostLoop) surroundingLoop;
			std::memcpy(&surroundingLoop, &innermostLoop, sizeof innermostLoop);
			innermostLoop.start = currentChunk().code.size();
			innermostLoop.scopeDepth = current->scopeDepth;

			consume(TokenType::left_paren, "Expect '(' after 'while'.");
			expression();
			consume(TokenType::right_paren, "Expect ')' after condition.");

			int exitJump = emitJump(OpCode::JUMP_IF_FALSE);
			innermostLoop.select = exitJump - 1;
			emitByte(OpCode::POP);
			statement();

			emitLoop(innermostLoop.start);

			patchJump(exitJump);
			emitByte(OpCode::POP);

			std::memcpy(&surroundingLoop, &innermostLoop, sizeof innermostLoop);
		}

		void emitLoop(std::size_t loopStart) {
			emitByte(OpCode::LOOP);

			std::size_t offset = currentChunk().code.size() - loopStart + 1;
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
			return currentChunk().code.size() - 1;
		}

		void patchJump(int offset) {
			// -2 to adjust for the bytecode for the jump offset itself.
			int jump = currentChunk().code.size() - offset - 1;
#ifdef OVER_MAX_JUMP_DISTANCE
			/ if (jump > UINT16_MAX) {
				error("Too much code to jump over.");
			}
#endif
			currentChunk().code[offset] = jump;
		}

		void endScope() {
			current->scopeDepth--;

			while (current->localCount > 0 &&
				current->locals[current->localCount - 1].depth >
				current->scopeDepth) {
				if (current->locals[current->localCount - 1].isCaptured) {
					emitByte(OpCode::CLOSE_UPVALUE);
				}
				else {
					emitByte(OpCode::POP);
				}
				//emitByte(OpCode::POP);
				current->localCount--;
			}

		}

		void beginScope() {
			current->scopeDepth++;
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

		std::optional<std::shared_ptr<Function>> compile(const char* source) {
			scanner = Scanner{ source };
			advance();
			while (!match(TokenType::eof)) {
				declaration();
			}
			/*expression();
			consume(TokenType::eof, "Expect end of expression.");*/
			auto func = std::move(endCompiler());
			return parser.hadError ? std::optional<std::shared_ptr<Function>>(std::nullopt) : func;
		}

		std::shared_ptr<Function> endCompiler() {
			emitReturn();
			auto func = current->func;
#ifdef DEBUG_PRINT_CODE
			if (!parser.hadError) {
				disassembleChunk(currentChunk(), !func->name.empty()
					? func->name : "<script>");
			}
#endif
			if (current->enclosing != nullptr)
			current = current->enclosing;
			return func;
		}

		void emitReturn() {
			emitByte(OpCode::NIL);
			emitByte(OpCode::RETURN);
		}

		template<class ...Ts, std::enable_if_t<(... && std::is_convertible_v<Ts, code_t>), int> = 0>
		void emitBytes(const Ts & ...ts) {
			(emitByte(ts), ...);
		}

		void emitByte(const code_t& byte) {
			if (std::holds_alternative<OpCode>(byte)) previousOp = currentChunk().code.size();
			currentChunk().write(byte, parser.previous.line);
		}

		void push(const value_t& value) {
			stack.push(value);
		}

		value_t pop() {
			return stack.pop();
		}

		interpretResult  interpret(const char* source) {
			auto func = compile(source);
			if (!func.has_value()) return interpretResult::COMPILE_ERROR;
			push(current->func);
			auto closure = std::make_shared<Closure>(*func);
			pop();
			push(closure);
			call(closure.get(), 0);
			return run();
		}

		value_t& peek(int distance) {
			return stack[stack.top - 1 - distance];
		}

		template<class ... Ts>
		void runtimeError(const char* format, Ts ... args) {
			std::cerr << string_format(format, args...) << "\n";
			for (auto i = frameCount - 1; i >= 0; i--) {
				CallFrame* frame = &frames[i];
				auto func = frame->closure->function;
				size_t instruction = frame->ip - 1;
				int line = func->chunk.lines[instruction];
				std::cerr << string_format("[line %d] in %s\n", line,
					func->name.empty() ? "script" : std::string(func->name).c_str());
			}
			resetStack();
		}

		void resetStack() {
			stack.reset();
			frameCount = 0;
		}

		bool isFalsey(const value_t& value) {
			return std::holds_alternative<std::nullptr_t>(value) || (std::holds_alternative<bool>(value) && !std::get<bool>(value));
		}

		bool valuesEqual(const value_t& a, const value_t& b) {
			if (a.index() != b.index()) return false;
			if (std::holds_alternative<bool>(a)) return std::get<bool>(a) == std::get<bool>(b);
			else if (std::holds_alternative<std::nullptr_t>(a)) return true;
			else if (std::holds_alternative<num>(a)) return std::get<num>(a) == std::get<num>(b);
			else return false;		// Unreachable.
		}

		interpretResult run() {
			CallFrame* frame = &frames[frameCount - 1];
			auto chunk = [&]()->Chunk& {return frame->closure->function->chunk; };
			auto read_byte = [&]() {return std::get<std::size_t>(chunk().code[frame->ip++]); };
			value_t* var_ptr = nullptr;
#define DEBUG_TRACE_EXECUTION 1
#define ONLYPRINTCODE 0
#define BINARY_OP(op) \
do { \
if(!std::holds_alternative<num>(peek(0)) || !std::holds_alternative<num>(peek(1))){\
runtimeError("Operands must be numbers");\
return interpretResult::RUNTIME_ERROR;\
}\
num b = std::get<num>(pop()); \
num a = std::get<num>(pop()); \
push(a op b); \
}while (false)

			for (;;) {
#if ONLYPRINTCODE
				frame->ip = Debug::disassembleInstruction(chunk(), frame->ip);
				if (frame->ip + 1 > currentChunk().code.size()) {
					return interpretResult::OK;
				}
				continue;
#else

#if DEBUG_TRACE_EXECUTION
				std::cout << "\n          ";
				for (std::size_t index = 0; index < stack.top; ++index) {
					std::cout << "|" << stack[index] << "|";
				}
				std::cout << "\n";
				Debug::disassembleInstruction(chunk(), frame->ip);
#endif
				auto instruction = chunk().code[frame->ip++];
				if (std::holds_alternative<OpCode>(instruction))
					switch (std::get<OpCode>(instruction)) {
					case OpCode::CONSTANT: {
						auto constant = chunk().values[read_byte()];
						push(constant);
						break;
					}
					case OpCode::NIL: push(nullptr); break;
					case OpCode::TRUE: push(true); break;
					case OpCode::FALSE: push(false); break;
					case OpCode::POP: pop(); break;
					case OpCode::INC: {
						if (var_ptr != nullptr) {
							auto arg = read_byte();
							int prefix = (arg & 2) >> 1, neg = arg & 1 ? -1 : 1;
							auto& top = peek(0);
							if (std::holds_alternative<num>(top) && std::holds_alternative<num>(*var_ptr)) {
								std::get<num>(*var_ptr) += neg;
								if (prefix) std::get<num>(top) += neg;
							}
						}
						break;
					}
					case OpCode::GET_LOCAL: {
						auto slot = read_byte();
						var_ptr = &stack[frame->slots + slot];
						push(stack[frame->slots + slot]);
						break;
					}
					case OpCode::SET_LOCAL: {
						auto slot = read_byte();
						stack[frame->slots + slot] = peek(0);
						break;
					}
					case OpCode::DEFINE_GLOBAL: {
						auto name = std::string_view{ std::get<std::string>(chunk().values[read_byte()]) };
						globals[name] = peek(0);
						pop();
						break;
					}
					case OpCode::GET_GLOBAL: {
						auto name = std::string_view{ std::get<std::string>(chunk().values[read_byte()]) };
						auto value = globals.find(name);
						if (value == globals.end()) {
							runtimeError("Undefined variable '%s'.", name.data());
							return interpretResult::RUNTIME_ERROR;
						}
						var_ptr = &value->second;
						push(value->second);
						break;
					}
					case OpCode::SET_GLOBAL: {
						auto name = std::string_view{ std::get<std::string>(chunk().values[read_byte()]) };
						auto value = globals.find(name);
						if (value == globals.end()) {
							runtimeError("Undefined variable '%s'.", name.data());
							return interpretResult::RUNTIME_ERROR;
						}
						else globals[name] = peek(0);
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
						if (!std::holds_alternative<num>(peek(0))) {
							runtimeError("Operand must be a number.");
							return interpretResult::RUNTIME_ERROR;
						}
						push(-std::get<num>(pop())); break;
					case OpCode::PRINT: {
						std::cout << pop() << "\n";
						break;
					}
					case OpCode::JUMP: {
						auto offset = read_byte();
						frame->ip += offset;
						break;
					}
					case OpCode::JUMP_IF_FALSE: {
						auto offset = read_byte();
						if (isFalsey(peek(0))) frame->ip += offset;
						break;
					}
					case OpCode::LOOP: {
						auto offset = read_byte();
						frame->ip -= offset;
						break;
					}
					case OpCode::CALL: {
						auto argCount = read_byte();
						if (!callValue(peek(argCount), argCount)) {
							return  interpretResult::RUNTIME_ERROR;
						}
						frame = &frames[frameCount - 1];
						break;
					}
					case OpCode::SET_UPVALUE: {
						auto slot = read_byte();
						if(std::holds_alternative<std::nullptr_t>(frame->closure->upvalues[slot]->closed))
						stack[frame->closure->upvalues[slot]->location] = peek(0);
						else frame->closure->upvalues[slot]->closed = peek(0);
						break;
					}
					case OpCode::GET_UPVALUE: {
						auto slot = read_byte();
						if (std::holds_alternative<std::nullptr_t>(frame->closure->upvalues[slot]->closed))
						push(stack[frame->closure->upvalues[slot]->location]);
						else push(frame->closure->upvalues[slot]->closed);
						break;
					}
					case OpCode::CLOSE_UPVALUE: {
						closeUpvalues(stack.top - 1);
						pop();
						break;
					}
					case OpCode::CLOSURE: {
						auto function = std::get<std::shared_ptr<Function>>(std::get<Object>(chunk().values[read_byte()]));
						auto closure = std::make_shared<Closure>(function);
						for (int i = 0; i < closure->upvalueCount; i++) {
							auto isLocal = read_byte();
							auto index = read_byte();
							if (isLocal) {
								closure->upvalues[i] = captureUpvalue(frame->slots + index);
							}
							else {
								closure->upvalues[i] = frame->closure->upvalues[index];
							}
						}
						push(closure);
						break;
					}
					case OpCode::RETURN: {
						auto result = pop();
						closeUpvalues(frame->slots);
						frameCount--;
						if (frameCount == 0) {
							pop();
							return interpretResult::OK;
						}
						stack.top = frame->slots - 1;
						push(result);
						frame = &frames[frameCount - 1];
						break;
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