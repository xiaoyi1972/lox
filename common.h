#pragma once
#include <algorithm>
#include <array>
#include <optional>
#include <vector>

#include "debug.h"
#include "precedence.h"
#include "scanner.h"

namespace Lox {
	static std::string initString("init");

	struct Local {
		Token name;
		int depth;
		bool isCaptured;
		Local(const Token& _name, int _depth, bool _isCaptured = false)
			: name(_name), depth(_depth), isCaptured(_isCaptured) {}
		Local() : depth(0), isCaptured(false) {}
	};

	struct CallFrame {
		Closure* closure = nullptr;
		std::size_t ip = 0;
		std::size_t slots = 0;
	};

	struct Compiler {
		static constexpr std::size_t COUNT_MAX = 100;
		static constexpr std::size_t FRAMES_MAX = 64;
		Compiler* enclosing;
		std::shared_ptr<Fn> func;
		FunctionType type;
		std::vector<Local> locals;
		std::array<Upvalue, COUNT_MAX> upvalues;
		int localCount, scopeDepth;
		Parser parser;
		Scanner scanner{ nullptr };
		rule_t<Compiler> rules;
		std::optional<size_t> previousOp;

		struct ClassCompiler {
			ClassCompiler* enclosing;
			bool hasSuperclass;
		} *currentClass = nullptr;

		struct LoopCompiler {
			LoopCompiler* enclosing = nullptr;
			int start = 0;
			int scopeDepth = 0;
			int select = 0;
		} *currentLoop = nullptr;

		Compiler() :Compiler(nullptr, FunctionType::SCRIPT) {
			enclosing = this;
		}

		Compiler(Compiler* previous,
			FunctionType _type)
			: enclosing(previous), func(new Fn), type(_type), locals(1),
			localCount(1), scopeDepth(0),
			rules(ParseRuleHelp<Compiler>(allTokenType{})) {
			if (type != FunctionType::FUNCTION)
				locals.front().name.lexeme = "this";
			else
				locals.front().name.lexeme = "";
		}

		Chunk& currentChunk() { return enclosing->func->chunk; }

		auto& getRule(TokenType type) {
			return rules[static_cast<std::size_t>(type)];
		}

		void errorAtCurrent(const char* message) { errorAt(parser.current, message); }

		void error(const char* message) { errorAt(parser.previous, message); }

		void errorAt(const Token& token, const char* message) {
			if (parser.panicMode)
				return;
			parser.panicMode = true;
			std::cerr << string_format("[line %d] Error", token.line);
			if (token.type == TokenType::eof) {
				std::cerr << string_format(" at end");
			}
			else if (token.type == TokenType::error) { // Nothing.
			}
			else {
				std::cerr << string_format(" at '%.*s'", token.lexeme.size(),
					token.lexeme.data());
			}
			std::cerr << string_format(": %s\n", message, token.lexeme.size(),
				token.lexeme.data());
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
				if (parser.current.type != TokenType::error)
					break;
				errorAtCurrent(parser.current.lexeme.data());
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
			case TokenType::kw_false:
				emitByte(OpCode::FALSE);
				break;
			case TokenType::nil:
				emitByte(OpCode::NIL);
				break;
			case TokenType::kw_true:
				emitByte(OpCode::TRUE);
				break;
			default:
				return; // Unreachable.
			}
		}

		void string(bool canAssign) {
			emitConstant(std::string{
				parser.previous.lexeme.substr(1, parser.previous.lexeme.size() - 2) });
		}

		void number(bool canAssign) {
			emitConstant(std::atof(parser.previous.lexeme.data()));
		}

		void grouping(bool canAssign) {
			expression();
			consume(TokenType::right_paren, "Expect ')' after expression.");
		}

		void binary(bool canAssign) {
			TokenType operatorType = parser.previous.type;
			auto& rule = getRule(operatorType);

			if (any_v(operatorType, TokenType::inc, TokenType::dec))
				selfInc<false>(operatorType);
			else
				parsePrecedence((Precedence)(static_cast<int>(rule.precedence) + 1));

			switch (operatorType) {
			case TokenType::plus:
				emitByte(OpCode::ADD);
				break;
			case TokenType::minus:
				emitByte(OpCode::SUBTRACT);
				break;
			case TokenType::star:
				emitByte(OpCode::MULTIPLY);
				break;
			case TokenType::slash:
				emitByte(OpCode::DIVIDE);
				break;
			case TokenType::bang_equal:
				emitBytes(OpCode::EQUAL, OpCode::NOT);
				break;
			case TokenType::equal_equal:
				emitByte(OpCode::EQUAL);
				break;
			case TokenType::greater:
				emitByte(OpCode::GREATER);
				break;
			case TokenType::greater_equal:
				emitBytes(OpCode::LESS, OpCode::NOT);
				break;
			case TokenType::less:
				emitByte(OpCode::LESS);
				break;
			case TokenType::less_equal:
				emitBytes(OpCode::GREATER, OpCode::NOT);
				break;
			default:
				return; // Unreachable.
			}
		}

		void unary(bool canAssign) {
			TokenType operatorType = parser.previous.type;

			// expression;	// Compile the operand.
			parsePrecedence(Precedence::UNARY);

			switch (operatorType) { // Emit the operator instruction.
			case TokenType::bang:
				emitByte(OpCode::NOT);
				break;
			case TokenType::minus:
				emitByte(OpCode::NEGATE);
				break;
			case TokenType::inc:
			case TokenType::dec:
				selfInc(operatorType);
				break;
			default:
				return; // Unreachable.
			}
		}

		template <bool infix = true> void selfInc(TokenType operatorType) {
			if (previousOp) {
				auto get = std::get<OpCode>(currentChunk().code[*previousOp]);
				bool isVariable =
					any_v(get, OpCode::GET_GLOBAL, OpCode::GET_LOCAL,
						OpCode::GET_UPVALUE) ||
					(get == OpCode::INC &&
						std::get<std::size_t>(currentChunk().code[*previousOp + 1]) >> 1);
				if (isVariable) {
					emitBytes(OpCode::INC,
						static_cast<std::size_t>(infix << 1 |
							operatorType == TokenType::dec));
				}
				else
					error("Invalid assignment target.");
			}
		}

		code_t makeConstant(const value_t& value) {
			auto constant = currentChunk().addConstant(value);
#ifdef MAX_CONSTANT
			if (constant > UINT_MAX) {
				error("Too many constants in one chunk.");
				return 0;
			}
#endif
			return constant;
		}

		void emitConstant(const value_t& value) {
			emitBytes(OpCode::CONSTANT, makeConstant(value));
		}

		void expression() { parsePrecedence(Precedence::ASSIGNMENT); }

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
			Compiler compiler(enclosing, type);
			enclosing = &compiler;

			if (type != FunctionType::SCRIPT) {
				enclosing->func->name = parser.previous.lexeme;
			}

			beginScope();

			consume(TokenType::left_paren, "Expect '(' after function name.");
			if (!check(TokenType::right_paren)) {
				do {
					enclosing->func->arity++;
					if (enclosing->func->arity > 255) {
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

			for (auto i = 0; i < compiler.func->upvalueCount; i++) {
				emitByte(static_cast<std::size_t>(compiler.upvalues[i].isLocal ? 1 : 0));
				emitByte(compiler.upvalues[i].index);
			}
		}

		void declaration() {
			if (match(TokenType::kw_class)) {
				classDeclaration();
			}
			else if (match(TokenType::fun)) {
				funDeclaration();
			}
			else if (match(TokenType::var)) {
				varDeclaration();
			}
			else {
				statement();
			}
			if (parser.panicMode)
				synchronize();
		}

		Token syntheticToken(const std::string& text) {
			Token token;
			token.lexeme = text;
			return token;
		}

		void super(bool canAssign) {
			if (currentClass == nullptr)
				error("Can't use 'super' outside of a class.");
			else if (!currentClass->hasSuperclass)
				error("Can't use 'super' in a class with no superclass.");

			consume(TokenType::dot, "Expect '.' after 'super'.");
			consume(TokenType::identifier, "Expect superclass method name.");
			auto name = identifierConstant(parser.previous);

			namedVariable(syntheticToken("this"), false);
			if (match(TokenType::left_paren)) {
				auto argCount = argumentList();
				namedVariable(syntheticToken("super"), false);
				emitBytes(OpCode::SUPER_INVOKE, name);
				emitByte(argCount);
			}
			else {
				namedVariable(syntheticToken("super"), false);
				emitBytes(OpCode::GET_SUPER, name);
			}
		}

		void classDeclaration() {
			consume(TokenType::identifier, "Expect class name.");
			auto className = parser.previous;
			auto nameConstant = identifierConstant(parser.previous);
			declareVariable();

			emitBytes(OpCode::CLASS, nameConstant);
			defineVariable(nameConstant);

			ClassCompiler classCompiler;
			classCompiler.enclosing = currentClass;
			classCompiler.hasSuperclass = false;
			currentClass = &classCompiler;

			if (match(TokenType::less)) {
				consume(TokenType::identifier, "Expect superclass name.");
				variable(false);
				if (identifiersEqual(className, parser.previous)) {
					error("A class can't inherit from itself.");
				}

				beginScope();
				addLocal(syntheticToken("super"));
				defineVariable(static_cast<std::size_t>(0));

				namedVariable(className, false);
				emitByte(OpCode::INHERIT);
				classCompiler.hasSuperclass = true;
			}

			namedVariable(className, false);
			consume(TokenType::left_brace, "Expect '{' before class body.");
			while (!check(TokenType::right_brace) && !check(TokenType::eof)) {
				method();
			}
			consume(TokenType::right_brace, "Expect '}' after class body.");
			emitByte(OpCode::POP);

			if (classCompiler.hasSuperclass) {
				endScope();
			}
			currentClass = currentClass->enclosing;
		}

		void method() {
			consume(TokenType::identifier, "Expect method name.");
			auto constant = identifierConstant(parser.previous);
			FunctionType type = FunctionType::METHOD;
			if (parser.previous.lexeme == initString) {
				type = FunctionType::INITIALIZER;
			}
			function(type);
			emitBytes(OpCode::METHOD, constant);
		}

		void this_(bool canAssign) {
			if (currentClass == nullptr) {
				error("Can't use 'this' outside of a class.");
				return;
			}

			variable(false);
		}

		void dot(bool canAssign) {
			consume(TokenType::identifier, "Expect property name after '.'.");
			auto name = identifierConstant(parser.previous);

			if (canAssign && match(TokenType::equal)) {
				expression();
				emitBytes(OpCode::SET_PROPERTY, name);
			}
			else if (match(TokenType::left_paren)) {
				auto argCount = argumentList();
				emitBytes(OpCode::INVOKE, name);
				emitByte(argCount);
			}
			else {
				emitBytes(OpCode::GET_PROPERTY, name);
			}
		}

		code_t parseVariable(const char* errorMessage) {
			consume(TokenType::identifier, errorMessage);
			declareVariable();
			if (enclosing->scopeDepth > 0)
				return static_cast<std::size_t>(0);
			return identifierConstant(parser.previous);
		}

		void declareVariable() {
			if (enclosing->scopeDepth == 0)
				return;
			auto& name = parser.previous;
			for (int i = enclosing->localCount - 1; i >= 0; i--) {
				auto& local = enclosing->locals[i];
				if (local.depth != -1 && local.depth < enclosing->scopeDepth) {
					break;
				}

				if (identifiersEqual(name, local.name)) {
					error("Already a variable with this name in this scope.");
				}
			}
			addLocal(name);
		}

		bool identifiersEqual(const Token& a, const Token& b) {
			return a.lexeme == b.lexeme;
		}

		void addLocal(const Token& name) {
#ifdef LIMIT_LOCAL_VARIABLE_COUNT
			if (enclosing->localCount == 50) {
				error("Too many local variables in function.");
				return;
			}
			auto& local = enclosing->locals[enclosing->localCount++];
#endif
			if (enclosing->localCount >= enclosing->locals.size())
				enclosing->locals.emplace_back(name, -1);
			else
				enclosing->locals[enclosing->localCount] = Local(name, -1);
			enclosing->localCount++;

#ifdef LIMIT_LOCAL_VARIABLE_COUNT
			local.name = name;
			local.depth = -1;
			local.depth = enclosing->scopeDepth;
#endif
		}

		code_t identifierConstant(const Token& name) {
			return makeConstant(name.lexeme);
		}

		void varDeclaration() {
			code_t global = parseVariable("Expect variable name.");

			if (match(TokenType::equal)) {
				expression();
			}
			else {
				emitByte(OpCode::NIL);
			}
			// consume(TokenType::semicolon, "Expect ';' after variable declaration.");
			defineVariable(global);
			if (match(TokenType::comma)) {
				varDeclaration();
				return;
			}
			consume(TokenType::semicolon, "Expect ';' after variable declaration.");
		}

		void defineVariable(const code_t& global) {
			if (enclosing->scopeDepth > 0) {
				markInitialized();
				return;
			}
			emitBytes(OpCode::DEFINE_GLOBAL, global);
		}

		void markInitialized() {
			if (enclosing->scopeDepth == 0)
				return;
			enclosing->locals[enclosing->localCount - 1].depth = enclosing->scopeDepth;
		}

		void variable(bool canAssign) { namedVariable(parser.previous, canAssign); }

		int resolveLocal(Compiler* compiler, const Token& name) {
			for (int i = compiler->localCount - 1; i >= 0; i--) {
				auto& local = compiler->locals[i];
				if (name.lexeme == local.name.lexeme) {
					if (local.depth == -1) {
						error("Can't read local variable in its own initializer.");
					}
					return i;
				}
			}
			return -1;
		}

		int resolveUpvalue(Compiler* compiler, const Token& name) {
			if (compiler->enclosing == nullptr || compiler == this)
				return -1;
			auto local = resolveLocal(compiler->enclosing, name);
			if (local != -1) {
				compiler->enclosing->locals[local].isCaptured = true;
				return addUpvalue(compiler, local, true);
			}
			auto upvalue = resolveUpvalue(compiler->enclosing, name);
			if (upvalue != -1) {
				return addUpvalue(compiler, upvalue, false);
			}
			return -1;
		}

		int addUpvalue(Compiler* compiler, std::size_t index, bool isLocal) {
			int upvalueCount = compiler->func->upvalueCount;
			for (auto i = 0; i < upvalueCount; i++) {
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

		void namedVariable(const Token& name, bool canAssign) {
			code_t arg; // = identifierConstant(name);
			code_t getOp, setOp;
			auto localIndex = resolveLocal(enclosing, name);
			if (localIndex != -1) {
				arg = static_cast<std::size_t>(localIndex);
				getOp = OpCode::GET_LOCAL, setOp = OpCode::SET_LOCAL;
			}
			else if ((localIndex = resolveUpvalue(enclosing, name)) != -1) {
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
				match(TokenType::plus_equal, TokenType::minus_equal,
					TokenType::star_equal, TokenType::slash_equal)) {
				auto type = parser.previous.type;
				emitBytes(getOp, arg);
				expression();
				switch (type) {
				case TokenType::plus_equal:
					emitByte(OpCode::ADD);
					break;
				case TokenType::minus_equal:
					emitByte(OpCode::SUBTRACT);
					break;
				case TokenType::star_equal:
					emitByte(OpCode::MULTIPLY);
					break;
				case TokenType::slash_equal:
					emitByte(OpCode::DIVIDE);
					break;
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
				if (parser.previous.type == TokenType::semicolon)
					return;
				switch (parser.current.type) {
				case TokenType::kw_class:
				case TokenType::fun:
				case TokenType::var:
				case TokenType::kw_for:
				case TokenType::kw_if:
				case TokenType::kw_while:
				case TokenType::print:
				case TokenType::kw_return:
					return;
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
			if (enclosing->type == FunctionType::SCRIPT) {
				error("Can't return from top-level code.");
			}
			if (match(TokenType::semicolon)) {
				emitReturn();
			}
			else {
				if (enclosing->type == FunctionType::INITIALIZER) {
					error("Can't return a value from an initializer.");
				}
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
			else if (match(TokenType::var)) {
				varDeclaration();
			}
			else {
				expressionStatement();
			}

			LoopCompiler loopCompiler;
			loopCompiler.enclosing = currentLoop;
			currentLoop = &loopCompiler;

			currentLoop->start = currentChunk().code.size();
			currentLoop->scopeDepth = enclosing->scopeDepth;

			int exitJump = -1;
			if (!match(TokenType::semicolon)) {
				expression();
				consume(TokenType::semicolon, "Expect ';' after loop condition.");
				exitJump = emitJump(OpCode::JUMP_IF_FALSE); // Jump out of the loop if
															// the condition is false.
				currentLoop->select = exitJump - 1;
				emitByte(OpCode::POP); // Condition.
			}

			if (!match(TokenType::right_paren)) {
				int bodyJump = emitJump(OpCode::JUMP);
				std::size_t incrementStart = currentChunk().code.size();
				expression();
				emitByte(OpCode::POP);
				consume(TokenType::right_paren, "Expect ')' after for clauses.");

				emitLoop(currentLoop->start);
				currentLoop->start = incrementStart;
				patchJump(bodyJump);
			}

			statement();
			emitLoop(currentLoop->start);

			if (exitJump != -1) {
				patchJump(exitJump);
				emitByte(OpCode::POP); // Condition.
			}

			endScope();
			currentLoop = currentLoop->enclosing;
		}

		void continueStatement() {
			if (currentLoop == nullptr) {
				error("Can't use 'continue' outside of a loop");
				return;
			}
			consume(TokenType::semicolon, "Expect ';' after 'continue'.");
			for (auto i = enclosing->localCount - 1;
				i >= 0 && enclosing->locals[i].depth > currentLoop->scopeDepth; --i) {
				emitByte(OpCode::POP);
			}
			emitLoop(currentLoop->start); // Jump to top of current innermost loop.
		}

		void breakStatement() {
			if (currentLoop == nullptr) {
				error("Can't use 'break' outside of a loop");
				return;
			}
			consume(TokenType::semicolon, "Expect ';' after 'break'.");
			for (auto i = enclosing->localCount - 1;
				i >= 0 && enclosing->locals[i].depth > currentLoop->scopeDepth; --i) {
				emitByte(OpCode::POP);
			}

			if (currentLoop->select > 0) {
				emitByte(OpCode::FALSE);
				emitLoop(currentLoop->select);
			}
		}

		void whileStatement() {
			LoopCompiler loopCompiler;
			loopCompiler.enclosing = currentLoop;
			currentLoop = &loopCompiler;

			currentLoop->start = currentChunk().code.size();
			currentLoop->scopeDepth = enclosing->scopeDepth;

			consume(TokenType::left_paren, "Expect '(' after 'while'.");
			expression();
			consume(TokenType::right_paren, "Expect ')' after condition.");

			int exitJump = emitJump(OpCode::JUMP_IF_FALSE);
			currentLoop->select = exitJump - 1;
			emitByte(OpCode::POP);
			statement();

			emitLoop(currentLoop->start);

			patchJump(exitJump);
			emitByte(OpCode::POP);

			currentLoop = currentLoop->enclosing;
		}

		void emitLoop(std::size_t loopStart) {
			emitByte(OpCode::LOOP);

			std::size_t offset = currentChunk().code.size() - loopStart + 1;
			if (offset > std::numeric_limits<std::size_t>::max())
				error("Loop body too large.");

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
			if (match(TokenType::elif))
				ifStatement();
			if (match(TokenType::kw_else))
				statement();
			patchJump(elseJump);
		}

		int emitJump(const code_t& instruction) {
			emitByte(instruction);
			emitByte(std::numeric_limits<std::size_t>::max());
			// emitByte(0xff);
			return currentChunk().code.size() - 1;
		}

		void patchJump(int offset) {
			// -2 to adjust for the bytecode for the jump offset itself.
			int jump = currentChunk().code.size() - offset - 1;
#ifdef OVER_MAX_JUMP_DISTANCE
			/ if (jump > UINT_MAX) { error("Too much code to jump over."); }
#endif
			currentChunk().code[offset] = static_cast<std::size_t>(jump);
		}

		void endScope() {
			enclosing->scopeDepth--;

			while (enclosing->localCount > 0 &&
				enclosing->locals[enclosing->localCount - 1].depth >
				enclosing->scopeDepth) {
				if (enclosing->locals[enclosing->localCount - 1].isCaptured) {
					emitByte(OpCode::CLOSE_UPVALUE);
				}
				else {
					emitByte(OpCode::POP);
				}
				// emitByte(OpCode::POP);
				enclosing->localCount--;
			}
		}

		void beginScope() { enclosing->scopeDepth++; }

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

		template <
			class... Ts,
			std::enable_if_t<(... && std::is_convertible_v<Ts, TokenType>), int> = 0>
			bool match(const Ts... ts) {
			if ((check(ts) || ...)) {
				advance();
				return true;
			}
			return false;
		}

		bool check(TokenType type) { return parser.current.type == type; }

		void printStatement() {
			expression();
			consume(TokenType::semicolon, "Expect ';' after value.");
			emitByte(OpCode::PRINT);
		}

		std::optional<std::shared_ptr<Fn>> compile(const char* source) {
			scanner = Scanner{ source };
			advance();
			while (!match(TokenType::eof)) {
				declaration();
			}
			/*expression();
			consume(TokenType::eof, "Expect end of expression.");*/
			auto func = std::move(endCompiler());
			return parser.hadError ? std::optional<std::shared_ptr<Fn>>(std::nullopt)
				: func;
		}

		std::shared_ptr<Fn> endCompiler() {
			emitReturn();
			auto func = enclosing->func;
#ifdef DEBUG_PRINT_CODE
			if (!parser.hadError) {
				disassembleChunk(enclosingChunk(),
					!func->name.empty() ? func->name : "<script>");
			}
#endif
			if (enclosing->enclosing != nullptr) {
				enclosing = enclosing->enclosing;
			}
			return func;
		}

		void emitReturn() {
			if (enclosing->type == FunctionType::INITIALIZER) {
				emitBytes(OpCode::GET_LOCAL, static_cast<std::size_t>(0));
			}
			else {
				emitByte(OpCode::NIL);
			}
			emitByte(OpCode::RETURN);
		}

		template <
			class... Ts,
			std::enable_if_t<(... && std::is_convertible_v<Ts, code_t>), int> = 0>
			void emitBytes(const Ts &...ts) {
			(emitByte(ts), ...);
		}

		void emitByte(const code_t& byte) {
			if (std::holds_alternative<OpCode>(byte))
				previousOp = currentChunk().code.size();
			currentChunk().write(byte, parser.previous.line);
		}
	};

	struct VM {
		enum struct interpretResult { OK, COMPILE_ERROR, RUNTIME_ERROR };

		static constexpr std::size_t FRAMES_MAX = 64;
		std::array<CallFrame, FRAMES_MAX> frames;
		int frameCount = 0;
		Stack stack;
		std::unordered_map<std::string, value_t> globals;
		std::shared_ptr<ObjUpvalue> openUpvalues = nullptr;

		template <class... Ts> void runtimeError(const char* format, Ts... args) {
			std::cerr << string_format(format, args...) << "\n";
			for (auto i = frameCount - 1; i >= 0; i--) {
				CallFrame* frame = &frames[i];
				auto func = frame->closure->function;
				size_t instruction = frame->ip - 1;
				int line = func->chunk.lines[instruction];
				std::cerr << string_format(
					"[line %d] in %s\n", line,
					func->name.empty() ? "script" : std::string(func->name).c_str());
			}
			resetStack();
		}

		bool isFalsey(const value_t& value) {
			return std::holds_alternative<std::nullptr_t>(value) ||
				(std::holds_alternative<bool>(value) && !std::get<bool>(value));
		}

		bool valuesEqual(const value_t& a, const value_t& b) {
			if (a.index() != b.index())
				return false;
			if (std::holds_alternative<bool>(a))
				return std::get<bool>(a) == std::get<bool>(b);
			else if (std::holds_alternative<std::nullptr_t>(a))
				return true;
			else if (std::holds_alternative<num>(a))
				return std::get<num>(a) == std::get<num>(b);
			else
				return false; // Unreachable.
		}

		bool call(Closure* closure, int argCount = 0) {
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
			frame->slots = stack.top - argCount - 1;
			return true;
		}

		bool callValue(value_t& callee, int argCount) {
			return std::visit(
				[&, this](auto&& arg) {
					using T = std::decay_t<decltype(arg)>;
					if constexpr (std::is_same_v<T, std::shared_ptr<Closure>>) {
						return call(arg.get(), argCount);
					}
					else if constexpr (std::is_same_v<T, std::shared_ptr<NativeFn>>) {
						auto nativeFunc = arg.get();
						if (argCount != nativeFunc->arity) {
							runtimeError("Expected %d arguments but got %d.",
								nativeFunc->arity, argCount);
							return false;
						}
						nativeFunc->func(this, nativeFunc->arity);
						return true;
					}
					else if constexpr (std::is_same_v<T, std::shared_ptr<Class>>) {
						auto klass = arg;
						auto instance = std::make_shared<Instance>(std::move(klass));
						stack[stack.top - argCount - 1] = std::move(instance);
						if (auto it = klass->methods.find(initString);
							it != klass->methods.end()) {
							auto& initializer =
								std::get<std::shared_ptr<Closure>>(it->second);
							return call(initializer.get(), argCount);
						}
						return true;
					}
					else if constexpr (std::is_same_v<T,
						std::shared_ptr<BoundMethod>>) {
						auto bound = arg;
						stack[stack.top - argCount - 1] = bound->receiver;
						return call(bound->method.get(), argCount);
					}
					else if (argCount != 0) {
						runtimeError("Expected 0 arguments but got %d.", argCount);
						return false;
					}
					runtimeError("Can only call functions and classes.");
					return false;
				},
				callee);
		}

		template <class... Args>
		void defineNative(std::pair<const char*, Args> &&... function) {
			auto callee = [this](auto&& arg) {
				globals[arg.name] = std::make_shared<NativeFn>(arg);
			};
			(callee(Lox::details::create_nativeFunc<VM>(function.first, function.second)), ...);
		}

		void defineMethod(const std::string& name) {
			auto method = peek(0);
			auto klass = std::get<std::shared_ptr<Class>>(peek(1));
			klass->methods[name] = method;
			pop();
		}

		bool bindMethod(Class* klass, const std::string& name) {
			value_t method;
			if (auto it = klass->methods.find(name); it != klass->methods.end())
				method = it->second;
			else {
				runtimeError("Undefined property '%s'.", name.data());
				return false;
			}

			auto bound = std::make_shared<BoundMethod>(
				peek(0), std::get<std::shared_ptr<Closure>>(method));
			pop();
			push(bound);
			return true;
		}

		bool invoke(const std::string& name, int argCount) {
			auto receiver = peek(argCount);
			if (!std::holds_alternative<std::shared_ptr<Instance>>(receiver)) {
				runtimeError("Only instances have methods.");
				return false;
			}

			auto instance = std::get<std::shared_ptr<Instance>>(receiver).get();
			if (auto it = instance->fields.find(name);
				it != instance->fields.end()) {
				auto& value = it->second;
				stack[stack.top - argCount - 1] = value;
				return callValue(value, argCount);
			}

			return invokeFromClass(instance->klass.get(), name, argCount);
		}

		bool invokeFromClass(Class* klass, const std::string& name, int argCount) {
			if (auto it = klass->methods.find(name);
				it != klass->methods.end()) {
				auto method = std::get<std::shared_ptr<Closure>>(it->second).get();
				return call(method, argCount);
			}
			else {
				runtimeError("Undefined property '%s'.", name.c_str());
				return false;
			}
		}

		std::shared_ptr<ObjUpvalue> captureUpvalue(std::size_t local) {
			auto* upvalue = &openUpvalues;
			decltype(upvalue) prevUpvalue = nullptr;
			while (*upvalue != nullptr && (*upvalue)->location > local) {
				prevUpvalue = upvalue;
				upvalue = &(*upvalue)->next;
			}
			if (*upvalue != nullptr && (*upvalue)->location == local) {
				return *upvalue;
			}
			auto createdUpvalue = std::make_shared<ObjUpvalue>(&stack.value, local);
			createdUpvalue->next = *upvalue;
			if (prevUpvalue == nullptr) {
				openUpvalues = std::move(createdUpvalue);
				prevUpvalue = &openUpvalues;
			}
			else {
				(*prevUpvalue)->next = std::move(createdUpvalue);
				prevUpvalue = &(*prevUpvalue)->next;
			}
			return *prevUpvalue;
		}

		void closeUpvalues(std::size_t last) {
			while (openUpvalues != nullptr && openUpvalues->location >= last) {
				auto* upvalue = &openUpvalues;
				(*upvalue)->fork(stack[(*upvalue)->location]);
				openUpvalues = std::move((*upvalue)->next);
				if (openUpvalues != nullptr)
					(*upvalue)->next.reset();
			}
		}

		void push(const value_t& value) { stack.push(value); }

		value_t pop() { return stack.pop(); }

		value_t& peek(int distance) { return stack[stack.top - 1 - distance]; }

		void resetStack() {
			stack.reset();
			frameCount = 0;
		}

		interpretResult interpret(const char* source) {
			Compiler compiler;
			auto func = compiler.compile(source);
			if (!func.has_value())
				return interpretResult::COMPILE_ERROR;
			push(*func);
			auto closure = std::make_shared<Closure>(*func);
			pop();
			push(closure);
			call(closure.get());
			return run();
		}

		interpretResult run() {
			CallFrame* frame = &frames[frameCount - 1];
			auto chunk = [&]() -> Chunk& { return frame->closure->function->chunk; };
			auto read_byte = [&]() {
				return std::get<std::size_t>(chunk().code[frame->ip++]);
			};
			value_t* var_ptr = nullptr;
#define DEBUG_TRACE_EXECUTION 0
#define ONLYPRINTCODE 0
#define BINARY_OP(op) \
  do { \
    if (!std::holds_alternative<num>(peek(0)) || \
        !std::holds_alternative<num>(peek(1))) { \
      runtimeError("Operands must be numbers"); \
      return interpretResult::RUNTIME_ERROR; \
    } \
    num b = std::get<num>(pop()), a = std::get<num>(pop()); \
    push(a op b); \
  } while (false)

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
					case OpCode::NIL: {
						push(nullptr);
						break;
					}
					case OpCode::TRUE: {
						push(true);
						break;
					}
					case OpCode::FALSE: {
						push(false);
						break;
					}
					case OpCode::POP: {
						pop();
						break;
					}
					case OpCode::INC: {
						if (var_ptr != nullptr) {
							auto arg = read_byte();
							int prefix = arg >> 1, neg = arg & 1 ? -1 : 1;
							auto& top = peek(0);
							if (std::holds_alternative<num>(top) &&
								std::holds_alternative<num>(*var_ptr)) {
								std::get<num>(*var_ptr) += neg;
								if (prefix)
									std::get<num>(top) += neg;
								else
									var_ptr = nullptr;
							}
						}
						break;
					}
					case OpCode::GET_LOCAL: {
						auto slot = read_byte();
						push(stack[frame->slots + slot]);
						var_ptr = &stack[frame->slots + slot];
						break;
					}
					case OpCode::SET_LOCAL: {
						auto slot = read_byte();
						stack[frame->slots + slot] = peek(0);
						break;
					}
					case OpCode::DEFINE_GLOBAL: {
						auto& name = std::get<std::string>(chunk().values[read_byte()]);
						if (!globals.try_emplace(name, peek(0)).second) {
							runtimeError("Already a variable named '%s' in this scope.", name.data());
							return interpretResult::RUNTIME_ERROR;
						}
						pop();
						break;
					}
					case OpCode::GET_GLOBAL: {
						auto& name = std::get<std::string>(chunk().values[read_byte()]);
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
						auto& name = std::get<std::string>(chunk().values[read_byte()]);
						auto value = globals.find(name);
						if (value == globals.end()) {
							runtimeError("Undefined variable '%s'.", name.data());
							return interpretResult::RUNTIME_ERROR;
						}
						else
							globals[name] = peek(0);
						break;
					}
					case OpCode::EQUAL: {
						auto b = pop();
						auto a = pop();
						push(valuesEqual(a, b));
						break;
					}
					case OpCode::GREATER:
						BINARY_OP(>);
						break;
					case OpCode::LESS:
						BINARY_OP(<);
						break;
					case OpCode::ADD: {
						if (std::holds_alternative<std::string>(peek(0)) ||
							std::holds_alternative<std::string>(peek(1))) {
							auto a = std::move(pop());
							auto b = std::move(pop());
							push(std::to_string(b) + std::to_string(a));
						}
						else {
							BINARY_OP(+);
						}
						break;
					}
					case OpCode::SUBTRACT: {
						BINARY_OP(-);
						break;
					}
					case OpCode::MULTIPLY: {
						BINARY_OP(*);
						break;
					}
					case OpCode::DIVIDE: {
						BINARY_OP(/);
						break;
					}
					case OpCode::NOT: {
						push(isFalsey(pop()));
						break;
					}
					case OpCode::NEGATE: {
						if (!std::holds_alternative<num>(peek(0))) {
							runtimeError("Operand must be a number.");
							return interpretResult::RUNTIME_ERROR;
						}
						push(-std::get<num>(pop()));
						break;
					}
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
						if (isFalsey(peek(0)))
							frame->ip += offset;
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
							return interpretResult::RUNTIME_ERROR;
						}
						frame = &frames[frameCount - 1];
						break;
					}
					case OpCode::SET_UPVALUE: {
						auto slot = read_byte();
						frame->closure->upvalues[slot]->value() = peek(0);
						break;
					}
					case OpCode::GET_UPVALUE: {
						auto slot = read_byte();
						push(frame->closure->upvalues[slot]->value());
						var_ptr = &frame->closure->upvalues[slot]->value();
						break;
					}
					case OpCode::CLOSE_UPVALUE: {
						closeUpvalues(stack.top - 1);
						pop();
						break;
					}
					case OpCode::CLOSURE: {
						auto function =
							std::get<std::shared_ptr<Fn>>(chunk().values[read_byte()]);
						auto closure = std::make_shared<Closure>(function);
						for (auto i = 0; i < closure->upvalueCount; i++) {
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
					case OpCode::CLASS: {
						push(std::make_shared<Class>(
							std::get<std::string>(chunk().values[read_byte()])));
						break;
					}
					case OpCode::GET_PROPERTY: {
						if (!std::holds_alternative<std::shared_ptr<Instance>>(peek(0))) {
							runtimeError("Only instances have properties.");
							return interpretResult::RUNTIME_ERROR;
						}
						auto instance = std::get<std::shared_ptr<Instance>>(peek(0));
						auto& name = std::get<std::string>(chunk().values[read_byte()]);
						if (auto it = instance->fields.find(name);
							it != instance->fields.end()) {
							pop(); // Instance.
							push(it->second);
							break;
						}

						if (!bindMethod(instance->klass.get(), name)) {
							return interpretResult::RUNTIME_ERROR;
						}
						break;
					}
					case OpCode::SET_PROPERTY: {
						if (!std::holds_alternative<std::shared_ptr<Instance>>(peek(1))) {
							runtimeError("Only instances have fields.");
							return interpretResult::RUNTIME_ERROR;
						}
						auto instance = std::get<std::shared_ptr<Instance>>(peek(1));
						auto& name = std::get<std::string>(chunk().values[read_byte()]);
						instance->fields[name] = peek(0);
						auto value = pop();
						pop();
						push(value);
						break;
					}
					case OpCode::METHOD: {
						defineMethod(std::get<std::string>(chunk().values[read_byte()]));
						break;
					}
					case OpCode::INVOKE: {
						auto& method = std::get<std::string>(chunk().values[read_byte()]);
						auto argCount = read_byte();
						if (!invoke(method, argCount)) {
							return interpretResult::RUNTIME_ERROR;
						}
						frame = &frames[frameCount - 1];
						break;
					}
					case OpCode::INHERIT: {
						if (!std::holds_alternative<std::shared_ptr<Class>>(peek(1))) {
							runtimeError("Superclass must be a class.");
							return interpretResult::RUNTIME_ERROR;
						}
						auto subclass = std::get<std::shared_ptr<Class>>(peek(0));
						auto superclass = std::get<std::shared_ptr<Class>>(peek(1));
						subclass->methods = superclass->methods;
						pop(); // Subclass.
						break;
					}
					case OpCode::GET_SUPER: {
						auto& name = std::get<std::string>(chunk().values[read_byte()]);
						auto superclass = std::get<std::shared_ptr<Class>>(pop()).get();
						if (!bindMethod(superclass, std::string(name))) {
							return interpretResult::RUNTIME_ERROR;
						}
						break;
					}
					case OpCode::SUPER_INVOKE: {
						auto& method = std::get<std::string>(chunk().values[read_byte()]);
						auto argCount = read_byte();
						Class* superclass = std::get<std::shared_ptr<Class>>(pop()).get();
						if (!invokeFromClass(superclass, method, argCount)) {
							return interpretResult::RUNTIME_ERROR;
						}
						frame = &frames[frameCount - 1];
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
						stack.top = frame->slots;
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