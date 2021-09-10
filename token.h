#pragma once
#include <string_view>
#include <vector>
#include <variant>
#include "tool.h"
namespace Lox {
	enum struct OpCode {
		CONSTANT,
		NIL,
		TRUE,
		FALSE,
		POP,
		GET_LOCAL,
		SET_LOCAL,
		GET_GLOBAL,
		DEFINE_GLOBAL,
		SET_GLOBAL,
		EQUAL,
		GREATER,
		LESS,
		ADD,
		SUBTRACT,
		MULTIPLY,
		DIVIDE,
		NOT,
		NEGATE,
		PRINT,
		JUMP,
		JUMP_IF_FALSE,
		LOOP,
		RETURN
	};

	using code_t = std::variant<OpCode, std::size_t>;
	using value_t = std::variant<double, bool, std::string, std::nullptr_t>;

	inline std::ostream& operator<<(std::ostream& out, const value_t& value) {
		std::visit(overloaded{
			[&](double arg) {out << "num| " << arg; },
			[&](bool arg) {out << "bool| " << (arg ? "true" : "false"); },
			[&](const std::string& arg) {out << "str| " << arg; },
			[&](std::nullptr_t arg) {out << "none| nil"; }
			}, value
		);
		return out;
	}

	struct Chunk {
		void write(const code_t& byte, std::size_t line) {
			code.emplace_back(byte);
			lines.emplace_back(line);
		}
		std::size_t addConstant(const value_t& value) {
			values.emplace_back(value);
			return values.size() - 1;
		}
		std::vector<code_t> code;
		std::vector<std::size_t> lines;
		std::vector<value_t> values;
	};

	enum struct TokenType {
		// Single-character tokens.
		left_paren, right_paren, left_brace, right_brace,
		comma, dot, minus, plus, semicolon, slash, star,
		// one or two character tokens.
		bang, bang_equal,
		equal, equal_equal,
		greater, greater_equal,
		less, less_equal,
		// literals.
		identifier, string, number,
		// keywords.
		kw_and, kw_class, kw_else, kw_false, fun, kw_for, kw_if, nil, kw_or,
		print, kw_return, super, kw_this, kw_true, var, kw_while,
		//custom
		elif, kw_continue, kw_break,
		error, eof
	};

	struct Token {
		TokenType type;
		std::string_view lexem;
		int line;
	};
}
