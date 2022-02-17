#pragma once
#include <cctype>
#include<cstring>

#include "token.h"

namespace Lox {
	struct Parser {
		Token current;
		Token previous;
		bool hadError = false;
		bool panicMode = false;
	};

	struct Scanner {
		const char* start;
		const char* current;
		int line;

		Scanner(const char* source) : start(source), current(source), line(1) {}

		bool isAtEnd() { return *current == '\0'; }

		Token makeToken(TokenType type) {
			Token token{};
			token.type = type;
			token.lexem = std::string_view{ start }.substr(0, current - start);
			token.line = line;
			return token;
		}

		Token errorToken(const char* message) {
			Token token{};
			token.type = TokenType::error;
			token.lexem = message;
			token.line = line;
			return token;
		}

		char advance() {
			++current;
			return current[-1];
		}

		bool match(char expected) {
			if (isAtEnd())
				return false;
			if (*current != expected)
				return false;
			++current;
			return true;
		}

		char peek() { return *current; }

		char peekNext() {
			if (isAtEnd())
				return '\0';
			return current[1];
		}

		void skipWhitespace() {
			while (true) {
				auto c = peek();
				switch (c) {
				case ' ':
				case '\r':
				case '\t':
					advance();
					break;
				case '\n':
					line++;
					advance();
					break;
				case '/':
					if (peekNext() == '/') { // A comment goes until the end of the line.
						while (peek() != '\n' && !isAtEnd())
							advance();
					}
					else if (peekNext() == '*') {
						advance();
						for (char last{}; !isAtEnd() && (last != '*' || peek() != '/');) {
							last = peek();
							if (last == '\n')
								line++;
							advance();
						}
						if (isAtEnd())
							errorToken("Unterminated block comment.");
						else
							advance();
					}
					else {
						return;
					}
					break;
				default:
					return;
				}
			}
		}

		Token string() {
			while (peek() != '"' && !isAtEnd()) {
				if (peek() == '\n')
					line++;
				advance();
			}
			if (isAtEnd())
				return errorToken("Unterminated string.");
			advance(); // The closing quote.
			return makeToken(TokenType::string);
		}

		Token number() {
			while (std::isdigit(peek()))
				advance();
			if (peek() == '.' &&
				std::isdigit(peekNext())) { // Look for a fractional part.
				advance();                    // Consume the ".".
				while (std::isdigit(peek()))
					advance();
			}
			return makeToken(TokenType::number);
		}

		Token identifier() {
			for (auto c = peek(); std::isalpha(c) || std::isdigit(c) || c == '_';
				c = peek())
				advance();
			return makeToken(identifierType());
		}

		template <std::size_t len>
		TokenType checkKeyword(int start, const char(&rest)[len], TokenType type) {
			if (current - this->start == start + len - 1 &&
				std::memcmp(this->start + start, rest, len - 1) == 0) {
				return type;
			}
			return TokenType::identifier;
		}

		TokenType identifierType() {
			switch (start[0]) {
			case 'a':
				return checkKeyword(1, "nd", TokenType::kw_and);
			case 'b':
				return checkKeyword(1, "reak", TokenType::kw_break);
			case 'c':
				if (current - start > 1) {
					switch (start[1]) {
					case 'l':
						return checkKeyword(2, "ass", TokenType::kw_class);
					case 'o':
						return checkKeyword(2, "ntinue", TokenType::kw_continue);
					}
				}
				break;
			case 'e':
				if (current - start > 1) {
					if (start[1] == 'l')
						switch (start[2]) {
						case 's':
							return checkKeyword(3, "e", TokenType::kw_else);
						case 'i':
							return checkKeyword(3, "f", TokenType::elif);
						}
				}
				break;
			case 'f':
				if (current - start > 1) {
					switch (start[1]) {
					case 'a':
						return checkKeyword(2, "lse", TokenType::kw_false);
					case 'o':
						return checkKeyword(2, "r", TokenType::kw_for);
					case 'u':
						return checkKeyword(2, "n", TokenType::fun);
					}
				}
				break;
			case 'i':
				return checkKeyword(1, "f", TokenType::kw_if);
			case 'n':
				return checkKeyword(1, "il", TokenType::nil);
			case 'o':
				return checkKeyword(1, "r", TokenType::kw_or);
			case 'p':
				return checkKeyword(1, "rint", TokenType::print);
			case 'r':
				return checkKeyword(1, "eturn", TokenType::kw_return);
			case 's':
				return checkKeyword(1, "uper", TokenType::super);
			case 't':
				if (current - start > 1) {
					switch (start[1]) {
					case 'h':
						return checkKeyword(2, "is", TokenType::kw_this);
					case 'r':
						return checkKeyword(2, "ue", TokenType::kw_true);
					}
				}
				break;
			case 'v':
				return checkKeyword(1, "ar", TokenType::var);
			case 'w':
				return checkKeyword(1, "hile", TokenType::kw_while);
			}
			return TokenType::identifier;
		}

		Token scanToken() {
			skipWhitespace();
			start = current;
			if (isAtEnd())
				return makeToken(TokenType::eof);
			char c = advance();
			if (std::isalpha(c) || c == '_')
				return identifier();
			if (std::isdigit(c))
				return number();
			switch (c) {
			case '(':
				return makeToken(TokenType::left_paren);
			case ')':
				return makeToken(TokenType::right_paren);
			case '{':
				return makeToken(TokenType::left_brace);
			case '}':
				return makeToken(TokenType::right_brace);
			case ';':
				return makeToken(TokenType::semicolon);
			case ',':
				return makeToken(TokenType::comma);
			case '.':
				return makeToken(TokenType::dot);
			case '-':
				return makeToken(match('=') ? TokenType::minus_equal
					: match('-') ? TokenType::dec
					: TokenType::minus);
			case '+':
				return makeToken(match('=') ? TokenType::plus_equal
					: match('+') ? TokenType::inc
					: TokenType::plus);
			case '/':
				return makeToken(match('=') ? TokenType::slash_equal : TokenType::slash);
			case '*':
				return makeToken(match('=') ? TokenType::star_equal : TokenType::star);
			case '!':
				return makeToken(match('=') ? TokenType::bang_equal : TokenType::bang);
			case '=':
				return makeToken(match('=') ? TokenType::equal_equal : TokenType::equal);
			case '<':
				return makeToken(match('=') ? TokenType::less_equal : TokenType::less);
			case '>':
				return makeToken(match('=') ? TokenType::greater_equal
					: TokenType::greater);
			case '"':
				return string();
			}
			return errorToken("Unexpected character.");
		}
	};
}
