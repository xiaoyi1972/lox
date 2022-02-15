#pragma once
#include <array>

#include "token.h"
namespace Lox {
    enum struct Precedence {
        NONE,
        ASSIGNMENT, // =
        OR,         // or
        AND,        // and
        EQUALITY,   // == !=
        COMPARISON, // < > <= >=
        TERM,       // + -
        FACTOR,     // * /
        UNARY,      // ! -
        CALL,       // . ()
        PRIMARY
    };

    template <class T> struct ParseRule {
        using ParseFn = void (T::*)(bool);
        ParseFn prefix;
        ParseFn infix;
        Precedence precedence;
    };

    template <TokenType type, class T> struct EnumForType {
        static constexpr ParseRule<T> value{ nullptr, nullptr, Precedence::NONE };
    };
    template <class T> struct EnumForType<TokenType::left_paren, T> {
        static constexpr ParseRule<T> value{ &T::grouping, &T::call, Precedence::CALL };
    };
    template <class T> struct EnumForType<TokenType::minus, T> {
        static constexpr ParseRule<T> value{ &T::unary, &T::binary, Precedence::TERM };
    };
    template <class T> struct EnumForType<TokenType::plus, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::binary, Precedence::TERM };
    };
    template <class T> struct EnumForType<TokenType::slash, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::binary, Precedence::FACTOR };
    };
    template <class T> struct EnumForType<TokenType::star, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::binary, Precedence::FACTOR };
    };
    template <class T> struct EnumForType<TokenType::bang, T> {
        static constexpr ParseRule<T> value{ &T::unary, nullptr, Precedence::NONE };
    };
    template <class T> struct EnumForType<TokenType::bang_equal, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::binary,
                                            Precedence::EQUALITY };
    };
    template <class T> struct EnumForType<TokenType::equal_equal, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::binary,
                                            Precedence::EQUALITY };
    };
    template <class T> struct EnumForType<TokenType::greater, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::binary,
                                            Precedence::EQUALITY };
    };
    template <class T> struct EnumForType<TokenType::greater_equal, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::binary,
                                            Precedence::EQUALITY };
    };
    template <class T> struct EnumForType<TokenType::less, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::binary,
                                            Precedence::EQUALITY };
    };
    template <class T> struct EnumForType<TokenType::less_equal, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::binary,
                                            Precedence::EQUALITY };
    };
    template <class T> struct EnumForType<TokenType::identifier, T> {
        static constexpr ParseRule<T> value{ &T::variable, nullptr, Precedence::NONE };
    };
    template <class T> struct EnumForType<TokenType::string, T> {
        static constexpr ParseRule<T> value{ &T::string, nullptr, Precedence::NONE };
    };
    template <class T> struct EnumForType<TokenType::number, T> {
        static constexpr ParseRule<T> value{ &T::number, nullptr, Precedence::NONE };
    };
    template <class T> struct EnumForType<TokenType::kw_and, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::and_, Precedence::AND };
    };
    template <class T> struct EnumForType<TokenType::kw_false, T> {
        static constexpr ParseRule<T> value{ &T::literal, nullptr, Precedence::NONE };
    };
    template <class T> struct EnumForType<TokenType::nil, T> {
        static constexpr ParseRule<T> value{ &T::literal, nullptr, Precedence::NONE };
    };
    template <class T> struct EnumForType<TokenType::kw_or, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::or_, Precedence::OR };
    };
    template <class T> struct EnumForType<TokenType::kw_true, T> {
        static constexpr ParseRule<T> value{ &T::literal, nullptr, Precedence::NONE };
    };
    template <class T> struct EnumForType<TokenType::inc, T> {
        static constexpr ParseRule<T> value{ &T::unary, &T::binary, Precedence::UNARY };
    };
    template <class T> struct EnumForType<TokenType::dec, T> {
        static constexpr ParseRule<T> value{ &T::unary, &T::binary, Precedence::UNARY };
    };
    template <class T> struct EnumForType<TokenType::dot, T> {
        static constexpr ParseRule<T> value{ nullptr, &T::dot, Precedence::CALL };
    };
    template <class T> struct EnumForType<TokenType::kw_this, T> {
        static constexpr ParseRule<T> value{ &T::this_, nullptr, Precedence::NONE };
    };
    template <class T> struct EnumForType<TokenType::super, T> {
        static constexpr ParseRule<T> value{ &T::super, nullptr, Precedence::NONE };
    };

    template <class T, std::size_t... Is>
    std::array<ParseRule<T>, sizeof...(Is)>
        ParseRuleHelp(std::index_sequence<Is...> const&) {
        return { EnumForType<static_cast<TokenType>(Is), T>::value... };
    }

    using allTokenType =
        std::make_index_sequence<static_cast<size_t>(TokenType::eof) + 1>;
    template <class T> using rule_t = decltype(ParseRuleHelp<T>(allTokenType{}));
}
