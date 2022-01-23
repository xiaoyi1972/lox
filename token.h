#pragma once
#include <string_view>
#include <vector>
#include <variant>
#include <functional>
#include "tool.h"
namespace Lox {
	enum struct OpCode {
		CONSTANT, 
		NIL, TRUE, FALSE,
		POP,
		GET_LOCAL, SET_LOCAL,
		DEFINE_GLOBAL, GET_GLOBAL, SET_GLOBAL,
		GET_UPVALUE, SET_UPVALUE, CLOSE_UPVALUE,
		EQUAL, GREATER, LESS,
		ADD, SUBTRACT, MULTIPLY, DIVIDE,
		INC,
		NOT, NEGATE,
		JUMP, JUMP_IF_FALSE, LOOP,
		CALL, RETURN,
		CLOSURE,
		PRINT
	};

	using num = double;
	struct NativeFn;
	struct Function;
	struct Closure;
	using Object = std::variant<std::shared_ptr<Function>, std::shared_ptr<NativeFn>,std::shared_ptr<Closure>>;
	using code_t = std::variant<OpCode, std::size_t>;
	using value_t = std::variant<num, bool, std::string, std::nullptr_t, Object>;

	struct Stack {
		std::size_t top;
		std::vector<value_t> value;

		Stack() :top(0) {}
		void reset() { top = 0; }
		void push(const value_t& v) {
			if (++top > value.size()) 
				value.emplace_back(v);
			else value[top - 1] = v;
		}
		value_t pop() { return value[--top]; }
		value_t& operator[](std::size_t index) { return value[index]; }
	};

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

	enum struct FunctionType {
		FUNCTION,
		SCRIPT
	};

	struct Upvalue {
		std::size_t index;
		bool isLocal;
	};

	struct Function {
		std::size_t arity = 0;
		std::size_t upvalueCount = 0;
		Chunk chunk;
		std::string_view name;
	};

	struct ObjUpvalue {
		std::size_t location;
		value_t closed;
		std::shared_ptr<ObjUpvalue>  next;
		ObjUpvalue(std::size_t _location) :location(_location), closed(nullptr), next(nullptr) {}
	};

	struct Closure{
		std::shared_ptr<Function> function;
		std::vector<std::shared_ptr<ObjUpvalue>> upvalues;
		int upvalueCount;
		Closure(const std::shared_ptr<Function> &_function) : function(_function), 
			upvalueCount(_function->upvalueCount),
			upvalues(_function->upvalueCount, nullptr) {}
	};

	struct NativeFn {
		std::function<std::optional<value_t>(Stack&, std::size_t)> func;
		std::string_view name;
		std::size_t arity;
	};

	namespace details {
		template<class T>
		struct function_traits : public function_traits<decltype(&std::remove_reference_t<T>::operator())> {};
		template<class R, class C, class...Ts>
		struct function_traits<R(C::*)(Ts...) const> :public function_traits<R(*)(Ts...)> {};
		template<class R, class C, class...Ts>
		struct function_traits<R(C::*)(Ts...)> :public function_traits<R(*)(Ts...)> {};
		template<class R, class... Ts>
		struct function_traits<R(*)(Ts...)> :public function_traits<R(Ts...)> {};
		template<class R, class...Ts>struct function_traits<R(Ts...)> {
			using result_type = R;
			using arg_tuple = std::tuple<Ts...>;
			static constexpr auto arity = sizeof...(Ts);
		};

		template <class F, std::size_t ... Is, class T>
		auto lambda_to_func_impl(F f, std::index_sequence<Is...>, T) {
			return std::function<typename T::result_type(std::tuple_element_t<Is, typename T::arg_tuple>...)>(f);
		}

		template <class F> auto lambda_to_func(F f) {
			using traits = function_traits<F>;
			return lambda_to_func_impl(f, std::make_index_sequence<traits::arity>{}, traits{});
		}

		template<class R, class unpacked, class... left>
		struct unpacker {};

		template<class R, class... unpacked, class left0, class... left>
		struct unpacker<R, std::tuple<unpacked...>, std::tuple<left0, left...>> {
			R operator()(Stack& stack, std::size_t index,
				const std::function<R(unpacked..., left0, left...)>& f,
				std::tuple<unpacked...> t) const {
				using next_unpacker = unpacker<R, std::tuple<unpacked..., left0>, std::tuple<left...>>;
				return next_unpacker()(stack, index, f,
					std::tuple_cat(
						std::move(t),
						std::tuple<left0>{std::get<left0>(stack[index + std::size_t(sizeof...(unpacked))])}
				));
			}
		};

		template<class R, class... unpacked>
		struct unpacker<R, std::tuple<unpacked...>, std::tuple<>> {
			R operator()(Stack& stack,
				std::size_t index,
				const std::function<R(unpacked...)>& f,
				std::tuple<unpacked...> t) const {
				return std::apply(f, t);
			}
		};

		template<class R, class ... Args>
		auto call_impl(std::function<R(Args...)> f) {
			return[f = std::move(f)](Stack& stack, std::size_t n)->std::optional<value_t>{
				if constexpr (std::is_same<R, void>::value) {
					unpacker<R, std::tuple<>, std::tuple<Args...>>()(stack, n, f, std::tuple<>());
					return std::make_optional<value_t>();
				}
				else {
					R retval = unpacker<R, std::tuple<>, std::tuple<Args...>>()(stack, n, f, std::tuple<>());
					if constexpr (std::is_convertible<R, value_t>::value) {
						return std::make_optional<value_t>(retval);
					}
					else  static_assert(std::is_convertible<R, value_t>::value, "can't convert to value_t");
				}
			};
		}

		template<class F>
		auto create_nativeFunc(const char* name, F f) {
			using traits = function_traits<F>;
			auto fn = lambda_to_func_impl(f, std::make_index_sequence<traits::arity>{}, traits{});
			return NativeFn{ call_impl(fn), name, traits::arity };
		}
	}


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
		//extra
		elif, kw_continue, kw_break,
		plus_equal, minus_equal, star_equal, slash_equal, mod_equal,
		inc, dec,
		//default
		error, eof
	};

	struct Token {
		TokenType type;
		std::string_view lexem;
		int line;
	};
}

namespace std {
	inline std::ostream& operator<<(std::ostream& out, const Lox::value_t& value) {
		std::visit(
			overloaded
			{
			[&](double arg) {out << arg; },
			[&](bool arg) {out << (arg ? "true" : "false"); },
			[&](const std::string& arg) {out << arg; },
			[&](std::nullptr_t arg) {out << "nil"; },
			[&](const Lox::Object& arg) {
				std::visit(
				overloaded{
					[&](const std::shared_ptr<Lox::Function>& arg) {
						if (arg->name.empty()) {
							out << "<script>";
							return;
						}
						out << "<fn>" << arg->name;
					},
					[&](const std::shared_ptr<Lox::NativeFn>& arg) {
						out << "<native fn>" << arg->name;
					},
					[&](const std::shared_ptr<Lox::Closure>& arg) {
						if (arg->function->name.empty()) {
							out << "<script>";
							return;
						}
						out << "<fn>" << arg->function->name;
					}
				}, arg);
			}
			}, value);
		return out;
	}

	inline std::string to_string(const Lox::value_t& value) {
		return std::visit(
			overloaded{
			[&](double arg) {
				std::string str = std::to_string(arg);
				str.erase(str.find_last_not_of('0') + 1, std::string::npos);
				str.pop_back();
				return str;
			},
			[&](bool arg) {return std::string{ (arg ? "true" : "false") }; },
			[&](const std::string& arg) {return arg; },
			[&](std::nullptr_t arg) {return std::string{}; },
			[&](const Lox::Object& arg) {
				return std::visit(
					overloaded{
						[&](const std::shared_ptr<Lox::Function>& arg) {
							std::string str = std::to_string(arg);
							str += "<fn>" + std::string(arg->name);
							return str;
						},
						[&](const std::shared_ptr<Lox::NativeFn>& arg) {
							return std::string{arg->name};
						},
						[&](const std::shared_ptr<Lox::Closure>& arg) {
						std::string str("<fn>");
						str += arg->function->name;
						return str;
					}
					}, arg);
			}
			}, value);
	}
}
