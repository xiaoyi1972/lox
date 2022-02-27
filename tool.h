#pragma once
#include <memory>
#include <stdexcept>
#include <string>

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...)->overloaded<Ts...>;

template <class... Args>
std::string string_format(const std::string& format, Args... args) {
	std::size_t size =
		snprintf(nullptr, 0, format.c_str(), args...) + 1; // Extra space for '\0'
	if (size <= 0) {
		throw std::runtime_error("Error during formatting.");
	}
	std::unique_ptr<char[]> buf(new char[size]);
	snprintf(buf.get(), size, format.c_str(), args...);
	return std::string(buf.get(),
		buf.get() + size - 1); // We don't want the '\0' inside
}

template <class T, class... Ts,
	std::enable_if_t<(... && std::is_convertible_v<Ts, T>), int> = 0>
	bool any_v(const T& value, const Ts &...ts) {
	return ((value == ts) || ...);
}

template <class T, class U> struct has_type;
template <class T, class... U>
struct has_type<T, std::variant<U...>>
	: std::disjunction<std::is_same<T, U>...> {};

template <class> struct is_variant : std::false_type {};
template <class... Ts>
struct is_variant<std::variant<Ts...>> : std::true_type {};

template <class T, class... U> struct concatenator;
template <class... T, class... U>
struct concatenator<std::variant<T...>, std::variant<U...>> {
	using type = std::variant<T..., U...>;
};

template <class T, class... U>
using concatenator_t = typename concatenator<T, U...>::type;

template <class... T> struct ObjListImpl {
	using type = std::variant<std::shared_ptr<T>...>;
};

template <class... T> using ObjListImpl_t = typename ObjListImpl<T...>::type;
