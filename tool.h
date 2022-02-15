#pragma once
#include <stdexcept>
#include <string>

template <class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...)->overloaded<Ts...>;

template <class... Args>
std::string string_format(const std::string& format, Args... args) {
    std::size_t size = snprintf(nullptr, 0, format.c_str(), args...) +
        1;  // Extra space for '\0'
    if (size <= 0) {
        throw std::runtime_error("Error during formatting.");
    }
    std::unique_ptr<char[]> buf(new char[size]);
    snprintf(buf.get(), size, format.c_str(), args...);
    return std::string(buf.get(),
        buf.get() + size - 1);  // We don't want the '\0' inside
}

template <class T, class... Ts,
    std::enable_if_t<(... && std::is_convertible_v<Ts, T>), int> = 0>
    bool any_v(const T& value, const Ts&... ts) {
    return ((value == ts) || ...);
}