#include "common.h"
#include <chrono>
#include <fstream>
#include <iostream>
#include <string>

#define USE_FILE 0

int main(int argc, char** argv) {
#ifndef Test
    {
        using namespace Lox;
        VM vm;
        using namespace Lox::details;
        vm.defineNative(Fn_impl("clock", []() {
            auto stap = std::chrono::high_resolution_clock::now();
            return double(
                std::chrono::time_point_cast<std::chrono::milliseconds>(stap)
                .time_since_epoch()
                .count());
            }));
        std::string input;
#if USE_FILE
        std::string path = __FILE__;
        path = path.substr(0, path.find_last_of("/\\") + 1) + "script.lox";
        if (std::ifstream fs{ path, std::ios::binary }) {
            std::string s(std::istreambuf_iterator<char>(fs), {});
            input = std::move(s);
            fs.close();
        }
        std::cout << input << "\n\n";
#else
        if (argc == 1) {
            static constexpr auto repl = [](auto&& vm, auto&& input) {
                for (;;) {
                    std::cout << ("> ");
                    if (!std::getline(std::cin, input)) {
                        std::cout << ("\n");
                        break;
                    }
                    vm.interpret(input.c_str());
                }
            };
            repl(vm, input);
        }
        else if (argc == 2) {
            if (std::ifstream fs{ argv[1], std::ios::binary }) {
                std::string s(std::istreambuf_iterator<char>(fs), {});
                input = std::move(s);
                fs.close();
                std::cout << input << "\n\n";
            }
        }
        else {
            std::cerr << "Usage: clox [path]\n";
            exit(64);
        }
#endif
        vm.interpret(input.c_str());
    }
#else

#endif
    return 1;
}