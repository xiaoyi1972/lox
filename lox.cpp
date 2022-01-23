#include<iostream>
#include<string>
#include<fstream>
#include<chrono>
#include"common.h"


#define Compile_chapter 3
#define USE_FILE 2

void hello() {
	std::cout << "hello world\n";
}

template <typename T>
void print_arity(T t) {
	std::cout << "arity is: " << Lox::details::function_traits<T>::arity << "\n";
}

int main()
{
#if Compile_chapter == 1 //chapter 1
	{
		using namespace Lox;
		Chunk chunk{};
		chunk.write(OpCode::CONSTANT, 123);
		chunk.write(chunk.addConstant(1.2), 123);
		chunk.write(OpCode::NEGATE, 123);
		chunk.write(OpCode::RETURN, 123);
		//	Debug::disassembleChunk(chunk, "test chunk");
		VM vm{ chunk };
		vm.interpret();
	}
#elif Compile_chapter == 2 //chapter 2
	{
		std::string input;
#if USE_FILE
		if (std::ifstream fs{ "./script.lox", std::ios::binary }) {
			std::string s(std::istreambuf_iterator<char>(fs), {});
			input = std::move(s);
			fs.close();
		}
		std::cout << input << "\n\n";
#else
		std::getline(std::cin, input);
#endif
		using namespace Lox;
		Chunk ck; //Debug::disassembleChunk(chunk, "test chunk");
		VM vm(ck);
		//vm.compile(input.c_str());
		vm.interpret(input.c_str());
	}
#elif Compile_chapter == 3 //chapter 2
	{
		std::string input;
#if USE_FILE
		if (std::ifstream fs{ "./script.lox", std::ios::binary }) {
			std::string s(std::istreambuf_iterator<char>(fs), {});
			input = std::move(s);
			fs.close();
}
		std::cout << input << "\n\n";
#else
		std::getline(std::cin, input);
#endif
		using namespace Lox;
		VM vm;
		using namespace Lox::details;
		vm.defineNative(
		     create_nativeFunc("clock",
				[]() {
					auto stap = std::chrono::high_resolution_clock::now();
					return double(std::chrono::time_point_cast<std::chrono::milliseconds>(stap).time_since_epoch().count());
				}),
			create_nativeFunc("hello", hello)
			);
		vm.interpret(input.c_str());
	}
#endif
#if Compile_chapter == -1
	{
		using namespace Lox::details;
		auto args_count = create_external_function(
			"add", 
			[](double a, double b) {
				return a + b;
			}
		);
	}
#endif
	return 1;
}