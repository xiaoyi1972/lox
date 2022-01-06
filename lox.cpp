#include<iostream>
#include<string>
#include<fstream>
#include"common.h"

#define Compile_chapter 2
#define USE_FILE 1
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
		vm.compile(input.c_str());
		vm.interpret();
	}
#endif
#if Compile_chapter == -1
	int a = 2, b;
	b = a++;
	std::cout << "a:" << a << " b:" << b;
#endif
	return 1;
}