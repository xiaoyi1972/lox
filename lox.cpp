#include<iostream>
#include<string>
#include"common.h"


int main()
{


	//return 1;
	/*
	{ 	//chapter 1
		using namespace Lox;
		Chunk chunk{};
		chunk.write(OpCode::OP_CONSTANT, 123);
		chunk.write(chunk.addConstant(1.2), 123);
		chunk.write(OpCode::OP_NEGATE, 123);
		chunk.write(OpCode::OP_RETURN,123);
	//	Debug::disassembleChunk(chunk, "test chunk");
		VM vm{ chunk };
		vm.interpret();
	}
	*/

	 { 	//chapter 2
		std::string input;
		std::getline(std::cin, input);
		using namespace Lox;
		Chunk chunk{};
		//	Debug::disassembleChunk(chunk, "test chunk");
		VM vm{ chunk };
		vm.compile(input.c_str());
		vm.interpret();
	}

/*	int a = 1, b = 0;  // get ++ advance
	int c = a++ + b;
	std::cout << "c:" << c;
	*/
	return 1;
}