#pragma once
#include <iostream>

#include "token.h"

namespace Lox {
	struct Debug {
		static std::size_t simpleInstruction(const char* name, std::size_t offset) {
			std::cout << name << "\n";
			return offset + 1;
		}

		static std::size_t constantInstruction(const char* name, Chunk& chunk,
			std::size_t offset) {
			auto constant = std::get<std::size_t>(chunk.code[offset + 1]);
			std::cout << string_format("%-17s %4d '", name, constant);
			std::cout << chunk.values[constant] << "'\n";
			return offset + 2;
		}

		static std::size_t byteInstruction(const char* name, Chunk& chunk,
			int offset) {
			auto slot = std::get<std::size_t>(chunk.code[offset + 1]);
			std::cout << string_format("%-17s %4d \n", name, slot);
			return offset + 2;
		}

		static int jumpInstruction(const char* name, int sign, Chunk& chunk,
			int offset) {
			auto jump = std::get<size_t>(chunk.code[offset + 1]);
			std::cout << string_format("%-17s %4d -> %d\n", name, offset,
				offset + 2 + sign * jump);
			return offset + 2;
		}

		static int incInstruction(const char* name, Chunk& chunk, int offset) {
			auto arg = std::get<std::size_t>(chunk.code[offset + 1]);
			auto prefix = (arg & 2) >> 1, neg = arg & 1;
			std::cout << string_format("%-17s %4s %s\n", name, neg ? "-" : "+",
				prefix ? "prefix" : "suffix");
			return offset + 2;
		}

		static int invokeInstruction(const char* name, Chunk& chunk, int offset) {
			auto constant = std::get<std::size_t>(chunk.code[offset + 1]),
				argCount = std::get<std::size_t>(chunk.code[offset + 2]);
			std::cout << string_format("%-19s (%d args) %4d '", name, argCount,
				constant)
				<< chunk.values[constant] << "\n";
			return offset + 3;
		}

		static std::size_t disassembleInstruction(Chunk& chunk, std::size_t offset) {
			std::cout << string_format("%04d ", offset);
			if (offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1])
				std::cout << "   | ";
			else
				std::cout << string_format("%4d ", chunk.lines[offset]);
			auto instruction = chunk.code[offset];
			if (std::holds_alternative<OpCode>(instruction)) {
				switch (std::get<OpCode>(instruction)) {
				case OpCode::NEGATE:
					return simpleInstruction("OP_NEGATE", offset);
				case OpCode::PRINT:
					return simpleInstruction("OP_PRINT", offset);
				case OpCode::JUMP:
					return jumpInstruction("OP_JUMP", 1, chunk, offset);
				case OpCode::JUMP_IF_FALSE:
					return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
				case OpCode::LOOP:
					return jumpInstruction("OP_LOOP", -1, chunk, offset);
				case OpCode::CLOSE_UPVALUE:
					return simpleInstruction("OP_CLOSE_UPVALUE", offset);
				case OpCode::RETURN:
					return simpleInstruction("OP_RETURN", offset);
				case OpCode::CONSTANT:
					return constantInstruction("OP_CONSTANT", chunk, offset);
				case OpCode::NIL:
					return simpleInstruction("OP_NIL", offset);
				case OpCode::TRUE:
					return simpleInstruction("OP_TRUE", offset);
				case OpCode::FALSE:
					return simpleInstruction("OP_FALSE", offset);
				case OpCode::GET_LOCAL:
					return byteInstruction("OP_GET_LOCAL", chunk, offset);
				case OpCode::SET_LOCAL:
					return byteInstruction("OP_SET_LOCAL", chunk, offset);
				case OpCode::GET_GLOBAL:
					return constantInstruction("OP_GET_GLOBAL", chunk, offset);
				case OpCode::DEFINE_GLOBAL:
					return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
				case OpCode::SET_GLOBAL:
					return constantInstruction("OP_SET_GLOBAL", chunk, offset);
				case OpCode::POP:
					return simpleInstruction("OP_POP", offset);
				case OpCode::EQUAL:
					return simpleInstruction("OP_EQUAL", offset);
				case OpCode::GREATER:
					return simpleInstruction("OP_GREATER", offset);
				case OpCode::LESS:
					return simpleInstruction("OP_LESS", offset);
				case OpCode::ADD:
					return simpleInstruction("OP_ADD", offset);
				case OpCode::SUBTRACT:
					return simpleInstruction("OP_SUBTRACT", offset);
				case OpCode::MULTIPLY:
					return simpleInstruction("OP_MULTIPLY", offset);
				case OpCode::DIVIDE:
					return simpleInstruction("OP_DIVIDE", offset);
				case OpCode::NOT:
					return simpleInstruction("OP_NOT", offset);
				case OpCode::INC:
					return incInstruction("OP_INC", chunk, offset);
				case OpCode::CALL:
					return byteInstruction("OP_CALL", chunk, offset);
				case OpCode::GET_UPVALUE:
					return byteInstruction("OP_GET_UPVALUE", chunk, offset);
				case OpCode::SET_UPVALUE:
					return byteInstruction("OP_SET_UPVALUE", chunk, offset);
				case OpCode::CLOSURE: {
					offset++;
					auto constant = std::get<std::size_t>(chunk.code[offset++]);
					std::cout << string_format("%-17s %4d ", "OP_CLOSURE", constant)
						<< (chunk.values[constant]) << "\n";
					auto function = std::get<std::shared_ptr<Fn>>(chunk.values[constant]);
					for (auto j = 0; j < function->upvalueCount; j++) {
						auto isLocal = std::get<std::size_t>(chunk.code[offset++]);
						auto index = std::get<std::size_t>(chunk.code[offset++]);
						std::cout << string_format("%04d      |                      %s %d\n",
							offset - 2, isLocal ? "local" : "upvalue",
							index);
					}
					return offset;
				}
				case OpCode::CLASS:
					return constantInstruction("OP_CLASS", chunk, offset);
				case OpCode::GET_PROPERTY:
					return constantInstruction("OP_GET_PROPERTY", chunk, offset);
				case OpCode::SET_PROPERTY:
					return constantInstruction("OP_SET_PROPERTY", chunk, offset);
				case OpCode::METHOD:
					return constantInstruction("OP_METHOD", chunk, offset);
				case OpCode::INVOKE:
					return invokeInstruction("OP_INVOKE", chunk, offset);
				case OpCode::INHERIT:
					return simpleInstruction("OP_INHERIT", offset);
				case OpCode::GET_SUPER:
					return constantInstruction("OP_GET_SUPER", chunk, offset);
				case OpCode::SUPER_INVOKE:
					return invokeInstruction("OP_SUPER_INVOKE", chunk, offset);
				default:
					std::cout << "unknown opcode"
						<< static_cast<int>(std::get<OpCode>(instruction));
					return offset + 1;
				}
			}
			else {
				std::cout << "OPCODE ARGS("
					<< static_cast<int>(std::get<size_t>(instruction)) << ")\n";
				std::cout << "\n";
				return offset + 1;
			}
		}

		static void disassembleChunk(Chunk& chunk, const char* name) {
			std::cout << "== " << name << " == \n";
			for (std::size_t offset = 0; offset < chunk.code.size();) {
				offset = disassembleInstruction(chunk, offset);
			}
		}
	};
}
