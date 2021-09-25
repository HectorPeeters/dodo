use crate::architecture::Architecture;
use crate::instruction::Instruction;
use crate::instructionstream::InstructionStream;
use crate::x86::X86Register;

pub struct Assembler {}

impl Assembler {
    pub fn allocate_registers<C: Copy>(
        stream: InstructionStream<u32, C>,
    ) -> InstructionStream<X86Register, C> {
        use Instruction::*;

        assert!(stream.get_max_registers() <= 4);

        stream.map(|instruction| match instruction {
            MovImm(r, c) => Instruction::MovImm(X86Register::from_index(r), c),
            Ret(r) => Instruction::Ret(X86Register::from_index(r)),
            Add(a, b, c) => Instruction::Add(
                X86Register::from_index(a),
                X86Register::from_index(b),
                X86Register::from_index(c),
            ),
            _ => todo!(),
        })
    }

    pub fn assemble<A: Architecture>(
        stream: InstructionStream<A::OutputRegister, A::Constant>,
    ) -> Vec<u8> {
        let mut result = vec![];
        for instr in stream {
            let bytes: Vec<u8> = A::emit_instruction(instr);
            result.extend(bytes);
        }
        result
    }
}
