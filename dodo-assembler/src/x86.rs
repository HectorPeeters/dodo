use crate::architecture::Architecture;
use crate::instruction::Instruction;

pub struct X86 {}

#[derive(Clone, Copy, Debug)]
pub enum X86Register {
    Eax,
    Ecx,
    Edx,
    Ebx,
    Esp,
    Ebp,
    Esi,
    Edi,
}

impl X86Register {
    pub fn as_byte(&self) -> u8 {
        *self as u8
    }

    pub fn from_index(i: u32) -> Self {
        use X86Register::*;

        match i {
            0 => Eax,
            1 => Ecx,
            2 => Edx,
            3 => Ebx,
            _ => todo!(),
        }
    }
}

impl X86 {
    fn gen_add_reg_reg(left: &X86Register, right: &X86Register) -> Vec<u8> {
        vec![0x01, 0xc0 + (right.as_byte() + left.as_byte() * 8)]
    }

    fn gen_mov_reg_reg(from: &X86Register, to: &X86Register) -> Vec<u8> {
        vec![0x89, 0xc0 + (to.as_byte() + from.as_byte() * 8)]
    }
}

impl Architecture for X86 {
    type OutputRegister = X86Register;
    type Constant = u32;

    fn emit_instruction(instr: Instruction<Self::OutputRegister, Self::Constant>) -> Vec<u8> {
        println!("{:?}", instr);
        use Instruction::*;
        match instr {
            MovImm(r, c) => {
                let mut result = vec![0xb8 + r.as_byte()];
                result.extend(c.to_le_bytes().to_vec());
                result
            }
            Ret(r) => {
                let mut result = X86::gen_mov_reg_reg(&r, &X86Register::Edi);
                result.append(&mut vec![
                    0xb8, 0x3c, 0x00, 0x00, 0x00, // mov $60, %eax
                    0x0f, 0x05, // syscall
                ]);
                result
            }
            Add(left, right, dest) => {
                let mut result = X86::gen_add_reg_reg(&left, &right);
                result.extend_from_slice(&X86::gen_mov_reg_reg(&right, &dest));
                result
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assembler::Assembler, instructionstream::InstructionStream};
    use Instruction::*;
    use X86Register::*;

    #[test]
    fn x86_mov_imm() {
        let mut stream = InstructionStream::new();
        for reg in [Eax, Ecx, Edx, Ebx, Esi, Edi, Esp, Ebp] {
            stream.instr(MovImm(reg, 0x12345678));
        }
        let assembled = Assembler::assemble::<X86>(stream);
        assert_eq!(
            assembled,
            vec![
                0xb8, 0x78, 0x56, 0x34, 0x12, // eax
                0xb9, 0x78, 0x56, 0x34, 0x12, // ecx
                0xba, 0x78, 0x56, 0x34, 0x12, // edx
                0xbb, 0x78, 0x56, 0x34, 0x12, // ebx
                0xbe, 0x78, 0x56, 0x34, 0x12, // esi
                0xbf, 0x78, 0x56, 0x34, 0x12, // edi
                0xbc, 0x78, 0x56, 0x34, 0x12, // esp
                0xbd, 0x78, 0x56, 0x34, 0x12, // ebp
            ]
        );
    }

    #[test]
    fn x86_add_reg() {
        let registers_from = [
            X86Register::Eax,
            X86Register::Ecx,
            X86Register::Edx,
            X86Register::Ebx,
            X86Register::Esp,
            X86Register::Ebp,
            X86Register::Esi,
            X86Register::Edi,
        ];
        let registers_to = [X86Register::Eax, X86Register::Ecx];

        let mut result = vec![];

        for to in registers_to {
            for from in registers_from {
                result.extend_from_slice(&X86::gen_add_reg_reg(&from, &to));
            }
        }

        assert_eq!(
            result,
            vec![
                0x01, 0xc0, 0x01, 0xc1, 0x01, 0xc2, 0x01, 0xc3, 0x01, 0xc4, 0x01, 0xc5, 0x01, 0xc6,
                0x01, 0xc7, 0x01, 0xc8, 0x01, 0xc9, 0x01, 0xca, 0x01, 0xcb, 0x01, 0xcc, 0x01, 0xcd,
                0x01, 0xce, 0x01, 0xcf
            ]
        );
    }
}
