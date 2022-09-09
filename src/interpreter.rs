use crate::ir::{IrBlockIndex, IrBuilder, IrInstruction, IrRegisterSize, IrValue};

macro_rules! binop {
    ($regs:expr, $dest:expr, $left:expr, $right:expr, $op:tt) => {
        {
            use IrValue::*;

            let left_value = $regs[$left.index];
            let right_value = $regs[$right.index];

            let value = match (left_value, right_value) {
                (U8(a), U8(b)) => U8(a $op b),
                (U16(a), U16(b)) => U16(a $op b),
                (U32(a), U32(b)) => U32(a $op b),
                (U64(a), U64(b)) => U64(a $op b),
                (_, _) => unreachable!(),
            };
            $regs[$dest.index] = value;
        }
    };
}

macro_rules! comparison {
    ($regs:expr, $dest:expr, $left:expr, $right:expr, $op:tt) => {
        {
            use IrValue::*;

            let left_value = $regs[$left.index];
            let right_value = $regs[$right.index];

            let value = match (left_value, right_value) {
                (U8(a), U8(b)) => Bool(a $op b),
                (U16(a), U16(b)) => Bool(a $op b),
                (U32(a), U32(b)) => Bool(a $op b),
                (U64(a), U64(b)) => Bool(a $op b),
                (_, _) => unreachable!(),
            };
            $regs[$dest.index] = value;
        }
    };
}

macro_rules! equality {
    ($regs:expr, $dest:expr, $left:expr, $right:expr, $op:tt) => {
        {
            use IrValue::*;

            let left_value = $regs[$left.index];
            let right_value = $regs[$right.index];

            let value = match (left_value, right_value) {
                (U8(a), U8(b)) => Bool(a $op b),
                (U16(a), U16(b)) => Bool(a $op b),
                (U32(a), U32(b)) => Bool(a $op b),
                (U64(a), U64(b)) => Bool(a $op b),
                (Bool(a), Bool(b)) => Bool(a $op b),
                (_, _) => unreachable!(),
            };
            $regs[$dest.index] = value;
        }
    };
}

fn print_printf_string(format_string: &str, args: &[IrValue], builder: &IrBuilder) {
    // TODO: make this less hacky
    let format_specifiers = vec!["%hhu", "%hu", "%u", "%lu", "%llu", "%d", "%s", "%p"];

    let mut split_string = vec![format_string];

    for format_specifier in format_specifiers {
        let mut new_string_parts = vec![];
        for string_part in split_string {
            let mut split_part = string_part.split(format_specifier).collect::<Vec<_>>();
            new_string_parts.append(&mut split_part);
        }
        split_string = new_string_parts;
    }

    assert_eq!(split_string.len() - 1, args.len());

    print!("{}", split_string[0]);
    for i in 0..args.len() {
        print!("{}", args[i].to_string(builder));
        print!("{}", split_string[i + 1]);
    }
}

pub struct Interpreter<'a> {
    ir: &'a IrBuilder,
    registers: Vec<IrValue>,
    arg_stack: Vec<IrValue>,
    call_stack: Vec<(usize, IrBlockIndex)>,
}

enum InstructionExecuteResult {
    Normal,
    ChangeBlock(IrBlockIndex),
    PushBlock(IrBlockIndex),
    PopBlock,
}

impl<'a> Interpreter<'a> {
    pub fn new(ir: &'a IrBuilder, block_index: IrBlockIndex) -> Self {
        Self {
            ir,
            registers: vec![IrValue::Uninitialized(); ir.ir_registers.len()],
            arg_stack: vec![],
            call_stack: vec![(0, block_index)],
        }
    }

    fn execute_instruction(&mut self, instr: &IrInstruction) -> InstructionExecuteResult {
        use IrInstruction::*;
        match instr {
            Mov(dest, src) => {
                self.registers[dest.index] = self.registers[src.index];
            }
            MovImm(dest, value) => {
                self.registers[dest.index] = *value;
            }
            MovZx(dest, src) => {
                use IrRegisterSize::*;
                use IrValue::*;

                let result_value = match (src.size, self.registers[src.index], dest.size) {
                    (Byte, U8(x), Word) => U16(x as u16),
                    (Byte, U8(x), Double) => U32(x as u32),
                    (Byte, U8(x), Quad) => U64(x as u64),
                    (Word, U16(x), Double) => U32(x as u32),
                    (Word, U16(x), Quad) => U64(x as u64),
                    (Double, U32(x), Quad) => U64(x as u64),
                    _ => unreachable!(),
                };

                self.registers[dest.index] = result_value;
            }
            Add(dest, left, right) => binop!(self.registers, dest, left, right, +),
            Sub(dest, left, right) => binop!(self.registers, dest, left, right, -),
            Mul(dest, left, right) => binop!(self.registers, dest, left, right, *),
            Div(dest, left, right) => binop!(self.registers, dest, left, right, /),
            Mod(dest, left, right) => binop!(self.registers, dest, left, right, %),
            Eq(dest, left, right) => equality!(self.registers, dest, left, right, ==),
            Ne(dest, left, right) => equality!(self.registers, dest, left, right, !=),
            Gt(dest, left, right) => comparison!(self.registers, dest, left, right, >),
            GtE(dest, left, right) => comparison!(self.registers, dest, left, right, >=),
            Lt(dest, left, right) => comparison!(self.registers, dest, left, right, <),
            LtE(dest, left, right) => comparison!(self.registers, dest, left, right, <=),
            Shl(_, _, _) => todo!(),
            Shr(_, _, _) => todo!(),
            Jmp(block_index) => {
                return InstructionExecuteResult::ChangeBlock(*block_index);
            }
            CondJmp(true_index, false_index, reg) => {
                use IrValue::*;

                let value = self.registers[reg.index];
                let condition = match value {
                    Bool(x) => x,
                    _ => unreachable!(),
                };

                let new_block = if condition { true_index } else { false_index };

                return InstructionExecuteResult::ChangeBlock(*new_block);
            }
            Push(reg) => {
                self.arg_stack.push(self.registers[reg.index]);
            }
            Pop(reg) => {
                assert!(!self.arg_stack.is_empty());
                self.registers[reg.index] = self.arg_stack.pop().unwrap();
            }
            Call(block_id) => {
                return InstructionExecuteResult::PushBlock(*block_id);
            }
            CallExtern(name, arg_count) => {
                assert!(self.arg_stack.len() >= *arg_count);

                let mut args = (0..*arg_count)
                    .map(|_| self.arg_stack.pop().unwrap())
                    .collect::<Vec<_>>();

                args.reverse();

                match &name[..] {
                    "printf" => {
                        let format_string = args.remove(0);
                        if let IrValue::String(index) = format_string {
                            print_printf_string(&self.ir.strings[index], &args, self.ir);
                        }
                        self.arg_stack.push(IrValue::U64(0));
                    }
                    "exit" => {
                        // TODO: use correct error code here
                        std::process::exit(0);
                    }
                    _ => unreachable!("Unknown external function {}", name),
                }
            }
            Ret() => return InstructionExecuteResult::PopBlock,
        }

        InstructionExecuteResult::Normal
    }

    pub fn execute(&mut self) {
        loop {
            if self.call_stack.is_empty() {
                break;
            }

            let (instruction_index, block_index) = self.call_stack.last_mut().unwrap();
            let block = &self.ir.blocks[*block_index];

            let instruction = &block.instructions[*instruction_index];
            *instruction_index += 1;

            let result = self.execute_instruction(instruction);

            match result {
                InstructionExecuteResult::Normal => {}
                InstructionExecuteResult::ChangeBlock(new_block) => {
                    self.call_stack.pop().unwrap();
                    self.call_stack.push((0, new_block));
                }
                InstructionExecuteResult::PushBlock(new_block) => {
                    self.call_stack.push((0, new_block));
                }
                InstructionExecuteResult::PopBlock => {
                    self.call_stack.pop().unwrap();
                }
            }
        }
    }
}
