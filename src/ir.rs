use std::fmt::Display;

pub type IrString = usize;
pub type IrBlockIndex = usize;
pub type IrRegister = usize;

pub enum IrValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Bool(bool),
}

impl Display for IrValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrValue::*;

        match self {
            U8(x) => write!(f, "{}u8", *x),
            U16(x) => write!(f, "{}u16", *x),
            U32(x) => write!(f, "{}u32", *x),
            U64(x) => write!(f, "{}u64", *x),
            Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
        }
    }
}

pub enum IrInstruction {
    Mov(IrRegister, IrRegister),
    MovImm(IrRegister, IrValue),
    Add(IrRegister, IrRegister, IrRegister),
    Gt(IrRegister, IrRegister, IrRegister),
    Jmp(IrBlockIndex),
    JmpNz(IrBlockIndex, IrRegister),
    Push(IrRegister),
    Pop(IrRegister),
    Call(IrBlockIndex),
    CallExtern(String),
    Ret(),
}

impl Display for IrInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrInstruction::*;

        match self {
            Mov(dest_reg, source_reg) => write!(f, "mov {} {}", dest_reg, source_reg),
            MovImm(reg, value) => write!(f, "movimm {} {}", reg, value),
            Add(dest, a, b) => write!(f, "add {} {} {}", dest, a, b),
            Gt(dest, a, b) => write!(f, "gt {} {} {}", dest, a, b),
            Jmp(index) => write!(f, "jmp {}", index),
            JmpNz(index, reg) => write!(f, "jmpnz {} {}", index, reg),
            Push(value) => write!(f, "push {}", value),
            Pop(value) => write!(f, "pop {}", value),
            Call(index) => write!(f, "call {}", index),
            CallExtern(name) => write!(f, "ecall {}", name),
            Ret() => write!(f, "ret"),
        }
    }
}

pub struct IrBlock {
    pub name: String,
    instructions: Vec<IrInstruction>,
}

impl IrBlock {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            instructions: vec![],
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn add_instruction(&mut self, instr: IrInstruction) {
        self.instructions.push(instr);
    }
}

impl Display for IrBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instr in &self.instructions {
            writeln!(f, "{}", instr)?;
        }

        Ok(())
    }
}

pub struct IrBuilder {
    blocks: Vec<IrBlock>,
    register_index: IrRegister,
    block_stack: Vec<IrBlockIndex>,
}

impl Display for IrBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, block) in self.blocks.iter().enumerate() {
            writeln!(f, "==== {}: {} ====", index, block.name)?;
            writeln!(f, "{}\n", block)?;
        }
        Ok(())
    }
}

impl IrBuilder {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            register_index: 0,
            block_stack: vec![],
        }
    }

    pub fn new_register(&mut self) -> IrRegister {
        self.register_index += 1;
        self.register_index - 1
    }

    pub fn add_block(&mut self, name: &str) -> IrBlockIndex {
        let block = IrBlock::new(name);
        self.blocks.push(block);
        self.blocks.len() - 1
    }

    pub fn add_instruction(&mut self, instr: IrInstruction) {
        let block_index = self.current_block();
        self.blocks[block_index].add_instruction(instr);
    }

    pub fn push_block(&mut self, block: IrBlockIndex) {
        assert!(block < self.blocks.len());
        self.block_stack.push(block);
    }

    pub fn pop_block(&mut self) {
        assert!(self.block_stack.len() > 0);
        self.block_stack.remove(self.block_stack.len() - 1);
    }

    fn current_block(&self) -> IrBlockIndex {
        assert!(self.block_stack.len() > 0);
        *self.block_stack.last().unwrap()
    }

    pub fn find_block_index(&mut self, name: &str) -> Option<IrBlockIndex> {
        self.blocks.iter().position(|b| b.name == name)
    }

    pub fn can_reach_blocks(&mut self, block_index: IrBlockIndex) -> Vec<IrBlockIndex> {
        let mut result = vec![];

        for instr in &self.blocks[block_index].instructions {
            match instr {
                IrInstruction::Jmp(target) => result.push(*target),
                IrInstruction::Call(target) => result.push(*target),
                _ => {}
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_block() {
        let mut builder = IrBuilder::new();
        let main_block = builder.add_block("main");
        assert_eq!(main_block, 0);
    }

    #[test]
    fn jump_block() {
        let mut builder = IrBuilder::new();
        let main_block = builder.add_block("main");

        let target_block = builder.add_block("target");
        builder.push_block(main_block);
        builder.add_instruction(IrInstruction::Jmp(target_block));
        builder.pop_block();

        assert_eq!(builder.can_reach_blocks(main_block), vec![1]);
        assert_eq!(builder.can_reach_blocks(target_block), vec![]);
    }

    #[test]
    fn cyclic_jump_block() {
        let mut builder = IrBuilder::new();
        let main_block = builder.add_block("main");

        let target_block = builder.add_block("target");
        builder.push_block(main_block);
        builder.add_instruction(IrInstruction::Jmp(target_block));
        builder.pop_block();
        builder.push_block(target_block);
        builder.add_instruction(IrInstruction::Jmp(main_block));
        builder.pop_block();

        assert_eq!(builder.can_reach_blocks(main_block), vec![target_block]);
        assert_eq!(builder.can_reach_blocks(target_block), vec![main_block]);
    }

    #[test]
    fn add_two_numbers() {
        use IrInstruction::*;

        let mut builder = IrBuilder::new();

        let main_block = builder.add_block("main");

        builder.push_block(main_block);
        let a_register = builder.new_register();
        builder.add_instruction(MovImm(a_register, IrValue::U32(12)));
        let b_register = builder.new_register();
        builder.add_instruction(MovImm(b_register, IrValue::U32(13)));

        let result_register = builder.new_register();
        builder.add_instruction(Add(result_register, a_register, b_register));
        builder.pop_block();
    }

    #[test]
    fn add_number_function() {
        use IrInstruction::*;

        let mut builder = IrBuilder::new();

        let add_block = builder.add_block("add");
        builder.push_block(add_block);

        let a_reg = builder.new_register();
        builder.add_instruction(Pop(a_reg));

        let b_reg = builder.new_register();
        builder.add_instruction(Pop(b_reg));

        let result_reg = builder.new_register();
        builder.add_instruction(Add(result_reg, a_reg, b_reg));

        builder.add_instruction(Push(result_reg));
        builder.add_instruction(Ret());

        builder.pop_block();

        let main_block = builder.add_block("main");
        builder.push_block(main_block);

        let arg1_reg = builder.new_register();
        builder.add_instruction(MovImm(arg1_reg, IrValue::U32(12)));

        let arg2_reg = builder.new_register();
        builder.add_instruction(MovImm(arg2_reg, IrValue::U32(13)));

        builder.add_instruction(Push(arg1_reg));
        builder.add_instruction(Push(arg2_reg));
        builder.add_instruction(Call(add_block));

        let final_result_reg = builder.new_register();
        builder.add_instruction(Pop(final_result_reg));

        builder.pop_block();
    }
}
