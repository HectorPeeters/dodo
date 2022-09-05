use std::fmt::Display;

pub type IrString = usize;
pub type IrBlockIndex = usize;
pub type IrRegister = usize;

pub enum IrValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u32),
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
    Jmp(IrBlockIndex),
    MovImm(IrRegister, IrValue),
    Add(IrRegister, IrRegister, IrRegister),
    Push(IrRegister),
    Pop(IrRegister),
    Call(IrBlockIndex),
    Ret(),
}

impl Display for IrInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrInstruction::*;

        match self {
            Jmp(index) => write!(f, "jmp {}", index),
            MovImm(reg, value) => write!(f, "mov {} {}", reg, value),
            Add(dest, a, b) => write!(f, "add {} {} {}", dest, a, b),
            Push(value) => write!(f, "push {}", value),
            Pop(value) => write!(f, "pop {}", value),
            Call(index) => write!(f, "call {}", index),
            Ret() => write!(f, "ret"),
        }
    }
}

pub struct IrBlock {
    name: String,
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

pub struct IrBuilder {
    blocks: Vec<IrBlock>,
    register_index: IrRegister,
}

impl IrBuilder {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            register_index: 0,
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

    pub fn add_instruction(&mut self, block_index: IrBlockIndex, instr: IrInstruction) {
        self.blocks[block_index].add_instruction(instr);
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
        builder.add_instruction(main_block, IrInstruction::Jmp(target_block));

        assert_eq!(builder.can_reach_blocks(main_block), vec![1]);
        assert_eq!(builder.can_reach_blocks(target_block), vec![]);
    }

    #[test]
    fn cyclic_jump_block() {
        let mut builder = IrBuilder::new();
        let main_block = builder.add_block("main");

        let target_block = builder.add_block("target");
        builder.add_instruction(main_block, IrInstruction::Jmp(target_block));
        builder.add_instruction(target_block, IrInstruction::Jmp(main_block));

        assert_eq!(builder.can_reach_blocks(main_block), vec![target_block]);
        assert_eq!(builder.can_reach_blocks(target_block), vec![main_block]);
    }

    #[test]
    fn add_two_numbers() {
        use IrInstruction::*;

        let mut builder = IrBuilder::new();

        let main_block = builder.add_block("main");

        let a_register = builder.new_register();
        builder.add_instruction(main_block, MovImm(a_register, IrValue::U32(12)));
        let b_register = builder.new_register();
        builder.add_instruction(main_block, MovImm(b_register, IrValue::U32(13)));

        let result_register = builder.new_register();
        builder.add_instruction(main_block, Add(result_register, a_register, b_register));
    }

    #[test]
    fn add_number_function() {
        use IrInstruction::*;

        let mut builder = IrBuilder::new();

        let add_block = builder.add_block("add");

        let a_reg = builder.new_register();
        builder.add_instruction(add_block, Pop(a_reg));

        let b_reg = builder.new_register();
        builder.add_instruction(add_block, Pop(b_reg));

        let result_reg = builder.new_register();
        builder.add_instruction(add_block, Add(result_reg, a_reg, b_reg));

        builder.add_instruction(add_block, Push(result_reg));
        builder.add_instruction(add_block, Ret());

        let main_block = builder.add_block("main");

        let arg1_reg = builder.new_register();
        builder.add_instruction(main_block, MovImm(arg1_reg, IrValue::U32(12)));

        let arg2_reg = builder.new_register();
        builder.add_instruction(main_block, MovImm(arg2_reg, IrValue::U32(13)));

        builder.add_instruction(main_block, Push(arg1_reg));
        builder.add_instruction(main_block, Push(arg2_reg));
        builder.add_instruction(main_block, Call(add_block));

        let final_result_reg = builder.new_register();
        builder.add_instruction(main_block, Pop(final_result_reg));
    }
}
