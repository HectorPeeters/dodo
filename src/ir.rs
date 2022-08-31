use crate::{
    ast::{BinaryOperatorType, ConsumingAstVisitor, Expression, Statement},
    error::Result,
    scope::Scope,
    types::Type,
};

pub type IrReg = usize;
pub type IrValue = u64;
pub type IrLocation = usize;
pub type IrStringIndex = usize;

#[derive(Debug, Clone)]
pub enum IrInstruction {
    Mov(IrReg, IrReg),
    MovImm(IrReg, IrValue),
    Add(IrReg, IrReg, IrReg),
    Sub(IrReg, IrReg, IrReg),
    Ret(IrReg),
    Jmp(IrLocation),
    Call(IrStringIndex),
}

pub struct IrBlock {
    name: String,
    instructions: Vec<IrInstruction>,
}

impl IrBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instructions: vec![],
        }
    }

    pub fn add(&mut self, instr: IrInstruction) {
        self.instructions.push(instr);
    }
}

pub struct IrBuilder {
    blocks: Vec<IrBlock>,
    strings: Vec<String>,
    scope: Scope<IrReg>,
    index: usize,
}

impl IrBuilder {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            strings: vec![],
            scope: Scope::new(),
            index: 0,
        }
    }

    pub fn new_block(&mut self, name: String) -> &mut IrBlock {
        let block = IrBlock::new(name);
        self.blocks.push(block);
        self.blocks.last_mut().unwrap()
    }

    pub fn add_string(&mut self, value: String) -> usize {
        self.strings.push(value);
        self.strings.len() - 1
    }

    pub fn current_block(&mut self) -> &mut IrBlock {
        self.blocks.last_mut().unwrap()
    }

    pub fn new_reg(&mut self) -> IrReg {
        let reg = self.index;
        self.index += 1;
        reg
    }

    pub fn print_blocks(&self) {
        for block in &self.blocks {
            println!("=== {} ===", block.name);
            println!("{:?}", block.instructions);
        }
    }
}

impl Default for IrBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ConsumingAstVisitor<Type, (), IrReg> for IrBuilder {
    fn visit_statement(&mut self, statement: Statement<Type>) -> Result<()> {
        use IrInstruction::*;

        match statement {
            Statement::Block(statements, scoped, _, _) => {
                if scoped {
                    self.scope.push();
                }

                for stmt in statements {
                    self.visit_statement(stmt)?;
                }

                if scoped {
                    self.scope.pop();
                }
                Ok(())
            }
            Statement::Declaration(_, _, _, _) => todo!(),
            Statement::Assignment(_, _, _, _) => todo!(),
            Statement::Expression(expr, _, _) => {
                self.visit_expression(expr)?;
                Ok(())
            }
            Statement::While(_, _, _, _) => todo!(),
            Statement::If(_cond, _body, _, _) => todo!(),
            Statement::Return(expr, _, _) => {
                let expr_reg = self.visit_expression(expr)?;
                let block = self.current_block();

                block.add(Ret(expr_reg));

                Ok(())
            }
            Statement::Function(name, args, _return_type, body, _annotations, _, range) => {
                let _fn_block = self.new_block(format!("fn_{}", name));

                self.scope.push();

                // Add scope entries for all the function arguments
                for (arg_name, _) in &args {
                    let arg_index = self.index;
                    self.index += 1;
                    self.scope
                        .insert(arg_name, arg_index)
                        .map_err(|x| x.with_range(range))?;
                }

                self.visit_statement(*body)?;

                self.scope.pop();

                Ok(())
            }
        }
    }

    fn visit_expression(&mut self, expression: Expression<Type>) -> Result<IrReg> {
        use IrInstruction::*;

        match expression {
            Expression::BinaryOperator(op, left, right, _, _) => {
                let left_reg = self.visit_expression(*left)?;
                let right_reg = self.visit_expression(*right)?;
                assert_eq!(op, BinaryOperatorType::Add);

                let result_reg = self.new_reg();
                let block = self.current_block();
                block.add(Add(left_reg, right_reg, result_reg));

                Ok(result_reg)
            }
            Expression::UnaryOperator(_, _, _, _) => todo!(),
            Expression::FunctionCall(_, _, _, _) => Ok(100000),
            Expression::Literal(value, _, _) => {
                let reg = self.index;
                self.index += 1;

                let block = self.current_block();
                block.add(MovImm(reg, value));

                Ok(reg)
            }
            Expression::VariableRef(name, _, range) => {
                self.scope.find(&name).map_err(|x| x.with_range(range))
            }
            Expression::StringLiteral(_, _, _) => todo!(),
            Expression::Widen(_, _, _) => todo!(),
        }
    }
}
