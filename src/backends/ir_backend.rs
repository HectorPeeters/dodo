use super::Backend;
use crate::ast::{BinaryOperatorType, Expression, Statement};
use crate::error::Result;
use crate::ir::{IrBuilder, IrInstruction, IrRegister, IrValue};
use crate::project::{
    BUILTIN_TYPE_U16, BUILTIN_TYPE_U32, BUILTIN_TYPE_U64, BUILTIN_TYPE_U8, BUILTIN_TYPE_VOID,
};
use crate::scope::Scope;
use crate::{ast::UpperStatement, project::Project};
use std::path::Path;

pub struct IrBackend<'a> {
    project: &'a mut Project,
    builder: IrBuilder,
    scope: Scope<IrRegister>,
}

impl<'a> IrBackend<'a> {
    pub fn new(project: &'a mut Project) -> Self {
        Self {
            project,
            builder: IrBuilder::new(),
            scope: Scope::new(),
        }
    }

    fn gen_statement(&mut self, statement: Statement) -> Result<()> {
        match statement {
            Statement::Block(children, scoped, _) => {
                if scoped {
                    self.scope.push();
                }

                for child in children {
                    self.gen_statement(child)?;
                }

                if scoped {
                    self.scope.pop();
                }

                Ok(())
            }
            Statement::Declaration(name, value_type, range) => {
                let destination_reg = self
                    .builder
                    .new_register(self.project.get_type_size(value_type).into());

                self.scope
                    .insert(&name, destination_reg)
                    .map_err(|e| e.with_range(range))
            }
            Statement::Assignment(lhs, rhs, _) => {
                if let Expression::VariableRef(name, _, range) = lhs {
                    let value_reg = self.gen_expression(rhs)?;

                    let result_reg = self.scope.find(&name).map_err(|e| e.with_range(range))?;

                    self.builder
                        .add_instruction(IrInstruction::Mov(result_reg, value_reg));

                    return Ok(());
                }

                todo!()
            }
            Statement::Expression(expr, _) => self.gen_expression(expr).map(|_| ()),
            Statement::While(condition, body, _) => {
                let condition_reg = self.gen_expression(condition)?;
                let condition_block = self.builder.add_block("condition_block");
                let while_body_block = self.builder.add_block("while_block");
                let continue_block = self.builder.add_block("continue_block");

                self.builder
                    .add_instruction(IrInstruction::Jmp(condition_block));

                self.builder.push_block(condition_block);
                self.builder
                    .add_instruction(IrInstruction::JmpNz(continue_block, condition_reg));
                self.builder
                    .add_instruction(IrInstruction::Jmp(while_body_block));
                self.builder.pop_block();
                self.builder.pop_block();

                self.builder.push_block(while_body_block);

                self.gen_statement(*body)?;
                self.builder
                    .add_instruction(IrInstruction::Jmp(condition_block));

                self.builder.pop_block();

                self.builder.push_block(continue_block);

                Ok(())
            }
            Statement::If(condition, body, else_body, _) => {
                if let Some(_) = else_body {
                    todo!();
                }

                let condition_reg = self.gen_expression(condition)?;
                let if_body_block = self.builder.add_block("if_block");
                let continue_block = self.builder.add_block("continue_block");

                self.builder
                    .add_instruction(IrInstruction::JmpNz(continue_block, condition_reg));
                self.builder
                    .add_instruction(IrInstruction::Jmp(if_body_block));
                self.builder.pop_block();

                self.builder.push_block(if_body_block);

                self.gen_statement(*body)?;
                self.builder
                    .add_instruction(IrInstruction::Jmp(continue_block));

                self.builder.pop_block();

                self.builder.push_block(continue_block);

                Ok(())
            }
            Statement::Return(value, _) => {
                let value_reg = self.gen_expression(value)?;
                self.builder.add_instruction(IrInstruction::Push(value_reg));
                self.builder.add_instruction(IrInstruction::Ret());

                Ok(())
            }
        }
    }

    fn gen_expression(&mut self, expression: Expression) -> Result<IrRegister> {
        match expression {
            Expression::BinaryOperator(op, left, right, result_type, _) => {
                let left_reg = self.gen_expression(*left)?;
                let right_reg = self.gen_expression(*right)?;

                let destination_reg = self
                    .builder
                    .new_register(self.project.get_type_size(result_type).into());

                let instruction = match op {
                    BinaryOperatorType::Add => {
                        IrInstruction::Add(destination_reg, left_reg, right_reg)
                    }
                    BinaryOperatorType::Subtract => {
                        IrInstruction::Sub(destination_reg, left_reg, right_reg)
                    }
                    BinaryOperatorType::Multiply => todo!(),
                    BinaryOperatorType::Divide => todo!(),
                    BinaryOperatorType::Modulo => todo!(),
                    BinaryOperatorType::ShiftLeft => todo!(),
                    BinaryOperatorType::ShiftRight => todo!(),
                    BinaryOperatorType::Equal => todo!(),
                    BinaryOperatorType::NotEqual => todo!(),
                    BinaryOperatorType::LessThan => {
                        IrInstruction::Lt(destination_reg, left_reg, right_reg)
                    }
                    BinaryOperatorType::LessThanEqual => {
                        IrInstruction::LtE(destination_reg, left_reg, right_reg)
                    }
                    BinaryOperatorType::GreaterThan => {
                        IrInstruction::Gt(destination_reg, left_reg, right_reg)
                    }
                    BinaryOperatorType::GreaterThanEqual => todo!(),
                };

                self.builder.add_instruction(instruction);

                Ok(destination_reg)
            }
            Expression::UnaryOperator(_, _, _, _) => todo!(),
            Expression::FunctionCall(name, args, return_type, _) => {
                for arg in args {
                    let arg_reg = self.gen_expression(arg)?;
                    self.builder.add_instruction(IrInstruction::Push(arg_reg));
                }

                let function_block_id = self.builder.find_block_index(&name);

                if let Some(function_block_id) = function_block_id {
                    self.builder
                        .add_instruction(IrInstruction::Call(function_block_id));
                } else {
                    self.builder
                        .add_instruction(IrInstruction::CallExtern(name));
                }

                if return_type != BUILTIN_TYPE_VOID {
                    let result_reg = self
                        .builder
                        .new_register(self.project.get_type_size(return_type).into());
                    self.builder.add_instruction(IrInstruction::Pop(result_reg));
                    Ok(result_reg)
                } else {
                    let result_reg = self.builder.new_register(64.into());
                    Ok(result_reg)
                }
            }
            Expression::IntegerLiteral(value, value_type, _) => {
                let result_reg = self
                    .builder
                    .new_register(self.project.get_type_size(value_type).into());

                let value = match value_type {
                    BUILTIN_TYPE_U8 => IrValue::U8(value as u8),
                    BUILTIN_TYPE_U16 => IrValue::U16(value as u16),
                    BUILTIN_TYPE_U32 => IrValue::U32(value as u32),
                    BUILTIN_TYPE_U64 => IrValue::U64(value),
                    _ => unreachable!(),
                };

                self.builder
                    .add_instruction(IrInstruction::MovImm(result_reg, value));

                Ok(result_reg)
            }
            Expression::BooleanLiteral(_, _, _) => todo!(),
            Expression::VariableRef(name, _, range) => {
                self.scope.find(&name).map_err(|e| e.with_range(range))
            }
            Expression::StringLiteral(value, _, _) => Ok(self.builder.add_string(value)),
            Expression::StructLiteral(_, _, _) => todo!(),
            Expression::FieldAccessor(_, _, _, _) => todo!(),
            // TODO: this probably isn't sufficient
            Expression::Widen(expr, target_type, _) => {
                let result_reg = self
                    .builder
                    .new_register(self.project.get_type_size(target_type).into());
                let expr_reg = self.gen_expression(*expr)?;

                self.builder
                    .add_instruction(IrInstruction::Mov(result_reg, expr_reg));

                Ok(result_reg)
            }
            Expression::Cast(_, _, _) => todo!(),
        }
    }
}

impl<'a> Backend for IrBackend<'a> {
    fn process_upper_statement(&mut self, statement: UpperStatement) -> Result<()> {
        match statement {
            UpperStatement::Function(name, params, _, body, _, range) => {
                let function_block = self.builder.add_block(&name);

                self.builder.push_block(function_block);
                self.scope.push();

                for (param_name, param_type) in params.into_iter().rev() {
                    let param_reg = self
                        .builder
                        .new_register(self.project.get_type_size(param_type).into());
                    self.builder.add_instruction(IrInstruction::Pop(param_reg));

                    self.scope
                        .insert(&param_name, param_reg)
                        .map_err(|e| e.with_range(range))?;
                }

                self.gen_statement(*body)?;

                self.scope.pop();
                self.builder.pop_block();

                Ok(())
            }
            UpperStatement::StructDeclaratin(_, _) => todo!(),
            UpperStatement::ConstDeclaration(_, _, _, _, _) => todo!(),
            UpperStatement::ExternDeclaration(_, _) => Ok(()),
        }
    }

    fn finalize(&mut self, _output: &Path, _dont_compile: bool) -> Result<()> {
        eprintln!("{}", self.builder);

        println!("{}", self.builder.get_dot_graph());

        Ok(())
    }

    fn name(&self) -> &'static str {
        "ir"
    }
}
