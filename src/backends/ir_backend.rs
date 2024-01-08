use super::Backend;
use crate::ast::{
    AssignmentStatement, BinaryOperatorType, BlockStatement, BooleanLiteralExpr, ConstDeclaration,
    DeclarationStatement, Expression, FunctionCallExpr, FunctionDeclaration, IfStatement,
    IntegerLiteralExpr, Statement, StringLiteralExpr, VariableRefExpr, WhileStatement, WidenExpr,
};
use crate::error::Result;
use crate::interpreter::Interpreter;
use crate::ir::{IrBlockIndex, IrBuilder, IrInstruction, IrRegister, IrRegisterSize, IrValue};
use crate::project::{
    BUILTIN_TYPE_BOOL, BUILTIN_TYPE_U16, BUILTIN_TYPE_U32, BUILTIN_TYPE_U64, BUILTIN_TYPE_U8,
    BUILTIN_TYPE_VOID,
};
use crate::scope::Scope;
use crate::types::{Type, TypeId};
use crate::{ast::UpperStatement, project::Project};
use std::path::Path;

#[derive(Debug, Copy, Clone)]
enum IrScopeLocation {
    Reg(IrRegister),
    Global(usize),
}

pub struct IrBackend<'a> {
    project: &'a mut Project,
    builder: IrBuilder,
    scope: Scope<IrScopeLocation>,
    global_consts: Vec<IrValue>,
    main_block: Option<IrBlockIndex>,
}

impl<'a, 'b> IrBackend<'a> {
    pub fn new(project: &'a mut Project) -> Self {
        Self {
            project,
            builder: IrBuilder::new(),
            scope: Scope::new(),
            global_consts: vec![],
            main_block: None,
        }
    }

    fn gen_statement(&mut self, statement: Statement<'b>) -> Result<()> {
        match statement {
            Statement::Block(BlockStatement {
                children,
                scoped,
                range: _,
            }) => {
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
            Statement::Declaration(DeclarationStatement {
                name,
                type_id,
                range,
            }) => {
                let destination_reg = self
                    .builder
                    .new_register(self.project.get_type_size(type_id).into());

                self.scope
                    .insert(name, IrScopeLocation::Reg(destination_reg))
                    .map_err(|e| e.with_range(range))
            }
            Statement::Assignment(AssignmentStatement {
                left,
                right,
                range: _,
            }) => {
                if let Expression::VariableRef(VariableRefExpr {
                    name,
                    type_id: _,
                    range,
                }) = left
                {
                    let value_reg = self.gen_expression(right)?;

                    let result_location = self.scope.find(name).map_err(|e| e.with_range(range))?;

                    if let IrScopeLocation::Reg(result_reg) = result_location {
                        self.builder
                            .add_instruction(IrInstruction::Mov(result_reg, value_reg));
                    } else {
                        unreachable!()
                    }

                    return Ok(());
                }

                todo!()
            }
            Statement::Expression(expr_stmt) => self.gen_expression(expr_stmt.expr).map(|_| ()),
            Statement::While(WhileStatement {
                condition,
                body,
                range: _,
            }) => {
                let condition_block = self.builder.add_block("condition_block");
                let while_body_block = self.builder.add_block("while_block");
                let continue_block = self.builder.add_block("continue_block");

                self.builder
                    .add_instruction(IrInstruction::Jmp(condition_block));

                self.builder.push_block(condition_block);
                let condition_reg = self.gen_expression(condition)?;
                self.builder.add_instruction(IrInstruction::CondJmp(
                    while_body_block,
                    continue_block,
                    condition_reg,
                ));
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
            Statement::If(IfStatement {
                condition,
                if_body,
                else_body,
                range: _,
            }) => {
                let condition_reg = self.gen_expression(condition)?;
                let if_body_block = self.builder.add_block("if_block");
                let continue_block = self.builder.add_block("continue_block");

                if let Some(else_body) = else_body {
                    let else_body_block = self.builder.add_block("if_block");

                    self.builder.add_instruction(IrInstruction::CondJmp(
                        if_body_block,
                        else_body_block,
                        condition_reg,
                    ));
                    self.builder.pop_block();

                    self.builder.push_block(if_body_block);

                    self.gen_statement(*if_body)?;
                    self.builder
                        .add_instruction(IrInstruction::Jmp(continue_block));

                    self.builder.pop_block();

                    self.builder.push_block(else_body_block);

                    self.gen_statement(*else_body)?;
                    self.builder
                        .add_instruction(IrInstruction::Jmp(continue_block));

                    self.builder.pop_block();
                } else {
                    self.builder.add_instruction(IrInstruction::CondJmp(
                        if_body_block,
                        continue_block,
                        condition_reg,
                    ));
                    self.builder.pop_block();

                    self.builder.push_block(if_body_block);

                    self.gen_statement(*if_body)?;
                    self.builder
                        .add_instruction(IrInstruction::Jmp(continue_block));

                    self.builder.pop_block();
                }

                self.builder.push_block(continue_block);

                Ok(())
            }
            Statement::Return(ret_stmt) => {
                let value_reg = self.gen_expression(ret_stmt.expr)?;
                self.builder.add_instruction(IrInstruction::Push(value_reg));
                self.builder.add_instruction(IrInstruction::Ret());

                Ok(())
            }
        }
    }

    fn gen_constant(
        &mut self,
        expression: Expression<'b>,
        expression_type: TypeId,
    ) -> Result<IrValue> {
        use IrValue::*;

        match expression {
            Expression::IntegerLiteral(IntegerLiteralExpr {
                value,
                type_id,
                range: _,
            }) => {
                assert_eq!(expression_type, type_id);

                match type_id {
                    BUILTIN_TYPE_U8 => Ok(U8(value as u8)),
                    BUILTIN_TYPE_U16 => Ok(U16(value as u16)),
                    BUILTIN_TYPE_U32 => Ok(U32(value as u32)),
                    BUILTIN_TYPE_U64 => Ok(U64(value)),
                    _ => unreachable!(),
                }
            }
            Expression::BooleanLiteral(BooleanLiteralExpr {
                value,
                type_id,
                range: _,
            }) => {
                assert_eq!(expression_type, BUILTIN_TYPE_BOOL);
                assert_eq!(type_id, BUILTIN_TYPE_BOOL);

                Ok(IrValue::Bool(value))
            }
            Expression::StringLiteral(StringLiteralExpr {
                value,
                type_id,
                range: _,
            }) => {
                let u8_ptr = self.project.find_or_add_type(Type::Ptr(BUILTIN_TYPE_U8));
                assert_eq!(expression_type, u8_ptr);
                assert_eq!(type_id, u8_ptr,);

                let index = self.builder.add_string(value.to_string());

                Ok(String(index))
            }
            Expression::StructLiteral(_) => todo!(),
            Expression::Widen(WidenExpr {
                expr,
                type_id,
                range: _,
            }) => {
                let inner_type = expr.type_id();
                let inner_value = self.gen_constant(*expr, inner_type)?;

                let result_value = match (inner_value, type_id) {
                    (U8(x), BUILTIN_TYPE_U16) => U16(x as u16),
                    (U8(x), BUILTIN_TYPE_U32) => U32(x as u32),
                    (U8(x), BUILTIN_TYPE_U64) => U64(x as u64),
                    (U16(x), BUILTIN_TYPE_U32) => U32(x as u32),
                    (U16(x), BUILTIN_TYPE_U64) => U64(x as u64),
                    (U32(x), BUILTIN_TYPE_U64) => U64(x as u64),
                    _ => unreachable!(),
                };

                Ok(result_value)
            }
            _ => unreachable!(),
        }
    }

    fn gen_expression(&mut self, expression: Expression<'b>) -> Result<IrRegister> {
        match expression {
            Expression::BinaryOperator(binop) => {
                let left_reg = self.gen_expression(*binop.left)?;
                let right_reg = self.gen_expression(*binop.right)?;

                let destination_reg = self
                    .builder
                    .new_register(self.project.get_type_size(binop.type_id).into());

                let instruction = match binop.op_type {
                    BinaryOperatorType::Add => IrInstruction::Add,
                    BinaryOperatorType::Subtract => IrInstruction::Sub,
                    BinaryOperatorType::Multiply => IrInstruction::Mul,
                    BinaryOperatorType::Divide => IrInstruction::Div,
                    BinaryOperatorType::Modulo => IrInstruction::Mod,
                    BinaryOperatorType::ShiftLeft => IrInstruction::Shl,
                    BinaryOperatorType::ShiftRight => IrInstruction::Shr,
                    BinaryOperatorType::Equal => IrInstruction::Eq,
                    BinaryOperatorType::NotEqual => IrInstruction::Ne,
                    BinaryOperatorType::LessThan => IrInstruction::Lt,
                    BinaryOperatorType::LessThanEqual => IrInstruction::LtE,
                    BinaryOperatorType::GreaterThan => IrInstruction::Gt,
                    BinaryOperatorType::GreaterThanEqual => IrInstruction::GtE,
                    _ => todo!(),
                };

                self.builder
                    .add_instruction(instruction(destination_reg, left_reg, right_reg));

                Ok(destination_reg)
            }
            Expression::UnaryOperator(_) => todo!(),
            Expression::FunctionCall(FunctionCallExpr {
                name,
                args,
                type_id,
                range: _,
            }) => {
                let arg_count = args.len();
                for arg in args {
                    let arg_reg = self.gen_expression(arg)?;
                    self.builder.add_instruction(IrInstruction::Push(arg_reg));
                }

                let function_block_id = self.builder.find_block_index(name);

                if let Some(function_block_id) = function_block_id {
                    self.builder
                        .add_instruction(IrInstruction::Call(function_block_id));
                } else {
                    self.builder
                        .add_instruction(IrInstruction::CallExtern(name.to_string(), arg_count));
                }

                if type_id != BUILTIN_TYPE_VOID {
                    let result_reg = self
                        .builder
                        .new_register(self.project.get_type_size(type_id).into());
                    self.builder.add_instruction(IrInstruction::Pop(result_reg));
                    Ok(result_reg)
                } else {
                    let result_reg = self.builder.new_register(64.into());
                    Ok(result_reg)
                }
            }
            Expression::IntegerLiteral(IntegerLiteralExpr {
                value,
                type_id,
                range: _,
            }) => {
                let result_reg = self
                    .builder
                    .new_register(self.project.get_type_size(type_id).into());

                let value = match type_id {
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
            Expression::BooleanLiteral(bool_lit) => {
                let reg = self.builder.new_register(IrRegisterSize::Byte);
                self.builder
                    .add_instruction(IrInstruction::MovImm(reg, IrValue::Bool(bool_lit.value)));

                Ok(reg)
            }
            Expression::VariableRef(VariableRefExpr {
                name,
                type_id: _,
                range,
            }) => {
                let location = self.scope.find(name).map_err(|e| e.with_range(range))?;
                match location {
                    IrScopeLocation::Reg(reg) => Ok(reg),
                    IrScopeLocation::Global(index) => {
                        let reg = self
                            .builder
                            .new_register(self.global_consts[index].get_reg_size());
                        self.builder
                            .add_instruction(IrInstruction::MovImm(reg, self.global_consts[index]));
                        Ok(reg)
                    }
                }
            }
            Expression::StringLiteral(str_lit) => {
                let string_index = self.builder.add_string(str_lit.value.to_string());
                let dest_reg = self.builder.new_register(IrRegisterSize::Quad);
                self.builder.add_instruction(IrInstruction::MovImm(
                    dest_reg,
                    IrValue::String(string_index),
                ));

                Ok(dest_reg)
            }
            Expression::StructLiteral(_) => todo!(),
            Expression::FieldAccessor(_) => todo!(),
            // TODO: this probably isn't sufficient
            Expression::Widen(WidenExpr {
                expr,
                type_id,
                range: _,
            }) => {
                let result_reg = self
                    .builder
                    .new_register(self.project.get_type_size(type_id).into());
                let expr_reg = self.gen_expression(*expr)?;

                self.builder
                    .add_instruction(IrInstruction::MovZx(result_reg, expr_reg));

                Ok(result_reg)
            }
            Expression::Cast(_) => todo!(),
            Expression::Type(_) => unreachable!(),
        }
    }
}

impl<'a, 'b> Backend<'b> for IrBackend<'a> {
    fn process_upper_statements(&mut self, statements: Vec<UpperStatement<'b>>) -> Result<()> {
        for statement in statements {
            match statement {
                UpperStatement::Function(FunctionDeclaration {
                    name,
                    params,
                    return_type,
                    body,
                    annotations: _,
                    range,
                }) => {
                    let function_block = self.builder.add_block(name);

                    if name == "main" {
                        self.main_block = Some(function_block);
                    }

                    self.builder.push_block(function_block);
                    self.scope.push();

                    for (param_name, param_type) in params.into_iter().rev() {
                        let param_reg = self
                            .builder
                            .new_register(self.project.get_type_size(param_type).into());
                        self.builder.add_instruction(IrInstruction::Pop(param_reg));

                        self.scope
                            .insert(param_name, IrScopeLocation::Reg(param_reg))
                            .map_err(|e| e.with_range(range))?;
                    }

                    self.gen_statement(body)?;

                    if return_type == BUILTIN_TYPE_VOID {
                        self.builder.add_instruction(IrInstruction::Ret());
                    }

                    self.scope.pop();
                    self.builder.pop_block();
                }
                UpperStatement::StructDeclaration(_) => todo!(),
                UpperStatement::ConstDeclaration(ConstDeclaration {
                    name,
                    value,
                    annotations: _,
                    type_id,
                    range: _,
                }) => {
                    let const_value = self.gen_constant(value, type_id)?;

                    self.global_consts.push(const_value);
                    let index = self.global_consts.len() - 1;

                    self.scope.insert(name, IrScopeLocation::Global(index))?;
                }
                UpperStatement::ExternDeclaration(_) => {}
            }
        }

        Ok(())
    }

    fn finalize(&mut self, _output: &Path, _dont_compile: bool) -> Result<()> {
        // println!("{}\n\n\n", self.builder.get_dot_graph());

        Ok(())
    }

    fn run(&mut self, _output: &Path) -> Result<String> {
        let mut interpreter = Interpreter::new(
            &self.builder,
            self.main_block.expect("Main block must be present"),
        );

        interpreter.execute();

        Ok(interpreter.output_buffer)
    }

    fn name(&self) -> &'static str {
        "ir"
    }
}
