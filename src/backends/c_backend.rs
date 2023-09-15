use super::Backend;
use crate::ast::{
    Annotations, AssignmentStatement, Ast, AstVisitor, BinaryOperatorType, BlockStatement,
    CastExpr, ConstDeclaration, DeclarationStatement, Expression, ExpressionId,
    ExpressionStatement, FieldAccessorExpr, FunctionCallExpr, FunctionDeclaration, IfStatement,
    ReturnStatement, Statement, StatementId, StructDeclaration, StructLiteralExpr,
    UnaryOperatorType, UpperStatement, UpperStatementId, WhileStatement,
};
use crate::error::{Error, ErrorType, Result};
use crate::sema::{
    DeclarationId, Sema, BUILTIN_TYPE_BOOL, BUILTIN_TYPE_U16, BUILTIN_TYPE_U32, BUILTIN_TYPE_U64,
    BUILTIN_TYPE_U8, BUILTIN_TYPE_VOID,
};
use crate::types::TypeId;
use std::path::Path;
use std::process::Command;

pub struct CBackend<'a> {
    ast: &'a Ast<'a>,
    sema: &'a Sema<'a>,
    buffer: String,
}

impl<'a> CBackend<'a> {
    pub fn new(ast: &'a Ast<'a>, sema: &'a Sema<'a>) -> Self {
        Self {
            ast,
            sema,
            buffer: "#include <stdio.h>\n#include<stdlib.h>\n#include<stdbool.h>\n\n".to_string(),
        }
    }
}

impl<'a> Backend<'a> for CBackend<'a> {
    fn process(&mut self) -> Result<()> {
        for statement in &self.ast.upper_statements {
            if let UpperStatement::StructDeclaration(struct_decl) = statement {
                self.buffer
                    .push_str(&format!("struct {};\n", struct_decl.name));
            }
        }

        self.buffer.push('\n');

        for statement in &self.ast.upper_statements {
            if let UpperStatement::Function(FunctionDeclaration {
                name,
                params,
                return_type,
                body: _,
                annotations,
                range: _,
            }) = statement
            {
                self.write_fuction_declaration(name, params, *return_type, annotations)?;
                self.buffer.push_str(";\n");
            }
        }

        self.buffer.push('\n');

        for upper_statement_id in 0..self.ast.upper_statements.len() {
            self.visit_upper_statement((upper_statement_id as u32).into())?;
        }

        Ok(())
    }

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()> {
        let c_file = output.with_extension("c");
        std::fs::write(&c_file, &self.buffer).unwrap();

        if dont_compile {
            return Ok(());
        }

        let compile_output = Command::new("clang")
            .args([c_file.to_str().unwrap(), "-o", output.to_str().unwrap()])
            .output()
            .expect("Failed to execute C compiler");

        compile_output.status.exit_ok().map_err(|_| {
            Error::new(
                ErrorType::Postprocess,
                format!(
                    "C compilation failed\n\n{}",
                    String::from_utf8(compile_output.stderr).unwrap()
                ),
            )
        })?;

        Ok(())
    }

    fn name(&self) -> &'static str {
        "c++"
    }
}

impl<'a> CBackend<'a> {
    fn to_c_type(&self, id: TypeId) -> Result<String> {
        match id {
            BUILTIN_TYPE_U8 => Ok("char".to_string()),
            BUILTIN_TYPE_U16 => Ok("unsigned short".to_string()),
            BUILTIN_TYPE_U32 => Ok("unsigned int".to_string()),
            BUILTIN_TYPE_U64 => Ok("unsigned long".to_string()),
            BUILTIN_TYPE_BOOL => Ok("bool".to_string()),
            BUILTIN_TYPE_VOID => Ok("void".to_string()),
            _ if self.sema.get_type_info(id)?.is_ptr() => Ok(format!(
                "{}*",
                self.to_c_type(self.sema.get_type_info(id)?.get_deref()?)?
            )),
            _ if self.sema.get_type_info(id)?.is_struct() => {
                Ok(format!("struct {}", self.sema.get_struct(id)?.name))
            }
            _ => unreachable!(),
        }
    }

    fn write_fuction_declaration(
        &mut self,
        name: &str,
        params: &[DeclarationId],
        return_type: TypeId,
        annotations: &Annotations<'a>,
    ) -> Result<()> {
        let params = params
            .iter()
            .map(|declaration_id| {
                let type_id = self.sema.get_declaration(*declaration_id).type_id;
                self.to_c_type(type_id)
                    .map(|t| format!("{t} {}", declaration_id))
            })
            .collect::<Result<Vec<_>>>()?
            .join(", ");

        let return_type = if name == "main" {
            "int".to_string()
        } else {
            self.to_c_type(return_type)?
        };

        let section_annotation = annotations.get_string("section", self.ast);

        let section_attribute = match section_annotation {
            Some(name) => format!("__attribute__((section(\"{name}\")))"),
            None => String::new(),
        };

        self.buffer.push_str(&format!(
            "{return_type} {name}({params}) {section_attribute}"
        ));

        Ok(())
    }
}

impl<'a> AstVisitor<'a, (), (), String> for CBackend<'a> {
    fn visit_upper_statement(&mut self, statement_id: UpperStatementId) -> Result<()> {
        match self.ast.get_upper_statement(statement_id) {
            UpperStatement::StructDeclaration(StructDeclaration {
                name,
                declaration_id: _,
                fields,
                range: _,
            }) => {
                let formatted_fields = fields
                    .iter()
                    .map(|(name, field_type)| {
                        self.to_c_type(*field_type)
                            .map(|t| format!("\t{t} {name};"))
                    })
                    .collect::<Result<Vec<_>>>()?
                    .join("\n");

                self.buffer
                    .push_str(&format!("struct {name} {{\n{formatted_fields}\n}};\n"));

                Ok(())
            }
            UpperStatement::ExternDeclaration(_) => Ok(()),
            UpperStatement::Function(FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                annotations,
                range: _,
            }) => {
                self.write_fuction_declaration(name, params, *return_type, annotations)?;
                self.buffer.push('\n');
                self.visit_statement(*body)?;

                self.buffer.push('\n');

                Ok(())
            }
            UpperStatement::ConstDeclaration(ConstDeclaration {
                value,
                annotations,
                declaration_id,
                range: _,
            }) => {
                let type_id = self.sema.get_declaration(*declaration_id).type_id;
                let c_type = self.to_c_type(type_id)?;

                let c_value = self.visit_expression(*value)?;

                let section_annotation = annotations.get_string("section", self.ast);

                let section_attribute = match section_annotation {
                    Some(name) => format!("__attribute__((section(\"{name}\")))"),
                    None => String::new(),
                };

                self.buffer.push_str(&format!(
                    "const {c_type} {} {section_attribute} = {c_value};\n",
                    declaration_id
                ));

                Ok(())
            }
        }
    }

    fn visit_statement(&mut self, statement_id: StatementId) -> Result<()> {
        let statement = self.ast.get_statement(statement_id);

        match statement {
            Statement::Block(BlockStatement {
                children_ids,
                scoped,
                range: _,
            }) => {
                if *scoped {
                    self.buffer.push_str("{\n");
                }

                for statement in children_ids {
                    self.visit_statement(*statement)?;
                }

                if *scoped {
                    self.buffer.push_str("\n}\n");
                }

                Ok(())
            }
            Statement::Declaration(DeclarationStatement {
                declaration_id,
                range: _,
            }) => {
                let type_id = self.sema.get_declaration(*declaration_id).type_id;
                self.buffer.push_str(&format!(
                    "{} {};\n",
                    self.to_c_type(type_id)?,
                    declaration_id
                ));
                Ok(())
            }
            Statement::Assignment(AssignmentStatement {
                left_id,
                right_id,
                range: _,
            }) => {
                let left = self.visit_expression(*left_id)?;
                let right = self.visit_expression(*right_id)?;
                self.buffer.push_str(&format!("{left} = {right};\n"));
                Ok(())
            }
            Statement::Expression(ExpressionStatement { expr_id, range: _ }) => {
                let expr = self.visit_expression(*expr_id)?;
                self.buffer.push_str(&expr);
                self.buffer.push_str(";\n");
                Ok(())
            }
            Statement::While(WhileStatement {
                condition_id,
                body_id,
                range: _,
            }) => {
                let condition = self.visit_expression(*condition_id)?;

                self.buffer.push_str(&format!("while ({condition}) {{"));
                self.visit_statement(*body_id)?;
                self.buffer.push('}');

                Ok(())
            }
            Statement::If(IfStatement {
                condition_id,
                if_body_id,
                else_body_id,
                range: _,
            }) => {
                let condition = self.visit_expression(*condition_id)?;

                self.buffer.push_str(&format!("if ({condition}) "));
                self.visit_statement(*if_body_id)?;

                if let Some(else_body) = else_body_id {
                    self.buffer.push_str(" else ");
                    self.visit_statement(*else_body)?;
                }

                Ok(())
            }
            Statement::Return(ReturnStatement { expr_id, range: _ }) => {
                let value = self.visit_expression(*expr_id)?;
                self.buffer.push_str(&format!("return {value};"));

                Ok(())
            }
        }
    }

    fn visit_expression(&mut self, expression_id: ExpressionId) -> Result<String> {
        let expression = self.ast.get_expression(expression_id);
        match expression {
            Expression::BinaryOperator(binop) => {
                let left = self.visit_expression(binop.left_id)?;
                let right = self.visit_expression(binop.right_id)?;

                Ok(match binop.op_type {
                    BinaryOperatorType::Add => format!("({left} + {right})"),
                    BinaryOperatorType::Subtract => format!("({left} - {right})"),
                    BinaryOperatorType::Multiply => format!("({left} * {right})"),
                    BinaryOperatorType::Divide => format!("({left} / {right})"),
                    BinaryOperatorType::Modulo => format!("({left} % {right})"),
                    BinaryOperatorType::ShiftLeft => format!("({left} << {right})"),
                    BinaryOperatorType::ShiftRight => format!("({left} >> {right})"),
                    BinaryOperatorType::Equal => format!("({left} == {right})"),
                    BinaryOperatorType::NotEqual => format!("({left} != {right})"),
                    BinaryOperatorType::LessThan => format!("({left} < {right})"),
                    BinaryOperatorType::LessThanEqual => format!("({left} <= {right})"),
                    BinaryOperatorType::GreaterThan => format!("({left} > {right})"),
                    BinaryOperatorType::GreaterThanEqual => format!("({left} >= {right})"),
                    BinaryOperatorType::LogicalOr => format!("({left} || {right})"),
                    BinaryOperatorType::LogicalAnd => format!("({left} && {right})"),
                })
            }
            Expression::UnaryOperator(unop) => {
                let expr = self.visit_expression(unop.expr_id)?;

                Ok(match unop.op_type {
                    UnaryOperatorType::Negate => format!("(-{expr})"),
                    UnaryOperatorType::Ref => format!("(&{expr})"),
                    UnaryOperatorType::Deref => format!("(*{expr})"),
                })
            }
            Expression::FunctionCall(FunctionCallExpr {
                name,
                arg_ids,
                range: _,
            }) => {
                let args = arg_ids
                    .iter()
                    .map(|x| self.visit_expression(*x))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ");

                Ok(format!("{name}({args})"))
            }
            Expression::IntegerLiteral(int_lit) => Ok(format!("{}", int_lit.value)),
            Expression::BooleanLiteral(bool_lit) => Ok(format!("{}", bool_lit.value)),
            Expression::VariableRef(var_ref) => Ok(format!("{}", var_ref.declaration_id)),
            Expression::StringLiteral(str_lit) => Ok(format!("\"{}\"", str_lit.value.inner())),
            Expression::StructLiteral(StructLiteralExpr { fields, range: _ }) => {
                let type_id = self.sema.get_type(expression_id);
                let c_type = self.to_c_type(type_id)?;

                let formatted_fields = fields
                    .iter()
                    .map(|(name, value)| match self.visit_expression(*value) {
                        Ok(value) => Ok(format!(".{name} = {value}")),
                        Err(e) => Err(e),
                    })
                    .collect::<Result<Vec<_>>>()?
                    .join(",\n");

                Ok(format!("({c_type}){{\n{formatted_fields}\n}}"))
            }
            Expression::FieldAccessor(FieldAccessorExpr {
                name,
                expr_id,
                range: _,
            }) => {
                let child_source = self.visit_expression(*expr_id)?;
                let expression_type = self.sema.get_type(*expr_id);

                if self.sema.get_type_info(expression_type)?.is_ptr() {
                    Ok(format!("{child_source}->{name}"))
                } else {
                    Ok(format!("{child_source}.{name}"))
                }
            }
            Expression::Widen(widen) => self.visit_expression(widen.expr_id),
            Expression::Cast(CastExpr { expr_id, range: _ }) => {
                let type_id = self.sema.get_type(expression_id);
                Ok(format!(
                    "({})({})",
                    self.to_c_type(type_id)?,
                    self.visit_expression(*expr_id)?
                ))
            }
            Expression::Type(_) => unreachable!(),
        }
    }
}
