use crate::ast::{
    BinaryOperatorType, ConsumingAstVisitor, Expression, Statement, UnaryOperatorType,
    UpperStatement,
};
use crate::backend::Backend;
use crate::error::{Error, ErrorType, Result};
use crate::project::{
    Project, BUILTIN_TYPE_BOOL, BUILTIN_TYPE_U16, BUILTIN_TYPE_U32, BUILTIN_TYPE_U64,
    BUILTIN_TYPE_U8, BUILTIN_TYPE_VOID,
};
use crate::types::TypeId;
use std::path::Path;
use std::process::Command;

pub struct CppGenerator<'a> {
    project: &'a mut Project,
    buffer: String,
}

impl<'a> CppGenerator<'a> {
    pub fn new(project: &'a mut Project) -> Self {
        Self {
            project,
            buffer: "#include <stdio.h>\n#include<stdlib.h>\n\n".to_string(),
        }
    }
}

impl<'a> Backend for CppGenerator<'a> {
    fn process_upper_statement(&mut self, statement: UpperStatement) -> Result<()> {
        self.visit_upper_statement(statement)
    }

    fn finalize(&mut self, output: &Path) -> Result<()> {
        let cpp_file = output.with_extension("cpp");
        std::fs::write(&cpp_file, &self.buffer).unwrap();

        let compile_output = Command::new("clang++")
            .args([cpp_file.to_str().unwrap(), "-o", output.to_str().unwrap()])
            .output()
            .expect("Failed to execute cpp compiler");

        compile_output.status.exit_ok().map_err(|_| {
            Error::new(
                ErrorType::Postprocess,
                format!(
                    "Cpp compilation failed\n\n{}",
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

impl<'a> CppGenerator<'a> {
    fn to_cpp_type(&self, id: TypeId) -> String {
        match id {
            BUILTIN_TYPE_U8 => "char".to_string(),
            BUILTIN_TYPE_U16 => "unsigned short".to_string(),
            BUILTIN_TYPE_U32 => "unsigned int".to_string(),
            BUILTIN_TYPE_U64 => "unsigned long".to_string(),
            BUILTIN_TYPE_BOOL => "bool".to_string(),
            BUILTIN_TYPE_VOID => "void".to_string(),
            _ => format!("{}*", self.to_cpp_type(self.project.get_inner_type(id))),
        }
    }
}

impl<'a> ConsumingAstVisitor<(), (), String> for CppGenerator<'a> {
    fn visit_upper_statement(&mut self, statement: UpperStatement) -> Result<()> {
        match statement {
            UpperStatement::ExternDeclaration(_, _) => Ok(()),
            UpperStatement::Function(name, args, return_type, body, annotations, _) => {
                let args = args
                    .into_iter()
                    .map(|(name, type_)| format!("{} {}", self.to_cpp_type(type_), name))
                    .collect::<Vec<_>>()
                    .join(", ");

                let return_type = if name == "main" {
                    "int".to_string()
                } else {
                    self.to_cpp_type(return_type)
                };

                let no_return = if annotations.iter().any(|(name, _)| name == "noreturn") {
                    "[[noreturn]]"
                } else {
                    ""
                };

                let section_annotation = annotations
                    .into_iter()
                    .find(|(name, value)|  matches!(value, Some(Expression::StringLiteral(..)) if name == "section" ))
                    .map(|(_, value)| match value {
                        Some(Expression::StringLiteral(value, _, _)) => value,
                        _ => unreachable!()
                    });

                let section_attribute = match section_annotation {
                    Some(name) => format!("__attribute__((section(\"{name}\")))"),
                    None => String::new(),
                };

                self.buffer.push_str(&format!(
                    "{} {} {}({}) {}\n",
                    no_return, return_type, name, args, section_attribute
                ));

                self.visit_statement(*body)?;

                self.buffer.push('\n');

                Ok(())
            }
            UpperStatement::ConstDeclaration(name, value_type, value, _range) => {
                let cpp_type = self.to_cpp_type(value_type);

                let cpp_value = self.visit_expression(value)?;

                self.buffer
                    .push_str(&format!("const {} {} = {};\n", cpp_type, name, cpp_value));

                Ok(())
            }
        }
    }

    fn visit_statement(&mut self, statement: Statement) -> Result<()> {
        match statement {
            Statement::Block(statements, scoped, _) => {
                if scoped {
                    self.buffer.push_str("{\n");
                }

                for statement in statements {
                    self.visit_statement(statement)?;
                }

                if scoped {
                    self.buffer.push_str("\n}\n");
                }

                Ok(())
            }
            Statement::Declaration(name, type_, _) => {
                self.buffer
                    .push_str(&format!("{} {};\n", self.to_cpp_type(type_), name));
                Ok(())
            }
            Statement::Assignment(lhs, rhs, _) => {
                let lhs = self.visit_expression(lhs)?;
                let rhs = self.visit_expression(rhs)?;
                self.buffer.push_str(&format!("{} = {};\n", lhs, rhs));
                Ok(())
            }
            Statement::Expression(expr, _) => {
                let expr = self.visit_expression(expr)?;
                self.buffer.push_str(&expr);
                self.buffer.push_str(";\n");
                Ok(())
            }
            Statement::While(cond, body, _) => {
                let cond = self.visit_expression(cond)?;

                self.buffer.push_str(&format!("while ({}) {{", cond));
                self.visit_statement(*body)?;
                self.buffer.push('}');

                Ok(())
            }
            Statement::If(cond, body, _) => {
                let cond = self.visit_expression(cond)?;

                self.buffer.push_str(&format!("if ({}) {{", cond));
                self.visit_statement(*body)?;
                self.buffer.push('}');

                Ok(())
            }
            Statement::Return(expr, _) => {
                let expr = self.visit_expression(expr)?;
                self.buffer.push_str(&format!("return {};", expr));

                Ok(())
            }
        }
    }

    fn visit_expression(&mut self, expression: Expression) -> Result<String> {
        match expression {
            Expression::BinaryOperator(op, left, right, _, _) => {
                let left = self.visit_expression(*left)?;
                let right = self.visit_expression(*right)?;

                Ok(match op {
                    BinaryOperatorType::Add => format!("({} + {})", left, right),
                    BinaryOperatorType::Subtract => format!("({} - {})", left, right),
                    BinaryOperatorType::Multiply => format!("({} * {})", left, right),
                    BinaryOperatorType::Divide => format!("({} / {})", left, right),
                    BinaryOperatorType::Modulo => format!("({} % {})", left, right),
                    BinaryOperatorType::ShiftLeft => format!("({} << {})", left, right),
                    BinaryOperatorType::ShiftRight => format!("({} >> {})", left, right),
                    BinaryOperatorType::Equal => format!("({} == {})", left, right),
                    BinaryOperatorType::NotEqual => format!("({} != {})", left, right),
                    BinaryOperatorType::LessThan => format!("({} < {})", left, right),
                    BinaryOperatorType::LessThanEqual => format!("({} <= {})", left, right),
                    BinaryOperatorType::GreaterThan => format!("({} > {})", left, right),
                    BinaryOperatorType::GreaterThanEqual => format!("({} >= {})", left, right),
                })
            }
            Expression::UnaryOperator(op, expr, _, _) => {
                let expr = self.visit_expression(*expr)?;

                Ok(match op {
                    UnaryOperatorType::Negate => format!("(-{})", expr),
                    UnaryOperatorType::Ref => format!("(&{})", expr),
                    UnaryOperatorType::Deref => format!("(*{})", expr),
                })
            }
            Expression::FunctionCall(name, args, _, _) => {
                let args = args
                    .into_iter()
                    .map(|x| self.visit_expression(x))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ");

                Ok(format!("{}({})", name, args))
            }
            Expression::IntegerLiteral(value, _, _) => Ok(format!("{}", value)),
            Expression::BooleanLiteral(value, _, _) => Ok(format!("{}", value)),
            Expression::VariableRef(name, _, _) => Ok(name),
            Expression::StringLiteral(value, _, _) => Ok(format!(
                "\"{}\"",
                value
                    .replace('\n', "\\n")
                    .replace('\t', "\\t")
                    .replace('\"', "\\\"")
            )),
            Expression::Widen(expr, _, _) => self.visit_expression(*expr),
        }
    }
}
