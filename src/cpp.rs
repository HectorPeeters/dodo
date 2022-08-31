use crate::ast::{
    BinaryOperatorType, ConsumingAstVisitor, Expression, Statement, TypedExpression,
    TypedStatement, TypedUpperStatement, UnaryOperatorType, UpperStatement,
};
use crate::backend::Backend;
use crate::error::{Error, ErrorType, Result};
use crate::types::Type;
use std::path::Path;
use std::process::Command;

pub struct CppGenerator {
    buffer: String,
}

impl CppGenerator {
    pub fn new() -> Self {
        Self {
            buffer: "#include \"runtime.h\"\n\n".to_string(),
        }
    }
}

impl Backend for CppGenerator {
    fn process_upper_statement(&mut self, statement: TypedUpperStatement) -> Result<()> {
        self.visit_upper_statement(statement)
    }

    fn finalize(&mut self, output: &Path) -> Result<()> {
        let cpp_file = output.with_extension("cpp");
        std::fs::write(&cpp_file, &self.buffer).unwrap();

        let compile_output = Command::new("clang++")
            .args([
                cpp_file.to_str().unwrap(),
                "-o",
                output.to_str().unwrap(),
                "-Iruntime",
            ])
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

fn to_cpp_type(type_: Type) -> String {
    use Type::*;

    match type_ {
        UInt8() => "char".to_string(),
        UInt16() => "unsigned short".to_string(),
        UInt32() => "unsigned int".to_string(),
        UInt64() => "unsigned long".to_string(),
        Bool() => "bool".to_string(),
        Ref(inner) => format!("{}*", to_cpp_type(*inner)),
        Void() => "void".to_string(),
        _ => unreachable!(),
    }
}

impl ConsumingAstVisitor<Type, (), String> for CppGenerator {
    fn visit_upper_statement(&mut self, statement: UpperStatement<Type>) -> Result<()> {
        match statement {
            UpperStatement::Function(name, args, return_type, body, annotations, _, _) => {
                let mut name = name;
                if name == "main" {
                    name = "dodo_main".to_string();
                }

                let args = args
                    .into_iter()
                    .map(|(name, type_)| format!("{} {}", to_cpp_type(type_), name))
                    .collect::<Vec<_>>()
                    .join(", ");

                let return_type = to_cpp_type(return_type);

                let section_annotation = annotations
                    .into_iter()
                    .find(|(name, value)|  matches!(value, Expression::StringLiteral(..) if name == "section" ))
                    .map(|(_, value)| match value {
                        Expression::StringLiteral(value, _, _) => value,
                        _ => unreachable!()
                    });

                let section_attribute = match section_annotation {
                    Some(name) => format!("__attribute__((section(\"{name}\")))"),
                    None => String::new(),
                };

                self.buffer.push_str(&format!(
                    "{} {}({}) {}\n",
                    return_type, name, args, section_attribute
                ));

                self.visit_statement(*body)?;

                self.buffer.push_str("\n");

                Ok(())
            }
        }
    }

    fn visit_statement(&mut self, statement: TypedStatement) -> Result<()> {
        match statement {
            Statement::Block(statements, scoped, _, _) => {
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
            Statement::Declaration(name, type_, _, _) => {
                self.buffer
                    .push_str(&format!("{} {};\n", to_cpp_type(type_), name));
                Ok(())
            }
            Statement::Assignment(name, value, _, _) => {
                let value = self.visit_expression(value)?;
                self.buffer.push_str(&format!("{} = {};\n", name, value));
                Ok(())
            }
            Statement::Expression(expr, _, _) => {
                let expr = self.visit_expression(expr)?;
                self.buffer.push_str(&expr);
                self.buffer.push_str(";\n");
                Ok(())
            }
            Statement::While(cond, body, _, _) => {
                let cond = self.visit_expression(cond)?;

                self.buffer.push_str(&format!("while ({}) {{", cond));
                self.visit_statement(*body)?;
                self.buffer.push_str("}");

                Ok(())
            }
            Statement::If(cond, body, _, _) => {
                let cond = self.visit_expression(cond)?;

                self.buffer.push_str(&format!("if ({}) {{", cond));
                self.visit_statement(*body)?;
                self.buffer.push_str("}");

                Ok(())
            }
            Statement::Return(expr, _, _) => {
                let expr = self.visit_expression(expr)?;
                self.buffer.push_str(&format!("return {};", expr));

                Ok(())
            }
        }
    }

    fn visit_expression(&mut self, expression: TypedExpression) -> Result<String> {
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
            Expression::Literal(value, _, _) => Ok(format!("{}", value)),
            Expression::VariableRef(name, _, _) => Ok(format!("{}", name)),
            Expression::StringLiteral(value, _, _) => Ok(format!(
                "\"{}\"",
                value
                    .replace("\n", "\\n")
                    .replace("\t", "\\t")
                    .replace("\"", "\\\"")
            )),
            Expression::Widen(expr, _, _) => self.visit_expression(*expr),
        }
    }
}

impl Default for CppGenerator {
    fn default() -> Self {
        Self::new()
    }
}
