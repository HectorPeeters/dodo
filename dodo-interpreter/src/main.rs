use dodo_core::{Result, Type};
use dodo_parser::{ast::{BinaryOperatorType, Expression, Statement}, parser::Parser, tokenizer};

#[derive(Debug)]
pub enum Value {
    Integer(u32),
}

fn evaluate_expression(expr: &Expression<u32>) -> Result<Value> {
    match expr {
        Expression::BinaryOperator(op_type, left, right) => {
            let left_eval = evaluate_expression(left)?;
            let right_eval = evaluate_expression(right)?;
            match (left_eval, right_eval) {
                (Value::Integer(l), Value::Integer(r)) => match op_type {
                    BinaryOperatorType::Add => Ok(Value::Integer(l + r)),
                    BinaryOperatorType::Subtract => Ok(Value::Integer(l - r)),
                    BinaryOperatorType::Multiply => Ok(Value::Integer(l * r)),
                    BinaryOperatorType::Divide => Ok(Value::Integer(l / r)),
                },
            }
        }
        Expression::Constant(value, typ) => match typ {
            Type::Nil() | Type::Ref(_) => todo!(),
            _ => Ok(Value::Integer(*value)),
        },
        _ => todo!(),
    }
}

fn evaluate_statement(stmt: &Statement<u32>) {
    match stmt {
        Statement::Return(expr) => println!("{:?}", evaluate_expression(expr)),
        _ => todo!(),
    }
}

fn main() -> Result<()> {
    let input = "return 9 + 1;";
    let tokens = tokenizer::tokenize(input)?;
    let mut parser = Parser::<u32>::new(&tokens);

    let ast = parser.parse_statement()?;

    evaluate_statement(&ast);

    Ok(())
}
