#![allow(clippy::redundant_closure_call)]

use crate::ast::{BinaryOperatorExpr, BinaryOperatorType, Expression, IntegerLiteralExpr};
use crate::error::Result;
use crate::sema::Sema;

use super::ast_walker::AstWalker;
use super::OptimisationStep;

pub struct ConstantFold<'b> {
    performed_optimisations: usize,
    sema: &'b Sema<'b>,
}

impl<'b> ConstantFold<'b> {
    pub fn new(sema: &'b Sema<'b>) -> Self {
        Self {
            performed_optimisations: 0,
            sema,
        }
    }
}

impl<'a, 'b> OptimisationStep<'a> for ConstantFold<'b> {
    fn name(&self) -> &'static str {
        "constant fold"
    }

    fn performed_optimisations(&self) -> usize {
        self.performed_optimisations
    }
}

macro_rules! impl_binop {
    ($left:expr, $right:expr, $binop:expr, $op:expr, $type_id:expr, $range:expr, $self:ident) => {{
        assert_eq!($left.type_id, $right.type_id);

        // TODO: this should be fixed for shl and shr
        let mut result = IntegerLiteralExpr::new($op($left.value, $right.value), $binop.range);

        if $self.sema.get_type_size(result.type_id)? > $self.sema.get_type_size($binop.type_id)? {
            return Ok(Expression::BinaryOperator(BinaryOperatorExpr {
                op_type: $binop.op_type,
                left: Box::new(Expression::IntegerLiteral($left)),
                right: Box::new(Expression::IntegerLiteral($right)),
                type_id: $type_id,
                range: $range,
            }));
        }

        $self.performed_optimisations += 1;

        result.type_id = $binop.type_id;

        Ok(Expression::IntegerLiteral(result))
    }};
}

impl<'a, 'b> AstWalker<'a> for ConstantFold<'b> {
    fn visit_binop(&mut self, binop: BinaryOperatorExpr<'a>) -> Result<Expression<'a>> {
        match (binop.op_type, binop.left, binop.right) {
            (
                BinaryOperatorType::Add,
                box Expression::IntegerLiteral(left),
                box Expression::IntegerLiteral(right),
            ) => {
                impl_binop!(
                    left,
                    right,
                    binop,
                    |a: u64, b: u64| a.wrapping_add(b),
                    binop.type_id,
                    binop.range,
                    self
                )
            }
            (
                BinaryOperatorType::Subtract,
                box Expression::IntegerLiteral(left),
                box Expression::IntegerLiteral(right),
            ) => {
                impl_binop!(
                    left,
                    right,
                    binop,
                    |a: u64, b: u64| a.wrapping_sub(b),
                    binop.type_id,
                    binop.range,
                    self
                )
            }
            (
                BinaryOperatorType::Multiply,
                box Expression::IntegerLiteral(left),
                box Expression::IntegerLiteral(right),
            ) => {
                impl_binop!(
                    left,
                    right,
                    binop,
                    |a: u64, b: u64| a.wrapping_mul(b),
                    binop.type_id,
                    binop.range,
                    self
                )
            }
            (
                BinaryOperatorType::Divide,
                box Expression::IntegerLiteral(left),
                box Expression::IntegerLiteral(right),
            ) => {
                impl_binop!(
                    left,
                    right,
                    binop,
                    |a: u64, b: u64| a.wrapping_div(b),
                    binop.type_id,
                    binop.range,
                    self
                )
            }
            (
                BinaryOperatorType::ShiftLeft,
                box Expression::IntegerLiteral(left),
                box Expression::IntegerLiteral(right),
            ) => {
                impl_binop!(
                    left,
                    right,
                    binop,
                    |a: u64, b: u64| a.wrapping_shl(b as u32),
                    binop.type_id,
                    binop.range,
                    self
                )
            }
            (
                BinaryOperatorType::ShiftRight,
                box Expression::IntegerLiteral(left),
                box Expression::IntegerLiteral(right),
            ) => {
                impl_binop!(
                    left,
                    right,
                    binop,
                    |a: u64, b: u64| a.wrapping_shr(b as u32),
                    binop.type_id,
                    binop.range,
                    self
                )
            }
            x => Ok(Expression::BinaryOperator(BinaryOperatorExpr {
                op_type: x.0,
                left: x.1,
                right: x.2,
                type_id: binop.type_id,
                range: binop.range,
            })),
        }
    }
}
