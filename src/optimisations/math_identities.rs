use crate::ast::{BinaryOperatorExpr, BinaryOperatorType, Expression};

use super::ast_walker::AstWalker;
use super::OptimisationStep;

// This optimisation performs the following transformations
// x + 0 -> x

#[derive(Default)]
pub struct MathIdentities {
    performed_optimisations: usize,
}

impl<'a> OptimisationStep<'a> for MathIdentities {
    fn name(&self) -> &'static str {
        "math identities"
    }

    fn performed_optimisations(&self) -> usize {
        self.performed_optimisations
    }
}

impl<'a> AstWalker<'a> for MathIdentities {
    fn visit_binop(&mut self, binop: BinaryOperatorExpr<'a>) -> Expression<'a> {
        match (binop.op_type, binop.left, binop.right) {
            (
                BinaryOperatorType::Add,
                box Expression::IntegerLiteral(left),
                box Expression::IntegerLiteral(right),
            ) if left.value == 0 => Expression::IntegerLiteral(right),
            (
                BinaryOperatorType::Add,
                box Expression::IntegerLiteral(left),
                box Expression::IntegerLiteral(right),
            ) if right.value == 0 => Expression::IntegerLiteral(left),
            x => Expression::BinaryOperator(BinaryOperatorExpr {
                op_type: x.0,
                left: x.1,
                right: x.2,
                type_id: binop.type_id,
                range: binop.range,
            }),
        }
    }
}
