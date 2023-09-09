use crate::ast::{BinaryOperatorExpr, BinaryOperatorType, Expression, IntegerLiteralExpr};
use crate::error::Result;

use super::ast_walker::AstWalker;
use super::OptimisationStep;

// This optimisation performs the following transformations
// x + 0 -> x
// x - 0 -> x
// 0 + x -> x
// x * 0 -> 0
// 0 * x -> 0
// x * 1 -> x
// 1 * x -> x

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
    fn visit_binop(&mut self, binop: BinaryOperatorExpr<'a>) -> Result<Expression<'a>> {
        match (binop.op_type, binop.left, binop.right) {
            (
                BinaryOperatorType::Add | BinaryOperatorType::Subtract,
                left,
                box Expression::IntegerLiteral(right),
            ) if right.value == 0 => {
                self.performed_optimisations += 1;
                Ok(*left)
            }
            (BinaryOperatorType::Add, box Expression::IntegerLiteral(left), right)
                if left.value == 0 =>
            {
                self.performed_optimisations += 1;
                Ok(*right)
            }
            (BinaryOperatorType::Multiply, _, box Expression::IntegerLiteral(right))
                if right.value == 0 =>
            {
                // TODO: make sure we don't change the behavior of `some_function() * 0`
                self.performed_optimisations += 1;
                Ok(Expression::IntegerLiteral(IntegerLiteralExpr {
                    value: 0,
                    type_id: binop.type_id,
                    range: binop.range,
                }))
            }
            (BinaryOperatorType::Multiply, box Expression::IntegerLiteral(left), _)
                if left.value == 0 =>
            {
                self.performed_optimisations += 1;
                Ok(Expression::IntegerLiteral(IntegerLiteralExpr {
                    value: 0,
                    type_id: binop.type_id,
                    range: binop.range,
                }))
            }
            (BinaryOperatorType::Multiply, left, box Expression::IntegerLiteral(right))
                if right.value == 1 =>
            {
                self.performed_optimisations += 1;
                Ok(*left)
            }
            (BinaryOperatorType::Multiply, box Expression::IntegerLiteral(left), right)
                if left.value == 1 =>
            {
                self.performed_optimisations += 1;
                Ok(*right)
            }
            (BinaryOperatorType::Divide, left, box Expression::IntegerLiteral(right))
                if right.value == 1 =>
            {
                self.performed_optimisations += 1;
                Ok(*left)
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
