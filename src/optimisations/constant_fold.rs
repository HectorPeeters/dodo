use crate::ast::{BinaryOperatorExpr, BinaryOperatorType, Expression, IntegerLiteralExpr};
use crate::project::{BUILTIN_TYPE_U16, BUILTIN_TYPE_U32, BUILTIN_TYPE_U64, BUILTIN_TYPE_U8};

use super::ast_walker::AstWalker;
use super::OptimisationStep;

#[derive(Default)]
pub struct ConstantFold {
    performed_optimisations: usize,
}

impl<'a> OptimisationStep<'a> for ConstantFold {
    fn name(&self) -> &'static str {
        "constant fold"
    }

    fn performed_optimisations(&self) -> usize {
        self.performed_optimisations
    }
}

macro_rules! impl_binop {
    ($left:ident, $right:ident, $binop:ident, $op:ident, $opt_count:expr) => {{
        assert_eq!($left.type_id, $right.type_id);

        let value = match $left.type_id {
            BUILTIN_TYPE_U8 => ($left.value as u8).$op(($right.value as u8).into()) as u64,
            BUILTIN_TYPE_U16 => ($left.value as u16).$op(($right.value as u16).into()) as u64,
            BUILTIN_TYPE_U32 => ($left.value as u32).$op(($right.value as u32).into()) as u64,
            BUILTIN_TYPE_U64 => $left.value.$op(($right.value as u32).into()),
            _ => unreachable!(),
        };

        $opt_count += 1;

        Expression::IntegerLiteral(IntegerLiteralExpr {
            value,
            type_id: $left.type_id,
            range: $binop.range,
        })
    }};
}

impl<'a> AstWalker<'a> for ConstantFold {
    fn visit_binop(&mut self, binop: BinaryOperatorExpr<'a>) -> Expression<'a> {
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
                    wrapping_add,
                    self.performed_optimisations
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
                    wrapping_sub,
                    self.performed_optimisations
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
                    wrapping_mul,
                    self.performed_optimisations
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
                    wrapping_div,
                    self.performed_optimisations
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
                    wrapping_shl,
                    self.performed_optimisations
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
                    wrapping_shr,
                    self.performed_optimisations
                )
            }
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
