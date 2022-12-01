use crate::ast::{BinaryOperatorExpr, BinaryOperatorType, Expression, IntegerLiteralExpr};
use crate::project::Project;

use super::ast_walker::AstWalker;
use super::OptimisationStep;

pub struct ConstantFold<'b> {
    performed_optimisations: usize,
    project: &'b Project,
}

impl<'b> ConstantFold<'b> {
    pub fn new(project: &'b Project) -> Self {
        Self {
            performed_optimisations: 0,
            project,
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
    ($left:expr, $right:expr, $binop:expr, $op:ident, $type_id:expr, $range:expr, $self:ident) => {{
        assert_eq!($left.type_id, $right.type_id);

        // TODO: this should be fixed for shl and shr
        let mut result = IntegerLiteralExpr::new(
            $left.value.$op($right.value.try_into().unwrap()),
            $binop.range,
        );

        if $self.project.get_type_size(result.type_id) > $self.project.get_type_size($binop.type_id)
        {
            return Expression::BinaryOperator(BinaryOperatorExpr {
                op_type: $binop.op_type,
                left: Box::new(Expression::IntegerLiteral($left)),
                right: Box::new(Expression::IntegerLiteral($right)),
                type_id: $type_id,
                range: $range,
            });
        }

        $self.performed_optimisations += 1;

        result.type_id = $binop.type_id;

        Expression::IntegerLiteral(result)
    }};
}

impl<'a, 'b> AstWalker<'a> for ConstantFold<'b> {
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
                    wrapping_sub,
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
                    wrapping_mul,
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
                    wrapping_div,
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
                    wrapping_shl,
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
                    wrapping_shr,
                    binop.type_id,
                    binop.range,
                    self
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
