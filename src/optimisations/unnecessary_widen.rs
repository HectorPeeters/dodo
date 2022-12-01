use crate::ast::{BinaryOperatorExpr, Expression, IntegerLiteralExpr, WidenExpr};

use super::ast_walker::AstWalker;
use super::OptimisationStep;

#[derive(Default)]
pub struct UnnecessaryWiden {
    performed_optimisations: usize,
}

impl<'a> OptimisationStep<'a> for UnnecessaryWiden {
    fn name(&self) -> &'static str {
        "unnecessary widen"
    }

    fn performed_optimisations(&self) -> usize {
        self.performed_optimisations
    }
}

impl<'a> AstWalker<'a> for UnnecessaryWiden {
    fn visit_widen(&mut self, widen: WidenExpr<'a>) -> Expression<'a> {
        match widen {
            WidenExpr {
                expr: box Expression::IntegerLiteral(int_lit),
                type_id,
                range,
            } => {
                self.performed_optimisations += 1;

                Expression::IntegerLiteral(IntegerLiteralExpr {
                    value: int_lit.value,
                    type_id,
                    range,
                })
            }
            WidenExpr {
                expr: box Expression::BinaryOperator(binop),
                type_id,
                range,
            } => {
                self.performed_optimisations += 1;

                let left_range = *binop.left.range();
                let right_range = *binop.right.range();

                Expression::BinaryOperator(BinaryOperatorExpr {
                    op_type: binop.op_type,
                    left: Box::new(Expression::Widen(WidenExpr {
                        expr: binop.left,
                        type_id,
                        range: left_range,
                    })),
                    right: Box::new(Expression::Widen(WidenExpr {
                        expr: binop.right,
                        type_id,
                        range: right_range,
                    })),
                    type_id,
                    range,
                })
            }
            _ => Expression::Widen(widen),
        }
    }
}
