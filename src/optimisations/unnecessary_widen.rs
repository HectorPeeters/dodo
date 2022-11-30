use crate::ast::{Expression, IntegerLiteralExpr, WidenExpr};

use super::ast_walker::AstWalker;
use super::OptimisationStep;

#[derive(Default)]
pub struct UnnecessaryWiden {
    performed_optimisations: usize,
}

impl OptimisationStep for UnnecessaryWiden {
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
            _ => Expression::Widen(widen),
        }
    }
}
