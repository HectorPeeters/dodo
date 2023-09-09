use crate::ast::{Expression, VariableRefExpr};
use crate::error::Result;
use crate::sema::Sema;

use super::ast_walker::AstWalker;
use super::OptimisationStep;

pub struct ConstantPropagation<'a> {
    performed_optimisations: usize,
    sema: &'a Sema<'a>,
}

impl<'a> ConstantPropagation<'a> {
    pub fn new(sema: &'a Sema) -> Self {
        Self {
            performed_optimisations: 0,
            sema,
        }
    }
}

impl<'a> OptimisationStep<'a> for ConstantPropagation<'a> {
    fn name(&self) -> &'static str {
        "constant propagation"
    }

    fn performed_optimisations(&self) -> usize {
        self.performed_optimisations
    }
}

impl<'a> AstWalker<'a> for ConstantPropagation<'a> {
    fn visit_variable_ref(&mut self, var_ref: VariableRefExpr<'a>) -> Result<Expression<'a>> {
        let declaration = self.sema.get_declaration(var_ref.declaration_id);

        if !declaration.is_constant {
            return Ok(Expression::VariableRef(var_ref));
        }

        match &declaration.value {
            // TODO: once the nasm backend improves, the StructLiteral restriction can be removed
            Some(expr) if !matches!(expr, Expression::StructLiteral(_)) => return Ok(expr.clone()),
            _ => return Ok(Expression::VariableRef(var_ref)),
        }
    }
}
