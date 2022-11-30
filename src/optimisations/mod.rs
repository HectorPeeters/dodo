pub mod ast_walker;
pub mod unnecessary_widen;

pub use unnecessary_widen::*;

use crate::ast::UpperStatement;

use self::ast_walker::AstWalker;

pub trait OptimisationStep {
    fn name(&self) -> &'static str;

    fn performed_optimisations(&self) -> usize;
}

pub fn optimize(mut statements: Vec<UpperStatement<'_>>) -> Vec<UpperStatement<'_>> {
    loop {
        let mut performed_optimisation = false;

        let passes = [UnnecessaryWiden::default()];

        for mut pass in passes {
            statements = statements
                .into_iter()
                .map(|x| pass.visit_upper_statement(x))
                .collect::<Vec<_>>();

            if pass.performed_optimisations() != 0 {
                println!(
                    "Optimisation performed: {}({})",
                    pass.name(),
                    pass.performed_optimisations()
                );
                performed_optimisation = true;
            }
        }

        if !performed_optimisation {
            break;
        }
    }

    statements
}
