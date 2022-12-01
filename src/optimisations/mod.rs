mod ast_walker;
mod constant_fold;
mod math_identities;
mod unnecessary_widen;

use self::ast_walker::AstWalker;
use self::constant_fold::ConstantFold;
use self::math_identities::MathIdentities;
use self::unnecessary_widen::UnnecessaryWiden;
use crate::ast::UpperStatement;
use crate::project::Project;

pub trait OptimisationStep<'a>: AstWalker<'a> {
    fn name(&self) -> &'static str;

    fn performed_optimisations(&self) -> usize;
}

pub fn optimise<'a, 'b>(
    mut statements: Vec<UpperStatement<'a>>,
    project: &'b Project,
) -> Vec<UpperStatement<'a>> {
    loop {
        let mut performed_optimisation = false;

        let passes: [Box<dyn OptimisationStep>; 3] = [
            Box::<UnnecessaryWiden>::default(),
            Box::new(ConstantFold::new(project)),
            Box::<MathIdentities>::default(),
        ];

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
