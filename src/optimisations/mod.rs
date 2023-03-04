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

pub fn optimise<'a>(
    mut statements: Vec<UpperStatement<'a>>,
    project: &Project,
    print_optimisations: bool,
) -> Vec<UpperStatement<'a>> {
    let mut total_passes = 0;
    let mut total_optimisations = 0;

    loop {
        let mut performed_optimisation = false;

        let passes: [Box<dyn OptimisationStep>; 3] = [
            Box::<UnnecessaryWiden>::default(),
            Box::<MathIdentities>::default(),
            Box::new(ConstantFold::new(project)),
        ];

        for mut pass in passes {
            statements = statements
                .into_iter()
                .map(|x| pass.visit_upper_statement(x))
                .collect::<Vec<_>>();

            let performed_optimisations = pass.performed_optimisations();
            if performed_optimisations != 0 {
                if print_optimisations {
                    println!(
                        "Optimisation performed: {} x{}",
                        pass.name(),
                        pass.performed_optimisations()
                    );
                }
                performed_optimisation = true;

                total_passes += 1;
                total_optimisations += performed_optimisations;
            }
        }

        if !performed_optimisation {
            break;
        }
    }

    if print_optimisations {
        println!("Performed {total_passes} optimisation passes and {total_optimisations} individual optimisations.");
    }

    statements
}
