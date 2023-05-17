use std::path::Path;

use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{ast::UpperStatement, project::Project};

use super::Backend;

pub struct LlvmBackend<'a, 'ctx> {
    project: &'a mut Project,
    context: Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
}

impl<'a> LlvmBackend<'a> {
    pub fn new(project: &'a mut Project) -> Self {
        let context = Context::create();
        let builder = context.create_builder();
        let module = context.create_module("main_module");
        Self {
            project,
            context,
            builder,
            module,
        }
    }
}

impl<'a, 'b> Backend<'b> for LlvmBackend<'a> {
    fn process_upper_statement(
        &mut self,
        statement: UpperStatement<'b>,
    ) -> crate::error::Result<()> {
        todo!()
    }

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> crate::error::Result<()> {
        todo!()
    }

    fn name(&self) -> &'static str {
        todo!()
    }
}
