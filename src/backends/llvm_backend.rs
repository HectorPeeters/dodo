use std::path::Path;

use crate::{ast::UpperStatement, project::Project};

use super::Backend;

pub struct LlvmBackend<'a> {
    project: &'a mut Project,
}

impl<'a> LlvmBackend<'a> {
    pub fn new(project: &'a mut Project) -> Self {
        Self { project }
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
