use super::Backend;
use crate::error::Result;
use crate::{ast::UpperStatement, project::Project};
use std::path::Path;

pub struct IrBackend<'a> {
    project: &'a mut Project,
}

impl<'a> IrBackend<'a> {
    pub fn new(project: &'a mut Project) -> Self {
        Self { project }
    }
}

impl<'a> Backend for IrBackend<'a> {
    fn process_upper_statement(&mut self, statement: UpperStatement) -> Result<()> {
        todo!()
    }

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()> {
        todo!()
    }

    fn name(&self) -> &'static str {
        todo!()
    }
}
