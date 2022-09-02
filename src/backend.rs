use std::path::Path;

use clap::ArgEnum;

use crate::{ast::UpperStatement, error::Result};

#[derive(Debug, Clone, Copy, ArgEnum)]
pub enum BackendType {
    X86,
    Cpp,
}

pub trait Backend {
    fn process_upper_statement(&mut self, statement: UpperStatement) -> Result<()>;

    fn finalize(&mut self, output: &Path) -> Result<()>;

    fn name(&self) -> &'static str;
}
