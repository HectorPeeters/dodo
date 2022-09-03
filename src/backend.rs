use std::path::Path;

use clap::ArgEnum;

use crate::{ast::UpperStatement, error::Result};

#[derive(Debug, Clone, Copy, ArgEnum)]
pub enum BackendType {
    X86,
    C,
}

pub trait Backend {
    fn process_upper_statement(&mut self, statement: UpperStatement) -> Result<()>;

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()>;

    fn name(&self) -> &'static str;
}
