use crate::{ast::UpperStatement, error::Result};
use clap::ArgEnum;
use std::path::Path;

pub mod c_generator;
pub mod x86_instruction;
pub mod x86_nasm;

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
