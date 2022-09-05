use crate::{ast::UpperStatement, error::Result};
use clap::ArgEnum;
use std::path::Path;

pub mod c_backend;
pub mod ir_backend;
pub mod x86_instruction;
pub mod x86_nasm_backend;

#[derive(Debug, Clone, Copy, ArgEnum)]
pub enum BackendType {
    X86,
    C,
    Ir,
}

pub trait Backend {
    fn process_upper_statement(&mut self, statement: UpperStatement) -> Result<()>;

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()>;

    fn name(&self) -> &'static str;
}
