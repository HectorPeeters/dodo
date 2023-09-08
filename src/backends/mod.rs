use crate::{ast::UpperStatement, error::Result};
use clap::ArgEnum;
use std::{path::Path, process::Command};

pub mod c_backend;
pub mod cranelift_backend;
pub mod ir_backend;
pub mod x86_instruction;
pub mod x86_nasm_backend;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ArgEnum)]
pub enum BackendType {
    X86,
    C,
    Ir,
    Cranelift,
}

pub trait Backend<'a> {
    fn process_upper_statement(&mut self, statement: UpperStatement<'a>) -> Result<()>;

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()>;

    fn run(&mut self, output: &Path) -> Result<String> {
        let absolute_path = std::fs::canonicalize(output).unwrap();
        let output = Command::new(absolute_path).output().unwrap();

        Ok(String::from_utf8(output.stdout).unwrap())
    }

    fn name(&self) -> &'static str;
}
