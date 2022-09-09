use crate::{ast::UpperStatement, error::Result};
use clap::ArgEnum;
use std::{io::Write, path::Path, process::Command};

pub mod c_backend;
pub mod ir_backend;
pub mod x86_instruction;
pub mod x86_nasm_backend;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ArgEnum)]
pub enum BackendType {
    X86,
    C,
    Ir,
}

pub trait Backend {
    fn process_upper_statement(&mut self, statement: UpperStatement) -> Result<()>;

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()>;

    fn run(&mut self, output: &Path) -> Result<()> {
        let absolute_path = std::fs::canonicalize(output).unwrap();
        let output = Command::new(absolute_path).output().unwrap();

        std::io::stdout().write_all(&output.stdout).unwrap();
        std::io::stderr().write_all(&output.stderr).unwrap();

        Ok(())
    }

    fn name(&self) -> &'static str;
}
