use crate::error::Result;
use clap::ValueEnum;
use std::{path::Path, process::Command};

pub mod c_backend;
pub mod x86_instruction;
pub mod x86_nasm_backend;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum BackendType {
    X86,
    C,
    // Ir,
}

pub trait Backend<'a> {
    fn process(&mut self) -> Result<()>;

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()>;

    fn run(&mut self, output: &Path) -> Result<String> {
        let absolute_path = std::fs::canonicalize(output).unwrap();
        let output = Command::new(absolute_path).output().unwrap();

        Ok(String::from_utf8(output.stdout).unwrap())
    }

    fn name(&self) -> &'static str;
}
