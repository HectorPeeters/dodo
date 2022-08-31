#![feature(int_log)]
#![feature(exit_status_error)]
#![feature(custom_test_frameworks)]

pub mod ast;
pub mod backend;
pub mod cpp;
pub mod error;
pub mod ir;
pub mod parser;
pub mod scope;
pub mod tokenizer;
pub mod type_checker;
pub mod types;
pub mod x86_instruction;
pub mod x86_nasm;
