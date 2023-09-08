#![feature(exit_status_error)]
#![feature(custom_test_frameworks)]
#![feature(box_patterns)]

pub mod ast;
pub mod backends;
pub mod error;
pub mod interpreter;
pub mod ir;
pub mod parser;
pub mod scope;
pub mod sema;
pub mod tokenizer;
pub mod types;
