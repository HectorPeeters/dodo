#![feature(int_log)]
#![feature(exit_status_error)]
#![feature(custom_test_frameworks)]
#![feature(box_patterns)]

pub mod ast;
pub mod backends;
pub mod error;
pub mod interpreter;
pub mod ir;
pub mod optimisations;
pub mod parser;
pub mod project;
pub mod scope;
pub mod tokenizer;
pub mod type_checker;
pub mod types;
