#![feature(exit_status_error)]
#![feature(custom_test_frameworks)]
#![feature(box_patterns)]

pub mod ast;
pub mod backends;
pub mod error;
pub mod interpreter;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod scope;
pub mod sema;
pub mod types;

#[macro_export]
macro_rules! id_impl {
    ($name:ident) => {
        impl std::ops::Deref for $name {
            type Target = u32;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl From<u32> for $name {
            fn from(x: u32) -> $name {
                $name(x)
            }
        }
    };
}
