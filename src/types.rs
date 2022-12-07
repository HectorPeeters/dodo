use std::fmt::Display;

use crate::error::{Error, ErrorType, Result};
use crate::project::Project;

pub type TypeId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, TypeId)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    UInt8(),
    UInt16(),
    UInt32(),
    UInt64(),
    Bool(),
    Ptr(TypeId),
    Void(),
    Struct(StructType),
}

impl Type {
    pub fn size(&self, project: &Project) -> usize {
        use Type::*;
        match self {
            UInt8() => 8,
            UInt16() => 16,
            UInt32() => 32,
            UInt64() => 64,
            Bool() => 8,
            Ptr(_) => 64,
            Void() => unreachable!(),
            Struct(s) => s
                .fields
                .iter()
                .map(|(_, t)| project.get_type_size(*t))
                .sum(),
        }
    }

    pub fn get_deref(&self) -> Result<TypeId> {
        match self {
            Type::Ptr(x) => Ok(*x),
            _ => Err(Error::new(
                ErrorType::TypeCheck,
                "Trying to deref type which is not a ptr".to_owned(),
            )),
        }
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct(_))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::UInt8() => write!(f, "u8"),
            Type::UInt16() => write!(f, "u16"),
            Type::UInt32() => write!(f, "u32"),
            Type::UInt64() => write!(f, "u64"),
            Type::Bool() => write!(f, "bool"),
            Type::Ptr(x) => write!(f, "{x}*"),
            Type::Void() => write!(f, "void"),
            Type::Struct(s) => write!(f, "{}", s.name),
        }
    }
}
