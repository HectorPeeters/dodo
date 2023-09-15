use std::fmt::{Display, Formatter};

use crate::{
    error::{Error, ErrorType, Result},
    id_impl,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeId(u32);
id_impl!(TypeId);

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub const BUILTIN_TYPE_UNKNOWN: TypeId = TypeId(u32::MAX);
pub const BUILTIN_TYPE_VOID: TypeId = TypeId(0);
pub const BUILTIN_TYPE_U8: TypeId = TypeId(1);
pub const BUILTIN_TYPE_U16: TypeId = TypeId(2);
pub const BUILTIN_TYPE_U32: TypeId = TypeId(3);
pub const BUILTIN_TYPE_U64: TypeId = TypeId(4);
pub const BUILTIN_TYPE_BOOL: TypeId = TypeId(5);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, TypeId)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::UInt8() => write!(f, "u8"),
            Type::UInt16() => write!(f, "u16"),
            Type::UInt32() => write!(f, "u32"),
            Type::UInt64() => write!(f, "u64"),
            Type::Bool() => write!(f, "bool"),
            Type::Ptr(x) => write!(f, "{}*", **x),
            Type::Void() => write!(f, "void"),
            Type::Struct(s) => write!(f, "{}", s.name),
        }
    }
}
