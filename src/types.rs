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

pub mod builtin_types {
    use super::{Type, TypeId};

    pub const UNKNOWN: TypeId = TypeId(u32::MAX);
    pub const VOID: TypeId = TypeId(0);
    pub const U8: TypeId = TypeId(1);
    pub const U16: TypeId = TypeId(2);
    pub const U32: TypeId = TypeId(3);
    pub const U64: TypeId = TypeId(4);
    pub const BOOL: TypeId = TypeId(5);

    pub const ALL_TYPES: [Type; 6] = [
        Type::Void(),
        Type::UInt8(),
        Type::UInt16(),
        Type::UInt32(),
        Type::UInt64(),
        Type::Bool(),
    ];

    pub fn from_str(name: &str) -> Option<TypeId> {
        match name {
            "void" => Some(VOID),
            "u8" => Some(U8),
            "u16" => Some(U16),
            "u32" => Some(U32),
            "u64" => Some(U64),
            "bool" => Some(BOOL),
            _ => None,
        }
    }
}

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

pub struct TypeStore {
    types: Vec<Type>,
}

impl Default for TypeStore {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeStore {
    pub fn new() -> Self {
        Self {
            types: builtin_types::ALL_TYPES.to_vec(),
        }
    }

    pub fn register_type(&mut self, t: Type) -> TypeId {
        self.types.push(t);
        (self.types.len() as u32 - 1).into()
    }

    pub fn get_type_id(&mut self, name: &str) -> Result<TypeId> {
        match builtin_types::from_str(name) {
            Some(builtin) => Ok(builtin),
            None => self
                .types
                .iter()
                .position(|x| matches!(x, Type::Struct(s) if s.name == name))
                .map(|p| (p as u32).into())
                .ok_or_else(|| {
                    Error::new(ErrorType::TypeCheck, format!("Could not find type {name}"))
                }),
        }
    }

    pub fn get_type_info(&self, id: TypeId) -> Result<&Type> {
        self.types.get(*id as usize).ok_or_else(|| {
            Error::new(
                ErrorType::TypeCheck,
                format!("Could not find type with id {}", *id),
            )
        })
    }

    pub fn find_or_add_type(&mut self, t: Type) -> TypeId {
        match self.types.iter().position(|x| x == &t) {
            Some(t) => (t as u32).into(),
            None => self.register_type(t),
        }
    }

    pub fn get_type_size(&self, id: TypeId) -> Result<usize> {
        let value_type = self.get_type_info(id)?;

        Ok(match value_type {
            Type::UInt8() => 8,
            Type::UInt16() => 16,
            Type::UInt32() => 32,
            Type::UInt64() => 64,
            Type::Bool() => 8,
            Type::Ptr(_) => 64,
            Type::Void() => unreachable!(),
            Type::Struct(s) => s
                .fields
                .iter()
                .map(|(_, t)| self.get_type_size(*t))
                .collect::<Result<Vec<_>>>()?
                .iter()
                .sum(),
        })
    }

    pub fn get_type_name(&self, id: TypeId) -> Result<String> {
        let type_info = self.get_type_info(id)?;
        Ok(match type_info {
            Type::UInt8() => "u8".to_owned(),
            Type::UInt16() => "u16".to_owned(),
            Type::UInt32() => "u32".to_owned(),
            Type::UInt64() => "u64".to_owned(),
            Type::Bool() => "bool".to_owned(),
            Type::Ptr(_) => format!("*{}", self.get_type_name(type_info.get_deref()?)?),
            Type::Void() => "void".to_owned(),
            Type::Struct(s) => s.name.clone(),
        })
    }
}
