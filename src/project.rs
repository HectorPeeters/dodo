use crate::error::{Error, ErrorType, Result};
use crate::types::{StructType, Type, TypeId};

pub struct Project {
    pub name: String,
    pub types: Vec<Type>,
}

pub const BUILTIN_TYPE_UNKNOWN: usize = 999999;
pub const BUILTIN_TYPE_VOID: usize = 0;
pub const BUILTIN_TYPE_U8: usize = 1;
pub const BUILTIN_TYPE_U16: usize = 2;
pub const BUILTIN_TYPE_U32: usize = 3;
pub const BUILTIN_TYPE_U64: usize = 4;
pub const BUILTIN_TYPE_BOOL: usize = 5;

impl Project {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            types: vec![
                Type::Void(),
                Type::UInt8(),
                Type::UInt16(),
                Type::UInt32(),
                Type::UInt64(),
                Type::Bool(),
            ],
        }
    }

    pub fn register_type(&mut self, t: Type) -> TypeId {
        self.types.push(t);
        self.types.len() - 1
    }

    pub fn get_type_id(&mut self, name: &str) -> Result<TypeId> {
        match name {
            "void" => Ok(BUILTIN_TYPE_VOID),
            "u8" => Ok(BUILTIN_TYPE_U8),
            "u16" => Ok(BUILTIN_TYPE_U16),
            "u32" => Ok(BUILTIN_TYPE_U32),
            "u64" => Ok(BUILTIN_TYPE_U64),
            "bool" => Ok(BUILTIN_TYPE_BOOL),
            _ => self
                .types
                .iter()
                .position(|x| matches!(x, Type::Struct(s) if s.name == name))
                .ok_or_else(|| {
                    Error::new(ErrorType::TypeCheck, format!("Could not find type {name}"))
                }),
        }
    }

    pub fn get_type(&self, id: TypeId) -> Result<&Type> {
        self.types.get(id).ok_or_else(|| {
            Error::new(
                ErrorType::TypeCheck,
                format!("Could not find type with id {id}"),
            )
        })
    }

    pub fn get_struct(&self, id: TypeId) -> Result<&StructType> {
        match self.get_type(id)? {
            Type::Struct(s) => Ok(s),
            _ => Err(Error::new(
                ErrorType::TypeCheck,
                format!("Type with id {id} is not a struct"),
            )),
        }
    }

    pub fn find_or_add_type(&mut self, t: Type) -> TypeId {
        match self.types.iter().position(|x| x == &t) {
            Some(t) => t,
            None => {
                self.types.push(t);
                self.types.len() - 1
            }
        }
    }

    pub fn get_type_size(&self, id: TypeId) -> usize {
        self.types[id].size(self)
    }
}
