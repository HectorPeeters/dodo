use crate::types::{Type, TypeId};

pub struct Project {
    pub name: String,
    pub types: Vec<Type>,
}

pub const BUILTIN_TYPE_UNKNOWN: usize = 0;
pub const BUILTIN_TYPE_VOID: usize = 1;
pub const BUILTIN_TYPE_U8: usize = 2;
pub const BUILTIN_TYPE_U16: usize = 3;
pub const BUILTIN_TYPE_U32: usize = 4;
pub const BUILTIN_TYPE_U64: usize = 5;
pub const BUILTIN_TYPE_BOOL: usize = 6;

impl Project {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            types: vec![
                Type::Unknown(),
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

    pub fn lookup_builtin_type(&mut self, name: &str) -> Option<TypeId> {
        match name {
            "void" => Some(BUILTIN_TYPE_VOID),
            "u8" => Some(BUILTIN_TYPE_U8),
            "u16" => Some(BUILTIN_TYPE_U16),
            "u32" => Some(BUILTIN_TYPE_U32),
            "u64" => Some(BUILTIN_TYPE_U64),
            "bool" => Some(BUILTIN_TYPE_BOOL),
            _ => unreachable!(),
        }
    }

    pub fn is_ptr_type(&self, id: TypeId) -> bool {
        matches!(self.types[id], Type::Ptr(_))
    }

    pub fn get_inner_type(&self, id: TypeId) -> TypeId {
        match self.types[id] {
            Type::Ptr(inner) => inner,
            _ => unreachable!(),
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
        self.types[id].size()
    }
}
