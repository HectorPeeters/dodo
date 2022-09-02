use std::fmt::Display;

pub type TypeId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    UInt8(),
    UInt16(),
    UInt32(),
    UInt64(),
    Bool(),
    Ref(TypeId),
    Void(),
    Unknown(),
}

impl Type {
    pub fn size(&self) -> usize {
        use Type::*;
        match self {
            UInt8() => 8,
            UInt16() => 16,
            UInt32() => 32,
            UInt64() => 64,
            Bool() => 8,
            Ref(_) => 64,
            Void() => unreachable!(),
            Unknown() => unreachable!(),
        }
    }

    #[must_use]
    pub fn get_deref(self) -> TypeId {
        match self {
            Type::Ref(x) => x,
            // TODO: change this to return a result
            _ => panic!("Trying to deref type which is not a ref"),
        }
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, Type::Ref(_))
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
            Type::Ref(x) => write!(f, "{x}*"),
            Type::Void() => write!(f, "void"),
            Type::Unknown() => write!(f, "unknown"),
        }
    }
}
