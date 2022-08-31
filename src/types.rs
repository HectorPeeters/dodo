use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    UInt8(),
    UInt16(),
    UInt32(),
    UInt64(),
    Bool(),
    Ref(Box<Type>),
    Struct(String, Vec<(String, Type)>),
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
            Struct(_, fields) => fields.iter().map(|(_, x)| x.size()).sum(),
            Void() => unreachable!(),
            Unknown() => unreachable!(),
        }
    }

    #[must_use]
    pub fn get_ref(self) -> Self {
        Type::Ref(Box::new(self))
    }

    #[must_use]
    pub fn get_deref(self) -> Self {
        match self {
            Type::Ref(x) => *x,
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
            Type::Struct(_, _) => todo!(),
            Type::Void() => write!(f, "void"),
            Type::Unknown() => write!(f, "unknown"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_struct_single_field_size() {
        let struct_type = Type::Struct(
            "TestStruct".to_string(),
            vec![("a".to_string(), Type::UInt8())],
        );

        assert_eq!(struct_type.size(), Type::UInt8().size());
    }

    #[test]
    fn test_struct_multiple_fields_size() {
        let struct_type = Type::Struct(
            "TestStruct".to_string(),
            vec![
                ("a".to_string(), Type::UInt8()),
                ("b".to_string(), Type::Ref(Box::new(Type::Bool()))),
            ],
        );

        assert_eq!(
            struct_type.size(),
            Type::UInt8().size() + Type::Ref(Box::new(Type::Bool())).size()
        );
    }
}
