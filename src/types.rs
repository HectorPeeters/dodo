#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    UInt8(),
    UInt16(),
    UInt32(),
    UInt64(),
    Bool(),
    Ref(Box<Type>),
    Void(),
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
