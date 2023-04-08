use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Int,
    Bool,
    Str,
    Void,
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Primitive::Int => write!(f, "int"),
            Primitive::Bool => write!(f, "bool"),
            Primitive::Str => write!(f, "str"),
            Primitive::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DType {
    Data(Primitive),
    Pointer(Box<DType>),
}

impl fmt::Display for DType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            DType::Data(ref primitive) => write!(f, "{}", primitive),
            DType::Pointer(ref dtype) => write!(f, "{}*", dtype),
        }
    }
}

impl DType {
    pub fn get_primitive(&self) -> Primitive {
        Self::recursive_primitive(self)
    }
    fn recursive_primitive(dtype: &DType) -> Primitive {
        match dtype {
            DType::Data(prim) => prim.clone(),
            DType::Pointer(pointed) => Self::recursive_primitive(pointed),
        }
    }
}
