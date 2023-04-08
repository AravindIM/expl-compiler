use indexmap::IndexMap;

use crate::exception::compiler::CompilerError;

use super::{
    dimension::Dimension,
    dtype::DType,
    exception::{SymbolProp, SymbolTableError, SymbolType},
};

#[derive(Debug, Clone)]
pub enum Symbol {
    GVar {
        name: String,
        dtype: DType,
        dim: Dimension,
        binding: isize,
    },
    LVar {
        name: String,
        dtype: DType,
        binding: isize,
    },
    Fn {
        name: String,
        dtype: DType,
        params: Option<IndexMap<String, DType>>,
        flabel: usize,
    },
}

impl Symbol {
    pub fn get_type(self) -> DType {
        match self {
            Self::GVar { dtype, .. } | Self::LVar { dtype, .. } | Self::Fn { dtype, .. } => dtype,
        }
    }

    pub fn set_type(self, dtype: DType) -> Symbol {
        match self {
            Self::GVar {
                name, dim, binding, ..
            } => Self::GVar {
                name,
                dtype,
                dim,
                binding,
            },
            Self::LVar { name, binding, .. } => Self::LVar {
                name,
                dtype,
                binding,
            },
            Self::Fn {
                name,
                params,
                flabel,
                ..
            } => Self::Fn {
                name,
                dtype,
                params,
                flabel,
            },
        }
    }

    pub fn get_binding(&self) -> Result<isize, CompilerError> {
        match self {
            Symbol::GVar { binding, .. } | Symbol::LVar { binding, .. } => Ok(*binding),
            Symbol::Fn { name, .. } => {
                Err(CompilerError::symboltable(SymbolTableError::WrongSymbol {
                    name: name.clone(),
                    stype: SymbolType::Fn,
                    prop: SymbolProp::Binding,
                }))
            }
        }
    }

    pub fn get_dim(&self) -> Result<Dimension, CompilerError> {
        match self {
            Symbol::GVar { dim, .. } => Ok(dim.clone()),
            Symbol::LVar { name, .. } => {
                Err(CompilerError::symboltable(SymbolTableError::WrongSymbol {
                    name: name.clone(),
                    stype: SymbolType::Var,
                    prop: SymbolProp::Dim,
                }))
            }
            Symbol::Fn { name, .. } => {
                Err(CompilerError::symboltable(SymbolTableError::WrongSymbol {
                    name: name.clone(),
                    stype: SymbolType::Fn,
                    prop: SymbolProp::Dim,
                }))
            }
        }
    }

    pub fn get_params(&self) -> Result<Option<IndexMap<String, DType>>, CompilerError> {
        match self {
            Symbol::Fn { params, .. } => Ok(params.clone()),
            Self::GVar { name, .. } | Self::LVar { name, .. } => {
                Err(CompilerError::symboltable(SymbolTableError::WrongSymbol {
                    name: name.clone(),
                    stype: SymbolType::Fn,
                    prop: SymbolProp::Binding,
                }))
            }
        }
    }

    pub fn get_flabel(&self) -> Result<usize, CompilerError> {
        match self {
            Symbol::Fn { flabel, .. } => Ok(*flabel),
            Self::GVar { name, .. } | Self::LVar { name, .. } => {
                Err(CompilerError::symboltable(SymbolTableError::WrongSymbol {
                    name: name.clone(),
                    stype: SymbolType::Fn,
                    prop: SymbolProp::Binding,
                }))
            }
        }
    }
}
