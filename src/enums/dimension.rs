use crate::exception::semantic::SemanticError;

use super::{
    ast::Ast,
    dtype::{DType, Primitive},
    exception::{ArrayError, IndexError, SizeError, TypeError},
};

#[derive(Debug, Clone)]
pub enum Dimension {
    Unit,
    Array(Vec<Ast>),
}

impl Dimension {
    pub fn array_size(size: Option<Dimension>, dim: Ast) -> Result<Dimension, SemanticError> {
        match dim.clone() {
            Ast::Literal {
                span: _,
                dtype,
                value: _,
            } => match dtype {
                DType::Data(Primitive::Int) => {
                    return match size {
                        Some(size) => match size {
                            Dimension::Array(prevdim) => {
                                let mut new_dim = prevdim.clone();
                                new_dim.push(dim);
                                Ok(Dimension::Array(new_dim))
                            }
                            Dimension::Unit => {
                                Err(SemanticError::size(dim.span(), SizeError::NotArray))
                            }
                        },
                        None => Ok(Dimension::Array(vec![dim])),
                    };
                }
                _ => {
                    return Err(SemanticError::dtype(
                        dim.span(),
                        TypeError::Array(ArrayError::NotInteger),
                    ))
                }
            },
            _ => {
                return Err(SemanticError::dtype(
                    dim.span(),
                    TypeError::Array(ArrayError::NotConstant),
                ))
            }
        }
    }

    pub fn array_index(loc: Option<Dimension>, index: Ast) -> Result<Dimension, SemanticError> {
        if index
            .get_type()
            .map_err(SemanticError::from_compiler(index.span()))?
            == DType::Data(Primitive::Int)
        {
            return match loc {
                Some(loc) => match loc {
                    Dimension::Array(loc) => {
                        let mut loc = loc.clone();
                        loc.push(index);
                        return Ok(Dimension::Array(loc));
                    }
                    Dimension::Unit => {
                        return Err(SemanticError::index(
                            index.span(),
                            IndexError::NotArray {
                                name: "array variable".to_string(),
                            },
                        ))
                    }
                },
                None => Ok(Dimension::Array(vec![index])),
            };
        } else {
            return Err(SemanticError::dtype(
                index.span(),
                TypeError::Array(ArrayError::NotInteger),
            ));
        }
    }
}
