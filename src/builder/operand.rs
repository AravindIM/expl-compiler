use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};

use crate::{
    enums::{
        ast::Ast,
        dimension::Dimension,
        dtype::{DType, Primitive},
        exception::{FnError, IndexError, SymbolType, TypeError},
        operator::OpType,
        symbol::Symbol,
    },
    exception::semantic::SemanticError,
    GST, LST,
};

pub fn create_literal(
    span: Span,
    dtype: DType,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<Ast, SemanticError> {
    let value = lexer.span_str(span).to_string();
    let literal = Ast::Literal {
        span,
        dtype: dtype.clone(),
        value,
    };
    match dtype {
        DType::Data(Primitive::Int) | DType::Pointer(_) => {
            literal
                .to_int()
                .map_err(SemanticError::from_compiler(span))?;
        }
        DType::Data(Primitive::Str) => {}
        _ => return Err(SemanticError::dtype(span, TypeError::Literal(dtype))),
    };
    Ok(literal)
}

pub fn create_id(
    span: Span,
    name: DefaultLexeme,
    index: Dimension,
    args: Option<Vec<Ast>>,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<Ast, SemanticError> {
    let name_span = name.span();
    let name = lexer.span_str(name_span).to_string();
    let symbol = match LST.lock().unwrap().get(&name, SymbolType::Var) {
        Ok(symbol) => Ok(symbol),
        Err(_) => GST.lock().unwrap().get(&name, SymbolType::Var),
    }
    .map_err(SemanticError::from_compiler(name_span))?;
    match symbol {
        Symbol::GVar {
            dtype,
            dim,
            binding,
            ..
        } => {
            let mut address_type = dtype.clone();
            let mut address = Ast::Literal {
                span,
                dtype: DType::Pointer(Box::new(address_type.clone())),
                value: format!("{}", binding),
            };
            match dim {
                Dimension::Array(dim_size) => {
                    match index {
                        Dimension::Array(dim_index) => {
                            if dim_index.len() > dim_size.len() {
                                return Err(SemanticError::index(
                                    span,
                                    IndexError::Higher { name },
                                ));
                            }
                            for i in 0..dim_index.len() {
                                if dim_index[i]
                                    .get_type()
                                    .map_err(SemanticError::from_compiler(dim_index[i].span()))?
                                    == DType::Data(Primitive::Int)
                                {
                                    let mut product = dim_index[i].clone();
                                    address_type = DType::Pointer(Box::new(address_type.clone()));
                                    for j in i + 1..dim_index.len() {
                                        product = Ast::Op {
                                            span,
                                            dtype: DType::Data(Primitive::Int),
                                            optype: OpType::Mul,
                                            lhs: Box::new(product.clone()),
                                            rhs: Some(Box::new(dim_size[j].clone())),
                                        };
                                    }
                                    address = Ast::Op {
                                        span,
                                        dtype: address_type.clone(),
                                        optype: OpType::Add,
                                        lhs: Box::new(address),
                                        rhs: Some(Box::new(product)),
                                    };
                                }
                            }
                            // dbg!(address.clone());
                            if dim_index.len() < dim_size.len() {
                                return Ok(address);
                            }
                        }
                        Dimension::Unit => return Ok(address),
                    }
                }
                Dimension::Unit => {
                    if let Dimension::Array(_) = index {
                        return Err(SemanticError::index(span, IndexError::NotArray { name }));
                    }
                }
            }
            Ok(Ast::Var {
                span,
                dtype: dtype.clone(),
                name: name.clone(),
                address: Box::new(address.clone()),
                is_local: false,
            })
        }
        Symbol::LVar { dtype, binding, .. } => {
            let address_type = dtype.clone();
            let address = Ast::Literal {
                span,
                dtype: DType::Pointer(Box::new(address_type.clone())),
                value: format!("{}", binding),
            };

            if let Dimension::Array(_) = index {
                return Err(SemanticError::index(span, IndexError::NotArray { name }));
            }

            Ok(Ast::Var {
                span,
                dtype: dtype.clone(),
                name: name.clone(),
                address: Box::new(address.clone()),
                is_local: true,
            })
        }
        Symbol::Fn {
            dtype,
            params,
            flabel,
            ..
        } => {
            match params {
                Some(params) => {
                    let param_names: Vec<&String> = params.keys().collect();
                    match args.clone() {
                        Some(args) => {
                            for param in param_names {
                                let index = params.get_index_of(param).unwrap();
                                let arg = args.get(index).ok_or_else(|| {
                                    SemanticError::func(
                                        span,
                                        FnError::MissingArg {
                                            fname: name.clone(),
                                            arg: param.clone(),
                                        },
                                    )
                                })?;
                                let param_dtype = params.get(param).unwrap();
                                let arg_dtype = arg
                                    .get_type()
                                    .map_err(SemanticError::from_compiler(arg.span()))?;
                                if *param_dtype != arg_dtype {
                                    return Err(SemanticError::dtype(
                                        arg.span(),
                                        TypeError::Param {
                                            fname: name,
                                            param: param.clone(),
                                            expected: param_dtype.clone(),
                                            found: arg_dtype.clone(),
                                        },
                                    ));
                                }
                            }
                        }
                        None => {
                            return Err(SemanticError::func(
                                span,
                                FnError::MissingArg {
                                    fname: name,
                                    arg: param_names.clone().get(0).unwrap().to_string(),
                                },
                            ))
                        }
                    };
                }
                None => {
                    if let Some(_) = args {
                        return Err(SemanticError::func(span, FnError::NoParams { fname: name }));
                    }
                }
            };
            let args = match args {
                Some(args) => Some(Box::new(args)),
                None => None,
            };
            Ok(Ast::FnCall {
                span,
                dtype: dtype.clone(),
                name: name.clone(),
                flabel,
                args,
            })
        }
    }
}
