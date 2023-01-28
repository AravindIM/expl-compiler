use std::error::Error;
use crate::{errors::LangParseError, ST};

use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span, Lexeme};

#[derive(Debug, Clone, PartialEq)]
pub enum DType {
    Int,
    Bool,
    Str,
    Void
}

#[derive(Debug, Clone)]
pub enum OpType {
    Div,
    Mul,
    Add,
    Sub,
    Lt,
    Gt,
    Eq,
    NEq,
    LEq,
    GEq,
}

#[derive(Debug, Clone)]
pub enum FlowType {
    If,
    While,
    DoWhile,
    RepeatUntil,
    Continue,
    Break
}

#[derive(Debug, Clone)]
pub enum Tnode {
    NullProg,
    Op{
        dtype: DType,
        optype: OpType,
        lhs: Box<Tnode>,
        rhs: Box<Tnode>
    },
    Constant {
        dtype: DType,
        value: String,
    },
    Id {
        dtype: DType,
        name: String,
        address: Box<Tnode>,
    },
    Connector {
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Read {
        id: Box<Tnode>,
    },
    Write {
        expr: Box<Tnode>,
    },
    AsgStmt {
        id: Box<Tnode>,
        expr: Box<Tnode>,
    },
    FlowStmt {
        ftype: FlowType,
        bool_exprs: Option<Vec<Box<Tnode>>>,
        slists: Option<Vec<Box<Tnode>>>,
    },
}

impl Tnode {
    pub fn get_type(&self) -> Result<DType, Box<dyn Error>>  {
        match self {
            Tnode::Constant {dtype, ..} => Ok(dtype.to_owned()),
            Tnode::Id{dtype: _, name, ..} => ST.lock().unwrap().get_type(name).ok_or(Box::<dyn Error>::from("ERROR: Variable does not exist!")),
            Tnode::Op{dtype, ..} => Ok(dtype.to_owned()),
            _ => Ok(DType::Void),
        }
    }

    pub fn create_constant(span: Span, dtype: DType, lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>) -> Result<Tnode, LangParseError> {
        let value = lexer.span_str(span).to_string();
        match dtype {
            DType::Int => {
                let parse_result = value.parse::<i32>();
                match parse_result {
                    Ok(_) => return Ok(Tnode::Constant { dtype, value: value }),
                    Err(_) => Err(LangParseError(span, format!("ERROR: Invalid integer {}", value)))
                }
            }
            DType::Str => return Ok(Tnode::Constant { dtype, value: value }),
            _ => Err(LangParseError(span, format!("ERROR: Invalid datatype for constant {:?}", dtype)))
        }
    }

    pub fn create_id_node(name: &DefaultLexeme, offset: Option<Tnode>, lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>) -> Result<Tnode, LangParseError> {
        let varname = lexer.span_str(name.span()).to_string();
        let start_address = ST.lock().unwrap().get_address(&varname).ok_or(LangParseError(name.span(), format!("ERROR: Variable does not exist!")))?;
        let address = match offset {
            Some(offset) => {
                if offset.get_type().map_err(|_| LangParseError(name.span(), format!("Invalid type found for array index!"))).unwrap() == DType::Int {
                    // dbg!(start_address);
                    // dbg!(&offset);
                    Tnode::Op { dtype: DType::Int, optype: OpType::Add, lhs: Box::new(Tnode::Constant { dtype: DType::Int, value: format!("{}", start_address ) }), rhs: Box::new(offset) }

                } else {
                    return Err(LangParseError(name.span(), format!("Size unknown")));
                }
            },
            None => Tnode::Constant { dtype: DType::Int, value: format!("{}", start_address ) },
        };
        let id_node = Tnode::Id{ dtype: DType::Void, name: varname.clone(), address: Box::new(address.clone()) };
        let dtype = id_node.get_type().map_err(|_| LangParseError(name.span(), format!("variable `{}` was not declared before!", &name)))?;
        Ok( Tnode::Id{ dtype: dtype, name: varname, address: Box::new(address.clone()) } )
    }

    pub fn create_op_node(span: Span, operation: OpType, in_type: Vec<DType>, out_type: DType, lhs: Tnode, rhs: Tnode ) -> Result<Tnode, LangParseError> {
        let lhs_type = lhs.get_type().map_err(|_| LangParseError(span, format!("Invalid type found!"))).unwrap();
        let rhs_type = rhs.get_type().map_err(|_| LangParseError(span, format!("Invalid type found!"))).unwrap();
        if lhs_type == rhs_type && in_type.iter().any(|dtype| *dtype == lhs_type) {
            return Ok(Tnode::Op { dtype: out_type, optype: operation, lhs: Box::new(lhs), rhs: Box::new(rhs) });
        }
        Err(LangParseError(span, format!("ERROR: LHS (Type: {:?}) and RHS (Type: {:?}) have incompatible types for the operator: {:?}",lhs_type, rhs_type, operation)))
    }

    pub fn create_assign_node(span:Span, id: Tnode, expr: Tnode ) -> Result<Tnode, LangParseError> {
        let expr_dtype = expr.get_type().map_err(|_| LangParseError(span, format!("Invalid type found!"))).unwrap();
        match expr_dtype {
            DType::Int => {
                return Ok( Tnode::AsgStmt{ id: Box::new(id), expr: Box::new(expr)} );
            }
            DType::Str => {
                return Ok( Tnode::AsgStmt{ id: Box::new(id), expr: Box::new(expr)} );
            }
            _ => {
                if let Tnode::Id{ dtype, name, ..} = id {
                    return Err(LangParseError(span, format!("ERROR: Expected type {:?} but found {:?} for variable '{}'!", dtype, expr_dtype, name)))
                }
                return Err(LangParseError(span, format!("ERROR: Missing identifier")));
            }
        }
    }

    pub fn create_read_node(span:Span, id: Tnode) -> Result<Tnode, LangParseError> {
        let id_type = id.get_type().map_err(|_| LangParseError(span, format!("Invalid type found!"))).unwrap();
        match id_type {
            DType::Int => {
                if let Tnode::Id { .. } = id {
                    return Ok( Tnode::Read{ id: Box::new(id) } );
                }
            }
            DType::Str => {
                if let Tnode::Id { .. } = id {
                    return Ok( Tnode::Read{ id: Box::new(id) } );
                }
            }
            _ => return Err(LangParseError(span, format!("ERROR: Invalid datatype for identifier in read(): {:?}", id_type)))
        }

        Err(LangParseError(span, format!("ERROR: Invalid identifier!")))
    }

    pub fn create_write_node(span:Span, expr: Tnode) -> Result<Tnode, LangParseError> {
        let expr_type = expr.get_type().map_err(|_| LangParseError(span, format!("Invalid type found!"))).unwrap();
        match expr_type {
            DType::Int => return Ok( Tnode::Write{ expr: Box::new(expr) } ),
            DType::Str => return Ok( Tnode::Write{ expr: Box::new(expr) } ),
            _ => return Err(LangParseError(span, format!("ERROR: Invalid datatype for expression in write(): {:?}", expr_type)))
        }
    }

    pub fn create_flow_node(span:Span, ftype: FlowType, bool_exprs: Option<Vec<Tnode>>, slists: Option<Vec<Tnode>>) -> Result<Tnode, LangParseError> {
        match ftype {
            FlowType::Continue => return Ok(Tnode::FlowStmt{ ftype, bool_exprs: None, slists: None }),
            FlowType::Break => return Ok(Tnode::FlowStmt{ ftype, bool_exprs: None, slists: None }),
            _ => {
                match bool_exprs {
                    Some(bool_list) => {
                        let is_bool = bool_list.iter().all(|bool_expr| bool_expr.get_type().map_err(|_| LangParseError(span, format!("Invalid type found!"))).unwrap() == DType::Bool);
                        if is_bool {
                            if let Some(stmt_list) = slists {
                                return Ok(Tnode::FlowStmt{ ftype, bool_exprs: Some(bool_list.into_iter().map(|b| Box::new(b)).collect()), slists: Some(stmt_list.into_iter().map(|s| Box::new(s)).collect()) });
                            }
                        }
                        return Err(LangParseError(span, format!("ERROR: No Boolean expression found for {:?}", ftype)))
                    }
                    None => return Err(LangParseError(span, format!("ERROR: No Boolean expression found for {:?}", ftype)))
                }
            }
        }
    }

    pub fn get_address(&self) -> Result<usize, Box<dyn Error>> {
        
        if let Tnode::Id{ dtype: _, name, ..} = self {
            return ST.lock().unwrap().get_address(name).ok_or(Box::<dyn Error>::from("ERROR: Variable does not exist!"));
        }
        Err("ERROR: Cannot get address of anything other than Identifier".into())
    }

}
