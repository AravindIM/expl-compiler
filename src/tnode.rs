use std::error::Error;
use crate::{errors::LangParseError, ST, symboltable::Dimension};

use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span, Lexeme};

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Int,
    Bool,
    Str,
    Void
}

#[derive(Debug, Clone, PartialEq)]
pub enum DType {
    Data(Primitive),
    Pointer(Box<DType>)
}

impl DType {
    pub fn get_primitive(&self) -> Primitive {
        Self::recursive_primitive(self)
    }
    fn recursive_primitive(dtype: &DType) -> Primitive {
        match dtype {
            DType::Data(prim) => prim.clone(),
            DType::Pointer(pointed) => Self::recursive_primitive(pointed)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpType {
    Mod,
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
    Amp,
    Deref
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
    NullProg{span: Span},
    Op{
        span: Span,
        dtype: DType,
        optype: OpType,
        lhs: Box<Tnode>,
        rhs: Option<Box<Tnode>>
    },
    Literal {
        span: Span,
        dtype: DType,
        value: String,
    },
    Id {
        span: Span,
        dtype: DType,
        name: String,
        address: Box<Tnode>,
    },
    Connector {
        span: Span,
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Read {
        span: Span,
        id: Box<Tnode>,
    },
    Write {
        span: Span,
        expr: Box<Tnode>,
    },
    AsgStmt {
        span: Span,
        id: Box<Tnode>,
        expr: Box<Tnode>,
    },
    FlowStmt {
        span: Span,
        ftype: FlowType,
        bool_exprs: Option<Vec<Box<Tnode>>>,
        slists: Option<Vec<Box<Tnode>>>,
    },
}

impl Tnode {
    pub fn get_type(&self) -> Result<DType, Box<dyn Error>>  {
        match self {
            Tnode::Literal {span:_, dtype, ..} => Ok(dtype.to_owned()),
            Tnode::Id{span:_, dtype: _, name, ..} => ST.lock().unwrap().get_type(name).ok_or(Box::<dyn Error>::from("ERROR: Variable does not exist!")),
            Tnode::Op{span:_, dtype, ..} => Ok(dtype.to_owned()),
            _ => Ok(DType::Data(Primitive::Void)),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Tnode::AsgStmt { span, .. } => *span,
            Tnode::Connector { span, .. } => *span,
            Tnode::Literal { span, .. } => *span,
            Tnode::FlowStmt { span, .. } => *span,
            Tnode::Id { span, .. } => *span,
            Tnode::NullProg { span } => *span,
            Tnode::Op { span, .. } => *span,
            Tnode::Read { span, .. } => *span,
            Tnode::Write { span, .. } => *span
        }
    }

    pub fn create_constant(span: Span, dtype: DType, lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>) -> Result<Tnode, LangParseError> {
        let value = lexer.span_str(span).to_string();
        match dtype {
            DType::Data(_) => {
                match dtype.get_primitive() {
                    Primitive::Int => {
                        let parse_result = value.parse::<i32>();
                        match parse_result {
                            Ok(_) => return Ok(Tnode::Literal { span, dtype, value: value }),
                            Err(_) => return Err(LangParseError(span, format!("ERROR: Invalid integer {}", value)))
                        }
                    },
                    Primitive::Str => {
                        return Ok(Tnode::Literal { span, dtype, value: value })
                    }
                    _ => Err(LangParseError(span, format!("ERROR: Invalid datatype for constant {:?}", dtype)))
                }
            }
            DType::Pointer(_) => {
                match dtype.get_primitive() {
                    Primitive::Int | Primitive::Str => {}
                    _ => return Err(LangParseError(span, format!("ERROR: Invalid datatype for constant {:?}", dtype)))
                }
                let parse_result = value.parse::<i32>();
                match parse_result {
                    Ok(_) => return Ok(Tnode::Literal { span, dtype, value: value }),
                    Err(_) => Err(LangParseError(span, format!("ERROR: Invalid pointer {}", value)))
                }
            }
        }
    }

    pub fn create_id_node(span: Span, name: &DefaultLexeme, index: Dimension, lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>) -> Result<Tnode, LangParseError> {
        let varname = lexer.span_str(name.span()).to_string();
        let start_address = ST.lock().unwrap().get_address(&varname).ok_or(LangParseError(name.span(), format!("ERROR: Variable does not exist!")))?;
        let dtype = ST.lock().unwrap().get_type(&varname).ok_or(LangParseError(name.span(), format!("ERROR: Variable does not exist!")))?;
        let size = ST.lock().unwrap().get_dim(&varname).ok_or(LangParseError(span, format!("ERROR: Array size not specified!")))?;
        let mut address: Tnode = match dtype.clone() {
            DType::Data(prim) => Tnode::Literal { span: span, dtype: DType::Pointer(Box::new(DType::Data(prim))), value: format!("{}", start_address) },
            DType::Pointer(pointed) => Tnode::Literal { span: span, dtype: DType::Pointer(pointed), value: format!("{}", start_address) },
        };
        match size {
            Dimension::Array(dim_size) => {
                match index {
                    Dimension::Array(dim_index) => {
                        if dim_index.len() != dim_size.len() {
                            return Err(LangParseError(name.span(), format!("ERROR: Array indexing does not match the dimension of the Array!")));
                        }
                        for i in 0..dim_index.len() {
                            if dim_index[i].get_type().map_err(|_| LangParseError(dim_index[i].span(), format!("ERROR: Invalid type found for array index!"))).unwrap() == DType::Data(Primitive::Int) {
                                let mut product = dim_index[i].clone() ;
                                for j in i+1..dim_index.len() {
                                    product = Tnode::Op {span, dtype: DType::Data(Primitive::Int), optype: OpType::Mul, lhs: Box::new(product.clone()), rhs: Some(Box::new(dim_size[j].clone())) };
                                }
                                address = Tnode::Op {span, dtype: DType::Data(Primitive::Int), optype: OpType::Add, lhs: Box::new(address), rhs: Some(Box::new(product)) };
                            }
                        }
                        // dbg!(address.clone());
                        return Ok(Tnode::Id{span, dtype: dtype.clone(), name: varname.clone(), address: Box::new(address.clone()) })
                    }
                    Dimension::Unit => return Err(LangParseError(span, format!("ERROR: Array variable cannot be accessed without specifying the index!")))
                }
            }
            Dimension::Unit => {
                match index {
                    Dimension::Array(_) => return Err(LangParseError(span, format!("ERROR: Array variable cannot be accessed without specifying the index!"))),
                    Dimension::Unit => return Ok(Tnode::Id{span, dtype: dtype.clone(), name: varname.clone(), address: Box::new(address.clone()) })
                }
            }
        }
    }

    pub fn get_op_rule(operation: OpType, lhs_type: DType, rhs_type: Option<DType>) -> Option<DType> {
        match operation {
            OpType::Mod | OpType::Div | OpType::Mul => {
                match rhs_type {
                    Some(rhs_type) => {
                        if lhs_type == rhs_type && lhs_type == DType::Data(Primitive::Int) {
                            return Some(lhs_type);
                        }
                        None
                    }
                    None => None
                }
            },
            OpType::Add | OpType::Sub => {
                match rhs_type {
                    Some(rhs_type) => {
                        match lhs_type {
                            DType::Data(Primitive::Int) => {
                                match rhs_type {
                                    DType::Data(Primitive::Int) | DType::Pointer(_) => Some(rhs_type),
                                    _ => None,
                                }
                            }
                            DType::Pointer(_) => {
                                match rhs_type {
                                    DType::Data(Primitive::Int) => Some(lhs_type),
                                    _ => None
                                }
                            }
                            _ => None
                        }
                    }
                    None => None
                }
            },
            OpType::Lt | OpType::Gt | OpType::Eq | OpType::NEq |OpType::LEq | OpType::GEq => {
                match rhs_type {
                    Some(rhs_type) => {
                        if lhs_type == rhs_type{
                            return match lhs_type {
                                DType::Data(Primitive::Int) | DType::Pointer(_) => Some(DType::Data(Primitive::Bool)),
                                _ => None
                            }
                        }
                        None
                    }
                    None => None
                }
            },
            OpType::Amp => {
                match lhs_type {
                    DType::Data(Primitive::Int) => Some(DType::Pointer(Box::new(lhs_type))),
                    DType::Data(Primitive::Str) => Some(DType::Pointer(Box::new(lhs_type))),
                    DType::Pointer(_) => Some(DType::Pointer(Box::new(lhs_type))),
                    _ => None
                }
            }
            OpType::Deref => {
                match lhs_type {
                    DType::Pointer(pointed) => Some(*pointed),
                    _ => None
                }
            }
        }
    }

    pub fn create_op_node(span: Span, operation: OpType, lhs: Tnode, rhs: Option<Tnode> ) -> Result<Tnode, LangParseError> {
        let lhs_type = lhs.get_type().map_err(|_| LangParseError(span, format!("ERROR: Invalid type found!"))).unwrap();
        let rhs_type = match rhs.clone() {
            Some(rhs) => Some(rhs.get_type().map_err(|_| LangParseError(span, format!("ERROR: Invalid type found!"))).unwrap()),
            None => None
        };
        // dbg!(operation.clone());
        let select_rule = Tnode::get_op_rule(operation.clone(), lhs_type.clone(), rhs_type.clone());
        if operation.clone() == OpType::Amp {
            match lhs.clone() {
                Tnode::Id { .. } => {}
                _ => return Err(LangParseError(span, format!("ERROR: {:?} operator can only be used with Identifiers", operation)))
            }

        }
        if let Some(output_type) = select_rule {
            return match rhs {
                Some(rhs) => Ok(Tnode::Op {span, dtype: output_type, optype: operation, lhs: Box::new(lhs), rhs: Some(Box::new(rhs)) }),
                None => Ok(Tnode::Op {span, dtype: output_type, optype: operation, lhs: Box::new(lhs), rhs: None }),
            }
        }
        match rhs {
            Some(_) => Err(LangParseError(span, format!("ERROR: LHS (Type: {:?}) and RHS (Type: {:?}) have incompatible types for the operator: {:?}",lhs_type, rhs_type, operation))),
            None => Err(LangParseError(span, format!("ERROR: The Type: {:?} is incompatible type for the operator: {:?}",lhs_type, operation)))
        }
    }

    pub fn create_assign_node(span:Span, id: Tnode, expr: Tnode ) -> Result<Tnode, LangParseError> {
        let expr_dtype = expr.get_type().map_err(|_| LangParseError(span, format!("ERROR: Invalid type found!"))).unwrap();
        return match id.clone() {
            Tnode::Id{span, dtype, name, ..} => {
                // dbg!(dtype.clone());
                // dbg!(expr_dtype.clone());
                if dtype == expr_dtype {
                    return Ok( Tnode::AsgStmt{span, id: Box::new(id), expr: Box::new(expr)} );
                }
                Err(LangParseError(span, format!("ERROR: Expected type {:?} but found {:?} for variable '{}'!", dtype, expr_dtype, name)))
            }
            Tnode::Op { span: _, dtype, optype, lhs, .. } => {
                if optype == OpType::Deref {
                    if dtype == expr_dtype {
                        return Ok(Tnode::AsgStmt { span, id: Box::new(id), expr: Box::new(expr) });
                    }
                    if let Tnode::Id { span, dtype, name, address:_ } = *lhs {
                        return Err(LangParseError(span, format!("ERROR: Expected type {:?} but found {:?} for assignment of '*{}'!", dtype, expr_dtype, name)));
                    }
                    return Err(LangParseError(span, format!("ERROR: Expected type {:?} but found {:?} for assignment using deref!", dtype, expr_dtype)))
                }
                return Err(LangParseError(span, format!("ERROR: Invalid operation {:?} found in LHS of assignment!", optype)))
            }
            _ => Err(LangParseError(span, format!("ERROR: Missing identifier or dereference in lhs for assignment")))
        }
    }

    pub fn create_read_node(span:Span, id: Tnode) -> Result<Tnode, LangParseError> {
        let id_type = id.get_type().map_err(|_| LangParseError(span, format!("ERROR: Invalid type found!"))).unwrap();
        match id_type {
            DType::Data(Primitive::Int) => {
                if let Tnode::Id { .. } = id {
                    return Ok( Tnode::Read{span, id: Box::new(id) } );
                }
            }
            DType::Data(Primitive::Str) => {
                if let Tnode::Id { .. } = id {
                    return Ok( Tnode::Read{span, id: Box::new(id) } );
                }
            }
            _ => return Err(LangParseError(span, format!("ERROR: Invalid datatype for identifier in read(): {:?}", id_type)))
        }

        Err(LangParseError(span, format!("ERROR: Invalid identifier!")))
    }

    pub fn create_write_node(span:Span, expr: Tnode) -> Result<Tnode, LangParseError> {
        let expr_type = expr.get_type().map_err(|_| LangParseError(span, format!("ERROR: Invalid type found!"))).unwrap();
        match expr_type {
            DType::Data(Primitive::Int) => return Ok( Tnode::Write{span, expr: Box::new(expr) } ),
            DType::Data(Primitive::Str) => return Ok( Tnode::Write{span, expr: Box::new(expr) } ),
            _ => return Err(LangParseError(span, format!("ERROR: Invalid datatype for expression in write(): {:?}", expr_type)))
        }
    }

    pub fn create_flow_node(span:Span, ftype: FlowType, bool_exprs: Option<Vec<Tnode>>, slists: Option<Vec<Tnode>>) -> Result<Tnode, LangParseError> {
        match ftype {
            FlowType::Continue => return Ok(Tnode::FlowStmt{span, ftype, bool_exprs: None, slists: None }),
            FlowType::Break => return Ok(Tnode::FlowStmt{span, ftype, bool_exprs: None, slists: None }),
            _ => {
                match bool_exprs {
                    Some(bool_list) => {
                        let is_bool = bool_list.iter().all(|bool_expr| bool_expr.get_type().map_err(|_| LangParseError(span, format!("Invalid type found!"))).unwrap() == DType::Data(Primitive::Bool));
                        if is_bool {
                            if let Some(stmt_list) = slists {
                                return Ok(Tnode::FlowStmt{span, ftype, bool_exprs: Some(bool_list.into_iter().map(|b| Box::new(b)).collect()), slists: Some(stmt_list.into_iter().map(|s| Box::new(s)).collect()) });
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
        
        if let Tnode::Id{span:_, dtype: _, name, ..} = self {
            return ST.lock().unwrap().get_address(name).ok_or(Box::<dyn Error>::from("ERROR: Variable does not exist!"));
        }
        Err("ERROR: Cannot get address of anything other than Identifier".into())
    }

}
