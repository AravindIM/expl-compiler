use std::{error::Error};

use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Lexeme};

#[derive(Debug, Clone, PartialEq)]
pub enum DType {
    Int,
    Bool,
    Void
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum FlowType {
    If,
    While,
    DoWhile,
    RepeatUntil,
    Continue,
    Break
}

#[derive(Debug)]
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
    pub fn get_type(&self) -> DType {
        match self {
            Tnode::Constant {dtype, ..} => dtype.to_owned(),
            Tnode::Id{dtype, ..} => dtype.to_owned(),
            Tnode::Op{dtype, ..} => dtype.to_owned(),
            _ => DType::Void,
        }
    }

    pub fn create_constant(dtype: DType, lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>, value: &DefaultLexeme) -> Result<Tnode, Box<dyn Error>> {
        let value = lexer.span_str(value.span()).to_string();
        match dtype {
            DType::Int => {
                let parse_result = value.parse::<i32>();
                match parse_result {
                    Ok(_) => return Ok(Tnode::Constant { dtype, value: value }),
                    Err(_) => Err(format!("ERROR: Invalid integer {}", value).into())
                }
            }
            _ => Err(format!("ERROR: Invalid datatype for constant {:?}", dtype).into())
        }
    }

    pub fn create_id_node(lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>, name: &DefaultLexeme) -> Result<Tnode, Box<dyn Error>> {
        let name = lexer.span_str(name.span()).to_string();
        Ok( Tnode::Id{ dtype: DType::Void, name: name } )
    }

    pub fn create_op_node(operation: OpType, dtype: DType, lhs: Tnode, rhs: Tnode ) -> Result<Tnode, Box<dyn Error>> {
        if lhs.get_type() == rhs.get_type() && lhs.get_type() == dtype {
            return Ok(Tnode::Op { dtype: dtype, optype: operation, lhs: Box::new(lhs), rhs: Box::new(rhs) });
        }
        Err(format!("ERROR: LHS and RHS have incompatible types for the operator: {:?}", operation).into())
    }

    pub fn create_assign_node(id: &mut Tnode, expr: Tnode ) -> Result<Tnode, Box<dyn Error>> {
        let expr_dtype = expr.get_type();
        match expr_dtype {
            DType::Int => {
                if let Tnode::Id { ref mut dtype, name } = id {
                    *dtype = DType::Int;
                    let id = Tnode::Id{dtype:DType::Int, name: name.clone()};
                    return Ok( Tnode::AsgStmt{ id: Box::new(id), expr: Box::new(expr)} );
                }
            }
            _ => return Err(format!("ERROR: Invalid datatype for identifier: {:?}", expr_dtype).into())
        }

        Err(format!("ERROR: Invalid identifier!").into())
    }

    pub fn create_flow_node(ftype: FlowType, bool_exprs: Option<Vec<Tnode>>, slists: Option<Vec<Tnode>>) -> Result<Tnode, Box<dyn Error>> {
        match ftype {
            FlowType::Continue => return Ok(Tnode::FlowStmt{ ftype, bool_exprs: None, slists: None }),
            FlowType::Break => return Ok(Tnode::FlowStmt{ ftype, bool_exprs: None, slists: None }),
            _ => {
                match bool_exprs {
                    Some(bool_list) => {
                        let is_bool = bool_list.iter().all(|bool_expr| bool_expr.get_type() == DType::Bool);
                        if is_bool {
                            if let Some(stmt_list) = slists {
                                return Ok(Tnode::FlowStmt{ ftype, bool_exprs: Some(bool_list.into_iter().map(|b| Box::new(b)).collect()), slists: Some(stmt_list.into_iter().map(|s| Box::new(s)).collect()) });
                            }
                        }
                        return Err(format!("ERROR: No Boolean expression found for {:?}", ftype).into())
                    }
                    None => return Err(format!("ERROR: No Boolean expression found for {:?}", ftype).into())
                }
            }
        }
    }

    pub fn get_address(&self) -> Result<u32, Box<dyn Error>> {
        if let Tnode::Id{ dtype: _, name} = self {
            return Ok(name
                .chars()
                .nth(0)
                .ok_or(Box::<dyn Error>::from("ERROR: Invalid variable!"))? as u32
                - 'a' as u32
                + 4096);
        }
        Err("ERROR: Cannot get address of anything other than Identifier".into())
    }

}
