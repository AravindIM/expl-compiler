use lrpar::Span;

use crate::{
    enums::{
        ast::Ast,
        dtype::{DType, Primitive},
        exception::{AssignError, TypeError},
        flow::FlowType,
        operator::OpType,
    },
    exception::semantic::SemanticError,
};

pub fn create_read(span: Span, variable: Ast) -> Result<Ast, SemanticError> {
    let variable_type = variable
        .get_type()
        .map_err(SemanticError::from_compiler(span))?;
    match variable_type {
        DType::Data(Primitive::Int) | DType::Data(Primitive::Str) => {
            if let Ast::Var { .. } = variable {
                return Ok(Ast::Read {
                    span,
                    variable: Box::new(variable),
                });
            }
        }
        _ => {
            return Err(SemanticError::dtype(
                variable.span(),
                TypeError::InvalidParam {
                    fname: "read".to_string(),
                    param: "variable".to_string(),
                    dtype: variable_type,
                },
            ))
        }
    }

    Err(SemanticError::dtype(variable.span(), TypeError::Variable))
}

pub fn create_write(span: Span, expr: Ast) -> Result<Ast, SemanticError> {
    let expr_type = expr
        .get_type()
        .map_err(SemanticError::from_compiler(span))?;
    match expr_type {
        DType::Data(Primitive::Int) => Ok(Ast::Write {
            span,
            expr: Box::new(expr),
        }),
        DType::Data(Primitive::Str) => Ok(Ast::Write {
            span,
            expr: Box::new(expr),
        }),
        DType::Pointer(_) => Ok(Ast::Write {
            span,
            expr: Box::new(expr),
        }),
        _ => Err(SemanticError::dtype(
            expr.span(),
            TypeError::InvalidParam {
                fname: "write".to_string(),
                param: "expression".to_string(),
                dtype: expr_type,
            },
        )),
    }
}

pub fn create_flow(
    span: Span,
    ftype: FlowType,
    bool_exprs: Option<Vec<Ast>>,
    slists: Option<Vec<Ast>>,
) -> Result<Ast, SemanticError> {
    match ftype {
        FlowType::Continue => Ok(Ast::FlowStmt {
            span,
            ftype,
            bool_exprs: None,
            slists: None,
        }),
        FlowType::Break => Ok(Ast::FlowStmt {
            span,
            ftype,
            bool_exprs: None,
            slists: None,
        }),
        _ => match bool_exprs {
            Some(bool_list) => {
                let is_bool = bool_list.iter().all(|bool_expr| {
                    bool_expr.get_type().unwrap_or(DType::Data(Primitive::Void))
                        == DType::Data(Primitive::Bool)
                });
                if is_bool {
                    if let Some(stmt_list) = slists {
                        return Ok(Ast::FlowStmt {
                            span,
                            ftype,
                            bool_exprs: Some(bool_list.into_iter().map(|b| Box::new(b)).collect()),
                            slists: Some(stmt_list.into_iter().map(|s| Box::new(s)).collect()),
                        });
                    }
                }
                Err(SemanticError::dtype(span, TypeError::Flow))
            }
            None => Err(SemanticError::dtype(span, TypeError::Flow)),
        },
    }
}

pub fn create_return(span: Span, expr: Ast) -> Result<Ast, SemanticError> {
    // dbg!(expr.clone());
    Ok(Ast::ReturnStmt {
        span,
        dtype: expr
            .get_type()
            .map_err(SemanticError::from_compiler(expr.span()))?,
        expr: Box::new(expr),
    })
}

pub fn create_assign(span: Span, variable: Ast, expr: Ast) -> Result<Ast, SemanticError> {
    let expr_dtype = expr
        .get_type()
        .map_err(SemanticError::from_compiler(span))?;
    return match variable.clone() {
        Ast::Var {
            span, dtype, name, ..
        } => {
            // dbg!(dtype.clone());
            // dbg!(expr_dtype.clone());
            if dtype == expr_dtype {
                return Ok(Ast::AsgStmt {
                    span,
                    variable: Box::new(variable),
                    expr: Box::new(expr),
                });
            }
            Err(SemanticError::dtype(
                span,
                TypeError::VarAssign {
                    name,
                    lhs: dtype,
                    rhs: expr_dtype,
                },
            ))
        }
        Ast::Op {
            span: _,
            dtype,
            optype,
            lhs,
            ..
        } => {
            match optype {
                OpType::Deref => {
                    if dtype == expr_dtype {
                        return Ok(Ast::AsgStmt {
                            span,
                            variable: Box::new(variable),
                            expr: Box::new(expr),
                        });
                    }
                    return Err(SemanticError::dtype(
                        span,
                        TypeError::DerefAssign {
                            lhs: dtype,
                            rhs: expr_dtype,
                        },
                    ));
                }
                OpType::Add => {
                    if let Ast::Literal { span: _, dtype, .. } = *lhs {
                        if let DType::Pointer(_) = dtype {
                            return Err(SemanticError::assign(span, AssignError::ArrayAssign));
                        }
                    }
                }
                _ => {}
            }
            return Err(SemanticError::assign(
                span,
                AssignError::InvalidOp { operation: optype },
            ));
        }
        _ => Err(SemanticError::assign(span, AssignError::MissingLhs)),
    };
}

pub fn create_connector(span: Span, lhs: Ast, rhs: Ast) -> Result<Ast, SemanticError> {
    Ok(Ast::Connector {
        span,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    })
}

pub fn create_nullprog(span: Span) -> Result<Ast, SemanticError> {
    Ok(Ast::NullProg { span })
}
