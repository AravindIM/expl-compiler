use lrpar::Span;

use crate::enums::ast::Ast;
use crate::enums::dtype::{DType, Primitive};
use crate::enums::exception::{OpError, TypeError};
use crate::enums::operator::OpType;
use crate::exception::semantic::SemanticError;

fn get_op_rule(operation: OpType, lhs_type: DType, rhs_type: Option<DType>) -> Option<DType> {
    match operation {
        OpType::Mod | OpType::Div | OpType::Mul => match rhs_type {
            Some(rhs_type) => {
                if lhs_type == rhs_type && lhs_type == DType::Data(Primitive::Int) {
                    return Some(lhs_type);
                }
                None
            }
            None => None,
        },
        OpType::Add | OpType::Sub => match rhs_type {
            Some(rhs_type) => match lhs_type {
                DType::Data(Primitive::Int) => match rhs_type {
                    DType::Data(Primitive::Int) | DType::Pointer(_) => Some(rhs_type),
                    _ => None,
                },
                DType::Pointer(_) => match rhs_type {
                    DType::Data(Primitive::Int) => Some(lhs_type),
                    _ => None,
                },
                _ => None,
            },
            None => None,
        },
        OpType::Lt | OpType::Gt | OpType::Eq | OpType::NEq | OpType::LEq | OpType::GEq => {
            match rhs_type {
                Some(rhs_type) => {
                    if lhs_type == rhs_type {
                        return match lhs_type {
                            DType::Data(Primitive::Int) | DType::Pointer(_) => {
                                Some(DType::Data(Primitive::Bool))
                            }
                            _ => None,
                        };
                    }
                    None
                }
                None => None,
            }
        }
        OpType::And | OpType::Or => match rhs_type {
            Some(rhs_type) => {
                if lhs_type == rhs_type {
                    return match lhs_type {
                        DType::Data(Primitive::Bool) => Some(DType::Data(Primitive::Bool)),
                        _ => None,
                    };
                }
                None
            }
            None => None,
        },
        OpType::Amp => match lhs_type {
            DType::Data(Primitive::Int) => Some(DType::Pointer(Box::new(lhs_type))),
            DType::Data(Primitive::Str) => Some(DType::Pointer(Box::new(lhs_type))),
            DType::Pointer(_) => Some(DType::Pointer(Box::new(lhs_type))),
            _ => None,
        },
        OpType::Deref => match lhs_type {
            DType::Pointer(pointed) => Some(*pointed),
            _ => None,
        },
    }
}

pub fn create_op(
    span: Span,
    operation: OpType,
    lhs: Ast,
    rhs: Option<Ast>,
) -> Result<Ast, SemanticError> {
    let lhs_type = lhs.get_type().map_err(SemanticError::from_compiler(span))?;
    let rhs_type = match rhs.clone() {
        Some(rhs) => Some(rhs.get_type().map_err(SemanticError::from_compiler(span))?),
        None => None,
    };
    // dbg!(operation.clone());
    let select_rule = get_op_rule(operation.clone(), lhs_type.clone(), rhs_type.clone());
    if operation.clone() == OpType::Amp {
        match lhs.clone() {
            Ast::Var { .. } => {}
            _ => return Err(SemanticError::op(span, OpError::NotVar { operation })),
        }
    }
    if let Some(output_type) = select_rule {
        return match rhs {
            Some(rhs) => Ok(Ast::Op {
                span,
                dtype: output_type,
                optype: operation,
                lhs: Box::new(lhs),
                rhs: Some(Box::new(rhs)),
            }),
            None => Ok(Ast::Op {
                span,
                dtype: output_type,
                optype: operation,
                lhs: Box::new(lhs),
                rhs: None,
            }),
        };
    }
    match rhs {
        Some(_) => Err(SemanticError::dtype(
            span,
            TypeError::BinaryOp {
                operation,
                lhs: lhs_type,
                rhs: rhs_type.unwrap(),
            },
        )),
        None => Err(SemanticError::dtype(
            span,
            TypeError::UnaryOp {
                operation,
                operand: lhs_type,
            },
        )),
    }
}
