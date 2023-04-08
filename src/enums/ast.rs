use super::exception::{ASTError, SymbolType, TypeError};
use super::{
    dtype::{DType, Primitive},
    flow::FlowType,
    operator::OpType,
};
use crate::{exception::compiler::CompilerError, GST};
use lrpar::Span;

#[derive(Debug, Clone)]
pub enum Ast {
    NullProg {
        span: Span,
    },
    Op {
        span: Span,
        dtype: DType,
        optype: OpType,
        lhs: Box<Ast>,
        rhs: Option<Box<Ast>>,
    },
    Literal {
        span: Span,
        dtype: DType,
        value: String,
    },
    Var {
        span: Span,
        dtype: DType,
        name: String,
        address: Box<Ast>,
        is_local: bool,
    },
    FnCall {
        span: Span,
        dtype: DType,
        name: String,
        flabel: usize,
        args: Option<Box<Vec<Ast>>>,
    },
    Connector {
        span: Span,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    Read {
        span: Span,
        variable: Box<Ast>,
    },
    Write {
        span: Span,
        expr: Box<Ast>,
    },
    AsgStmt {
        span: Span,
        variable: Box<Ast>,
        expr: Box<Ast>,
    },
    FlowStmt {
        span: Span,
        ftype: FlowType,
        bool_exprs: Option<Vec<Box<Ast>>>,
        slists: Option<Vec<Box<Ast>>>,
    },
    ReturnStmt {
        span: Span,
        dtype: DType,
        expr: Box<Ast>,
    },
}

impl Ast {
    pub fn get_type(&self) -> Result<DType, CompilerError> {
        match self {
            Ast::Literal { dtype, .. } => Ok(dtype.to_owned()),
            Ast::Var { dtype, .. } => Ok(dtype.to_owned()),
            Ast::FnCall { dtype, .. } => Ok(dtype.to_owned()),
            Ast::Op { dtype, .. } => Ok(dtype.to_owned()),
            _ => Err(CompilerError::invalid_expr()),
        }
    }

    pub fn to_int(&self) -> Result<isize, CompilerError> {
        match self {
            Ast::Literal { dtype, value, .. } => match dtype {
                DType::Data(Primitive::Int) | DType::Pointer(_) => value
                    .parse()
                    .map_err(|_| CompilerError::invalid_literal(dtype.clone())),
                DType::Data(_) => Err(CompilerError::dtype(TypeError::Literal(dtype.clone()))),
            },
            _ => Err(CompilerError::not_literal()),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Ast::AsgStmt { span, .. } => *span,
            Ast::Connector { span, .. } => *span,
            Ast::Literal { span, .. } => *span,
            Ast::FlowStmt { span, .. } => *span,
            Ast::Var { span, .. } => *span,
            Ast::FnCall { span, .. } => *span,
            Ast::NullProg { span } => *span,
            Ast::Op { span, .. } => *span,
            Ast::Read { span, .. } => *span,
            Ast::Write { span, .. } => *span,
            Ast::ReturnStmt { span, .. } => *span,
        }
    }

    pub fn get_binding(&self) -> Result<isize, CompilerError> {
        match self {
            Ast::Var { name, .. } => GST
                .lock()
                .unwrap()
                .get(name, SymbolType::Var)?
                .get_binding(),
            _ => Err(CompilerError::ast(ASTError::NoBinding)),
        }
    }

    pub fn get_address(&self) -> Result<Ast, CompilerError> {
        match self {
            Ast::Var { dtype, span, .. } => Ok(Ast::Op {
                span: *span,
                dtype: DType::Pointer(Box::new(dtype.clone())),
                optype: OpType::Amp,
                lhs: Box::new(self.clone()),
                rhs: None,
            }),
            _ => Err(CompilerError::ast(ASTError::NoAddress)),
        }
    }
}
