use lrpar::Span;

use crate::enums::exception::{
    AssignError, FnError, IndexError, OpError, SizeError, SymbolTableError, TypeError,
};

use super::compiler::CompilerError;

#[derive(Debug, Clone)]
pub struct SemanticError(pub Span, pub String);

impl SemanticError {
    pub fn new(span: Span, message: String) -> Self {
        SemanticError(span, message)
    }
    pub fn dtype(span: Span, error: TypeError) -> Self {
        CompilerError::dtype(error).to_semantic(span)
    }

    pub fn symboltable(span: Span, error: SymbolTableError) -> Self {
        CompilerError::symboltable(error).to_semantic(span)
    }

    pub fn index(span: Span, error: IndexError) -> Self {
        CompilerError::index(error).to_semantic(span)
    }

    pub fn func(span: Span, error: FnError) -> Self {
        CompilerError::func(error).to_semantic(span)
    }

    pub fn op(span: Span, error: OpError) -> Self {
        CompilerError::op(error).to_semantic(span)
    }

    pub fn assign(span: Span, error: AssignError) -> Self {
        CompilerError::assign(error).to_semantic(span)
    }

    pub fn size(span: Span, error: SizeError) -> Self {
        CompilerError::size(error).to_semantic(span)
    }

    pub fn from_compiler(span: Span) -> impl Fn(CompilerError) -> Self {
        move |e| e.to_semantic(span)
    }
}
