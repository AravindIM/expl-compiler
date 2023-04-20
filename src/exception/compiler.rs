#[derive(Debug)]
pub struct CompilerError(pub String);
use lrpar::Span;

use crate::enums::dtype::DType;
use crate::enums::exception::{
    ASTError, ArrayError, AssignError, FnError, IndexError, OpError, SizeError, SymbolProp,
    SymbolTableError, SymbolType, TypeError,
};

use std::error::Error;
use std::fmt;
use std::io;

use super::semantic::SemanticError;

impl CompilerError {
    pub fn not_literal() -> Self {
        CompilerError(format!("Not a literal"))
    }

    pub fn invalid_literal(dtype: DType) -> Self {
        match dtype {
            DType::Data(prim) => CompilerError(format!("Invalid {}!", prim)),
            DType::Pointer(_) => CompilerError(format!("Invalid value for pointer!")),
        }
    }

    pub fn invalid_expr() -> Self {
        CompilerError(format!("Invalid expression!"))
    }

    pub fn dtype(error: TypeError) -> Self {
        match error {
            TypeError::Symbol {
                name,
                stype,
                expected,
                found,
            } => {
                let name = match stype {
                    SymbolType::Var => name,
                    SymbolType::Fn => format!("{}()", name),
                };
                CompilerError(format!(
                    "Expected type `{}` but found `{}` for `{}`",
                    expected, found, name
                ))
            }
            TypeError::Literal(dtype) => CompilerError(format!(
                "Invalid datatype `{}` found for literal!",
                dtype
            )),
            TypeError::Param {
                fname,
                param,
                expected,
                found,
            } => CompilerError(format!(
                "Expected type `{}` but found `{}` for parameter `{}` in `{}()`",
                expected, found, param, fname
            )),
            TypeError::InvalidParam {
                fname,
                param,
                dtype,
            } => CompilerError(format!(
                "Invalid type `{}` for parameter `{}` in `{}()`",
                dtype, param, fname
            )),
            TypeError::Return {
                fname,
                expected,
                found,
            } => CompilerError(format!(
                "Expected return type of `{}` but found `{}` in body of `{}()`",
                expected, found, fname
            )),
            TypeError::InvalidReturn { fname } => CompilerError(format!(
                "invalid return type in body of `{}()`",
                fname
            )),
            TypeError::Flow => CompilerError(format!("No boolean expression found!")),
            TypeError::Variable => CompilerError(format!("Invalid identifier!")),
            TypeError::BinaryOp { operation, lhs, rhs } => CompilerError(format!("LHS (Type: `{}`) and RHS (Type: `{}`) have incompatible types for the operator `{}`!",lhs, rhs, operation)),
            TypeError::UnaryOp { operation, operand} => CompilerError(format!("The type `{}` is incompatible type for the operator `{}`!",operand, operation)),
            TypeError::VarAssign { name, lhs, rhs } => CompilerError(format!("Expected type `{}` but found `{}` for assignment of variable `{}`!", lhs, rhs, name)),
            TypeError::DerefAssign { lhs, rhs } => CompilerError(format!("Expected type `{}` but found `{}` for assignment using dereference!", lhs, rhs)),
            TypeError::Array(error) => {
                let literal_type = match error {
                    ArrayError::NotConstant => "constant",
                    ArrayError::NotInteger => "integer",
                };

                CompilerError(format!(
                    "Array size should only be an {}!",
                    literal_type
                ))
            }
        }
    }

    pub fn symboltable(error: SymbolTableError) -> Self {
        match error {
            SymbolTableError::NotFound { name, stype } => {
                let name = match stype {
                    SymbolType::Var => name,
                    SymbolType::Fn => format!("{}()", name),
                };
                CompilerError(format!("No declaration found for `{}`!", name))
            }
            SymbolTableError::WrongSymbol { name, stype, prop } => match stype {
                SymbolType::Var => match prop {
                    SymbolProp::Flabel | SymbolProp::Param => {
                        CompilerError(format!("`{}` is a not a function!", name))
                    }
                    SymbolProp::Dim => CompilerError(format!("`{}` is not an array!", name)),
                    _ => panic!("Invalid error"),
                },
                SymbolType::Fn => match prop {
                    SymbolProp::Dim => {
                        CompilerError(format!("`{}`() is a function and is not indexable!", name))
                    }
                    SymbolProp::Binding => CompilerError(format!(
                        "`{}`() is a function and does not have binding address!",
                        name
                    )),
                    _ => panic!("Invalid error"),
                },
            },
            SymbolTableError::MultiDecl { name } => {
                CompilerError(format!("`{}` was declared multiple times!", name))
            }
            SymbolTableError::MultiLocalFn { name } => CompilerError(format!(
                "Cannot create variable `{}` because a function already exists with the name `{}`!",
                name, name
            )),
        }
    }

    pub fn index(error: IndexError) -> Self {
        match error {
            IndexError::Higher { name } => CompilerError(format!(
                "Number of indexes found to be greater than dimension of array `{}`!",
                name
            )),
            IndexError::NotArray { name } => CompilerError(format!(
                "Variable `{}` cannot be indexed as it is not an array!",
                name
            )),
        }
    }

    pub fn size(error: SizeError) -> Self {
        match error {
            SizeError::NotArray => CompilerError(format!("Variable is not an array!")),
        }
    }

    pub fn func(error: FnError) -> Self {
        match error {
            FnError::MissingArg { fname, arg } => CompilerError(format!(
                "Missing argument for parameter `{}` in `{}()`",
                arg, fname
            )),
            FnError::NoParams { fname } => CompilerError(format!(
                "`{}()` does not have any parameters in declaration!",
                fname
            )),
            FnError::MissingParamDef { fname, param } => CompilerError(format!(
                "Missing parameter `{}` in definition of {}()",
                param, fname
            )),
            FnError::InvalidParam { fname, param } => CompilerError(format!(
                "Invalid parameter `{}` in definition of {}()",
                param, fname
            )),
            FnError::MalformedBody { fname } => {
                CompilerError(format!("Malformed body in `{}()`!", fname))
            }
            FnError::MoreParams {
                fname,
                expected,
                found,
            } => CompilerError(format!(
                "Expected {} parameters but found {} parameters in defintion of {}()!",
                expected, found, fname
            )),
            FnError::MoreDef { fname } => CompilerError(format!(
                "Function {}() has multiple definitions!", fname
            )),
        }
    }

    pub fn ast(error: ASTError) -> Self {
        match error {
            ASTError::NoBinding => CompilerError(format!("Can only fetch bindings for variables!")),
            ASTError::NoAddress => CompilerError(format!("Can only fetch address for variables!")),
        }
    }

    pub fn op(error: OpError) -> Self {
        match error {
            OpError::NotVar { operation } => CompilerError(format!(
                "`{}` operator can only be used with variables!",
                operation
            )),
            OpError::Unary { operation } => {
                CompilerError(format!("Unary operator `{}` got two operands!", operation))
            }
            OpError::Binary { operation } => CompilerError(format!(
                "Binary operator `{}` only got one operand!",
                operation
            )),
        }
    }

    pub fn assign(error: AssignError) -> Self {
        match error {
            AssignError::ArrayAssign => CompilerError(format!("Array cannot be reassigned!")),
            AssignError::InvalidOp { operation } => CompilerError(format!(
                "Invalid operation `{}` found in LHS of assignment!",
                operation
            )),
            AssignError::MissingLhs => CompilerError(format!(
                "Missing identifier or dereference in lhs for assignment!"
            )),
        }
    }

    pub fn get_message(&self) -> String {
        self.0.clone()
    }

    pub fn to_semantic(&self, span: Span) -> SemanticError {
        SemanticError::new(span, self.0.clone())
    }
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for CompilerError {}

impl From<io::Error> for CompilerError {
    fn from(error: io::Error) -> Self {
        CompilerError(format!("I/O error: {}", error))
    }
}

impl From<regex::Error> for CompilerError {
    fn from(error: regex::Error) -> Self {
        CompilerError(format!("Regex error: {}", error))
    }
}
