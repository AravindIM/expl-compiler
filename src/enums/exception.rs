use super::{dtype::DType, operator::OpType};
pub enum SymbolTableError {
    NotFound {
        name: String,
        stype: SymbolType,
    },
    MultiDecl {
        name: String,
    },
    WrongSymbol {
        name: String,
        stype: SymbolType,
        prop: SymbolProp,
    },
    MultiLocalFn {
        name: String,
    },
}

pub enum ArrayError {
    NotInteger,
    NotConstant,
}

pub enum IndexError {
    Higher { name: String },
    NotArray { name: String },
}

pub enum SizeError {
    NotArray,
}

pub enum FnError {
    NoParams {
        fname: String,
    },
    MissingArg {
        fname: String,
        arg: String,
    },
    MissingParamDef {
        fname: String,
        param: String,
    },
    InvalidParam {
        fname: String,
        param: String,
    },
    MalformedBody {
        fname: String,
    },
    MoreParams {
        fname: String,
        expected: usize,
        found: usize,
    },
}

pub enum TypeError {
    Symbol {
        name: String,
        stype: SymbolType,
        expected: DType,
        found: DType,
    },
    Array(ArrayError),
    Literal(DType),
    Param {
        fname: String,
        param: String,
        expected: DType,
        found: DType,
    },
    InvalidParam {
        fname: String,
        param: String,
        dtype: DType,
    },
    Variable,
    Flow,
    Return {
        fname: String,
        expected: DType,
        found: DType,
    },
    InvalidReturn {
        fname: String,
    },
    BinaryOp {
        operation: OpType,
        lhs: DType,
        rhs: DType,
    },
    UnaryOp {
        operation: OpType,
        operand: DType,
    },
    VarAssign {
        name: String,
        lhs: DType,
        rhs: DType,
    },
    DerefAssign {
        lhs: DType,
        rhs: DType,
    },
}

pub enum OpError {
    NotVar { operation: OpType },
    Unary { operation: OpType },
    Binary { operation: OpType },
}

pub enum SymbolType {
    Var,
    Fn,
}

pub enum SymbolProp {
    DType,
    Dim,
    Binding,
    Param,
    Flabel,
}

pub enum ASTError {
    NoBinding,
    NoAddress,
}

pub enum AssignError {
    ArrayAssign,
    InvalidOp { operation: OpType },
    MissingLhs,
}
