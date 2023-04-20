use indexmap::IndexMap;
use lrlex::DefaultLexeme;

use crate::enums::{ast::Ast, dtype::DType};

pub type ParamList = IndexMap<String, (DefaultLexeme, DType)>;

pub struct FnDef {
    pub flabel: usize,
    pub argc: usize,
    pub lsize: usize,
    pub body: Ast,
}

impl FnDef {
    pub fn new(flabel: usize, argc: usize, lsize: usize, body: Ast) -> FnDef {
        FnDef {
            flabel,
            argc,
            lsize,
            body,
        }
    }
}
