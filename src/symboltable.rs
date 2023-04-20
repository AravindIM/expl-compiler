use std::collections::HashMap;

use crate::{
    enums::dtype::{DType, Primitive},
    enums::exception::{SymbolTableError, SymbolType},
    enums::symbol::Symbol,
    exception::compiler::CompilerError,
    exception::semantic::SemanticError,
};

// pub type VarMap = IndexMap<String, (Span, DType, Dimension, Option<ParamList>, Option<String>)>;

#[derive(Debug)]
pub struct SymbolTable {
    pub tail: isize,
    pub fl: usize,
    pub table: HashMap<String, Symbol>,
    pub queue: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            tail: 0,
            fl: 0,
            table: HashMap::new(),
            queue: vec![],
        }
    }

    pub fn get(&self, name: &String, stype: SymbolType) -> Result<Symbol, CompilerError> {
        match self.table.get(name) {
            Some(symbol) => Ok(symbol.clone()),
            None => Err(CompilerError::symboltable(SymbolTableError::NotFound {
                name: name.clone(),
                stype,
            })),
        }
    }

    pub fn test(&self, name: &String) -> Result<(), CompilerError> {
        match self.get(name, SymbolType::Var) {
            Ok(_) => Err(CompilerError::symboltable(SymbolTableError::MultiDecl {
                name: name.clone(),
            })),
            Err(_) => Ok(()),
        }
    }

    pub fn get_next_flabel(&mut self) -> usize {
        self.fl += 1;
        self.fl
    }

    pub fn get_tail(&self) -> isize {
        self.tail
    }

    fn update_prim(prim: &Primitive, dtype: &DType) -> DType {
        match dtype {
            DType::Data(_) => DType::Data(prim.clone()),
            DType::Pointer(pointed) => DType::Pointer(Box::new(Self::update_prim(prim, pointed))),
        }
    }

    pub fn reset(&mut self) {
        self.tail = 0;
        self.table.clear();
        self.queue.clear();
    }

    pub fn dequeue(&mut self, prim: Primitive) -> Result<(), SemanticError> {
        // dbg!(self.queue.clone());
        for name in self.queue.clone() {
            let symbol = self.get(&name, SymbolType::Var).unwrap();
            let datatype = symbol.clone().get_type();
            let datatype = Self::update_prim(&prim, &datatype);
            let symbol = symbol.set_type(datatype);
            self.table.insert(name.clone(), symbol);
        }
        self.queue.clear();
        Ok(())
    }
}
