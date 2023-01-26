use std::{collections::HashMap};

use crate::tnode::DType;

pub struct TableEntry {
    pub dtype: DType,
    pub size: usize,
    pub binding: usize
}

pub struct Declaration {
    pub dtype: DType,
    pub variables: Vec<String>
}

impl Declaration {
    pub fn new(dtype: DType, variables: Vec<String>) -> Declaration {
        Declaration { dtype: dtype.clone(), variables: variables.clone() }
    }
    
}

pub struct SymbolTable {
    bp: usize,
    table: HashMap<String, TableEntry>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable { bp: 4096, table: HashMap::new() }
    }

    pub fn get_bp(&self) -> usize {
        self.bp
    }

    pub fn append_decl(&mut self, decl: Declaration){
        for varname in &decl.variables {
            self.append(varname.to_string(), decl.dtype.clone(), 1);
        }
    }

    pub fn append(&mut self, name: String, dtype: DType, size: usize) {
        self.table.insert(name, TableEntry { dtype, size, binding: self.bp });
        self.bp += size;
    }

    pub fn get_type(&self, name: &String) -> Option<DType> {
        match self.table.get(name){
            Some(table_entry) => Some(table_entry.dtype.clone()),
            None => None
        }
    }

    pub fn get_address(&self, name: &String) -> Option<usize> {
        match self.table.get(name) {
            Some(table_entry) => Some(table_entry.binding),
            None => None            
        }
    }
}