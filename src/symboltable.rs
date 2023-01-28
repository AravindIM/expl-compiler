use std::collections::HashMap;

use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Lexeme};

use crate::{tnode::DType, errors::LangParseError};

pub struct TableEntry {
    pub dtype: DType,
    pub size: usize,
    pub binding: usize
}

pub struct Declaration {
    pub dtype: DType,
    pub variables: HashMap<String,usize>
}

impl Declaration {
    pub fn new(dtype: DType, variables: HashMap<String,usize>) -> Declaration {
        Declaration { dtype: dtype.clone(), variables: variables.clone() }
    }

    pub fn variable(name: &DefaultLexeme, size: Option<&DefaultLexeme>, lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>) -> Result<HashMap<String, usize>, LangParseError> {
        let name = lexer.span_str(name.span()).to_string();
        let var_size: usize = match size {
            Some(size) => lexer.span_str(size.span()).to_string().parse().unwrap(),
            None => 1
        };
        let mut var_entry = HashMap::new();
        var_entry.insert(name, var_size);
        Ok(var_entry.clone())
    }

    pub fn varlist(mut prevlist: HashMap<String, usize>, name: &DefaultLexeme, size: Option<&DefaultLexeme>, lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>) -> Result<HashMap<String, usize>, LangParseError> {
        let name = lexer.span_str(name.span()).to_string();
        let var_size: usize = match size {
            Some(size) => lexer.span_str(size.span()).to_string().parse().unwrap(),
            None => 1
        };
        prevlist.insert(name, var_size);
        Ok(prevlist)
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
        for varname in decl.variables.keys() {
            self.append(varname.to_string(), decl.dtype.clone(), *decl.variables.get(varname).unwrap());
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