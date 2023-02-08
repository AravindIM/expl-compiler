use std::collections::HashMap;

use indexmap::IndexMap;
use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};

use crate::{
    errors::LangParseError,
    tnode::{DType, Tnode, Primitive},
};

pub type VarMap = IndexMap<String, (Span, DType, Dimension)>;

pub struct VarData {
    pub name: DefaultLexeme,
    pub dtype: DType,
    pub dim: Dimension
}

#[derive(Debug, Clone)]
pub enum Dimension {
    Unit,
    Array(Vec<Tnode>),
}

impl Dimension {
    pub fn array_size(size: Option<Dimension>, dim: Tnode) -> Result<Dimension, LangParseError> {
        match dim.clone() {
            Tnode::Constant {
                span: _,
                dtype,
                value: _,
            } => {
                match dtype {
                    DType::Data(Primitive::Int) => {
                        // let append_dim: usize = value.parse().map_err(|_| LangParseError(span, format!("ERROR: Invalid size specified in array!")))?;
                        return match size {
                            Some(size) => match size{
                                Dimension::Array(prevdim) => {
                                    let mut new_dim = prevdim.clone();
                                    new_dim.push(dim);
                                    Ok(Dimension::Array(new_dim))
                                }
                                Dimension::Unit => Err(LangParseError(
                                    dim.span(),
                                    format!("ERROR: Cannot add dimension to non-array variables!"),
                                )),
                            },
                            None => Ok(Dimension::Array(vec![dim]))
                        };
                    }
                    _ => {
                        return Err(LangParseError(
                            dim.span(),
                            format!("ERROR: Array size can only be integer!"),
                        ))
                    }
                }
            }
            _ => {
                return Err(LangParseError(
                    dim.span(),
                    format!("ERROR: Array size can only be constant!"),
                ))
            }
        }
    }

    pub fn array_index(loc: Option<Dimension>, index: Tnode) -> Result<Dimension, LangParseError> {
        if index.get_type().unwrap() == DType::Data(Primitive::Int) {
            return match loc {
                Some(loc) => match loc {
                    Dimension::Array(loc) => {
                        let mut loc = loc.clone();
                        loc.push(index);
                        return Ok(Dimension::Array(loc));
                    }
                    Dimension::Unit => {
                        return Err(LangParseError(
                            index.span(),
                            format!("ERROR: Array index not found!"),
                        ))
                    }
                },
                None => Ok(Dimension::Array(vec![index]))
            }
        } else {
            return Err(LangParseError(
                index.span(),
                format!("ERROR: Array size can only be integer!"),
            ));
        }
    }
}

#[derive(Debug)]
pub struct TableEntry {
    pub dtype: DType,
    pub dim: Dimension,
    pub binding: usize,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub variables: VarMap,
}

impl Declaration {
    pub fn new(prim: Primitive, variables: VarMap) -> Declaration {
        let mut varlist = variables.clone();
        for (_, (_, dtype, _)) in varlist.iter_mut() {
            *dtype = Self::update_prim(&prim, dtype)
        }
        // dbg!(variables.clone());
        // dbg!(varlist.clone());
        Declaration {
            variables: varlist.clone(),
        }
    }

    fn update_prim(prim: &Primitive, dtype: &DType) -> DType {
        match dtype {
            DType::Data(_) => DType::Data(prim.clone()),
            DType::Pointer(pointed) => DType::Pointer(Box::new(Self::update_prim(prim, pointed)))
        }
    }

    pub fn variables(
        prevlist: Option<VarMap>,
        variable: VarData,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    ) -> Result<VarMap, LangParseError> {
        let varname = lexer.span_str(variable.name.span()).to_string();
        let mut prevlist = match prevlist {
            Some(prev) => prev.clone(),
            None => IndexMap::new(),
        };

        if prevlist.contains_key(&varname) {
            let var_data = prevlist.get(&varname).unwrap();
            // dbg!("Same line");
            return Err(LangParseError(
                var_data.0,
                format!("ERROR: Variable declared multiple times!"),
            ));
        }

        prevlist.insert(varname, (variable.name.span(), variable.dtype.clone(), variable.dim.clone()));
        Ok(prevlist)
    }

    pub fn join(&mut self, decl: Declaration) -> Result<Declaration, LangParseError> {
        for varname in decl.variables.keys() {
            let var_data = decl.variables.get(varname).unwrap();
            if self.variables.contains_key(varname) {
                // dbg!("Different line");
                return Err(LangParseError(
                    var_data.0,
                    format!("ERROR: Variable declared multiple times!"),
                ));
            }
        }
        Ok(self.clone())
    }

    pub fn attach(
        decl: Result<(), LangParseError>,
        slist: Result<Tnode, LangParseError>,
    ) -> Result<Tnode, LangParseError> {
        if let Err(err) = decl {
            return Err(err);
        }
        return slist;
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    bp: usize,
    table: HashMap<String, TableEntry>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            bp: 4096,
            table: HashMap::new(),
        }
    }

    pub fn get_bp(&self) -> usize {
        self.bp
    }

    pub fn append_decl(&mut self, decl: Declaration) -> Result<(), LangParseError> {
        for varname in decl.variables.keys() {
            let var_data = decl.variables.get(varname).unwrap().clone();
            self.append(varname.to_string(), var_data.1.clone(), var_data.2)?;
        }
        // dbg!(self);
        Ok(())
    }

    pub fn append(
        &mut self,
        name: String,
        dtype: DType,
        dim: Dimension,
    ) -> Result<(), LangParseError> {
        self.table.insert(
            name,
            TableEntry {
                dtype,
                dim: dim.clone(),
                binding: self.bp,
            },
        );
        match dim {
            Dimension::Array(dim) => {
                let mut dim_size: usize = 1;
                for dim_entry in dim {
                    match dim_entry {
                        Tnode::Constant { span, dtype, value } => match dtype {
                            DType::Data(Primitive::Int) => {
                                let value: usize = value.parse().map_err(|_| {
                                    LangParseError(
                                        span,
                                        format!("ERROR: Invalid integer found for array size"),
                                    )
                                })?;
                                dim_size *= value;
                            }
                            _ => {
                                return Err(LangParseError(
                                    span,
                                    format!("ERROR: Array size should be an integer"),
                                ))
                            }
                        },
                        _ => {
                            return Err(LangParseError(
                                dim_entry.span(),
                                format!("ERROR: Array size should be a constant"),
                            ))
                        }
                    }
                }
                self.bp += dim_size;
            }
            Dimension::Unit => {
                self.bp += 1;
            }
        };
        Ok(())
    }

    pub fn get_type(&self, name: &String) -> Option<DType> {
        match self.table.get(name) {
            Some(table_entry) => Some(table_entry.dtype.clone()),
            None => None,
        }
    }

    pub fn get_address(&self, name: &String) -> Option<usize> {
        match self.table.get(name) {
            Some(table_entry) => Some(table_entry.binding),
            None => None,
        }
    }

    pub fn get_dim(&self, name: &String) -> Option<Dimension> {
        match self.table.get(name) {
            Some(table_entry) => Some(table_entry.dim.clone()),
            None => None,
        }
    }
}
