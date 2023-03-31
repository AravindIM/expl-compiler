use std::collections::HashMap;

use indexmap::IndexMap;
use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};

use crate::{
    errors::LangParseError,
    tnode::{DType, Primitive, Tnode},
};

use std::error::Error;

pub type ParamList = IndexMap<String, DType>;
pub type VarMap = IndexMap<String, (Span, DType, Dimension, Option<ParamList>, Option<String>)>;

#[derive(Debug, Clone)]
pub enum Dimension {
    Unit,
    Array(Vec<Tnode>),
}

impl Dimension {
    pub fn array_size(size: Option<Dimension>, dim: Tnode) -> Result<Dimension, LangParseError> {
        match dim.clone() {
            Tnode::Literal {
                span: _,
                dtype,
                value: _,
            } => {
                match dtype {
                    DType::Data(Primitive::Int) => {
                        // let append_dim: usize = value.parse().map_err(|_| LangParseError(span, format!("ERROR: Invalid size specified in array!")))?;
                        return match size {
                            Some(size) => match size {
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
                            None => Ok(Dimension::Array(vec![dim])),
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
                None => Ok(Dimension::Array(vec![index])),
            };
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
    pub params: Option<IndexMap<String, DType>>,
    pub flabel: Option<usize>,
}

#[derive(Debug)]
pub struct SymbolTable {
    bp: usize,
    fl: usize,
    table: HashMap<String, TableEntry>,
    queue: IndexMap<String, (DType, Dimension, Option<ParamList>, Option<usize>)>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            bp: 0,
            fl: 0,
            table: HashMap::new(),
            queue: IndexMap::new(),
        }
    }

    pub fn get_next_flabel(&mut self) -> usize {
        self.fl += 1;
        self.fl
    }

    pub fn get_bp(&self) -> usize {
        self.bp
    }

    fn update_prim(prim: &Primitive, dtype: &DType) -> DType {
        match dtype {
            DType::Data(_) => DType::Data(prim.clone()),
            DType::Pointer(pointed) => DType::Pointer(Box::new(Self::update_prim(prim, pointed))),
        }
    }

    pub fn reset(&mut self) {
        self.bp = 0;
        self.table.clear();
        self.queue.clear();
    }

    pub fn enqueue(
        &mut self,
        varname: DefaultLexeme,
        vartype: DType,
        vardim: Dimension,
        params: Option<IndexMap<String, DType>>,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    ) -> Result<(), LangParseError> {
        let span = varname.span();
        let varname = lexer.span_str(varname.span()).to_string();
        if self.queue.contains_key(&varname) || self.table.contains_key(&varname) {
            return Err(LangParseError(
                span,
                format!("ERROR: Variable declared multiple times!"),
            ));
        }
        match params {
            Some(_) => {
                let flabel = self.get_next_flabel();
                self.queue
                    .insert(varname.to_owned(), (vartype, vardim, params, Some(flabel)))
            }
            None => self
                .queue
                .insert(varname.to_owned(), (vartype, vardim, params, None)),
        };
        Ok(())
    }

    pub fn dequeue(&mut self, varprim: Primitive) -> Result<(), LangParseError> {
        for varname in self.queue.clone().keys() {
            let vardata = self.queue.get(varname).unwrap();
            let datatype = vardata.0.clone();
            SymbolTable::update_prim(&varprim, &datatype);
            self.append(
                varname.to_owned(),
                datatype.clone(),
                vardata.1.clone(),
                vardata.2.clone(),
                vardata.3,
            )?;
        }
        self.queue.clear();
        Ok(())
    }

    pub fn append(
        &mut self,
        name: String,
        dtype: DType,
        dim: Dimension,
        params: Option<IndexMap<String, DType>>,
        flabel: Option<usize>,
    ) -> Result<(), LangParseError> {
        self.table.insert(
            name,
            TableEntry {
                dtype,
                dim: dim.clone(),
                binding: self.bp,
                params,
                flabel,
            },
        );
        match dim {
            Dimension::Array(dim) => {
                let mut dim_size: usize = 1;
                for dim_entry in dim {
                    match dim_entry {
                        Tnode::Literal { span, dtype, value } => match dtype {
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

    pub fn contains(&self, name: &String) -> bool {
        match self.table.get(name) {
            Some(_) => true,
            None => false,
        }
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

    pub fn get_params(&self, name: &String) -> Result<Option<ParamList>, Box<dyn Error>> {
        match self.table.get(name) {
            Some(table_entry) => Ok(table_entry.params.clone()),
            None => Err("ERROR: Cannot find the entry!".into()),
        }
    }

    pub fn get_flabel(&self, name: &String) -> Result<Option<usize>, Box<dyn Error>> {
        match self.table.get(name) {
            Some(table_entry) => Ok(table_entry.flabel),
            None => Err("ERROR: Cannot find the entry!".into()),
        }
    }

    pub fn create_params(
        oldp: Option<ParamList>,
        newp: Option<(String, DType, Span)>,
    ) -> Result<ParamList, LangParseError> {
        let mut plist = match oldp {
            Some(oldp) => oldp,
            None => ParamList::new(),
        };
        if let Some(newp) = newp {
            if plist.contains_key(&newp.0) {
                return Err(LangParseError(
                    newp.2,
                    format!("ERROR: Parameter already declared before!"),
                ));
            }
            plist.insert(newp.0, newp.1);
        }
        Ok(plist)
    }
}
