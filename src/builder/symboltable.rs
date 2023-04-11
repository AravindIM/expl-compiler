use indexmap::IndexMap;
use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer};

use crate::{
    enums::{
        ast::Ast,
        dimension::Dimension,
        dtype::{DType, Primitive},
        exception::{ArrayError, SymbolTableError, SymbolType, TypeError},
        symbol::Symbol,
    },
    exception::{semantic::SemanticError, compiler::CompilerError},
    function::ParamList,
    GST, LST,
};

pub fn append_gvar(
    name: DefaultLexeme,
    dtype: DType,
    dim: Dimension,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<(), SemanticError> {
    let span = name.span();
    let name = lexer.span_str(span).to_string();
    GST.lock().unwrap().queue.push(name.clone());
    GST.lock()
        .unwrap()
        .test(&name)
        .map_err(SemanticError::from_compiler(span))?;
    GST.lock().unwrap().tail += 1;
    let tail = GST.lock().unwrap().tail;
    GST.lock().unwrap().table.insert(
        name.clone(),
        Symbol::GVar {
            name,
            dtype,
            dim: dim.clone(),
            binding: tail,
        },
    );
    if let Dimension::Array(dim) = dim {
        let mut dim_size: isize = 1;
        for dim_entry in dim {
            match dim_entry.clone() {
                Ast::Literal { span, dtype, .. } => match dtype {
                    DType::Data(Primitive::Int) => {
                        let value = dim_entry
                            .to_int()
                            .map_err(SemanticError::from_compiler(span))?;
                        dim_size *= value;
                    }
                    _ => {
                        return Err(SemanticError::dtype(
                            span,
                            TypeError::Array(ArrayError::NotInteger),
                        ))
                    }
                },
                _ => {
                    return Err(SemanticError::dtype(
                        span,
                        TypeError::Array(ArrayError::NotConstant),
                    ))
                }
            }
            GST.lock().unwrap().tail += dim_size;
        }
    }
    Ok(())
}

pub fn append_lvar(
    name: DefaultLexeme,
    dtype: DType,
    is_param: bool,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<(), SemanticError> {
    let span = name.span();
    let name = lexer.span_str(span).to_string();
    if let Ok(symbol) = GST.lock().unwrap().get(&name, SymbolType::Fn) {
        if let Symbol::Fn { name, .. } = symbol {
            return Err(SemanticError::symboltable(
                span,
                SymbolTableError::MultiLocalFn { name },
            ));
        }
    }
    // dbg!("lvar", name.clone(), is_param);
    LST.lock().unwrap().queue.push(name.clone());
    LST.lock()
        .unwrap()
        .test(&name)
        .map_err(SemanticError::from_compiler(span))?;

    if is_param {
        if LST.lock().unwrap().tail >= 0 {
            LST.lock().unwrap().tail = 0 - 2;
        }
        LST.lock().unwrap().tail -= 1;
    } else {
        if LST.lock().unwrap().tail < 0 {
            LST.lock().unwrap().tail = 0;
        }
        LST.lock().unwrap().tail += 1;
    }
    let tail = LST.lock().unwrap().tail;
    LST.lock().unwrap().table.insert(
        name.clone(),
        Symbol::LVar {
            name,
            dtype,
            binding: tail,
        },
    );
    Ok(())
}

pub fn append_fn(
    name: DefaultLexeme,
    dtype: DType,
    params: Option<ParamList>,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<(), SemanticError> {
    let span = name.span();
    let name = lexer.span_str(span).to_string();
    GST.lock().unwrap().queue.push(name.clone());
    GST.lock()
        .unwrap()
        .test(&name)
        .map_err(SemanticError::from_compiler(span))?;
    LST.lock().unwrap().test(&name).map_err(|_| {
        SemanticError::symboltable(span, SymbolTableError::MultiLocalFn { name: name.clone() })
    })?;
    let flabel = GST.lock().unwrap().get_next_flabel();
    let params: Option<IndexMap<String, DType>> = match params {
        Some(params) => Some(
            params
                .into_iter()
                .map(|(key, (_, dtype))| (key, dtype))
                .collect(),
        ),
        None => None,
    };

    GST.lock().unwrap().table.insert(
        name.clone(),
        Symbol::Fn {
            name,
            dtype,
            params: params.clone(),
            flabel,
        },
    );
    Ok(())
}

pub fn get_global_symbol(name: &String, stype: SymbolType) -> Result<Symbol, CompilerError> {
    GST.lock().unwrap().get(&name, stype)
}

pub fn get_local_symbol(name: &String, stype: SymbolType) -> Result<Symbol, CompilerError> {
    LST.lock().unwrap().get(&name, stype)
}

pub fn get_gst_tail() -> isize {
    GST.lock().unwrap().get_tail()
}

pub fn get_lst_tail() -> isize {
    LST.lock().unwrap().get_tail()
}

pub fn reset_gst() {
    GST.lock().unwrap().reset();
}

pub fn reset_lst() {
    LST.lock().unwrap().reset();
}

pub fn dequeue_gst(prim: Primitive) -> Result<(), SemanticError> {
    GST.lock().unwrap().dequeue(prim)
}

pub fn dequeue_lst(prim: Primitive) -> Result<(), SemanticError> {
    LST.lock().unwrap().dequeue(prim)
}
