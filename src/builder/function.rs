use indexmap::IndexMap;
use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};

use crate::{
    enums::{
        ast::Ast,
        dtype::DType,
        exception::{FnError, SymbolType, TypeError},
        symbol::Symbol,
    },
    exception::semantic::SemanticError,
    function::{FnDef, ParamList},
};

use super::symboltable::{append_lvar, get_global_symbol, reset_lst, get_lst_tail, set_fn_defined};

pub fn create_fn(
    rtype: DType,
    name: DefaultLexeme,
    params: Option<ParamList>,
    lvars: Result<(), SemanticError>,
    body: Result<Ast, SemanticError>,
    span: Span,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<FnDef, SemanticError> {
    let name_span = name.span();
    let name = lexer.span_str(name_span).to_string();
    let symbol: Symbol;
    let mut flabel: usize = 0;
    let decl_params: IndexMap<String, DType>;
    let params = params.unwrap_or(IndexMap::new());

    if name != "main" {
        symbol = get_global_symbol(&name, SymbolType::Fn)
            .map_err(SemanticError::from_compiler(span))?;
        decl_params = symbol
            .get_params()
            .map_err(SemanticError::from_compiler(span))?
            .unwrap_or(IndexMap::new());
        flabel = symbol
            .get_flabel()
            .map_err(SemanticError::from_compiler(name_span))?;
        if symbol.is_fn_defined().map_err(SemanticError::from_compiler(span))? {
            return Err(SemanticError::func(span, FnError::MoreDef { fname: name }));
        }
    } else {
        decl_params = IndexMap::new();
    }

    let params_len = params.len();
    let decl_params_len = decl_params.len();
    if params_len > decl_params_len {
        return Err(SemanticError::func(
            span,
            FnError::MoreParams {
                fname: name,
                expected: decl_params_len,
                found: params_len,
            },
        ));
    }
    for dparam in decl_params.keys() {
        let dparam = dparam.to_string();
        let index = decl_params.get_index_of(&dparam).unwrap();
        match params.keys().collect::<Vec<&String>>().get(index) {
            Some(param_name) => {
                let param_name = param_name.to_string();
                let param = params.get(&param_name).unwrap().clone();
                let param_type = param.1;
                let param_lexeme = param.0;
                let param_span = param_lexeme.span();
                let dparam_type = decl_params.get(&dparam).unwrap().clone();
                // dbg!(dparam.clone());
                // dbg!(param_name.clone());
                if dparam != param_name {
                    return Err(SemanticError::func(
                        param_span,
                        FnError::InvalidParam {
                            fname: name,
                            param: param_name,
                        },
                    ));
                }
                // dbg!(param_type.clone());
                // dbg!(dparam_type.clone());
                if param_type != dparam_type {
                    return Err(SemanticError::dtype(
                        param_span,
                        TypeError::Param {
                            fname: name,
                            param: dparam.clone(),
                            expected: dparam_type,
                            found: param_type,
                        },
                    ));
                }
            }
            None => {
                return Err(SemanticError::func(
                    span,
                    FnError::MissingParamDef {
                        fname: name,
                        param: dparam.clone(),
                    },
                ))
            }
        }
    }

    lvars?;

    let body = body?;

    if let Ast::Connector { lhs: _, rhs, .. } = body.clone() {
        let mut lsize = get_lst_tail();
        // dbg!(lsize);
        if lsize < 0 {
            lsize = 0;
        }
        let lsize = lsize as usize;

        if let Ast::ReturnStmt {
            span: _,
            dtype: _,
            expr,
        } = *rhs.clone()
        {
            let rstmt_dtype = expr.get_type().map_err(|_| {
                SemanticError::dtype(
                    rhs.span(),
                    TypeError::InvalidReturn {
                        fname: name.clone(),
                    },
                )
            })?;
            if rstmt_dtype != rtype {
                return Err(SemanticError::dtype(
                    rhs.span(),
                    TypeError::Return {
                        fname: name,
                        expected: rtype,
                        found: rstmt_dtype,
                    },
                ));
            }
        }

        // dbg!(params.clone());

        set_fn_defined(&name).map_err(SemanticError::from_compiler(span))?;

        return Ok(FnDef::new(flabel, params.len(), lsize, body));
    }
    return Err(SemanticError::func(
        span,
        FnError::MalformedBody { fname: name },
    ));
}

pub fn create_fn_list(
    prevlist: Option<Vec<FnDef>>,
    fdef: FnDef,
) -> Result<Vec<FnDef>, SemanticError> {
    let mut prevl: Vec<FnDef> = match prevlist {
        Some(flist) => flist,
        None => vec![],
    };
    prevl.push(fdef);
    reset_lst();
    Ok(prevl)
}

pub fn create_params(
    oldp: Option<ParamList>,
    newp: (DefaultLexeme, DType),
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<Option<ParamList>, SemanticError> {
    let span = newp.0.span();
    let name = lexer.span_str(span).to_string();
    let lexeme = newp.0;
    let dtype = newp.1;
    let mut plist: ParamList;
    match oldp {
        Some(oldp) => {
            plist = oldp;
        }
        None => {
            plist = ParamList::new();
            reset_lst();
        }
    };
    append_lvar(lexeme, dtype.clone(), true, lexer)?;
    plist.insert(name, (lexeme, dtype));
    Ok(Some(plist))
}

pub fn create_args(
    arg_list: Option<Vec<Ast>>,
    expr: Ast,
) -> Result<Option<Vec<Ast>>, SemanticError> {
    let mut arg_list = match arg_list {
        Some(arg_list) => arg_list,
        None => vec![],
    };
    arg_list.push(expr);
    Ok(Some(arg_list))
}
