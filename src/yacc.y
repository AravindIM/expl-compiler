%start Prog

%avoid_insert "ID"
%avoid_insert "INT_LITERAL"
%avoid_insert "STR_LITERAL"

%token "=" ";" "(" ")" "[" "]" "{" "}" ","

%token "ID" "INT_LITERAL" "STR_LITERAL" 

%token "int" "str" 

%token "main"
%token "return"
%token "begin" "end"
%token "decl" "enddecl"

%token "read" "write"

%token "if" "then" "else" "endif"
%token "while" "do" "endwhile"
%token "repeat" "until"
%token "break" "continue"

%token "UNMATCHED"

%left "&&"
%left "||"
%left "<=" ">=" "!=" "==" "<" ">"
%left "+" "-"
%left "*" "/" "%"
%left "&"

%%
Prog -> Result<Vec<FnDef>, SemanticError>:
        GDeclBlock FnDefBlock MainBlock { $1?; create_fn_list(Some($2?), $3?) }
      | GDeclBlock MainBlock { $1?; create_fn_list(None, $2?) }
      | MainBlock { create_fn_list(None, $1?) }
      ;


/* Global Block */
GDeclBlock -> Result<(), SemanticError>:
              "decl" GDeclList "enddecl" { $2 }
            | "decl" "enddecl"/* Empty Declaration */ { Ok( () ) }
            ;

GDeclList -> Result<(), SemanticError>:
            GDeclList GDecl { $1?; $2 }
          | GDecl { $1 }
          ;

GDecl -> Result<(), SemanticError>:
         Type GidList ";" { $2?; GST.lock().unwrap().dequeue($1?) }
       ;


GidList -> Result<(), SemanticError>:
           GidList "," Gid { $1?; $3 }
         | Gid { $1 }
         ;

Gid -> Result<(), SemanticError>:
       Id { append_gvar($1?, DType::Data(Primitive::Void), Dimension::Unit, $lexer) }
      | Id VarSize { append_gvar($1?, DType::Data(Primitive::Void), $2?, $lexer) }
      | Id "(" ParamList ")" { append_fn($1?, DType::Data(Primitive::Void), $3?, $lexer) }
      | AsteriskList Id { append_gvar($2?, $1?, Dimension::Unit, $lexer) }
      | AsteriskList Id VarSize { append_gvar($2?, $1?, $3?, $lexer ) }
      ;

AsteriskList -> Result<DType, SemanticError>:
                AsteriskList "*" { Ok( DType::Pointer(Box::new($1?)) ) }
              | "*" { Ok( DType::Pointer(Box::new(DType::Data(Primitive::Void))) ) }
              ;

VarSize -> Result<Dimension, SemanticError>:
           VarSize "[" Expr "]" { Dimension::array_size(Some($1?), $3?) }
         | "[" Expr "]" { Dimension::array_size(None, $2?) }
         ;

/* Function Block */
FnDefBlock -> Result<Vec<FnDef>, SemanticError>:
              FnDefBlock FnDef { create_fn_list(Some($1?), $2?) }
            | FnDef { create_fn_list(None, $1?) }
            ;

FnDef -> Result<FnDef, SemanticError>:
        Type Id "(" ParamList ")" "{" LDeclBlock FBody "}" { create_fn(DType::Data($1?), $2?, $4?, $7, $8, $span, $lexer) }
      | Type Id "(" ParamList ")" "{" FBody "}" { create_fn(DType::Data($1?), $2?, $4?, Ok(()), $7, $span, $lexer) }
      ;

ParamList -> Result<Option<ParamList>, SemanticError>:
             ParamList "," Param { create_params($1?, $3?, $lexer) }
           | Param { create_params(None, $1?, $lexer) }
           | { Ok(None) }
           ;

Param -> Result<(DefaultLexeme, DType), SemanticError>:
         Type Id { Ok(($2?, DType::Data($1?))) }
       ;

Type -> Result<Primitive, SemanticError>:
        "int" { Ok( Primitive::Int ) }
      | "str" { Ok ( Primitive::Str ) }
      ;

/* Main Function Block */
MainBlock -> Result<FnDef, SemanticError>:
             Type Main "(" ")" "{" LDeclBlock FBody "}" { create_fn(DType::Data($1?), $2?, None, $6, $7, $span, $lexer) }
           | Type Main "(" ")" "{" FBody "}" { create_fn(DType::Data($1?), $2?, None, Ok(()), $6, $span, $lexer) }
           ;

/* Function Body */
FBody -> Result<Ast, SemanticError>:
         "begin" Slist ReturnStmt "end" { create_connector($span, $2?, $3?) }
       | "begin" ReturnStmt "end" { create_connector($span, create_nullprog($span)?, $2?) }
       ;

/* LDeclBlock */
LDeclBlock -> Result<(), SemanticError>:
              "decl" LDeclList "enddecl" { $2 }
            | "decl" "enddecl"/* Empty Declaration */ { Ok( () ) }
            ;

LDeclList -> Result<(), SemanticError>:
            LDeclList LDecl { $1?; $2 }
          | LDecl { $1 }
          ;

LDecl -> Result<(), SemanticError>:
         Type LidList ";" { $2?; LST.lock().unwrap().dequeue($1?) }
       ;


LidList -> Result<(), SemanticError>:
           LidList "," Lid { $1?; $3 }
         | Lid { $1 }
         ;

Lid -> Result<(), SemanticError>:
       Id { append_lvar($1?, DType::Data(Primitive::Void), false, $lexer) }
      /* | Id VarSize { LST.lock().unwrap().enqueue($1?, DType::Data(Primitive::Void), $2?, None, $lexer) } */
      /* | Id "(" ParamList ")" { LST.lock().unwrap($1?, DType::Data(Primitive::Void), $2?, None, $lexer) } */
      | AsteriskList Id { append_lvar($2?, $1?, false, $lexer) }
      /* | AsteriskList Id VarSize { LST.lock.unwrap().enqueue($2?, $1?, $3?, None, $lexer ) } */
      ;

/* Slist */
Slist -> Result<Ast, SemanticError>:
        Slist Stmt { create_connector($span, $1?, $2?)  }
      | Stmt { $1 }
      ;

Stmt -> Result<Ast, SemanticError>:
        InputStmt { $1 }
      | OutputStmt { $1 }
      | AsgStmt { $1 }
      | IfStmt { $1 }
      | WhileStmt { $1 }
      | DoWhileStmt { $1 }
      | RepeatUntilStmt { $1 }
      | ContinueStmt { $1 }
      | BreakStmt { $1 }
      | Expr ";" { $1 }
      ;

InputStmt -> Result<Ast, SemanticError>:
             "read" "(" Var ")" ";" { create_read($span, $3?) }
            ;

OutputStmt -> Result<Ast, SemanticError>:
              "write" "(" Expr ")" ";" { create_write($span, $3?) }
            ;

AsgStmt -> Result<Ast, SemanticError>:
           Expr "=" Expr ";" { create_assign($span, $1?, $3?) }
         ;

IfStmt -> Result<Ast, SemanticError>:
          "if" "(" Expr ")" "then" Slist "else" Slist "endif" ";" { create_flow($span, FlowType::If, Some(vec![$3?]), Some(vec![$6?, $8?])) }
        | "if" "(" Expr ")" "then" Slist "endif" ";" { create_flow($span, FlowType::If, Some(vec![$3?]), Some(vec![$6?])) }
        ;

WhileStmt -> Result<Ast, SemanticError>:
             "while" "(" Expr ")" "do" Slist "endwhile" ";" { create_flow($span, FlowType::While, Some(vec![$3?]), Some(vec![$6?])) }
           ;

DoWhileStmt -> Result<Ast, SemanticError>:
               "do" Slist "while" "(" Expr ")"  ";" { create_flow($span, FlowType::DoWhile, Some(vec![$5?]), Some(vec![$2?])) }
             ;

RepeatUntilStmt -> Result<Ast, SemanticError>:
                   "repeat" Slist "until" "(" Expr ")"  ";" { create_flow($span, FlowType::RepeatUntil, Some(vec![$5?]), Some(vec![$2?])) }
                 ;

ContinueStmt -> Result<Ast, SemanticError>:
                "continue" ";" { create_flow($span, FlowType::Continue, None, None) }
                ;

BreakStmt -> Result<Ast, SemanticError>:
                "break" ";" { create_flow($span, FlowType::Break, None, None) }
                ;

ReturnStmt -> Result<Ast, SemanticError>:
              "return" Expr ";" { create_return($span, $2?) }
            ;

Expr -> Result<Ast, SemanticError>:
        "(" Expr ")" { $2 }
      | Expr "%" Expr { create_op($span, OpType::Mod, $1?,  Some($3?) ) }
      | Expr "/" Expr { create_op($span, OpType::Div, $1?,  Some($3?) ) }
      | Expr "*" Expr { create_op($span, OpType::Mul, $1?,  Some($3?) ) }
      | Expr "+" Expr { create_op($span, OpType::Add, $1?,  Some($3?) ) }
      | Expr "-" Expr { create_op($span, OpType::Sub, $1?,  Some($3?) ) }
      | Expr "<" Expr { create_op($span, OpType::Lt, $1?, Some($3?) ) }
      | Expr ">" Expr { create_op($span, OpType::Gt, $1?, Some($3?) ) }
      | Expr "==" Expr { create_op($span, OpType::Eq, $1?, Some($3?) ) }
      | Expr "!=" Expr { create_op($span, OpType::NEq, $1?, Some($3?) ) }
      | Expr "<=" Expr { create_op($span, OpType::LEq, $1?, Some($3?) ) }
      | Expr ">=" Expr { create_op($span, OpType::GEq, $1?, Some($3?) ) }
      | Expr "&&" Expr { create_op($span, OpType::And, $1?, Some($3?) ) }
      | Expr "||" Expr { create_op($span, OpType::Or, $1?, Some($3?) ) }
      | "&" Expr { create_op($span, OpType::Amp, $2?, None) }
      | "*" Expr { create_op($span, OpType::Deref, $2?, None) }
      | Var { $1 }
      | "INT_LITERAL" { create_literal($span, DType::Data(Primitive::Int), $lexer) }
      | "STR_LITERAL" { create_literal($span, DType::Data(Primitive::Str), $lexer) }
      ;

VarIndex -> Result<Dimension, SemanticError>:
            VarIndex "[" Expr "]" { Dimension::array_index(Some($1?), $3?) }
          | "[" Expr "]" { Dimension::array_index(None, $2?) }
          ;

Var -> Result<Ast, SemanticError>:
       Id { create_id($span, $1?, Dimension::Unit, None, $lexer) }
     | Id VarIndex { create_id($span, $1?, $2?, None,  $lexer) }
     | Id "(" Arglist ")" { create_id($span, $1?, Dimension::Unit, $3?, $lexer) }
     ;

Arglist -> Result<Option<Vec<Ast>>, SemanticError>:
           Arglist "," Expr { create_args($1?, $3?) }
         | Expr { create_args(None, $1?) }
         | { Ok(None) }
         ;

Id -> Result<DefaultLexeme<u32>, SemanticError>:
      "ID" { $1.map_err(|e| SemanticError(e.span(), format!("Faulty lexeme"))) }
    ;

Main -> Result<DefaultLexeme<u32>, SemanticError>:
      "main" { $1.map_err(|e| SemanticError(e.span(), format!("Faulty lexeme"))) }
    ;



Unmatched -> ():
             "UNMATCHED" { }
           ;
%%
// Any functions here are in scope for all the grammar actions above.
use compiler::enums::flow::FlowType;
use compiler::enums::{
  ast::Ast,
  operator::OpType,
  dtype::{
    DType,
    Primitive
  },
  dimension::Dimension,
};
use compiler::exception::semantic::SemanticError;
use compiler::function::ParamList;
use compiler::{GST, LST};
use lrlex::DefaultLexeme;
use compiler::function::FnDef;
use compiler::builder::function::{create_fn, create_fn_list, create_params, create_args};
use compiler::builder::symboltable::{append_gvar, append_lvar, append_fn};
use compiler::builder::operand::{create_id, create_literal};
use compiler::builder::operator::create_op;
use compiler::builder::statement::{create_return, create_read, create_write, create_assign, create_flow, create_nullprog, create_connector};
