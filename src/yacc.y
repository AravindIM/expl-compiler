%start Prog

%epp ASG "="
%epp ADD "+"
%epp SUB "-"
%epp ASTERISK "*"
%epp DIV "/"
%epp LT "<"
%epp GT "<"
%epp EQ "=="
%epp NE "!="
%epp LE "<="
%epp GE ">="
%epp AMP "&"
%epp ENDSTMT ";"
%epp RETURN "return"
%epp MAIN "main"

%token "BEGIN" "END" "ENDSTMT" "ID" "ASG" "READ" "WRITE" "IF" "THEN" "ELSE" "ENDIF" "WHILE" "DO" "ENDWHILE" "BREAK" "CONTINUE" "DECL" "ENDDECL" "INT" "STR" "CONST_INT" "CONST_STR" "MAIN" "RETURN" "(" ")" "[" "]" "{" "}" "," "UNMATCHED"
%nonassoc "LE" "GE" "NE" "EQ" "LT" "GT"
%left "ADD" "SUB"
%left "ASTERISK" "DIV" "MOD"
%left "AMP"

%%
Prog -> Result<Vec<FDef>, LangParseError>:
        GDeclBlock FDefBlock MainBlock { $1?; LST.lock().unwrap().reset(); FDef::create_list(Some($2?), $3?) }
      | GDeclBlock MainBlock { $1?; LST.lock().unwrap().reset(); FDef::create_list(None, $2?) }
      | MainBlock { LST.lock().unwrap().reset(); FDef::create_list(None, $1?) }
      ;


/* Global Block */
GDeclBlock -> Result<(), LangParseError>:
              "DECL" GDeclList "ENDDECL" { $2 }
            | "DECL" "ENDDECL"/* Empty Declaration */ { Ok( () ) }
            ;

GDeclList -> Result<(), LangParseError>:
            GDeclList GDecl { $1?; $2 }
          | GDecl { $1 }
          ;

GDecl -> Result<(), LangParseError>:
         Type GidList "ENDSTMT" { $2?; ST.lock().unwrap().dequeue($1?) }
       ;


GidList -> Result<(), LangParseError>:
           GidList "," Gid { $1?; $3 }
         | Gid { $1 }
         ;

Gid -> Result<(), LangParseError>:
       Id { ST.lock().unwrap().enqueue($1.unwrap(), DType::Data(Primitive::Void), Dimension::Unit, None, $lexer) }
      | Id VarSize { ST.lock().unwrap().enqueue($1.unwrap(), DType::Data(Primitive::Void), $2?, None, $lexer) }
      | Id "(" ParamList ")" { ST.lock().unwrap().enqueue($1.unwrap(), DType::Data(Primitive::Void), Dimension::Unit, Some($3?), $lexer) }
      | AsteriskList Id { ST.lock().unwrap().enqueue($2.unwrap(), $1?, Dimension::Unit, None, $lexer) }
      | AsteriskList Id VarSize { ST.lock().unwrap().enqueue($2.unwrap(), $1?, $3?, None, $lexer ) }
      ;

AsteriskList -> Result<DType, LangParseError>:
                AsteriskList "ASTERISK" { Ok( DType::Pointer(Box::new($1?)) ) }
              | "ASTERISK" { Ok( DType::Pointer(Box::new(DType::Data(Primitive::Void))) ) }
              ;

VarSize -> Result<Dimension, LangParseError>:
           VarSize "[" Expr "]" { Dimension::array_size(Some($1?), $3?) }
         | "[" Expr "]" { Dimension::array_size(None, $2?) }
         ;

/* Function Block */
FDefBlock -> Result<Vec<FDef>, LangParseError>:
              FDefBlock FDef { LST.lock().unwrap().reset(); FDef::create_list(Some($1?), $2?) }
            | FDef { LST.lock().unwrap().reset(); FDef::create_list(None, $1?) }
            ;

FDef -> Result<FDef, LangParseError>:
        Type Id "(" ParamList ")" "{" LDeclBlock FBody "}" { $7?; FDef::create(DType::Data($1?), $lexer.span_str($2.as_ref().unwrap().span()).to_string(), $4?, $8?, $span) }
      ;

ParamList -> Result<ParamList, LangParseError>:
             ParamList "," Param { SymbolTable::create_params(Some($1?), Some($3?)) }
           | Param { SymbolTable::create_params(None, Some($1?)) }
           | { SymbolTable::create_params(None, None) }
           ;

Param -> Result<(String, DType, Span), LangParseError>:
         Type Id { Ok(($lexer.span_str($2.as_ref().unwrap().span()).to_string(), DType::Data($1?), $span)) }
       ;

Type -> Result<Primitive, LangParseError>:
        "INT" { Ok( Primitive::Int ) }
      | "STR" { Ok ( Primitive::Str ) }
      ;

/* Main Function Block */
MainBlock -> Result<FDef, LangParseError>:
             Type "MAIN" "(" ")" "{" LDeclBlock FBody "}" { $6?; FDef::create(DType::Data($1?), format!("main"), ParamList::new(), $7?, $span) }
           ;

/* Function Body */
FBody -> Result<Tnode, LangParseError>:
         "BEGIN" Slist ReturnStmt "END" { Ok( Tnode::Connector{ span: $span, lhs: Box::new($2?), rhs: Box::new($3?) } ) }
       | "BEGIN" ReturnStmt "END" { Ok( Tnode::Connector{ span: $span, lhs: Box::new(Tnode::NullProg{ span: $span }), rhs: Box::new($2?) } ) }
       ;

/* LDeclBlock */
LDeclBlock -> Result<(), LangParseError>:
              "DECL" LDeclList "ENDDECL" { $2 }
            | "DECL" "ENDDECL"/* Empty Declaration */ { Ok( () ) }
            ;

LDeclList -> Result<(), LangParseError>:
            LDeclList LDecl { $1?; $2 }
          | LDecl { $1 }
          ;

LDecl -> Result<(), LangParseError>:
         Type LidList "ENDSTMT" { ST.lock().unwrap().dequeue($1?) }
       ;


LidList -> Result<(), LangParseError>:
           LidList "," Lid { $1?; $3 }
         | Lid { $1 }
         ;

Lid -> Result<(), LangParseError>:
       Id { LST.lock().unwrap().enqueue($1.unwrap(), DType::Data(Primitive::Void), Dimension::Unit, None, $lexer) }
      /* | Id VarSize { LST.lock().unwrap().enqueue($1.unwrap(), DType::Data(Primitive::Void), $2?, None, $lexer) } */
      /* | Id "(" ParamList ")" { LST.lock().unwrap($1.unwrap(), DType::Data(Primitive::Void), $2?, None, $lexer) } */
      | AsteriskList Id { LST.lock().unwrap().enqueue($2.unwrap(), $1?, Dimension::Unit, None, $lexer) }
      /* | AsteriskList Id VarSize { LST.lock.unwrap().enqueue($2.unwrap(), $1?, $3?, None, $lexer ) } */
      ;

/* Slist */
Slist -> Result<Tnode, LangParseError>:
        Slist Stmt { Ok( Tnode::Connector{ span: $span, lhs: Box::new($1?), rhs: Box::new($2?) } ) }
      | Stmt { $1 }
      ;

Stmt -> Result<Tnode, LangParseError>:
        InputStmt { $1 }
      | OutputStmt { $1 }
      | AsgStmt { $1 }
      | IfStmt { $1 }
      | WhileStmt { $1 }
      | DoWhileStmt { $1 }
      | RepeatUntilStmt { $1 }
      | ContinueStmt { $1 }
      | BreakStmt { $1 }
      ;

InputStmt -> Result<Tnode, LangParseError>:
             "READ" "(" Var ")" "ENDSTMT" { Tnode::create_read_node($span, $3?) }
            ;

OutputStmt -> Result<Tnode, LangParseError>:
              "WRITE" "(" Expr ")" "ENDSTMT" { Tnode::create_write_node($span, $3?) }
            ;

AsgStmt -> Result<Tnode, LangParseError>:
           Expr "ASG" Expr "ENDSTMT" { Tnode::create_assign_node($span, $1?, $3?) }
         ;

IfStmt -> Result<Tnode, LangParseError>:
          "IF" "(" Expr ")" "THEN" Slist "ELSE" Slist "ENDIF" "ENDSTMT" { Tnode::create_flow_node($span, FlowType::If, Some(vec![$3?]), Some(vec![$6?, $8?])) }
        | "IF" "(" Expr ")" "THEN" Slist "ENDIF" "ENDSTMT" { Tnode::create_flow_node($span, FlowType::If, Some(vec![$3?]), Some(vec![$6?])) }
        ;

WhileStmt -> Result<Tnode, LangParseError>:
             "WHILE" "(" Expr ")" "DO" Slist "ENDWHILE" "ENDSTMT" { Tnode::create_flow_node($span, FlowType::While, Some(vec![$3?]), Some(vec![$6?])) }
           ;

DoWhileStmt -> Result<Tnode, LangParseError>:
               "DO" Slist "WHILE" "(" Expr ")"  "ENDSTMT" { Tnode::create_flow_node($span, FlowType::DoWhile, Some(vec![$5?]), Some(vec![$2?])) }
             ;

RepeatUntilStmt -> Result<Tnode, LangParseError>:
                   "REPEAT" Slist "UNTIL" "(" Expr ")"  "ENDSTMT" { Tnode::create_flow_node($span, FlowType::RepeatUntil, Some(vec![$5?]), Some(vec![$2?])) }
                 ;

ContinueStmt -> Result<Tnode, LangParseError>:
                "CONTINUE" "ENDSTMT" { Tnode::create_flow_node($span, FlowType::Continue, None, None) }
                ;

BreakStmt -> Result<Tnode, LangParseError>:
                "BREAK" "ENDSTMT" { Tnode::create_flow_node($span, FlowType::Break, None, None) }
                ;

ReturnStmt -> Result<Tnode, LangParseError>:
              "RETURN" Expr "ENDSTMT" { Tnode::create_return_node($span, $2?) }
            ;

Expr -> Result<Tnode, LangParseError>:
        "(" Expr ")" { $2 }
      | Expr "MOD" Expr { Tnode::create_op_node($span, OpType::Mod, $1?,  Some($3?) ) }
      | Expr "DIV" Expr { Tnode::create_op_node($span, OpType::Div, $1?,  Some($3?) ) }
      | Expr "ASTERISK" Expr { Tnode::create_op_node($span, OpType::Mul, $1?,  Some($3?) ) }
      | Expr "ADD" Expr { Tnode::create_op_node($span, OpType::Add, $1?,  Some($3?) ) }
      | Expr "SUB" Expr { Tnode::create_op_node($span, OpType::Sub, $1?,  Some($3?) ) }
      | Expr "LT" Expr { Tnode::create_op_node($span, OpType::Lt, $1?, Some($3?) ) }
      | Expr "GT" Expr { Tnode::create_op_node($span, OpType::Gt, $1?, Some($3?) ) }
      | Expr "EQ" Expr { Tnode::create_op_node($span, OpType::Eq, $1?, Some($3?) ) }
      | Expr "NE" Expr { Tnode::create_op_node($span, OpType::NEq, $1?, Some($3?) ) }
      | Expr "LE" Expr { Tnode::create_op_node($span, OpType::LEq, $1?, Some($3?) ) }
      | Expr "GE" Expr { Tnode::create_op_node($span, OpType::GEq, $1?, Some($3?) ) }
      | "AMP" Expr { Tnode::create_op_node($span, OpType::Amp, $2?, None) }
      | "ASTERISK" Expr { Tnode::create_op_node($span, OpType::Deref, $2?, None) }
      | Var { $1 }
      | "CONST_INT" { Tnode::create_literal($span, DType::Data(Primitive::Int), $lexer) }
      | "CONST_STR" { Tnode::create_literal($span, DType::Data(Primitive::Str), $lexer) }
      ;

VarIndex -> Result<Dimension, LangParseError>:
           VarIndex "[" Expr "]" { Dimension::array_index(Some($1?), $3?) }
         | "[" Expr "]" { Dimension::array_index(None, $2?) }
         ;

Var -> Result<Tnode, LangParseError>:
      Id { Tnode::create_id_node($span, $1.as_ref().unwrap(), Dimension::Unit, $lexer) }
    | Id VarIndex { Tnode::create_id_node($span, $1.as_ref().unwrap(), $2?,  $lexer) }
    ;

Id -> Result<DefaultLexeme<u32>, LangParseError>:
      "ID" { $1.map_err(|e| LangParseError(e.span(), format!("Faulty lexeme"))) }
    ;



Unmatched -> ():
             "UNMATCHED" { }
           ;
%%
// Any functions here are in scope for all the grammar actions above.
use compiler::tnode::{Tnode, OpType, DType, Primitive, FlowType, FDef};
use compiler::errors::LangParseError;
use compiler::symboltable::{ParamList, Dimension, SymbolTable};
use compiler::{ST, LST};
use lrpar::Span;
use lrlex::DefaultLexeme;
// use indexmap::IndexMap;
