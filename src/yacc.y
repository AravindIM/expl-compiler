%start Prog

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

%token "BEGIN" "END" "ENDSTMT" "ID" "ASG" "READ" "WRITE" "IF" "THEN" "ELSE" "ENDIF" "WHILE" "DO" "ENDWHILE" "BREAK" "CONTINUE" "DECL" "ENDDECL" "INT" "STR" "CONST_INT" "CONST_STR" "(" ")" "[" "]" "," "UNMATCHED"
%nonassoc "LE" "GE" "NE" "EQ" "LT" "GT"
%left "ADD" "SUB"
%left "ASTERISK" "DIV" "MOD"
%left "AMP"

%%

Prog -> Result<Tnode, LangParseError>:
        "BEGIN" Declarations Slist "END" { Declaration::attach($2, $3) }
      | "BEGIN" Declarations "END" { Declaration::attach($2, Ok(Tnode::NullProg{ span: $span }) ) }
      |  "BEGIN" Slist "END" { $2 }
      | "BEGIN" "END" { Ok( Tnode::NullProg{ span: $span } ) }
      ;

Declarations -> Result<(), LangParseError>:
                "DECL" DeclList "ENDDECL" { ST.lock().unwrap().append_decl($2?) }
              | "DECL" "ENDDECL" { Ok( () ) }
              ;

DeclList -> Result<Declaration, LangParseError>:
            DeclList Decl { $1?.join($2?) }
          | Decl { $1 }
          ;


Decl -> Result<Declaration, LangParseError>:
        Type VarList "ENDSTMT" { Ok( Declaration::new($1?, $2?.to_owned()) ) }
      ;

Type -> Result<Primitive, LangParseError>:
        "INT" { Ok( Primitive::Int ) }
      | "STR" { Ok ( Primitive::Str ) }
      ;

VarList -> Result<VarMap, LangParseError>:
           VarList "," Var { Declaration::variables(Some($1?), $3?, $lexer ) }
         | Var { Declaration::variables(None, $1?, $lexer) }
         ;

Var -> Result<VarData, LangParseError>:
       "ID" { Ok( VarData{ name: $1.unwrap(), dtype: DType::Data(Primitive::Void), dim: Dimension::Unit } ) }
      | "ID" VarSize { Ok( VarData{ name: $1.unwrap(), dtype: DType::Data(Primitive::Void), dim: $2? } ) }
      | AsteriskList "ID" { Ok( VarData{ name: $2.unwrap(), dtype: $1?, dim: Dimension::Unit } ) }
      | AsteriskList "ID" VarSize { Ok( VarData{ name: $2.unwrap(), dtype: $1?, dim: $3? } ) }
      ;

AsteriskList -> Result<DType, LangParseError>:
                AsteriskList "ASTERISK" { Ok( DType::Pointer(Box::new($1?)) ) }
              | "ASTERISK" { Ok( DType::Pointer(Box::new(DType::Data(Primitive::Void))) ) }
             ;

VarSize -> Result<Dimension, LangParseError>:
           VarSize "[" Expr "]" { Dimension::array_size(Some($1?), $3?) }
         | "[" Expr "]" { Dimension::array_size(None, $2?) }
         ;

VarIndex -> Result<Dimension, LangParseError>:
           VarIndex "[" Expr "]" { Dimension::array_index(Some($1?), $3?) }
         | "[" Expr "]" { Dimension::array_index(None, $2?) }
         ;

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
             "READ" "(" Id ")" "ENDSTMT" { Tnode::create_read_node($span, $3?) }
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
      | Id { $1 }
      | "CONST_INT" { Tnode::create_constant($span, DType::Data(Primitive::Int), $lexer) }
      | "CONST_STR" { Tnode::create_constant($span, DType::Data(Primitive::Str), $lexer) }
      ;

Id -> Result<Tnode, LangParseError>:
      "ID" { Tnode::create_id_node($span, $1.as_ref().unwrap(), Dimension::Unit, $lexer) }
    | "ID" VarIndex { Tnode::create_id_node($span, $1.as_ref().unwrap(), $2?,  $lexer) }
    ;

Unmatched -> ():
             "UNMATCHED" { }
           ;
%%
// Any functions here are in scope for all the grammar actions above.
use compiler::tnode::{Tnode, OpType, DType, Primitive, FlowType};
use compiler::errors::LangParseError;
use compiler::symboltable::{VarData, VarMap, Declaration, Dimension};
use compiler::ST;
// use indexmap::IndexMap;