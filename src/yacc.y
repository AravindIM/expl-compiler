%start Prog

%epp ADD "+"
%epp SUB "-"
%epp MUL "*"
%epp DIV "/"
%epp LT "<"
%epp GT "<"
%epp EQ "=="
%epp NE "!="
%epp LE "<="
%epp GE ">="
%epp ENDSTMT ";"

%token "BEGIN" "END" "ENDSTMT" "ID" "ASG" "READ" "WRITE" "IF" "THEN" "ELSE" "ENDIF" "WHILE" "DO" "ENDWHILE" "BREAK" "CONTINUE" "DECL" "ENDDECL" "INT" "STR" "CONST_INT" "CONST_STR" "(" ")" "[" "]" "," "UNMATCHED"
%nonassoc "LE" "GE" "NE" "EQ" "LT" "GT"
%left "ADD" "SUB"
%left "MUL" "DIV"

%%

Prog -> Result<Tnode, LangParseError>:
        "BEGIN" Declarations Slist "END" { $3 }
      | "BEGIN" Declarations "END" { Ok( Tnode::NullProg ) }
      |  "BEGIN" Slist "END" { $2 }
      | "BEGIN" "END" { Ok( Tnode::NullProg ) }
      ;

Declarations -> Result<(), LangParseError>:
                "DECL" DeclList "ENDDECL" { Ok( () ) }
              | "DECL" "ENDDECL" { Ok( () ) }
              ;

DeclList -> Result<(), LangParseError>:
            DeclList Decl { Ok( () ) }
          | Decl { Ok( () ) }
          ;


Decl -> Result<(), LangParseError>:
        Type VarList "ENDSTMT" { Ok( ST.lock().unwrap().append_decl( Declaration::new($1?, $2?.to_owned()) ) ) }
      ;

Type -> Result<DType, LangParseError>:
        "INT" { Ok( DType::Int ) }
      | "STR" { Ok ( DType::Str ) }
      ;

VarList -> Result<HashMap<String,usize>, LangParseError>:
           VarList "," "ID" { Declaration::varlist($1?, $3.as_ref().unwrap(), None, $lexer ) }
         | "ID" { Declaration::variable($1.as_ref().unwrap(), None, $lexer) }
         | VarList "," "ID" "[" "CONST_INT" "]" { Declaration::varlist($1?, $3.as_ref().unwrap(), Some($5.as_ref().unwrap()), $lexer ) }
         | "ID" "[" "CONST_INT" "]" { Declaration::variable($1.as_ref().unwrap(), Some($3.as_ref().unwrap()), $lexer) }
         ;

Slist -> Result<Tnode, LangParseError>:
        Slist Stmt { Ok( Tnode::Connector{ lhs: Box::new($1?), rhs: Box::new($2?) } ) }
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
           Id "ASG" Expr "ENDSTMT" { Tnode::create_assign_node($span, $1?, $3?) }
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
      | Expr "DIV" Expr { Tnode::create_op_node($span, OpType::Div, vec![DType::Int], DType::Int, $1?,  $3? ) }
      | Expr "MUL" Expr { Tnode::create_op_node($span, OpType::Mul, vec![DType::Int], DType::Int, $1?,  $3? ) }
      | Expr "ADD" Expr { Tnode::create_op_node($span, OpType::Add, vec![DType::Int], DType::Int, $1?,  $3? ) }
      | Expr "SUB" Expr { Tnode::create_op_node($span, OpType::Sub, vec![DType::Int], DType::Int, $1?,  $3? ) }
      | Expr "LT" Expr { Tnode::create_op_node($span, OpType::Lt, vec![DType::Int, DType::Str], DType::Bool, $1?, $3? ) }
      | Expr "GT" Expr { Tnode::create_op_node($span, OpType::Gt, vec![DType::Int, DType::Str], DType::Bool, $1?, $3? ) }
      | Expr "EQ" Expr { Tnode::create_op_node($span, OpType::Eq, vec![DType::Int, DType::Str], DType::Bool, $1?, $3? ) }
      | Expr "NE" Expr { Tnode::create_op_node($span, OpType::NEq, vec![DType::Int, DType::Str], DType::Bool, $1?, $3? ) }
      | Expr "LE" Expr { Tnode::create_op_node($span, OpType::LEq, vec![DType::Int, DType::Str], DType::Bool, $1?, $3? ) }
      | Expr "GE" Expr { Tnode::create_op_node($span, OpType::GEq, vec![DType::Int, DType::Str], DType::Bool, $1?, $3? ) }
      | Id { $1 }
      | "CONST_INT" { Tnode::create_constant($span, DType::Int, $lexer) }
      | "CONST_STR" { Tnode::create_constant($span, DType::Str, $lexer) }
      ;

Id -> Result<Tnode, LangParseError>:
      "ID" { Tnode::create_id_node($1.as_ref().unwrap(), None, $lexer) }
    | "ID" "[" Expr "]" { Tnode::create_id_node($1.as_ref().unwrap(), Some($3?),  $lexer) }
    ;

Unmatched -> ():
             "UNMATCHED" { }
           ;
%%
// Any functions here are in scope for all the grammar actions above.
use compiler::tnode::{Tnode, OpType, DType, FlowType};
use compiler::errors::LangParseError;
use compiler::symboltable::{Declaration};
use compiler::ST;
use std::collections::HashMap;