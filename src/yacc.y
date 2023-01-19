%start Prog

/* %epp ADD "+" */
/* %epp SUB "-" */
/* %epp MUL "*" */
/* %epp DIV "/" */

%token "BEGIN" "END" "ENDSTMT" "ID" "ASG" "READ" "WRITE" "NUM" "IF" "THEN" "ELSE" "ENDIF" "WHILE" "DO" "ENDWHILE" "BREAK" "CONTINUE" "(" ")" "UNMATCHED"
%nonassoc "LE" "GE" "NE" "EQ" "LT" "GT"
%left "ADD" "SUB"
%left "MUL" "DIV"

%%

Prog -> Result<Tnode, Box<dyn Error>>:
        "BEGIN" Slist "END" { $2 }
      | "BEGIN" "END" { Ok( Tnode::NullProg ) }
      ;

Slist -> Result<Tnode, Box<dyn Error>>:
        Slist Stmt { Ok( Tnode::Connector{ lhs: Box::new($1?), rhs: Box::new($2?) } ) }
      | Stmt { $1 }
      ;

Stmt -> Result<Tnode, Box<dyn Error>>:
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

InputStmt -> Result<Tnode, Box<dyn Error>>:
             "READ" "(" Id ")" "ENDSTMT" { Ok( Tnode::Read{ id: Box::new($3?) } ) }
             ;

OutputStmt -> Result<Tnode, Box<dyn Error>>:
              "WRITE" "(" Expr ")" "ENDSTMT" { Ok( Tnode::Write{ expr: Box::new($3?) } ) }
              ;

AsgStmt -> Result<Tnode, Box<dyn Error>>:
           Id "ASG" Expr "ENDSTMT" { Tnode::create_assign_node(&mut $1?, $3?) }
           ;

IfStmt -> Result<Tnode, Box<dyn Error>>:
          "IF" "(" Expr ")" "THEN" Slist "ELSE" Slist "ENDIF" "ENDSTMT" { Tnode::create_flow_node(FlowType::If, Some(vec![$3?]), Some(vec![$6?, $8?])) }
          | "IF" "(" Expr ")" "THEN" Slist "ENDIF" "ENDSTMT" { Tnode::create_flow_node(FlowType::If, Some(vec![$3?]), Some(vec![$6?])) }
          ;

WhileStmt -> Result<Tnode, Box<dyn Error>>:
             "WHILE" "(" Expr ")" "DO" Slist "ENDWHILE" "ENDSTMT" { Tnode::create_flow_node(FlowType::While, Some(vec![$3?]), Some(vec![$6?])) }
             ;

DoWhileStmt -> Result<Tnode, Box<dyn Error>>:
               "DO" Slist "WHILE" "(" Expr ")"  "ENDSTMT" { Tnode::create_flow_node(FlowType::DoWhile, Some(vec![$5?]), Some(vec![$2?])) }
               ;

RepeatUntilStmt -> Result<Tnode, Box<dyn Error>>:
            "REPEAT" Slist "UNTIL" "(" Expr ")"  "ENDSTMT" { Tnode::create_flow_node(FlowType::RepeatUntil, Some(vec![$5?]), Some(vec![$2?])) }
            ;

ContinueStmt -> Result<Tnode, Box<dyn Error>>:
                "CONTINUE" "ENDSTMT" { Tnode::create_flow_node(FlowType::Continue, None, None) }
                ;

BreakStmt -> Result<Tnode, Box<dyn Error>>:
                "BREAK" "ENDSTMT" { Tnode::create_flow_node(FlowType::Break, None, None) }
                ;

Expr -> Result<Tnode, Box<dyn Error>>:
        "(" Expr ")" { $2 }
      | Expr "DIV" Expr { Tnode::create_op_node(OpType::Div, DType::Int, $1?,  $3? ) }
      | Expr "MUL" Expr { Tnode::create_op_node(OpType::Mul, DType::Int, $1?,  $3? ) }
      | Expr "ADD" Expr { Tnode::create_op_node(OpType::Add, DType::Int, $1?,  $3? ) }
      | Expr "SUB" Expr { Tnode::create_op_node(OpType::Sub, DType::Int, $1?,  $3? ) }
      | Expr "LT" Expr { Tnode::create_op_node(OpType::Lt, DType::Bool, $1?, $3? ) }
      | Expr "GT" Expr { Tnode::create_op_node(OpType::Gt, DType::Bool, $1?, $3? ) }
      | Expr "EQ" Expr { Tnode::create_op_node(OpType::Eq, DType::Bool, $1?, $3? ) }
      | Expr "NE" Expr { Tnode::create_op_node(OpType::NEq, DType::Bool, $1?, $3? ) }
      | Expr "LE" Expr { Tnode::create_op_node(OpType::LEq, DType::Bool, $1?, $3? ) }
      | Expr "GE" Expr { Tnode::create_op_node(OpType::GEq, DType::Bool, $1?, $3? ) }
      | Id { $1 }
      | "NUM" { Tnode::create_constant(DType::Int, $lexer, &$1?) }
      ;

Id -> Result<Tnode, Box<dyn Error>>:
      "ID" { Tnode::create_id_node($lexer, &$1?) }
      ;

Unmatched -> ():
             "UNMATCHED" { }
             ;
%%
// Any functions here are in scope for all the grammar actions above.
use compiler::tnode::{Tnode, OpType, DType, FlowType};
use std::error::Error;