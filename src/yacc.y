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

Prog -> Result<Tnode, ()>:
        "BEGIN" Slist "END" { $2 }
      | "BEGIN" "END" { Ok( Tnode::NullProg ) }
      ;

Slist -> Result<Tnode, ()>:
        Slist Stmt { Ok( Tnode::Connector{ lhs: Box::new($1?), rhs: Box::new($2?) } ) }
      | Stmt { $1 }
      ;

Stmt -> Result<Tnode, ()>:
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

InputStmt -> Result<Tnode, ()>:
             "READ" "(" Id ")" "ENDSTMT" { Ok( Tnode::Read{ id: Box::new($3?) } ) }
             ;

OutputStmt -> Result<Tnode, ()>:
              "WRITE" "(" Expr ")" "ENDSTMT" { Ok( Tnode::Write{ expr: Box::new($3?) } ) }
              ;

AsgStmt -> Result<Tnode, ()>:
           Id "ASG" Expr "ENDSTMT" { Ok( Tnode::AsgStmt{ id: Box::new($1?), expr: Box::new($3?)} ) }
           ;

IfStmt -> Result<Tnode, ()>:
          "IF" "(" BoolExpr ")" "THEN" Slist "ELSE" Slist "ENDIF" "ENDSTMT" { Ok( Tnode::IfStmt{ bool_expr: Box::new($3?), if_slist: Box::new($6?), else_slist: Box::new(Some($8?)) } ) }
          | "IF" "(" BoolExpr ")" "THEN" Slist "ENDIF" "ENDSTMT" { Ok( Tnode::IfStmt{ bool_expr: Box::new($3?), if_slist: Box::new($6?), else_slist: Box::new(None) } ) }
          ;

WhileStmt -> Result<Tnode, ()>:
             "WHILE" "(" BoolExpr ")" "DO" Slist "ENDWHILE" "ENDSTMT" { Ok( Tnode::WhileStmt{ bool_expr: Box::new($3?), slist: Box::new($6?) } ) }
             ;

DoWhileStmt -> Result<Tnode, ()>:
               "DO" Slist "WHILE" "(" BoolExpr ")"  "ENDSTMT" { Ok( Tnode::DoWhileStmt{ bool_expr: Box::new($5?), slist: Box::new($2?) } ) }
               ;

RepeatUntilStmt -> Result<Tnode, ()>:
            "REPEAT" Slist "UNTIL" "(" BoolExpr ")"  "ENDSTMT" { Ok( Tnode::RepeatUntilStmt{ bool_expr: Box::new($5?), slist: Box::new($2?) } ) }
            ;

ContinueStmt -> Result<Tnode, ()>:
                "CONTINUE" "ENDSTMT" { Ok ( Tnode::ContinueStmt ) }
                ;

BreakStmt -> Result<Tnode, ()>:
                "BREAK" "ENDSTMT" { Ok ( Tnode::BreakStmt ) }
                ;

Expr -> Result<Tnode, ()>:
        "(" Expr ")" { $2 }
      | Expr "DIV" Expr { Ok( Tnode::Op( Op::Div{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      |	Expr "MUL" Expr { Ok( Tnode::Op( Op::Mul{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      |	Expr "ADD" Expr { Ok( Tnode::Op( Op::Add{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      |	Expr "SUB" Expr { Ok( Tnode::Op( Op::Sub{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      | "NUM" { Ok( Tnode::Num{ value: $lexer.span_str($1.as_ref().unwrap().span()).parse::<i32>().expect("Invalid number") } ) }
      | Id { $1 }
      ;

BoolExpr -> Result<Tnode, ()>:
            Expr "LE" Expr { Ok( Tnode::Op( Op::LEq{ lhs: Box::new($1?), rhs: Box::new($3?) } ) ) }
            |     Expr "GE" Expr { Ok( Tnode::Op( Op::GEq{ lhs: Box::new($1?), rhs: Box::new($3?) } ) ) }
            |     Expr "NE" Expr { Ok( Tnode::Op( Op::NEq{ lhs: Box::new($1?), rhs: Box::new($3?) } ) ) }
            |     Expr "EQ" Expr { Ok( Tnode::Op( Op::Eq{ lhs: Box::new($1?), rhs: Box::new($3?) } ) ) }
            |     Expr "LT" Expr { Ok( Tnode::Op( Op::Lt{ lhs: Box::new($1?), rhs: Box::new($3?) } ) ) }
            |     Expr "GT" Expr { Ok( Tnode::Op( Op::Gt{ lhs: Box::new($1?), rhs: Box::new($3?) } ) ) }
            ;

Id -> Result<Tnode, ()>:
      "ID" { Ok( Tnode::Id(Id{ name: $lexer.span_str($1.as_ref().unwrap().span()).to_string() } ) ) }
      ;

Unmatched -> ():
             "UNMATCHED" { }
             ;
%%
// Any functions here are in scope for all the grammar actions above.
use compiler::tnode::{Tnode, Id, Op};