%start Prog

/* %epp ADD "+" */
/* %epp SUB "-" */
/* %epp MUL "*" */
/* %epp DIV "/" */

%token "BEGIN" "END" "ENDSTMT" "ID" "ASG" "READ" "WRITE" "NUM" "(" ")" "UNMATCHED"
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

Expr -> Result<Tnode, ()>:
        "(" Expr ")" { $2 }
      | Expr "DIV" Expr { Ok( Tnode::Op(Op::Div{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      |	Expr "MUL" Expr { Ok( Tnode::Op(Op::Mul{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      |	Expr "ADD" Expr { Ok( Tnode::Op(Op::Add{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      |	Expr "SUB" Expr { Ok( Tnode::Op(Op::Sub{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      | "NUM" { Ok( Tnode::Num{ value: $lexer.span_str($1.as_ref().unwrap().span()).parse::<i32>().expect("Invalid number") } ) }
      | Id { $1 }
      ;

Id -> Result<Tnode, ()>:
      "ID" { Ok( Tnode::Id(Id{ name: $lexer.span_str($1.as_ref().unwrap().span()).to_string() }) ) }
      ;

Unmatched -> ():
             "UNMATCHED" { }
             ;
%%
// Any functions here are in scope for all the grammar actions above.
use compiler::*;