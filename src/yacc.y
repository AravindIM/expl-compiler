%start Expr

/* %epp ADD "+" */
/* %epp SUB "-" */
/* %epp MUL "*" */
/* %epp DIV "/" */

%token "(" ")"
%left "ADD" "SUB"
%left "MUL" "DIV"

%%

Expr -> Result<Tnode, ()>:
        "(" Expr ")" { $2 }
      | Expr "DIV" Expr { Ok( Tnode::Op(Op::Div{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      |	Expr "MUL" Expr { Ok( Tnode::Op(Op::Mul{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      |	Expr "ADD" Expr { Ok( Tnode::Op(Op::Add{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      |	Expr "SUB" Expr { Ok( Tnode::Op(Op::Sub{ lhs: Box::new($1?), rhs: Box::new($3?)} ) ) }
      | "NUM" { Ok( Tnode::Num{ value: $lexer.span_str($1.as_ref().unwrap().span()).parse::<i32>().expect("Invalid number") } ) }
      ;

%%
// Any functions here are in scope for all the grammar actions above.
use compiler::Tnode;
use compiler::Op;