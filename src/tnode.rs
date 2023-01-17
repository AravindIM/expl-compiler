use std::error::Error;

#[derive(Debug)]
pub struct Id {
    pub name: String,
}

impl Id {
    pub fn get_address(&self) -> Result<u32, Box<dyn Error>> {
        Ok(self
            .name
            .chars()
            .nth(0)
            .ok_or(Box::<dyn Error>::from("ERROR: Invalid variable"))? as u32
            - 'a' as u32
            + 4096)
    }
}

#[derive(Debug)]
pub enum Op {
    Div { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Mul { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Add { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Sub { lhs: Box<Tnode>, rhs: Box<Tnode> },
    LEq { lhs: Box<Tnode>, rhs: Box<Tnode> },
    GEq { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Eq { lhs: Box<Tnode>, rhs: Box<Tnode> },
    NEq { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Gt { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Lt { lhs: Box<Tnode>, rhs: Box<Tnode> },
}

#[derive(Debug)]
pub enum Tnode {
    NullProg,
    Op(Op),
    Num {
        value: i32,
    },
    Id(Id),
    Connector {
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Read {
        id: Box<Tnode>,
    },
    Write {
        expr: Box<Tnode>,
    },
    AsgStmt {
        id: Box<Tnode>,
        expr: Box<Tnode>,
    },
    IfStmt {
        bool_expr: Box<Tnode>,
        if_slist: Box<Tnode>,
        else_slist: Box<Option<Tnode>>,
    },
    WhileStmt {
        bool_expr: Box<Tnode>,
        slist: Box<Tnode>,
    },
    DoWhileStmt {
        bool_expr: Box<Tnode>,
        slist: Box<Tnode>,
    },
    RepeatUntilStmt {
        bool_expr: Box<Tnode>,
        slist: Box<Tnode>,
    },
    ContinueStmt,
    BreakStmt,
}