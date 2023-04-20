use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum OpType {
    Mod,
    Div,
    Mul,
    Add,
    Sub,
    Lt,
    Gt,
    Eq,
    NEq,
    LEq,
    GEq,
    Amp,
    Deref,
    And,
    Or,
}

impl fmt::Display for OpType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            OpType::Mod => write!(f, "%"),
            OpType::Div => write!(f, "/"),
            OpType::Mul => write!(f, "*"),
            OpType::Add => write!(f, "+"),
            OpType::Sub => write!(f, "-"),
            OpType::Lt => write!(f, "<"),
            OpType::Gt => write!(f, ">"),
            OpType::Eq => write!(f, "=="),
            OpType::NEq => write!(f, "!="),
            OpType::LEq => write!(f, "<="),
            OpType::GEq => write!(f, ">="),
            OpType::Amp => write!(f, "&"),
            OpType::Deref => write!(f, "*"),
            OpType::And => write!(f, "AND"),
            OpType::Or => write!(f, "OR"),
        }
    }
}
