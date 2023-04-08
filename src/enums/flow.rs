use std::fmt;

#[derive(Debug, Clone)]
pub enum FlowType {
    If,
    While,
    DoWhile,
    RepeatUntil,
    Continue,
    Break,
}

impl fmt::Display for FlowType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            FlowType::If => write!(f, "if statement"),
            FlowType::While => write!(f, "while loop"),
            FlowType::DoWhile => write!(f, "do-while loop"),
            FlowType::RepeatUntil => write!(f, "repeat-until loop"),
            FlowType::Continue => write!(f, "continue statement"),
            FlowType::Break => write!(f, "break statement"),
        }
    }
}
