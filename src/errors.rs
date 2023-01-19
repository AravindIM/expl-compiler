use lrpar::Span;

#[derive(Debug)]
pub struct ParseError(pub Span,pub String);