use lrpar::Span;

#[derive(Debug)]
pub struct LangParseError(pub Span,pub String);
