use crate::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
    // Lexer errors
    UnknownChar(char),
    UnclosedComment,
    UnclosedString,
    UnclosedChar,
    InvalidEscapeSequence(char),

    // Parser errors
    ExpectedTopLevelDeclaration,
    UnknownAnnotation(String),
    RedundantAnnotation(String),
    MisplacedAnnotation(String),
    ExpectedToken(String, String),
    ExpectedType(String),
    MisplacedElse,
    ExpectedFactor(String),
    NumberOverflow,
    UnclosedStringTemplate,
    ExpectedIdentifier(String),
    InvalidSuffix(String),
}

impl ParseError {
    pub fn message(&self) -> String {
        match self {
            ParseError::UnknownChar(ch) => {
                format!("unknown character {} (codepoint {}).", ch, *ch as usize)
            }
            ParseError::UnclosedComment => "unclosed comment.".into(),
            ParseError::UnclosedString => "unclosed string.".into(),
            ParseError::UnclosedChar => "unclosed char.".into(),
            ParseError::InvalidEscapeSequence(ch) => format!("unknown escape sequence `\\{}`.", ch),

            // Parser errors
            ParseError::ExpectedTopLevelDeclaration => {
                format!("expected top-level declaration.")
            }
            ParseError::MisplacedAnnotation(ref modifier) => {
                format!("misplaced annotation `{}`.", modifier)
            }
            ParseError::RedundantAnnotation(ref token) => {
                format!("redundant annotation {}.", token)
            }
            ParseError::UnknownAnnotation(ref token) => format!("unknown annotation {}.", token),
            ParseError::ExpectedToken(ref exp, ref got) => {
                format!("expected {} but got {}.", exp, got)
            }
            ParseError::NumberOverflow => "number too large to be represented.".into(),
            ParseError::ExpectedType(ref got) => format!("type expected but got {}.", got),
            ParseError::MisplacedElse => "misplace else.".into(),
            ParseError::ExpectedFactor(ref got) => format!("factor expected but got {}.", got),
            ParseError::UnclosedStringTemplate => "unclosed string template.".into(),
            ParseError::ExpectedIdentifier(ref tok) => {
                format!("identifier expected but got {}.", tok)
            }
            ParseError::InvalidSuffix(ref suffix) => format!("invalid suffix `{}`", suffix),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseErrorWithLocation {
    pub span: Span,
    pub error: ParseError,
}

impl ParseErrorWithLocation {
    pub fn new(span: Span, error: ParseError) -> ParseErrorWithLocation {
        ParseErrorWithLocation { span, error }
    }
}
