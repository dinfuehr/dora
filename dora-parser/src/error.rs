use crate::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
    // Lexer errors
    UnknownChar(char),
    UnclosedComment,
    UnclosedString,
    UnclosedChar,
    InvalidEscapeSequence,

    // Parser errors
    ExpectedTopLevelDeclaration,
    UnknownAnnotation(String),
    RedundantAnnotation(String),
    MisplacedAnnotation(String),
    ExpectedToken(String),
    ExpectedType,
    MisplacedElse,
    ExpectedFactor,
    NumberOverflow,
    UnclosedStringTemplate,
    ExpectedIdentifier,
    InvalidSuffix(String),
    ExpectedExpression,
    ExpectedImplElement,
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
            ParseError::InvalidEscapeSequence => "unknown escape sequence.".into(),

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
            ParseError::ExpectedToken(ref exp) => {
                format!("expected `{}`.", exp)
            }
            ParseError::NumberOverflow => "number too large to be represented.".into(),
            ParseError::ExpectedType => format!("type expected."),
            ParseError::MisplacedElse => "misplace else.".into(),
            ParseError::ExpectedFactor => format!("factor expected."),
            ParseError::UnclosedStringTemplate => "unclosed string template.".into(),
            ParseError::ExpectedIdentifier => {
                format!("identifier expected.")
            }
            ParseError::InvalidSuffix(ref suffix) => format!("invalid suffix `{}`", suffix),
            ParseError::ExpectedExpression => "expected expression.".into(),
            ParseError::ExpectedImplElement => "expected impl element (function).".into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseErrorWithLocation {
    pub span: Span,
    pub error: ParseError,
}

impl ParseErrorWithLocation {
    pub fn new(span: Span, error: ParseError) -> ParseErrorWithLocation {
        ParseErrorWithLocation { span, error }
    }
}
