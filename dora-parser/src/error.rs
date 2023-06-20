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
    ExpectedElement,
    ExpectedToken(String),
    ExpectedType,
    ExpectedTypeParam,
    ExpectedParams,
    ExpectedParam,
    ExpectedEnumVariants,
    ExpectedEnumVariant,
    ExpectedUsePath,
    MisplacedElse,
    ExpectedFactor,
    UnclosedStringTemplate,
    ExpectedIdentifier,
    ExpectedExpression,
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
            ParseError::ExpectedElement => {
                format!("expected top-level declaration.")
            }
            ParseError::ExpectedToken(ref exp) => {
                format!("expected `{}`.", exp)
            }
            ParseError::ExpectedParams => "expected parameters.".into(),
            ParseError::ExpectedParam => "expected param.".into(),
            ParseError::ExpectedTypeParam => "expected type param".into(),
            ParseError::ExpectedEnumVariants => "expected block with enum variants.".into(),
            ParseError::ExpectedEnumVariant => "expected enum variant.".into(),
            ParseError::ExpectedType => format!("type expected."),
            ParseError::ExpectedUsePath => "expected use path.".into(),
            ParseError::MisplacedElse => "misplace else.".into(),
            ParseError::ExpectedFactor => format!("factor expected."),
            ParseError::UnclosedStringTemplate => "unclosed string template.".into(),
            ParseError::ExpectedIdentifier => {
                format!("identifier expected.")
            }
            ParseError::ExpectedExpression => "expected expression.".into(),
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
