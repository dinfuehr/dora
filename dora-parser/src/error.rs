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
    ExpectedTraitElement,
    ExpectedToken(String),
    ExpectedType,
    ExpectedTypeParam,
    ExpectedParams,
    ExpectedParam,
    ExpectedEnumVariants,
    ExpectedEnumVariant,
    ExpectedUseTree,
    ExpectedPattern,
    ExpectedField,
    MisplacedElse,
    ExpectedFactor,
    UnclosedStringTemplate,
    ExpectedIdentifier,
    ExpectedPathSegment,
    ExpectedExpression,
    ExpectedStatement,
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
            ParseError::ExpectedTraitElement => {
                format!("expected trait element.")
            }
            ParseError::ExpectedToken(exp) => {
                format!("expected `{}`.", exp)
            }
            ParseError::ExpectedParams => "expected parameters.".into(),
            ParseError::ExpectedParam => "expected param.".into(),
            ParseError::ExpectedTypeParam => "expected type param".into(),
            ParseError::ExpectedEnumVariants => "expected block with enum variants.".into(),
            ParseError::ExpectedEnumVariant => "expected enum variant.".into(),
            ParseError::ExpectedType => format!("type expected."),
            ParseError::ExpectedUseTree => "expected use tree.".into(),
            ParseError::ExpectedField => "expected field.".into(),
            ParseError::ExpectedPattern => "expected pattern.".into(),
            ParseError::MisplacedElse => "misplace else.".into(),
            ParseError::ExpectedFactor => format!("factor expected."),
            ParseError::UnclosedStringTemplate => "unclosed string template.".into(),
            ParseError::ExpectedIdentifier => {
                format!("identifier expected.")
            }
            ParseError::ExpectedPathSegment => {
                format!("path segment expected.")
            }
            ParseError::ExpectedExpression => "expected expression.".into(),
            ParseError::ExpectedStatement => "expected statement.".into(),
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
