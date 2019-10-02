use std::fmt;
use std::result::Result;

use crate::lexer::position::{Position, Span};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TokenKind {
    StringTail(String),
    StringExpr(String),
    LitChar(char),
    LitInt(String, IntBase, IntSuffix),
    LitFloat(String, FloatSuffix),
    Identifier(String),
    End,

    // Keywords
    Class,
    This,
    CapitalThis,
    Super,
    Fun,
    Let,
    Var,
    While,
    If,
    Else,
    Loop,
    For,
    In,
    Break,
    Continue,
    Return,
    True,
    False,
    Nil,
    Throws,
    Throw,
    Try,
    TryForce,
    TryOpt,
    Do,
    Catch,
    Finally,
    Spawn,
    At,

    Enum,
    Type,
    Alias,
    Struct,
    Trait,
    Impl,
    Module,
    Const,

    Underscore,
    Defer,

    // Operators
    Add,
    AddEq,
    Sub,
    Mul,
    Div,
    Mod,
    Not,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Dot,
    Colon,
    Sep, // ::
    Arrow,
    Tilde,
    BitOr,
    BitAnd,
    Caret,
    And,
    Or,

    Eq,
    EqEq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    EqEqEq,
    NeEqEq,
    Is,
    As,

    GtGt,
    GtGtGt,
    LtLt,
}

impl TokenKind {
    pub fn name(&self) -> &str {
        match *self {
            TokenKind::StringTail(_) => "string tail",
            TokenKind::StringExpr(_) => "string epxr",
            TokenKind::LitInt(_, _, suffix) => match suffix {
                IntSuffix::Byte => "byte number",
                IntSuffix::Int => "int number",
                IntSuffix::Long => "long number",
            },

            TokenKind::LitChar(_) => "char",

            TokenKind::LitFloat(_, suffix) => match suffix {
                FloatSuffix::Float => "float number",
                FloatSuffix::Double => "double number",
            },

            TokenKind::Identifier(_) => "identifier",
            TokenKind::End => "<<EOF>>",

            // Keywords
            TokenKind::Class => "class",
            TokenKind::This => "self",
            TokenKind::CapitalThis => "Self",
            TokenKind::Super => "super",
            TokenKind::Fun => "fun",
            TokenKind::Let => "let",
            TokenKind::Var => "var",
            TokenKind::While => "while",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Loop => "loop",
            TokenKind::For => "for",
            TokenKind::In => "in",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Return => "return",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Nil => "nil",
            TokenKind::Throws => "throws",
            TokenKind::Throw => "throw",
            TokenKind::Try => "try",
            TokenKind::TryForce => "try!",
            TokenKind::TryOpt => "try?",
            TokenKind::Do => "do",
            TokenKind::Catch => "catch",
            TokenKind::Finally => "finally",
            TokenKind::Spawn => "spawn",
            TokenKind::At => "@",

            TokenKind::Enum => "enum",
            TokenKind::Type => "type",
            TokenKind::Alias => "alias",
            TokenKind::Struct => "struct",
            TokenKind::Trait => "trait",
            TokenKind::Impl => "impl",
            TokenKind::Module => "module",
            TokenKind::Const => "const",

            TokenKind::Underscore => "_",
            TokenKind::Defer => "defer",

            // Operators
            TokenKind::Add => "+",
            TokenKind::AddEq => "+=",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::Mod => "%",
            TokenKind::Not => "!",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Dot => ".",
            TokenKind::Colon => ":",
            TokenKind::Sep => "::",
            TokenKind::Arrow => "=>",
            TokenKind::Tilde => "~",
            TokenKind::BitOr => "|",
            TokenKind::BitAnd => "&",
            TokenKind::Caret => "^",
            TokenKind::And => "&&",
            TokenKind::Or => "||",

            TokenKind::Eq => "=",
            TokenKind::EqEq => "==",
            TokenKind::Ne => "!=",
            TokenKind::Lt => "<",
            TokenKind::Le => "<=",
            TokenKind::Gt => ">",
            TokenKind::Ge => ">=",

            TokenKind::GtGt => ">>",
            TokenKind::GtGtGt => ">>>",
            TokenKind::LtLt => "<<",

            TokenKind::EqEqEq => "===",
            TokenKind::NeEqEq => "!==",
            TokenKind::Is => "is",
            TokenKind::As => "as",
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum IntSuffix {
    Int,
    Long,
    Byte,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FloatSuffix {
    Float,
    Double,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
    pub span: Span,
}

impl Token {
    pub fn new(tok: TokenKind, pos: Position, span: Span) -> Token {
        Token {
            kind: tok,
            position: pos,
            span: span,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.kind == TokenKind::End
    }

    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn name(&self) -> String {
        match self.kind {
            TokenKind::LitInt(ref val, _, suffix) => {
                let suffix = match suffix {
                    IntSuffix::Byte => "B",
                    IntSuffix::Int => "",
                    IntSuffix::Long => "L",
                };

                format!("{}{}", val, suffix)
            }

            TokenKind::StringTail(ref val) => format!("\"{}\" tail", &val),
            TokenKind::StringExpr(ref val) => format!("\"{}\" expr", &val),

            TokenKind::Identifier(ref val) => val.clone(),

            _ => self.kind.name().into(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntBase {
    Bin,
    Dec,
    Hex,
}

impl IntBase {
    pub fn num(self) -> u32 {
        match self {
            IntBase::Bin => 2,
            IntBase::Dec => 10,
            IntBase::Hex => 16,
        }
    }
}
