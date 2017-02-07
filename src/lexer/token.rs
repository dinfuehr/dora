use std::fmt;
use std::result::Result;

use lexer::position::Position;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TokenKind {
    String(String),
    Number(String, NumberSuffix),
    Identifier(String),
    End,

    LQuote,
    RQuote,

    // Keywords
    Class,
    This,
    Super,
    Fun,
    Let,
    Var,
    While,
    If,
    Else,
    Loop,
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
    Final,
    Init,

    Enum,
    Type,
    Alias,
    Struct,
    Trait,

    Underscore,

    // Modifiers
    Open,
    Override,

    // Operators
    Add,
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
    Arrow,
    Tilde,
    BitOr,
    BitAnd,
    Caret,
    And,
    Or,
    Internal,

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
            TokenKind::String(_) => "string",
            TokenKind::Number(_, suffix) => {
                match suffix {
                    NumberSuffix::Byte => "byte number",
                    NumberSuffix::Int => "int number",
                    NumberSuffix::Long => "long number",
                }
            }

            TokenKind::Identifier(_) => "identifier",
            TokenKind::End => "<<EOF>>",

            TokenKind::LQuote => "<",
            TokenKind::RQuote => ">",

            // Keywords
            TokenKind::Class => "class",
            TokenKind::This => "this",
            TokenKind::Super => "super",
            TokenKind::Fun => "fun",
            TokenKind::Let => "let",
            TokenKind::Var => "var",
            TokenKind::While => "while",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Loop => "loop",
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
            TokenKind::Final => "final",
            TokenKind::Init => "init",

            TokenKind::Enum => "enum",
            TokenKind::Type => "type",
            TokenKind::Alias => "alias",
            TokenKind::Struct => "struct",
            TokenKind::Trait => "trait",

            TokenKind::Underscore => "_",

            // Modifiers
            TokenKind::Open => "open",
            TokenKind::Override => "override",

            // Operators
            TokenKind::Add => "+",
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
            TokenKind::Arrow => "=>",
            TokenKind::Tilde => "~",
            TokenKind::BitOr => "|",
            TokenKind::BitAnd => "&",
            TokenKind::Caret => "^",
            TokenKind::And => "&&",
            TokenKind::Or => "||",
            TokenKind::Internal => "internal",

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
pub enum NumberSuffix {
    Int,
    Long,
    Byte,
}

#[derive(PartialEq,Eq,Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

impl Token {
    pub fn new(tok: TokenKind, pos: Position) -> Token {
        Token {
            kind: tok,
            position: pos,
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
            TokenKind::Number(ref val, suffix) => {
                let suffix = match suffix {
                    NumberSuffix::Byte => "B",
                    NumberSuffix::Int => "",
                    NumberSuffix::Long => "L",
                };

                format!("{}{}", val, suffix)
            }

            TokenKind::String(ref val) => format!("\"{}\"", &val),
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
