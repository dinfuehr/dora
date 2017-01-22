use std::fmt;
use std::result::Result;

use lexer::position::Position;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TokenType {
    String(String),
    Number(String),
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

impl TokenType {
    pub fn name(&self) -> &str {
        match *self {
            TokenType::String(_) => "string",
            TokenType::Number(_) => "number",
            TokenType::Identifier(_) => "identifier",
            TokenType::End => "<<EOF>>",

            TokenType::LQuote => "<",
            TokenType::RQuote => ">",

            // Keywords
            TokenType::Class => "class",
            TokenType::This => "this",
            TokenType::Super => "super",
            TokenType::Fun => "fun",
            TokenType::Let => "let",
            TokenType::Var => "var",
            TokenType::While => "while",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Loop => "loop",
            TokenType::Break => "break",
            TokenType::Continue => "continue",
            TokenType::Return => "return",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::Nil => "nil",
            TokenType::Throws => "throws",
            TokenType::Throw => "throw",
            TokenType::Try => "try",
            TokenType::TryForce => "try!",
            TokenType::TryOpt => "try?",
            TokenType::Do => "do",
            TokenType::Catch => "catch",
            TokenType::Finally => "finally",
            TokenType::Final => "final",
            TokenType::Init => "init",

            TokenType::Enum => "enum",
            TokenType::Type => "type",
            TokenType::Alias => "alias",
            TokenType::Struct => "struct",
            TokenType::Trait => "trait",

            TokenType::Underscore => "_",

            // Modifiers
            TokenType::Open => "open",
            TokenType::Override => "override",

            // Operators
            TokenType::Add => "+",
            TokenType::Sub => "-",
            TokenType::Mul => "*",
            TokenType::Div => "/",
            TokenType::Mod => "%",
            TokenType::Not => "!",
            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBracket => "[",
            TokenType::RBracket => "]",
            TokenType::LBrace => "{",
            TokenType::RBrace => "}",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::Dot => ".",
            TokenType::Colon => ":",
            TokenType::Arrow => "=>",
            TokenType::Tilde => "~",
            TokenType::BitOr => "|",
            TokenType::BitAnd => "&",
            TokenType::Caret => "^",
            TokenType::And => "&&",
            TokenType::Or => "||",
            TokenType::Internal => "internal",

            TokenType::Eq => "=",
            TokenType::EqEq => "==",
            TokenType::Ne => "!=",
            TokenType::Lt => "<",
            TokenType::Le => "<=",
            TokenType::Gt => ">",
            TokenType::Ge => ">=",

            TokenType::GtGt => ">>",
            TokenType::GtGtGt => ">>>",
            TokenType::LtLt => "<<",

            TokenType::EqEqEq => "===",
            TokenType::NeEqEq => "!==",
            TokenType::Is => "is",
            TokenType::As => "as",
        }
    }
}

#[derive(PartialEq,Eq,Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub position: Position,
}

impl Token {
    pub fn new(tok: TokenType, pos: Position) -> Token {
        Token {
            token_type: tok,
            position: pos,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.token_type == TokenType::End
    }

    pub fn is(&self, token_type: TokenType) -> bool {
        self.token_type == token_type
    }

    pub fn name(&self) -> String {
        match self.token_type {
            TokenType::Number(ref val) => val.clone(),
            TokenType::String(ref val) => format!("\"{}\"", &val),
            TokenType::Identifier(ref val) => val.clone(),

            _ => self.token_type.name().into(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}
