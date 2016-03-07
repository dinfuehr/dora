use std::fmt;
use std::result::Result;

use lexer::position::Position;

#[derive(PartialEq,Eq,Debug,Copy,Clone)]
pub enum TokenType {
    String,
    Number,
    Identifier,
    End,

    // Keywords
    Class,
    This,
    Fn,
    Let,
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

    Enum,
    Type,
    Alias,
    Struct,
    Trait,

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

    Eq,
    EqEq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Is,
    IsNot,
}

#[derive(PartialEq,Eq,Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub position: Position
}

impl Token {
    pub fn new( tok: TokenType, pos: Position ) -> Token {
        Token { token_type: tok, value: "".to_string(), position: pos }
    }

    pub fn is_eof(&self) -> bool {
        self.token_type == TokenType::End
    }

    pub fn is(&self, token_type: TokenType) -> bool {
        self.token_type == token_type
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?} (with value {:?})", self.token_type, self.value)
    }
}
