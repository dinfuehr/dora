use std::mem;

use lexer::Lexer;
use lexer::token::{TokenType,Token};
use lexer::position::Position;
use lexer::reader::{CodeReader,StrReader,FileReader};
use error::ParseError;
use error::ErrorCode;

use ast::Expr;
use ast::Expr::ExprLitInt;
use ast::Expr::ExprLitStr;
use ast::Expr::ExprIdent;

pub struct Parser<T: CodeReader> {
    lexer: Lexer<T>,
    token: Token
}

impl Parser<StrReader> {
    pub fn from_str(code: &'static str) -> Parser<StrReader> {
        Parser::new(Lexer::from_str(code))
    }
}

impl Parser<FileReader> {
    pub fn from_file(filename: &'static str) -> Parser<FileReader> {
        Parser::new(Lexer::from_file(filename))
    }
}

impl<T: CodeReader> Parser<T> {
    pub fn new( lexer: Lexer<T> ) -> Parser<T> {
        let token = Token::new(TokenType::End, Position::new(1,1));
        let mut parser = Parser { lexer: lexer, token: token };

        parser
    }

    pub fn parse(&mut self) -> Result<Expr,ParseError> {
        // initialize parser
        try!(self.read_token());

        self.parse_factor()
    }

    fn parse_factor(&mut self) -> Result<Expr,ParseError> {
        match self.token.token_type {
            TokenType::Number => self.parse_number(),
            TokenType::String => self.parse_string(),
            TokenType::Identifier => self.parse_identifier(),
            _ => Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                code: ErrorCode::UnknownFactor,
                message: format!("factor expected but got {}", self.token)
            })
        }
    }

    fn parse_number(&mut self) -> Result<Expr,ParseError> {
        let num = try!(self.read_token());

        match num.value.parse::<i64>() {
            Some(num) => Ok(ExprLitInt(num)),
            None => Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                message: format!("number {} does not fit into range", num),
                code: ErrorCode::NumberOverflow
            } )
        }
    }

    fn parse_string(&mut self) -> Result<Expr,ParseError> {
        let string = try!(self.read_token());

        Ok(ExprLitStr(string.value))
    }

    fn parse_identifier(&mut self) -> Result<Expr,ParseError> {
        let ident = try!(self.read_token());

        Ok(ExprIdent(ident.value))
    }

    fn read_token(&mut self) -> Result<Token,ParseError> {
        let tok = try!(self.lexer.read_token());

        Ok(mem::replace(&mut self.token, tok))
    }
}

#[test]
fn test_number() {
    let mut parser = Parser::from_str("10");

    assert_eq!(ExprLitInt(10), parser.parse().unwrap());
}

#[test]
fn test_string() {
    let mut parser = Parser::from_str("\"abc\"");

    assert_eq!(ExprLitStr("abc".to_string()), parser.parse().unwrap());
}
