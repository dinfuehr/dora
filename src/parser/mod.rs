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

    fn read_token(&mut self) -> Result<Token,ParseError> {
        let tok = try!(self.lexer.read_token());

        Ok(mem::replace(&mut self.token, tok))
    }

    pub fn parse_number(&mut self) -> Result<Expr,ParseError> {
        // initialize parser
        try!(self.read_token());

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
}

#[test]
fn test_number() {
    let mut parser = Parser::from_str("10");

    assert_eq!(ExprLitInt(10), parser.parse_number().unwrap());
}

//#[test]
//fn test_string() {
    //let mut parser = Parser::from_str("\"abc\"");

    //assert_eq!(ExprLitStr("abc"), parser.parse_string().unwrap());
//}
