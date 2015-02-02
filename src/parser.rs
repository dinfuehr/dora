use std::mem;

use lexer::Lexer;
use lexer::token::{TokenType,Token};
use lexer::position::Position;
use lexer::reader::{CodeReader,StrReader,FileReader};
use error::ParseError;
use error::ErrorCode;

use ast::UnOp;
use ast::BinOp;
use ast::Expr;

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

type ExprResult = Result<Box<Expr>,ParseError>;

impl<T: CodeReader> Parser<T> {
    pub fn new( lexer: Lexer<T> ) -> Parser<T> {
        let token = Token::new(TokenType::End, Position::new(1,1));
        let mut parser = Parser { lexer: lexer, token: token };

        parser
    }

    pub fn parse(&mut self) -> ExprResult {
        // initialize parser
        try!(self.read_token());

        self.parse_expression()
    }

    fn parse_expression(&mut self) -> ExprResult {
        self.parse_expression_l0()
    }

    fn parse_expression_l0(&mut self) -> ExprResult {
        let left = try!(self.parse_expression_l1());

        if self.token.is(TokenType::Assign) {
            let op = try!(self.read_token());
            let right = try!(self.parse_expression_l0());

            Ok(box Expr::ExprAssign(left, right))
        } else {
            Ok(left)
        }
    }

    fn parse_expression_l1(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l2());

        while self.token.is(TokenType::Eq) || self.token.is(TokenType::NEq) {
            let op = try!(self.read_token());
            let op = match op.token_type {
                TokenType::Eq => BinOp::Eq,
                _ => BinOp::NEq
            };

            let right = try!(self.parse_expression_l2());
            left = box Expr::ExprBin(op, left, right);
        }

        Ok(left)
    }

    fn parse_expression_l2(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l3());

        while self.token.is(TokenType::LThan) || self.token.is(TokenType::LEq) ||
                self.token.is(TokenType::GThan) || self.token.is(TokenType::GEq) {

            let op = try!(self.read_token());
            let op = match op.token_type {
                TokenType::LThan => BinOp::LThan,
                TokenType::LEq => BinOp::LEq,
                TokenType::GThan => BinOp::GThan,
                _ => BinOp::GEq
            };

            let right = try!(self.parse_expression_l3());
            left = box Expr::ExprBin(op, left, right);
        }

        Ok(left)
    }

    fn parse_expression_l3(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l4());

        while self.token.is(TokenType::Add) || self.token.is(TokenType::Sub) {
            let op = try!(self.read_token());
            let op = match op.token_type {
                TokenType::Add => BinOp::Add,
                _ => BinOp::Sub
            };

            let right = try!(self.parse_expression_l4());
            left = box Expr::ExprBin(op, left, right);
        }

        Ok(left)
    }

    fn parse_expression_l4(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l5());

        while self.token.is(TokenType::Mul) || self.token.is(TokenType::Div) ||
                self.token.is(TokenType::Mod) {
            let op = try!(self.read_token());
            let op = match op.token_type {
                TokenType::Mul => BinOp::Mul,
                TokenType::Div => BinOp::Div,
                _ => BinOp::Mod
            };

            let right = try!(self.parse_expression_l5());
            left = box Expr::ExprBin(op, left, right);
        }

        Ok(left)
    }

    fn parse_expression_l5(&mut self) -> ExprResult {
        if self.token.is(TokenType::Add) || self.token.is(TokenType::Sub) {
            let op = try!(self.read_token());
            let op = match op.token_type {
                TokenType::Add => UnOp::Plus,
                _ => UnOp::Neg
            };

            let expr = try!(self.parse_factor());
            Ok(box Expr::ExprUn(op, expr))
        } else {
            self.parse_factor()
        }
    }

    fn parse_factor(&mut self) -> ExprResult {
        match self.token.token_type {
            TokenType::LParen => self.parse_parentheses(),
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

    fn parse_parentheses(&mut self) -> ExprResult {
        try!(self.read_token());
        let exp = try!(self.parse_expression());
        try!(self.match_token(TokenType::RParen));

        Ok(exp)
    }

    fn parse_number(&mut self) -> ExprResult {
        let num = try!(self.read_token());

        match num.value.parse() {
            Some(num) => Ok(box Expr::ExprLitInt(num)),
            None => Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                message: format!("number {} does not fit into range", num),
                code: ErrorCode::NumberOverflow
            } )
        }
    }

    fn parse_string(&mut self) -> ExprResult {
        let string = try!(self.read_token());

        Ok(box Expr::ExprLitStr(string.value))
    }

    fn parse_identifier(&mut self) -> ExprResult {
        let ident = try!(self.read_token());

        Ok(box Expr::ExprIdent(ident.value))
    }

    fn match_token(&mut self, token_type: TokenType) -> Result<(),ParseError> {
        if self.token.token_type == token_type {
            try!(self.read_token());

            Ok(())
        } else {
            Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                message: format!("Token {:?} expected, but got token {}", token_type, self.token),
                code: ErrorCode::UnexpectedToken
            } )
        }
    }

    fn read_token(&mut self) -> Result<Token,ParseError> {
        let tok = try!(self.lexer.read_token());

        Ok(mem::replace(&mut self.token, tok))
    }
}

#[cfg(test)]
mod tests {
    use ast::BinOp;
    use ast::Expr;
    use ast::UnOp;
    use parser::Parser;

    #[test]
    fn parse_ident() {
        let mut parser = Parser::from_str("x");

        assert_eq!(Expr::ExprIdent("x".to_string()), *parser.parse().unwrap());
    }

    #[test]
    fn parse_number() {
        let mut parser = Parser::from_str("10");

        assert_eq!(Expr::ExprLitInt(10), *parser.parse().unwrap());
    }

    #[test]
    fn parse_string() {
        let mut parser = Parser::from_str("\"abc\"");

        assert_eq!(Expr::ExprLitStr("abc".to_string()), *parser.parse().unwrap());
    }

    #[test]
    fn parse_l5() {
        let mut parser = Parser::from_str("-a");
        let exp = Expr::ExprUn(UnOp::Neg, box Expr::ExprIdent("a".to_string()));
        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("+a");
        let exp = Expr::ExprUn(UnOp::Plus, box Expr::ExprIdent("a".to_string()));
        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("- -a");
        parser.parse().unwrap_err();

        let mut parser = Parser::from_str("+ +a");
        parser.parse().unwrap_err();

        let mut parser = Parser::from_str("-(-a)");
        let exp = box Expr::ExprIdent("a".to_string());
        let exp = box Expr::ExprUn(UnOp::Neg, exp);
        let exp = Expr::ExprUn(UnOp::Neg, exp);

        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("+(+a)");
        let exp = box Expr::ExprIdent("a".to_string());
        let exp = box Expr::ExprUn(UnOp::Plus, exp);
        let exp = Expr::ExprUn(UnOp::Plus, exp);

        assert_eq!(exp, *parser.parse().unwrap());
    }

    #[test]
    fn parse_l4() {
        let mut parser = Parser::from_str("a*b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::Mul, a, b);

        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("a/b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::Div, a, b);

        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("a%b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::Mod, a, b);

        assert_eq!(exp, *parser.parse().unwrap());
    }

    #[test]
    fn parse_l3() {
        let mut parser = Parser::from_str("a+b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::Add, a, b);

        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("a-b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::Sub, a, b);

        assert_eq!(exp, *parser.parse().unwrap());
    }

    #[test]
    fn parse_l2() {
        let mut parser = Parser::from_str("a<b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::LThan, a, b);

        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("a<=b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::LEq, a, b);

        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("a>b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::GThan, a, b);

        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("a>=b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::GEq, a, b);

        assert_eq!(exp, *parser.parse().unwrap());
    }

    #[test]
    fn parse_l1() {
        let mut parser = Parser::from_str("a==b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::Eq, a, b);

        assert_eq!(exp, *parser.parse().unwrap());

        let mut parser = Parser::from_str("a!=b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprBin(BinOp::NEq, a, b);

        assert_eq!(exp, *parser.parse().unwrap());
    }

    #[test]
    fn parse_l0() {
        let mut parser = Parser::from_str("a=b");
        let a = box Expr::ExprIdent("a".to_string());
        let b = box Expr::ExprIdent("b".to_string());
        let exp = Expr::ExprAssign(a, b);

        assert_eq!(exp, *parser.parse().unwrap());
    }
}
