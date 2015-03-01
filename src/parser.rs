use std::mem;

use lexer::Lexer;
use lexer::token::{TokenType,Token};
use lexer::position::Position;
use lexer::reader::{CodeReader,StrReader,FileReader};
use error::ParseError;
use error::ErrorCode;

use ast::BinOp;
use ast::Expr;
use ast::ExprType;
use ast::Function;
use ast::LocalVar;
use ast::Program;
use ast::Statement;
use ast::UnOp;

use data_type::DataType;

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

type ExprResult = Result<Box<ExprType>,ParseError>;
type FunctionResult = Result<Function,ParseError>;
type DataTypeResult = Result<DataType,ParseError>;
type StatementResult = Result<Box<Statement>,ParseError>;

impl<T: CodeReader> Parser<T> {
    pub fn new( lexer: Lexer<T> ) -> Parser<T> {
        let token = Token::new(TokenType::End, Position::new(1,1));
        let mut parser = Parser { lexer: lexer, token: token };

        parser
    }

    pub fn parse(&mut self) -> Result<Program,ParseError> {
        // initialize parser
        try!(self.read_token());
        let mut functions = vec![];

        while !self.token.is_eof() {
            let function = try!(self.parse_top_level_element());
            functions.push(function);
        }

        Ok(Program { functions: functions })
    }

    fn parse_top_level_element(&mut self) -> FunctionResult {
        match self.token.token_type {
            TokenType::Fn => self.parse_function(),
            _ => Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                code: ErrorCode::ExpectedTopLevelElement,
                message: format!("top level element expected but got {}", self.token)
            })
        }
    }

    fn parse_function(&mut self) -> FunctionResult {
        let pos = try!(self.expect_token(TokenType::Fn)).position;
        let ident = try!(self.expect_identifier());
        let params = try!(self.parse_function_params());
        let block = try!(self.parse_block());


        Ok(Function { name: ident, params: params, block: block, position: pos })
    }

    fn parse_function_params(&mut self) -> Result<Vec<LocalVar>,ParseError> {
        let mut params = vec![];

        if self.token.is(TokenType::LParen) {
            try!(self.read_token());
            let mut comma = true;

            while !self.token.is(TokenType::RParen) && !self.token.is_eof() {
                if !comma {
                    return Err(ParseError {
                        filename: self.lexer.filename().to_string(),
                        position: self.token.position,
                        message: format!("Token {:?} expected, but got token {}", TokenType::Comma, self.token),
                        code: ErrorCode::UnexpectedToken
                    })
                }

                let pos = self.token.position;
                let name = try!(self.expect_identifier());
                let data_type = try!(self.parse_data_type());

                params.push(LocalVar { name: name, data_type: data_type, position: pos });

                comma = self.token.is(TokenType::Comma);

                if comma {
                  try!(self.read_token());
                }
            }

            try!(self.expect_token(TokenType::RParen));
        }

        Ok(params)
    }

    fn parse_statement_only(&mut self) -> StatementResult {
        try!(self.read_token());

        self.parse_statement()
    }

    fn parse_statement(&mut self) -> StatementResult {
        match self.token.token_type {
            TokenType::Var => self.parse_var(),
            TokenType::LBrace => self.parse_block(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Loop => self.parse_loop(),
            TokenType::Break => self.parse_break(),
            TokenType::Continue => self.parse_continue(),
            TokenType::Return => self.parse_return(),
            TokenType::Else => Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                code: ErrorCode::MisplacedElse,
                message: "misplaced else".to_string()
            }),
            _ => self.parse_expression_statement()
        }
    }

    fn parse_var(&mut self) -> StatementResult {
        try!(self.expect_token(TokenType::Var));
        let ident = try!(self.expect_identifier());
        let mut data_type = DataType::Int;

        if self.token.is(TokenType::Colon) {
            try!(self.read_token());
            data_type = try!(self.parse_data_type());
        }

        try!(self.expect_token(TokenType::Eq));
        let expr = try!(self.parse_expression());

        Ok(box Statement::Var(ident, data_type, expr))
    }

    fn parse_block(&mut self) -> StatementResult {
        try!(self.expect_token(TokenType::LBrace));
        let mut stmts = vec![];

        while !self.token.is(TokenType::RBrace) && !self.token.is_eof() {
            let stmt = try!(self.parse_statement());
            stmts.push(stmt);
        }

        try!(self.expect_token(TokenType::RBrace));

        Ok(box Statement::Block(stmts))
    }

    fn parse_if(&mut self) -> StatementResult {
        try!(self.expect_token(TokenType::If));
        let expr = try!(self.parse_expression());
        let then_block = try!(self.parse_block());
        let mut else_block = Statement::empty_block();

        if self.token.is(TokenType::Else) {
            try!(self.read_token());
            else_block = try!(self.parse_block());
        }

        Ok(box Statement::If(expr, then_block, else_block))
    }

    fn parse_while(&mut self) -> StatementResult {
        try!(self.expect_token(TokenType::While));
        let expr = try!(self.parse_expression());
        let block = try!(self.parse_block());

        Ok(box Statement::While(expr, block))
    }

    fn parse_loop(&mut self) -> StatementResult {
        try!(self.expect_token(TokenType::Loop));
        let block = try!(self.parse_block());

        Ok(box Statement::Loop(block))
    }

    fn parse_break(&mut self) -> StatementResult {
        try!(self.expect_token(TokenType::Break));
        try!(self.expect_semicolon());

        Ok(box Statement::Break)
    }

    fn parse_continue(&mut self) -> StatementResult {
        try!(self.expect_token(TokenType::Continue));
        try!(self.expect_semicolon());

        Ok(box Statement::Continue)
    }

    fn parse_return(&mut self) -> StatementResult {
        try!(self.expect_token(TokenType::Return));
        let expr = try!(self.parse_expression());
        try!(self.expect_semicolon());

        Ok(box Statement::Return(expr))
    }

    fn parse_expression_statement(&mut self) -> StatementResult {
        let expr = try!(self.parse_expression());
        self.expect_semicolon();

        Ok(box Statement::Expr(expr))
    }

    fn parse_data_type_only(&mut self) -> DataTypeResult {
        try!(self.read_token());

        self.parse_data_type()
    }

    fn parse_data_type(&mut self) -> DataTypeResult {
        let token = try!(self.read_token());

        match token.token_type {
            TokenType::Int => Ok(DataType::Int),
            TokenType::Bool => Ok(DataType::Bool),
            TokenType::Str => Ok(DataType::Str),
            _ => Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                code: ErrorCode::ExpectedType,
                message: format!("type expected but got {}", self.token)
            })
        }
    }

    fn parse_expression_only(&mut self) -> ExprResult {
        try!(self.read_token());

        self.parse_expression()
    }

    fn parse_expression(&mut self) -> ExprResult {
        self.parse_expression_l0()
    }

    fn parse_expression_l0(&mut self) -> ExprResult {
        let left = try!(self.parse_expression_l1());

        if self.token.is(TokenType::Eq) {
            let op = try!(self.read_token());
            let right = try!(self.parse_expression_l0());

            Ok(box ExprType::Assign(left, right))
        } else {
            Ok(left)
        }
    }

    fn parse_expression_l1(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l2());

        while self.token.is(TokenType::EqEq) || self.token.is(TokenType::Ne) {
            let op = try!(self.read_token());
            let op = match op.token_type {
                TokenType::EqEq => BinOp::Eq,
                _ => BinOp::Ne
            };

            let right = try!(self.parse_expression_l2());
            left = box ExprType::Bin(op, left, right);
        }

        Ok(left)
    }

    fn parse_expression_l2(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l3());

        while self.token.is(TokenType::Lt) || self.token.is(TokenType::Le) ||
                self.token.is(TokenType::Gt) || self.token.is(TokenType::Ge) {

            let op = try!(self.read_token());
            let op = match op.token_type {
                TokenType::Lt => BinOp::Lt,
                TokenType::Le => BinOp::Le,
                TokenType::Gt => BinOp::Gt,
                _ => BinOp::Ge
            };

            let right = try!(self.parse_expression_l3());
            left = box ExprType::Bin(op, left, right);
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
            left = box ExprType::Bin(op, left, right);
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
            left = box ExprType::Bin(op, left, right);
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
            Ok(box ExprType::Un(op, expr))
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
            TokenType::True => self.parse_bool_literal(),
            TokenType::False => self.parse_bool_literal(),
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
        try!(self.expect_token(TokenType::RParen));

        Ok(exp)
    }

    fn parse_number(&mut self) -> ExprResult {
        let num = try!(self.read_token());

        match num.value.parse() {
            Ok(num) => Ok(box ExprType::LitInt(num)),
            _ => Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                message: format!("number {} does not fit into range", num),
                code: ErrorCode::NumberOverflow
            })
        }
    }

    fn parse_string(&mut self) -> ExprResult {
        let string = try!(self.read_token());

        Ok(box ExprType::LitStr(string.value))
    }

    fn parse_identifier(&mut self) -> ExprResult {
        let ident = try!(self.read_token());

        Ok(box ExprType::Ident(ident.value))
    }

    fn parse_bool_literal(&mut self) -> ExprResult {
        let lit_true = self.token.is(TokenType::True);
        try!(self.read_token());

        if lit_true {
            Ok(box ExprType::LitTrue)
        } else {
            Ok(box ExprType::LitFalse)
        }
    }

    fn expect_identifier(&mut self) -> Result<String,ParseError> {
        if self.token.token_type == TokenType::Identifier {
            let ident = try!(self.read_token());

            Ok(ident.value)
        } else {
            Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                message: format!("identifier expected, but got token {}", self.token),
                code: ErrorCode::ExpectedIdentifier
            })
        }
    }

    fn expect_semicolon(&mut self) -> Result<Token,ParseError> {
        self.expect_token(TokenType::Semicolon)
    }

    fn expect_token(&mut self, token_type: TokenType) -> Result<Token,ParseError> {
        if self.token.token_type == token_type {
            let token = try!(self.read_token());

            Ok(token)
        } else {
            Err(ParseError {
                filename: self.lexer.filename().to_string(),
                position: self.token.position,
                message: format!("Token {:?} expected, but got token {}", token_type, self.token),
                code: ErrorCode::UnexpectedToken
            })
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
    use ast::Function;
    use ast::LocalVar;
    use ast::Program;
    use ast::Statement;
    use ast::UnOp;

    use data_type::DataType;
    use error::ErrorCode;
    use lexer::position::Position;
    use parser::Parser;

    #[test]
    fn parse_ident() {
        let mut parser = Parser::from_str("x");

        assert_eq!(ExprType::Ident("x".to_string()), *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_number() {
        let mut parser = Parser::from_str("10");

        assert_eq!(ExprType::LitInt(10), *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_string() {
        let mut parser = Parser::from_str("\"abc\"");

        assert_eq!(ExprType::LitStr("abc".to_string()), *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_true() {
        let mut parser = Parser::from_str("true");

        assert_eq!(ExprType::LitTrue, *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_false() {
        let mut parser = Parser::from_str("false");

        assert_eq!(ExprType::LitFalse, *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_l5() {
        let mut parser = Parser::from_str("-a");
        let exp = ExprType::Un(UnOp::Neg, box ExprType::Ident("a".to_string()));
        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("+a");
        let exp = ExprType::Un(UnOp::Plus, box ExprType::Ident("a".to_string()));
        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("- -a");
        parser.parse_expression_only().unwrap_err();

        let mut parser = Parser::from_str("+ +a");
        parser.parse_expression_only().unwrap_err();

        let mut parser = Parser::from_str("-(-a)");
        let exp = box ExprType::Ident("a".to_string());
        let exp = box ExprType::Un(UnOp::Neg, exp);
        let exp = ExprType::Un(UnOp::Neg, exp);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("+(+a)");
        let exp = box ExprType::Ident("a".to_string());
        let exp = box ExprType::Un(UnOp::Plus, exp);
        let exp = ExprType::Un(UnOp::Plus, exp);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_l4() {
        let mut parser = Parser::from_str("a*b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Mul, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("a/b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Div, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("a%b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Mod, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_l3() {
        let mut parser = Parser::from_str("a+b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Add, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("a-b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Sub, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_l2() {
        let mut parser = Parser::from_str("a<b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Lt, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("a<=b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Le, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("a>b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Gt, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("a>=b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Ge, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_l1() {
        let mut parser = Parser::from_str("a==b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Eq, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());

        let mut parser = Parser::from_str("a!=b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Bin(BinOp::Ne, a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_l0() {
        let mut parser = Parser::from_str("a=b");
        let a = box ExprType::Ident("a".to_string());
        let b = box ExprType::Ident("b".to_string());
        let exp = ExprType::Assign(a, b);

        assert_eq!(exp, *parser.parse_expression_only().unwrap());
    }

    #[test]
    fn parse_function() {
        let mut parser = Parser::from_str("fn a { }");
        let func = Function { name: "a".to_string(), params: vec![], block: Statement::empty_block(),
                position: Position::new(1,1) };
        let prog = Program { functions: vec![func] };
        assert_eq!(prog, parser.parse().unwrap());

        let mut parser = Parser::from_str("fn a() { }");
        assert_eq!(prog, parser.parse().unwrap());
    }

    #[test]
    fn parse_function_with_single_param() {
        let mut parser = Parser::from_str("fn f(a int) { }");
        let expr = box ExprType::LitInt(1);
        let p1 = LocalVar { name: "a".to_string(), data_type: DataType::Int,
                position: Position::new(1,6) };
        let params = vec![p1];
        let func = Function { name: "f".to_string(), params: params, block: Statement::empty_block(),
                position: Position::new(1,1) };
        let prog = Program { functions: vec![func] };
        assert_eq!(prog, parser.parse().unwrap());

        let mut parser = Parser::from_str("fn f(a int,) { }");
        assert_eq!(prog, parser.parse().unwrap());
    }

    #[test]
    fn parse_function_with_multiple_params() {
        let mut parser = Parser::from_str("fn f(a int, b int) { }");
        let p1 = LocalVar { name: "a".to_string(), data_type: DataType::Int, position: Position::new(1,6) };
        let p2 = LocalVar { name: "b".to_string(), data_type: DataType::Int, position: Position::new(1,13) };
        let func = Function { name: "f".to_string(),
            params: vec![p1, p2], block: Statement::empty_block(), position: Position::new(1,1) };
        let prog = Program { functions: vec![func] };
        assert_eq!(prog, parser.parse().unwrap());

        let mut parser = Parser::from_str("fn f(a int, b int,) { }");
        assert_eq!(prog, parser.parse().unwrap());
    }

    #[test]
    fn parse_multiple_functions() {
        let mut parser = Parser::from_str("fn f { } fn g { }");
        let f1 = Function {
            name: "f".to_string(),
            params: vec![],
            block: Statement::empty_block(),
            position: Position::new(1, 1)
        };
        let f2 = Function {
            name: "g".to_string(),
            params: vec![],
            block: Statement::empty_block(),
            position: Position::new(1, 10)
        };

        assert_eq!(Program { functions: vec![f1, f2] }, parser.parse().unwrap());
    }

    #[test]
    fn parse_if() {
        let mut parser = Parser::from_str("if 1 { 2; } else { 3; }");
        let b1 = Statement::block(ExprType::LitInt(2));
        let b2 = Statement::block(ExprType::LitInt(3));
        let stmt = box Statement::If(box ExprType::LitInt(1), b1, b2);

        assert_eq!(stmt, parser.parse_statement_only().unwrap());
    }

    #[test]
    fn parse_while() {
        let mut parser = Parser::from_str("while 1 { 2; }");
        let b = Statement::block(ExprType::LitInt(2));
        let stmt = box Statement::While(box ExprType::LitInt(1), b);

        assert_eq!(stmt, parser.parse_statement_only().unwrap());
    }

    #[test]
    fn parse_loop() {
        let mut parser = Parser::from_str("loop { 1; }");
        let b = Statement::block(ExprType::LitInt(1));
        let stmt = box Statement::Loop(b);

        assert_eq!(stmt, parser.parse_statement_only().unwrap());
    }

    #[test]
    fn parse_block() {
        let mut parser = Parser::from_str("{ 1; }");
        let b = Statement::block(ExprType::LitInt(1));

        assert_eq!(b, parser.parse_statement_only().unwrap());
    }

    #[test]
    fn parse_break() {
        let mut parser = Parser::from_str("break;");
        let b = box Statement::Break;

        assert_eq!(b, parser.parse_statement_only().unwrap());
    }

    #[test]
    fn parse_continue() {
        let mut parser = Parser::from_str("continue;");
        let b = box Statement::Continue;

        assert_eq!(b, parser.parse_statement_only().unwrap());
    }

    #[test]
    fn parse_return() {
        let mut parser = Parser::from_str("return 1;");
        let b = box Statement::Return(box ExprType::LitInt(1));

        assert_eq!(b, parser.parse_statement_only().unwrap());
    }

    #[test]
    fn parse_else() {
        let mut parser = Parser::from_str("else");

        assert_eq!(ErrorCode::MisplacedElse, parser.parse_statement_only().unwrap_err().code);
    }

    #[test]
    fn parse_int() {
        let mut parser = Parser::from_str("int");
        assert_eq!(DataType::Int, parser.parse_data_type_only().unwrap());
    }

    #[test]
    fn parse_str() {
        let mut parser = Parser::from_str("str");
        assert_eq!(DataType::Str, parser.parse_data_type_only().unwrap());
    }

    #[test]
    fn parse_bool() {
        let mut parser = Parser::from_str("bool");
        assert_eq!(DataType::Bool, parser.parse_data_type_only().unwrap());
    }
}
