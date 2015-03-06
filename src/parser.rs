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
use ast::StatementType;
use ast::UnOp;

use data_type::DataType;

pub struct Parser<T: CodeReader> {
    lexer: Lexer<T>,
    token: Token,
    fct: Option<Function>
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
type FunctionResult = Result<Function,ParseError>;
type DataTypeResult = Result<DataType,ParseError>;
type StatementResult = Result<Box<Statement>,ParseError>;

impl<T: CodeReader> Parser<T> {
    pub fn new( lexer: Lexer<T> ) -> Parser<T> {
        let token = Token::new(TokenType::End, Position::new(1,1));
        let mut parser = Parser { lexer: lexer, token: token, fct: None };

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
                position: self.token.position,
                code: ErrorCode::ExpectedTopLevelElement,
                message: format!("top level element expected but got {}", self.token)
            })
        }
    }

    fn parse_function(&mut self) -> FunctionResult {
        let pos = try!(self.expect_token(TokenType::Fn)).position;
        let ident = try!(self.expect_identifier());

        let mut fct = Function::new(ident, pos);
        try!(self.parse_function_params(&mut fct));

        self.fct = Some(fct);
        let block = try!(self.parse_block());

        let mut fct = self.fct.take().unwrap();
        fct.block = block;

        Ok(fct)
    }

    fn parse_function_params(&mut self, fct: &mut Function) -> Result<(),ParseError> {
        if self.token.is(TokenType::LParen) {
            try!(self.read_token());
            let mut comma = true;

            while !self.token.is(TokenType::RParen) && !self.token.is_eof() {
                if !comma {
                    return Err(ParseError {
                        position: self.token.position,
                        message: format!("Token {:?} expected, but got token {}",
                            TokenType::Comma, self.token),
                        code: ErrorCode::UnexpectedToken
                    })
                }

                try!(self.parse_function_param(fct));
                comma = self.token.is(TokenType::Comma);

                if comma {
                  try!(self.read_token());
                }
            }

            try!(self.expect_token(TokenType::RParen));
        }

        Ok(())
    }

    fn parse_function_param(&mut self, fct: &mut Function) -> Result<(), ParseError> {
        let pos = self.token.position;
        let name = try!(self.expect_identifier());
        let data_type = try!(self.parse_data_type());

        let var = LocalVar::new(name, data_type, pos);

        if fct.exists(&var.name) {
            return Err(ParseError {
                position: pos,
                message: format!("variable {} already exists", var.name),
                code: ErrorCode::VarAlreadyExists
            })
        }

        fct.add_param(var);

        Ok(())
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
                position: self.token.position,
                code: ErrorCode::MisplacedElse,
                message: "misplaced else".to_string()
            }),
            _ => self.parse_expression_statement()
        }
    }

    fn parse_var(&mut self) -> StatementResult {
        let tok = try!(self.expect_token(TokenType::Var));
        let ident = try!(self.expect_identifier());
        let mut data_type = None;

        if self.token.is(TokenType::Colon) {
            try!(self.read_token());
            data_type = Some(try!(self.parse_data_type()));
        }

        try!(self.expect_token(TokenType::Eq));
        let expr = try!(self.parse_expression());
        try!(self.expect_semicolon());

        let ty = expr.data_type;
        let fct = self.fct.as_mut().unwrap();

        if let Some(data_type) = data_type {
            if data_type != ty {
                return Err(ParseError {
                    position: tok.position,
                    code: ErrorCode::TypeMismatch,
                    message: format!("can not assign type {} to type {}",
                        ty, data_type)
                })
            }
        }

        let var = LocalVar::new(ident, ty, tok.position);

        if fct.exists(&var.name) {
            return Err(ParseError {
                position: tok.position,
                code: ErrorCode::VarAlreadyExists,
                message: format!("variable {} already exists", var.name)
            })
        }

        let ind = fct.add_var(var);

        Ok(Statement::new(tok.position, StatementType::Var(ind, ty, expr)))
    }

    fn parse_block(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::LBrace)).position;
        let mut stmts = vec![];

        while !self.token.is(TokenType::RBrace) && !self.token.is_eof() {
            let stmt = try!(self.parse_statement());
            stmts.push(stmt);
        }

        try!(self.expect_token(TokenType::RBrace));

        Ok(Statement::new(pos, StatementType::Block(stmts)))
    }

    fn parse_if(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::If)).position;
        let expr = try!(self.parse_expression());
        let then_block = try!(self.parse_block());
        let mut else_block = None;

        if self.token.is(TokenType::Else) {
            try!(self.read_token());
            else_block = Some(try!(self.parse_block()));
        }

        Ok(Statement::new(pos, StatementType::If(expr, then_block, else_block)))
    }

    fn parse_while(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::While)).position;
        let expr = try!(self.parse_expression());
        let block = try!(self.parse_block());

        Ok(Statement::new(pos, StatementType::While(expr, block)))
    }

    fn parse_loop(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::Loop)).position;
        let block = try!(self.parse_block());

        Ok(Statement::new(pos, StatementType::Loop(block)))
    }

    fn parse_break(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::Break)).position;
        try!(self.expect_semicolon());

        Ok(Statement::new(pos, StatementType::Break))
    }

    fn parse_continue(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::Continue)).position;
        try!(self.expect_semicolon());

        Ok(Statement::new(pos, StatementType::Continue))
    }

    fn parse_return(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::Return)).position;
        let expr = try!(self.parse_expression());
        try!(self.expect_semicolon());

        Ok(Statement::new(pos, StatementType::Return(expr)))
    }

    fn parse_expression_statement(&mut self) -> StatementResult {
        let pos = self.token.position;
        let expr = try!(self.parse_expression());
        self.expect_semicolon();

        Ok(Statement::new(pos, StatementType::Expr(expr)))
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
            if !left.lvalue {
                return Err(ParseError {
                    position: self.token.position,
                    code: ErrorCode::ExpectedLvalue,
                    message: "lvalue expected for assignment".to_string()
                })
            }

            let tok = try!(self.read_token());
            let right = try!(self.parse_expression_l0());

            if left.data_type != right.data_type {
                return Err(ParseError {
                    position: tok.position,
                    code: ErrorCode::TypeMismatch,
                    message: format!("can not assign type {} to type {}",
                        left.data_type, right.data_type)
                })
            }

            Ok(Expr::new(tok.position, left.data_type, ExprType::Assign(left, right)))
        } else {
            Ok(left)
        }
    }

    fn parse_expression_l1(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l2());

        while self.token.is(TokenType::EqEq) || self.token.is(TokenType::Ne) {
            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::EqEq => BinOp::Eq,
                _ => BinOp::Ne
            };

            let right = try!(self.parse_expression_l2());
            left = Expr::new(tok.position, DataType::Bool, ExprType::Bin(op, left, right));
        }

        Ok(left)
    }

    fn parse_expression_l2(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l3());

        while self.token.is(TokenType::Lt) || self.token.is(TokenType::Le) ||
                self.token.is(TokenType::Gt) || self.token.is(TokenType::Ge) {

            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::Lt => BinOp::Lt,
                TokenType::Le => BinOp::Le,
                TokenType::Gt => BinOp::Gt,
                _ => BinOp::Ge
            };

            let right = try!(self.parse_expression_l3());
            left = Expr::new(tok.position, DataType::Bool, ExprType::Bin(op, left, right));
        }

        Ok(left)
    }

    fn parse_expression_l3(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l4());

        while self.token.is(TokenType::Add) || self.token.is(TokenType::Sub) {
            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::Add => BinOp::Add,
                _ => BinOp::Sub
            };

            let right = try!(self.parse_expression_l4());
            left = Expr::new(tok.position, DataType::Int, ExprType::Bin(op, left, right));
        }

        Ok(left)
    }

    fn parse_expression_l4(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l5());

        while self.token.is(TokenType::Mul) || self.token.is(TokenType::Div) ||
                self.token.is(TokenType::Mod) {
            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::Mul => BinOp::Mul,
                TokenType::Div => BinOp::Div,
                _ => BinOp::Mod
            };

            let right = try!(self.parse_expression_l5());
            left = Expr::new(tok.position, DataType::Int, ExprType::Bin(op, left, right));
        }

        Ok(left)
    }

    fn parse_expression_l5(&mut self) -> ExprResult {
        if self.token.is(TokenType::Add) || self.token.is(TokenType::Sub) {
            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::Add => UnOp::Plus,
                _ => UnOp::Neg
            };

            let expr = try!(self.parse_factor());
            Ok(Expr::new(tok.position, DataType::Int, ExprType::Un(op, expr)))
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
        let tok = try!(self.read_token());

        match tok.value.parse() {
            Ok(num) => Ok(Expr::lit_int(tok.position, num)),
            _ => Err(ParseError {
                position: tok.position,
                message: format!("number {} does not fit into range", tok),
                code: ErrorCode::NumberOverflow
            })
        }
    }

    fn parse_string(&mut self) -> ExprResult {
        let string = try!(self.read_token());

        Ok(Expr::new(string.position, DataType::Str, ExprType::LitStr(string.value)))
    }

    fn parse_identifier(&mut self) -> ExprResult {
        let ident = try!(self.read_token());
        let fct = self.fct.as_mut().unwrap();

        if let Some((var,ind)) = fct.get(&ident.value) {
            Ok(Expr::ident(ident.position, var.data_type, ind))
        } else {
            Err(ParseError {
                position: ident.position,
                message: format!("variable {} does not exist", ident.value),
                code: ErrorCode::VarNotFound
            })
        }

    }

    fn parse_bool_literal(&mut self) -> ExprResult {
        let tok = try!(self.read_token());
        let ty = if tok.is(TokenType::True) { ExprType::LitTrue } else { ExprType::LitFalse };

        Ok(Expr::new(tok.position, DataType::Bool, ty))
    }

    fn expect_identifier(&mut self) -> Result<String,ParseError> {
        if self.token.token_type == TokenType::Identifier {
            let ident = try!(self.read_token());

            Ok(ident.value)
        } else {
            Err(ParseError {
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
                position: self.token.position,
                message: format!("Token {:?} expected, but got token {}",
                    token_type, self.token),
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
    use ast::ExprType;
    use ast::Function;
    use ast::LocalVar;
    use ast::Program;
    use ast::Statement;
    use ast::StatementType;
    use ast::UnOp;

    use data_type::DataType;
    use error::ErrorCode;
    use lexer::position::Position;
    use parser::Parser;

    fn parse_expr(code: &'static str) -> Box<Expr> {
        Parser::from_str(code).parse_expression_only().unwrap()
    }

    fn err_expr(code: &'static str, error_code: ErrorCode, line:u32, col:u32) {
        let err = Parser::from_str(code).parse_expression_only().unwrap_err();

        assert_eq!(error_code, err.code);
        assert_eq!(line, err.position.line);
        assert_eq!(col, err.position.column);
    }

    fn parse_stmt(code: &'static str) -> Box<Statement> {
        Parser::from_str(code).parse_statement_only().unwrap()
    }

    fn err_stmt(code: &'static str, error_code: ErrorCode, line:u32, col:u32) {
        let err = Parser::from_str(code).parse_statement_only().unwrap_err();

        assert_eq!(error_code, err.code);
        assert_eq!(line, err.position.line);
        assert_eq!(col, err.position.column);
    }

    fn parse(code: &'static str) -> Program {
        Parser::from_str(code).parse().unwrap()
    }

    fn err(code: &'static str, error_code: ErrorCode, line:u32, col:u32) {
        let err = Parser::from_str(code).parse().unwrap_err();

        assert_eq!(error_code, err.code);
        assert_eq!(line, err.position.line);
        assert_eq!(col, err.position.column);
    }

    #[test]
    fn parse_ident_param() {
        let expr = parse("fn f(a int) { return a; }");
    }

    #[test]
    fn parse_ident_var() {
        let expr = parse("fn f { var a = 1; return a; }");
    }

    #[test]
    fn parse_number() {
        let expr = parse_expr("10");
        let exp = Expr::lit_int(Position::new(1, 1), 10);

        assert_eq!(exp, expr);
    }

    #[test]
    fn parse_string() {
        let expr = parse_expr("\"abc\"");
        let exp = Expr::lit_str(Position::new(1, 1), "abc".to_string());

        assert_eq!(exp, expr);
    }

    #[test]
    fn parse_true() {
        let expr = parse_expr("true");
        let exp = Expr::new(Position::new(1, 1), DataType::Bool, ExprType::LitTrue);

        assert_eq!(exp, expr);
    }

    #[test]
    fn parse_false() {
        let expr = parse_expr("false");
        let exp = Expr::new(Position::new(1, 1), DataType::Bool, ExprType::LitFalse);

        assert_eq!(exp, expr);
    }

    #[test]
    fn parse_l5_neg() {
        let a = Expr::lit_int(Position::new(1, 2), 1);
        let exp = Expr::new(Position::new(1, 1), DataType::Int, ExprType::Un(UnOp::Neg, a));
        assert_eq!(exp, parse_expr("-1"));

        err_expr("- -3", ErrorCode::UnknownFactor, 1, 3);

        let a = Expr::lit_int(Position::new(1, 4), 8);
        let exp = Expr::new(Position::new(1, 3), DataType::Int, ExprType::Un(UnOp::Neg, a));
        let exp = Expr::new(Position::new(1, 1), DataType::Int, ExprType::Un(UnOp::Neg, exp));
        assert_eq!(exp, parse_expr("-(-8)"));
    }

    #[test]
    fn parse_l5_plus() {
        let a = Expr::lit_int(Position::new(1, 2), 2);
        let exp = Expr::new(Position::new(1, 1), DataType::Int, ExprType::Un(UnOp::Plus, a));
        assert_eq!(exp, parse_expr("+2"));

        err_expr("+ +4", ErrorCode::UnknownFactor, 1, 3);

        let a = Expr::lit_int(Position::new(1, 4), 9);
        let exp = Expr::new(Position::new(1, 3), DataType::Int, ExprType::Un(UnOp::Plus, a));
        let exp = Expr::new(Position::new(1, 1), DataType::Int, ExprType::Un(UnOp::Plus, exp));
        assert_eq!(exp, parse_expr("+(+9)"));
    }

    #[test]
    fn parse_l4_mul() {
        let a = Expr::lit_int(Position::new(1, 1), 6);
        let b = Expr::lit_int(Position::new(1, 3), 3);
        let exp = Expr::new(Position::new(1, 2), DataType::Int, ExprType::Bin(BinOp::Mul, a, b));
        assert_eq!(exp, parse_expr("6*3"));
    }

    #[test]
    fn parse_l4_div() {
        let a = Expr::lit_int(Position::new(1, 1), 4);
        let b = Expr::lit_int(Position::new(1, 3), 5);
        let exp = Expr::new(Position::new(1, 2), DataType::Int, ExprType::Bin(BinOp::Div, a, b));
        assert_eq!(exp, parse_expr("4/5"));
    }

    #[test]
    fn parse_l4_mod() {
        let a = Expr::lit_int(Position::new(1, 1), 2);
        let b = Expr::lit_int(Position::new(1, 3), 15);
        let exp = Expr::new(Position::new(1, 2), DataType::Int, ExprType::Bin(BinOp::Mod, a, b));
        assert_eq!(exp, parse_expr("2%15"));
    }

    #[test]
    fn parse_l3_add() {
        let a = Expr::lit_int(Position::new(1, 1), 2);
        let b = Expr::lit_int(Position::new(1, 3), 3);
        let exp = Expr::new(Position::new(1, 2), DataType::Int, ExprType::Bin(BinOp::Add, a, b));
        assert_eq!(exp, parse_expr("2+3"));
    }

    #[test]
    fn parse_l3_sub() {
        let a = Expr::lit_int(Position::new(1, 1), 1);
        let b = Expr::lit_int(Position::new(1, 3), 2);
        let exp = Expr::new(Position::new(1, 2), DataType::Int, ExprType::Bin(BinOp::Sub, a, b));
        assert_eq!(exp, parse_expr("1-2"));
    }

    #[test]
    fn parse_l2_lt() {
        let a = Expr::lit_int(Position::new(1, 1), 1);
        let b = Expr::lit_int(Position::new(1, 3), 2);
        let exp = Expr::new(Position::new(1, 2), DataType::Bool, ExprType::Bin(BinOp::Lt, a, b));
        assert_eq!(exp, parse_expr("1<2"));
    }

    #[test]
    fn parse_l2_le() {
        let a = Expr::lit_int(Position::new(1, 1), 1);
        let b = Expr::lit_int(Position::new(1, 4), 2);
        let exp = Expr::new(Position::new(1, 2), DataType::Bool, ExprType::Bin(BinOp::Le, a, b));
        assert_eq!(exp, parse_expr("1<=2"));
    }

    #[test]
    fn parse_l2_gt() {
        let a = Expr::lit_int(Position::new(1, 1), 1);
        let b = Expr::lit_int(Position::new(1, 3), 2);
        let exp = Expr::new(Position::new(1, 2), DataType::Bool, ExprType::Bin(BinOp::Gt, a, b));
        assert_eq!(exp, parse_expr("1>2"));
    }

    #[test]
    fn parse_l2_ge() {
        let a = Expr::lit_int(Position::new(1, 1), 1);
        let b = Expr::lit_int(Position::new(1, 4), 2);
        let exp = Expr::new(Position::new(1, 2), DataType::Bool, ExprType::Bin(BinOp::Ge, a, b));
        assert_eq!(exp, parse_expr("1>=2"));
    }

    #[test]
    fn parse_l1_eq() {
        let a = Expr::lit_int(Position::new(1, 1), 1);
        let b = Expr::lit_int(Position::new(1, 4), 2);
        let exp = Expr::new(Position::new(1, 2), DataType::Bool, ExprType::Bin(BinOp::Eq, a, b));
        assert_eq!(exp, parse_expr("1==2"));
    }

    #[test]
    fn parse_l1_ne() {
        let a = Expr::lit_int(Position::new(1, 1), 1);
        let b = Expr::lit_int(Position::new(1, 4), 2);
        let exp = Expr::new(Position::new(1, 2), DataType::Bool, ExprType::Bin(BinOp::Ne, a, b));
        assert_eq!(exp, parse_expr("1!=2"));
    }

    #[test]
    fn parse_assign() {
        let a = Expr::ident(Position::new(1, 15), DataType::Int, 0);
        let b = Expr::lit_int(Position::new(1, 17), 4);
        let e = Expr::new(Position::new(1, 16), DataType::Int, ExprType::Assign(a, b));
        let s = Statement::expr(Position::new(1,15), e);
        let exp = Statement::block(Position::new(1, 13), s);
        assert_eq!(exp, parse("fn f(a int) { a=4; }").functions[0].block);
    }

    #[test]
    fn parse_assign_to_non_lvalue() {
        err("fn f { 1=1; }", ErrorCode::ExpectedLvalue, 1, 9);
    }

    #[test]
    fn parse_assign_different_types() {
        err("fn f(a int) { a=true; }", ErrorCode::TypeMismatch, 1, 16);
    }

    #[test]
    fn parse_assign_same_name() {
        err("fn f { var a=1; var a=2; }", ErrorCode::VarAlreadyExists, 1, 17);
    }

    #[test]
    fn parse_function() {
        let fct = &parse("fn a { }").functions[0];
        assert_eq!("a", fct.name.as_slice());
        assert_eq!(vec![], fct.params);
        assert_eq!(vec![], fct.vars);
        assert_eq!(Position::new(1, 1), fct.position);

        let fct = &parse(" fn b() { }").functions[0];
        assert_eq!("b", fct.name.as_slice());
        assert_eq!(vec![], fct.params);
        assert_eq!(vec![], fct.vars);
        assert_eq!(Position::new(1, 2), fct.position);
    }

    #[test]
    fn parse_function_with_single_param() {
        let fct = &parse("fn f(a int) { }").functions[0];
        let p1 = LocalVar::new( "a".to_string(), DataType::Int, Position::new(1,6) );
        assert_eq!(vec![p1], fct.vars);
        assert_eq!(vec![0], fct.params);

        let fct = &parse("fn f( b int,) { }").functions[0];
        let p1 = LocalVar::new( "b".to_string(), DataType::Int, Position::new(1,7) );
        assert_eq!(vec![p1], fct.vars);
        assert_eq!(vec![0], fct.params);
    }

    #[test]
    fn parse_function_with_multiple_params() {
        let fct = &parse("fn f(a int, b int) { }").functions[0];
        let p1 = LocalVar::new( "a".to_string(), DataType::Int, Position::new(1,6) );
        let p2 = LocalVar::new( "b".to_string(), DataType::Int, Position::new(1,13) );
        let params = vec![p1, p2];

        assert_eq!(params, fct.vars);
        assert_eq!(vec![0, 1], fct.params);

        let fct = &parse("fn f(a int, b int,) { }").functions[0];
        assert_eq!(params, fct.vars);
        assert_eq!(vec![0, 1], fct.params);

        err("fn f(a int, a int) { }", ErrorCode::VarAlreadyExists, 1, 13);
    }

    #[test]
    fn parse_var_int() {
        let o = Expr::lit_int(Position::new(1, 16), 1);
        let v = StatementType::Var(0, DataType::Int, o);
        let s = Statement::new(Position::new(1, 8), v);
        let exp = Statement::block(Position::new(1, 6), s);

        let fct = &parse("fn f { var a = 1; }").functions[0];
        assert_eq!(exp, fct.block);

        let v = LocalVar::new("a".to_string(), DataType::Int, Position::new(1, 8));
        assert_eq!(vec![v], fct.vars);
    }

    #[test]
    fn parse_var_bool() {
        let o = Expr::lit_bool(Position::new(1, 16), true);
        let v = StatementType::Var(0, DataType::Bool, o);
        let s = Statement::new(Position::new(1, 8), v);
        let exp = Statement::block(Position::new(1, 6), s);

        let fct = &parse("fn f { var b = true; }").functions[0];
        assert_eq!(exp, fct.block);

        let v = LocalVar::new("b".to_string(), DataType::Bool, Position::new(1, 8));
        assert_eq!(vec![v], fct.vars);
    }

    #[test]
    fn parse_var_wrong_type() {
        err("fn f { var a : bool = 1; }", ErrorCode::TypeMismatch, 1, 8);
    }

    #[test]
    fn parse_var_right_type() {
        parse("fn f { var x : int = 1; }");
    }

    #[test]
    fn parse_multiple_functions() {
        let fcts = parse("fn f { } fn g { }").functions;
        assert_eq!(2, fcts.len());

        assert_eq!("f", fcts[0].name.as_slice());
        assert_eq!(Position::new(1, 1), fcts[0].position);

        assert_eq!("g", fcts[1].name.as_slice());
        assert_eq!(Position::new(1, 10), fcts[1].position);
    }

    #[test]
    fn parse_if() {
        let stmt = parse_stmt("if 1 { 2; } else { 3; }");

        let e1 = Expr::lit_int(Position::new(1, 8), 2);
        let s1 = Statement::expr(Position::new(1, 8), e1);
        let b1 = Statement::block(Position::new(1, 6), s1);

        let e2 = Expr::lit_int(Position::new(1, 20), 3);
        let s2 = Statement::expr(Position::new(1, 20), e2);
        let b2 = Statement::block(Position::new(1, 18), s2);

        let cond = Expr::lit_int(Position::new(1, 4), 1);

        let exp = Statement::new(Position::new(1, 1), StatementType::If(cond, b1, Some(b2)));

        assert_eq!(exp, stmt)
    }

    #[test]
    fn parse_if_without_else() {
        let stmt = parse_stmt("if 1 { 2; }");

        let e1 = Expr::lit_int(Position::new(1, 8), 2);
        let s1 = Statement::expr(Position::new(1, 8), e1);
        let b1 = Statement::block(Position::new(1, 6), s1);

        let cond = Expr::lit_int(Position::new(1, 4), 1);

        let exp = Statement::new(Position::new(1, 1), StatementType::If(cond, b1, None));

        assert_eq!(exp, stmt)
    }

    #[test]
    fn parse_while() {
        let stmt = parse_stmt("while 1 { 2; }");

        let e = Expr::lit_int(Position::new(1, 11), 2);
        let s = Statement::expr(Position::new(1, 11), e);
        let b = Statement::block(Position::new(1, 9), s);
        let cond = Expr::lit_int(Position::new(1, 7), 1);

        let exp = Statement::new(Position::new(1, 1), StatementType::While(cond, b));

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_loop() {
        let stmt = parse_stmt("loop { 1; }");

        let e = Expr::lit_int(Position::new(1, 8), 1);
        let s = Statement::expr(Position::new(1, 8), e);
        let b = Statement::block(Position::new(1, 6), s);

        let exp = Statement::new(Position::new(1, 1), StatementType::Loop(b));

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_block() {
        let stmt = parse_stmt("{ 1; }");

        let e = Expr::lit_int(Position::new(1, 3), 1);
        let s = Statement::expr(Position::new(1, 3), e);
        let exp = Statement::block(Position::new(1, 1), s);

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_break() {
        let stmt = parse_stmt("break;");
        let exp = Statement::new(Position::new(1, 1), StatementType::Break);

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_continue() {
        let stmt = parse_stmt("continue;");
        let exp = Statement::new(Position::new(1, 1), StatementType::Continue);

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_return() {
        let stmt = parse_stmt("return 1;");
        let e = Expr::lit_int(Position::new(1, 8), 1);
        let exp = Statement::new(Position::new(1, 1), StatementType::Return(e));

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_else() {
        err_stmt("else", ErrorCode::MisplacedElse, 1, 1);
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
