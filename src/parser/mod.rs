use std::mem;
use std::io::Error;

use ast::BinOp;
use ast::BlockStmt;
use ast::BreakStmt;
use ast::ContinueStmt;
use ast::Expr;
use ast::ExprStmt;
use ast::ExprType;
use ast::Function;
use ast::IfStmt;
use ast::LoopStmt;
use ast::Param;
use ast::Program;
use ast::ReturnStmt;
use ast::Statement;
use ast::TopLevelElement;
use ast::Type;
use ast::TypeParams;
use ast::UnOp;
use ast::VarStmt;
use ast::WhileStmt;

use error::ParseError;
use error::ErrorCode;

use lexer::Lexer;
use lexer::token::{TokenType,Token};
use lexer::position::Position;
use lexer::reader::{CodeReader,FileReader};

#[cfg(test)]
use lexer::reader::StrReader;

pub struct Parser<T: CodeReader> {
    lexer: Lexer<T>,
    token: Token,
}

#[cfg(test)]
impl Parser<StrReader> {
    pub fn from_str(code: &'static str) -> Parser<StrReader> {
        Parser::new(Lexer::from_str(code))
    }
}

impl Parser<FileReader> {
    pub fn from_file(filename: &str) -> Result<Parser<FileReader>,Error> {
        let reader = try!(Lexer::from_file(filename));

        Ok(Parser::new(reader))
    }
}

type ExprResult = Result<Box<Expr>,ParseError>;
type StatementResult = Result<Box<Statement>,ParseError>;

impl<T: CodeReader> Parser<T> {
    pub fn new( lexer: Lexer<T> ) -> Parser<T> {
        let token = Token::new(TokenType::End, Position::new(1,1));
        let parser = Parser { lexer: lexer, token: token };

        parser
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        try!(self.init());
        let mut elements = vec![];

        while !self.token.is_eof() {
            let el = try!(self.parse_top_level_element());
            elements.push(el);
        }

        Ok(Program { elements: elements })
    }

    fn init(&mut self) -> Result<(), ParseError> {
        try!(self.read_token());

        Ok(())
    }

    fn parse_top_level_element(&mut self) -> Result<TopLevelElement, ParseError> {
        match self.token.token_type {
            TokenType::Fn => {
                let fct = try!(self.parse_function());
                Ok(TopLevelElement::Function(fct))
            }

            _ => Err(ParseError {
                position: self.token.position,
                code: ErrorCode::ExpectedTopLevelElement,
                message: format!("top level element expected but got {}", self.token)
            })
        }
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        let pos = try!(self.expect_token(TokenType::Fn)).position;
        let ident = try!(self.expect_identifier());

        let type_params = try!(self.parse_type_params());
        let params = try!(self.parse_function_params());
        let return_type = try!(self.parse_function_type());
        let block = try!(self.parse_block());

        Ok(Function {
            name: ident,
            position: pos,
            type_params: type_params,
            params: params,
            return_type: return_type,
            block: block,
        })
    }

    fn parse_function_params(&mut self) -> Result<Vec<Param>,ParseError> {
        try!(self.expect_token(TokenType::LParen));

        let params = try!(self.parse_comma_list(TokenType::RParen, |p| {
            p.parse_function_param()
        }));

        Ok(params)
    }

    fn parse_comma_list<F,R>(&mut self, stop: TokenType, parse: F) -> Result<Vec<R>, ParseError>
        where F: Fn(&mut Parser<T>) -> Result<R, ParseError> {
        let mut data = vec![];
        let mut comma = true;

        while !self.token.is(stop) && !self.token.is_eof() {
            if !comma {
                return Err(ParseError {
                    position: self.token.position,
                    code: ErrorCode::CommaExpected,
                    message: format!("`,` expected but got {}", self.token)
                })
            }

            let entry = try!(parse(self));
            data.push(entry);

            comma = self.token.is(TokenType::Comma);
            if comma { try!(self.read_token()); }
        }

        try!(self.expect_token(stop));

        Ok(data)
    }

    fn parse_function_param(&mut self) -> Result<Param, ParseError> {
        let pos = self.token.position;
        let name = try!(self.expect_identifier());

        try!(self.expect_token(TokenType::Colon));
        let data_type = try!(self.parse_type());

        Ok(Param {
            name: name,
            position: pos,
            data_type: data_type,
        })
    }

    fn parse_function_type(&mut self) -> Result<Type, ParseError> {
        if self.token.is(TokenType::Arrow) {
            try!(self.read_token());
            let ty = try!(self.parse_type());

            Ok(ty)
        } else {
            Ok(Type::Tuple(Vec::new()))
        }
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.token.token_type {
            TokenType::Mul => {
                try!(self.read_token());
                let subtype = try!(self.parse_type());
                Ok(Type::Ptr(box subtype))
            }

            TokenType::Identifier => {
                let token = try!(self.read_token());

                if self.token.is(TokenType::Lt) {
                    try!(self.read_token());
                    let params = try!(self.parse_comma_list(TokenType::Gt, |p| p.parse_type()));

                    Ok(Type::Generic(token.value, params))
                } else {
                    Ok(Type::Basic(token.value))
                }

            }

            TokenType::LBracket => {
                try!(self.read_token());
                let subtype = try!(self.parse_type());
                try!(self.expect_token(TokenType::RBracket));

                Ok(Type::Slice(box subtype))
            }

            TokenType::LParen => {
                try!(self.read_token());
                let types = try!(self.parse_comma_list(TokenType::RParen, |p| p.parse_type()));

                Ok(Type::Tuple(types))
            }

            _ => Err(ParseError {
                position: self.token.position,
                code: ErrorCode::ExpectedType,
                message: "type expected".to_string()
            }),
        }
    }

    fn parse_type_params(&mut self) -> Result<TypeParams, ParseError> {
        let params = if self.token.is(TokenType::Lt) {
            try!(self.read_token());
            try!(self.parse_comma_list(TokenType::Gt, |p| p.expect_identifier()))
        } else {
            Vec::new()
        };

        Ok(TypeParams { params: params })
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
        let pos = try!(self.expect_token(TokenType::Var)).position;
        let ident = try!(self.expect_identifier());
        let data_type = try!(self.parse_var_type());
        let expr = try!(self.parse_var_assignment());

        try!(self.expect_semicolon());

        Ok(box Statement::Var(VarStmt {
            position: pos,
            name: ident,
            data_type: data_type,
            expression: expr
        }))
    }

    fn parse_var_type(&mut self) -> Result<Option<Type>, ParseError> {
        if self.token.is(TokenType::Colon) {
            try!(self.read_token());

            Ok(Some(try!(self.parse_type())))
        } else {
            Ok(None)
        }
    }

    fn parse_var_assignment(&mut self) -> Result<Option<Box<Expr>>, ParseError> {
        if self.token.is(TokenType::Eq) {
            try!(self.expect_token(TokenType::Eq));
            let expr = try!(self.parse_expression());

            Ok(Some(expr))
        } else {
            Ok(None)
        }
    }

    fn parse_block(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::LBrace)).position;
        let mut stmts = vec![];

        while !self.token.is(TokenType::RBrace) && !self.token.is_eof() {
            let stmt = try!(self.parse_statement());
            stmts.push(stmt);
        }

        try!(self.expect_token(TokenType::RBrace));

        Ok(box Statement::Block(BlockStmt {
            position: pos,
            statements: stmts
        }))
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

        Ok(box Statement::If(IfStmt {
            position: pos,
            condition: expr,
            then_block: then_block,
            else_block: else_block,
        }))
    }

    fn parse_while(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::While)).position;
        let expr = try!(self.parse_expression());

        let block = try!(self.parse_block());

        Ok(box Statement::While(WhileStmt {
            position: pos,
            condition: expr,
            block: block,
        }))
    }

    fn parse_loop(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::Loop)).position;
        let block = try!(self.parse_block());

        Ok(box Statement::Loop(LoopStmt {
            position: pos,
            block: block,
        }))
    }

    fn parse_break(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::Break)).position;
        try!(self.expect_semicolon());

        Ok(box Statement::Break(BreakStmt {
            position: pos,
        }))
    }

    fn parse_continue(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::Continue)).position;
        try!(self.expect_semicolon());

        Ok(box Statement::Continue(ContinueStmt {
            position: pos,
        }))
    }

    fn parse_return(&mut self) -> StatementResult {
        let pos = try!(self.expect_token(TokenType::Return)).position;
        let expr = if self.token.is(TokenType::Semicolon) {
            None
        } else {
            let expr = try!(self.parse_expression());
            Some(expr)
        };

        try!(self.expect_semicolon());

        Ok(box Statement::Return(ReturnStmt {
            position: pos,
            expression: expr,
        }))
    }

    fn parse_expression_statement(&mut self) -> StatementResult {
        let pos = self.token.position;
        let expr = try!(self.parse_expression());
        try!(self.expect_semicolon());

        Ok(box Statement::Expr(ExprStmt {
            position: pos,
            expression: expr,
        }))
    }

    fn parse_expression(&mut self) -> ExprResult {
        self.parse_expression_l0()
    }

    fn parse_expression_l0(&mut self) -> ExprResult {
        let left = try!(self.parse_expression_l1());

        if self.token.is(TokenType::Eq) {
            let tok = try!(self.read_token());
            let right = try!(self.parse_expression_l0());

            Ok(Expr::new(tok.position, ExprType::Assign(left, right)))
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
            left = Expr::new(tok.position, ExprType::Bin(op, left, right));
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
            left = Expr::new(tok.position, ExprType::Bin(op, left, right));
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
            left = Expr::new(tok.position, ExprType::Bin(op, left, right));
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
            left = Expr::new(tok.position, ExprType::Bin(op, left, right));
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
            Ok(Expr::new(tok.position, ExprType::Un(op, expr)))
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
            Ok(num) => Ok(Expr::new(tok.position, ExprType::LitInt(num))),
            _ => Err(ParseError {
                position: tok.position,
                message: format!("number {} does not fit into range", tok),
                code: ErrorCode::NumberOverflow
            })
        }
    }

    fn parse_string(&mut self) -> ExprResult {
        let string = try!(self.read_token());

        Ok(Expr::new(string.position, ExprType::LitStr(string.value)))
    }

    fn parse_identifier(&mut self) -> ExprResult {
        let pos = self.token.position;
        let ident = try!(self.expect_identifier());

        Ok(Expr::new(pos, ExprType::Ident(ident)))
    }

    fn parse_bool_literal(&mut self) -> ExprResult {
        let tok = try!(self.read_token());
        let ty = if tok.is(TokenType::True) { ExprType::LitTrue } else { ExprType::LitFalse };

        Ok(Expr::new(tok.position, ty))
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
    use ast::BlockStmt;
    use ast::BreakStmt;
    use ast::ContinueStmt;
    use ast::Expr;
    use ast::ExprStmt;
    use ast::ExprType;
    use ast::IfStmt;
    use ast::LoopStmt;
    use ast::Param;
    use ast::Program;
    use ast::ReturnStmt;
    use ast::Statement;
    use ast::Type;
    use ast::TypeParams;
    use ast::UnOp;
    use ast::VarStmt;
    use ast::WhileStmt;

    use error::ErrorCode;
    use lexer::position::Position;
    use parser::Parser;

    fn parse_expr(code: &'static str) -> Box<Expr> {
        let mut parser = Parser::from_str(code);
        assert!(parser.init().is_ok(), true);

        parser.parse_expression().unwrap()
    }

    fn err_expr(code: &'static str, error_code: ErrorCode, line:u32, col:u32) {
        let err = {
            let mut parser = Parser::from_str(code);

            assert!(parser.init().is_ok(), true);
            parser.parse_expression().unwrap_err()
        };

        assert_eq!(error_code, err.code);
        assert_eq!(line, err.position.line);
        assert_eq!(col, err.position.column);
    }

    fn parse_stmt(code: &'static str) -> Box<Statement> {
        let mut parser = Parser::from_str(code);
        assert!(parser.init().is_ok(), true);

        parser.parse_statement().unwrap()
    }

    fn err_stmt(code: &'static str, error_code: ErrorCode, line:u32, col:u32) {
        let err = {
            let mut parser = Parser::from_str(code);

            assert!(parser.init().is_ok(), true);
            parser.parse_statement().unwrap_err()
        };

        assert_eq!(error_code, err.code);
        assert_eq!(line, err.position.line);
        assert_eq!(col, err.position.column);
    }

    fn parse_type(code: &'static str) -> Type {
        let mut parser = Parser::from_str(code);
        assert!(parser.init().is_ok(), true);

        parser.parse_type().unwrap()
    }

    fn parse(code: &'static str) -> Program {
        Parser::from_str(code).parse().unwrap()
    }

    fn bstmt(line: u32, col: u32, stmts: Vec<Box<Statement>>) -> Box<Statement> {
        box Statement::Block(BlockStmt {
            position: Position::new(line, col),
            statements: stmts,
        })
    }

    fn estmt(line: u32, col: u32, expr: Box<Expr>) -> Box<Statement> {
        box Statement::Expr(ExprStmt {
            position: Position::new(line, col),
            expression: expr,
        })
    }

    fn e(line: u32, col: u32, expr: ExprType) -> Box<Expr> {
        Expr::new(Position::new(line, col), expr)
    }

    fn ident(line: u32, col: u32, value: &str) -> Box<Expr> {
        e(line, col, ExprType::Ident(value.to_string()))
    }

    fn lit_str(line: u32, col: u32, value: String) -> Box<Expr> {
        Expr::new(Position::new(line, col), ExprType::LitStr(value))
    }

    fn lit_bool(line: u32, col: u32, value: bool) -> Box<Expr> {
        let ty = if value { ExprType::LitTrue } else { ExprType::LitFalse };

        Expr::new(Position::new(line, col), ty)
    }

    fn lit_int(line: u32, col: u32, value: i64) -> Box<Expr> {
        Expr::new(Position::new(line, col), ExprType::LitInt(value))
    }

    #[test]
    fn parse_ident() {
        let expr = parse_expr("a");
        let exp = ident(1, 1, "a");

        assert_eq!(exp, expr);
    }

    #[test]
    fn parse_number() {
        let expr = parse_expr("10");
        let exp = lit_int(1, 1, 10);

        assert_eq!(exp, expr);
    }

    #[test]
    fn parse_string() {
        let expr = parse_expr("\"abc\"");
        let exp = lit_str(1, 1, "abc".to_string());

        assert_eq!(exp, expr);
    }

    #[test]
    fn parse_true() {
        let expr = parse_expr("true");
        let exp = e(1, 1, ExprType::LitTrue);

        assert_eq!(exp, expr);
    }

    #[test]
    fn parse_false() {
        let expr = parse_expr("false");
        let exp = e(1, 1, ExprType::LitFalse);

        assert_eq!(exp, expr);
    }

    #[test]
    fn parse_l5_neg() {
        let a = lit_int(1, 2, 1);
        let exp = e(1, 1, ExprType::Un(UnOp::Neg, a));
        assert_eq!(exp, parse_expr("-1"));

        err_expr("- -3", ErrorCode::UnknownFactor, 1, 3);

        let a = lit_int(1, 4, 8);
        let exp = e(1, 3, ExprType::Un(UnOp::Neg, a));
        let exp = e(1, 1, ExprType::Un(UnOp::Neg, exp));
        assert_eq!(exp, parse_expr("-(-8)"));
    }

    #[test]
    fn parse_l5_plus() {
        let a = lit_int(1, 2, 2);
        let exp = e(1, 1, ExprType::Un(UnOp::Plus, a));
        assert_eq!(exp, parse_expr("+2"));

        err_expr("+ +4", ErrorCode::UnknownFactor, 1, 3);

        let a = lit_int(1, 4, 9);
        let exp = e(1, 3, ExprType::Un(UnOp::Plus, a));
        let exp = e(1, 1, ExprType::Un(UnOp::Plus, exp));
        assert_eq!(exp, parse_expr("+(+9)"));
    }

    #[test]
    fn parse_l4_mul() {
        let a = lit_int(1, 1, 6);
        let b = lit_int(1, 3, 3);
        let exp = e(1, 2, ExprType::Bin(BinOp::Mul, a, b));
        assert_eq!(exp, parse_expr("6*3"));
    }

    #[test]
    fn parse_l4_div() {
        let a = lit_int(1, 1, 4);
        let b = lit_int(1, 3, 5);
        let exp = e(1, 2, ExprType::Bin(BinOp::Div, a, b));
        assert_eq!(exp, parse_expr("4/5"));
    }

    #[test]
    fn parse_l4_mod() {
        let a = lit_int(1, 1, 2);
        let b = lit_int(1, 3, 15);
        let exp = e(1, 2, ExprType::Bin(BinOp::Mod, a, b));
        assert_eq!(exp, parse_expr("2%15"));
    }

    #[test]
    fn parse_l3_add() {
        let a = lit_int(1, 1, 2);
        let b = lit_int(1, 3, 3);
        let exp = e(1, 2, ExprType::Bin(BinOp::Add, a, b));
        assert_eq!(exp, parse_expr("2+3"));
    }

    #[test]
    fn parse_l3_sub() {
        let a = lit_int(1, 1, 1);
        let b = lit_int(1, 3, 2);
        let exp = e(1, 2, ExprType::Bin(BinOp::Sub, a, b));
        assert_eq!(exp, parse_expr("1-2"));
    }

    #[test]
    fn parse_l2_lt() {
        let a = lit_int(1, 1, 1);
        let b = lit_int(1, 3, 2);
        let exp = e(1, 2, ExprType::Bin(BinOp::Lt, a, b));
        assert_eq!(exp, parse_expr("1<2"));
    }

    #[test]
    fn parse_l2_le() {
        let a = lit_int(1, 1, 1);
        let b = lit_int(1, 4, 2);
        let exp = e(1, 2, ExprType::Bin(BinOp::Le, a, b));
        assert_eq!(exp, parse_expr("1<=2"));
    }

    #[test]
    fn parse_l2_gt() {
        let a = lit_int(1, 1, 1);
        let b = lit_int(1, 3, 2);
        let exp = e(1, 2, ExprType::Bin(BinOp::Gt, a, b));
        assert_eq!(exp, parse_expr("1>2"));
    }

    #[test]
    fn parse_l2_ge() {
        let a = lit_int(1, 1, 1);
        let b = lit_int(1, 4, 2);
        let exp = e(1, 2, ExprType::Bin(BinOp::Ge, a, b));
        assert_eq!(exp, parse_expr("1>=2"));
    }

    #[test]
    fn parse_l1_eq() {
        let a = lit_int(1, 1, 1);
        let b = lit_int(1, 4, 2);
        let exp = e(1, 2, ExprType::Bin(BinOp::Eq, a, b));
        assert_eq!(exp, parse_expr("1==2"));
    }

    #[test]
    fn parse_l1_ne() {
        let a = lit_int(1, 1, 1);
        let b = lit_int(1, 4, 2);
        let exp = e(1, 2, ExprType::Bin(BinOp::Ne, a, b));
        assert_eq!(exp, parse_expr("1!=2"));
    }

    #[test]
    fn parse_assign() {
        let a = ident(1, 1, "a");
        let b = lit_int(1, 3, 4);
        let exp = e(1, 2, ExprType::Assign(a, b));

        assert_eq!(exp, parse_expr("a=4"));
    }

    #[test]
    fn parse_function() {
        let prog = parse("fn b() { }");
        let fct = prog.get_function("b").unwrap();

        assert_eq!("b", &fct.name);
        assert_eq!(0, fct.params.len());
        assert_eq!(0, fct.type_params.params.len());
        assert_eq!(Type::Tuple(Vec::new()), fct.return_type);
        assert_eq!(Position::new(1, 1), fct.position);
    }

    #[test]
    fn parse_function_with_single_param() {
        let p1 = parse("fn f(a:int) { }");
        let f1 = p1.get_function("f").unwrap();

        let p2 = parse("fn f(a:int,) { }");
        let f2 = p2.get_function("f").unwrap();

        assert_eq!(f1.params, f2.params);

        let param = Param {
            name: "a".to_string(),
            position: Position::new(1, 6),
            data_type: Type::Basic("int".to_string()),
        };

        assert_eq!(vec![param], f1.params);
    }

    #[test]
    fn parse_function_with_multiple_params() {
        let p1 = parse("fn f(a:int, b:str) { }");
        let f1 = p1.get_function("f").unwrap();

        let p2 = parse("fn f(a:int, b:str,) { }");
        let f2 = p2.get_function("f").unwrap();

        assert_eq!(f1.params, f2.params);

        let p1 = Param {
            name: "a".to_string(),
            position: Position::new(1, 6),
            data_type: Type::Basic("int".to_string()),
        };

        let p2 = Param {
            name: "b".to_string(),
            position: Position::new(1, 13),
            data_type: Type::Basic("str".to_string()),
        };

        assert_eq!(vec![p1, p2], f1.params);
    }

    #[test]
    fn parse_function_generic() {
        let prog = parse("fn f<T>() {}");
        let fct = prog.get_function("f").unwrap();

        let params = TypeParams { params: vec!["T".to_string()] };
        assert_eq!(params, fct.type_params);
    }

    #[test]
    fn parse_var_without_type() {
        let v = VarStmt {
            position: Position::new(1, 1),
            name: "a".to_string(),
            data_type: None,
            expression: Some(lit_int(1, 9, 1))
        };

        let v = Statement::Var(v);
        let stmt = parse_stmt("var a = 1;");

        assert_eq!(v, *stmt);
    }

    #[test]
    fn parse_var_with_type() {
        let v = VarStmt {
            position: Position::new(1, 1),
            name: "x".to_string(),
            data_type: Some(Type::Basic("int".to_string())),
            expression: Some(lit_int(1, 15, 1))
        };

        let s = Statement::Var(v);
        let stmt = parse_stmt("var x : int = 1;");

        assert_eq!(s, *stmt);
    }

    #[test]
    fn parse_var_with_type_but_without_assignment() {
        let v = VarStmt {
            position: Position::new(1, 1),
            name: "x".to_string(),
            data_type: Some(Type::Basic("int".to_string())),
            expression: None,
        };

        let s = Statement::Var(v);
        let stmt = parse_stmt("var x : int;");

        assert_eq!(s, *stmt);
    }

    #[test]
    fn parse_var_without_type_and_assignment() {
        let v = VarStmt {
            position: Position::new(1, 1),
            name: "x".to_string(),
            data_type: None,
            expression: None,
        };

        let s = Statement::Var(v);
        let stmt = parse_stmt("var x;");

        assert_eq!(s, *stmt);
    }

    #[test]
    fn parse_multiple_functions() {
        let prog = parse("fn f() { } fn g() { }");

        let f = prog.get_function("f").unwrap();
        assert_eq!("f", &f.name);
        assert_eq!(Position::new(1, 1), f.position);

        let g = prog.get_function("g").unwrap();
        assert_eq!("g", &g.name);
        assert_eq!(Position::new(1, 12), g.position);
    }

    #[test]
    fn parse_expr_stmt() {
        let stmt = parse_stmt("1;");
        let expr = estmt(1, 1, lit_int(1, 1, 1));

        assert_eq!(expr, stmt);

        err_stmt("1", ErrorCode::UnexpectedToken, 1, 2);
    }

    #[test]
    fn parse_if() {
        let stmt = parse_stmt("if true { 2; } else { 3; }");

        let e1 = lit_int(1, 11, 2);
        let s1 = estmt(1, 11, e1);
        let b1 = bstmt(1, 9, vec![s1]);

        let e2 = lit_int(1, 23, 3);
        let s2 = estmt(1, 23, e2);
        let b2 = bstmt(1, 21, vec![s2]);

        let cond = lit_bool(1, 4, true);
        let exp = Statement::If(IfStmt {
            position: Position::new(1, 1),
            condition: cond,
            then_block: b1,
            else_block: Some(b2)
        });

        assert_eq!(exp, *stmt)
    }

    #[test]
    fn parse_if_without_else() {
        let stmt = parse_stmt("if true { 2; }");

        let e1 = lit_int(1, 11, 2);
        let s1 = estmt(1, 11, e1);
        let b1 = bstmt(1, 9, vec![s1]);

        let cond = lit_bool(1, 4, true);

        let exp = Statement::If(IfStmt {
            position: Position::new(1, 1),
            condition: cond,
            then_block: b1,
            else_block: None
        });

        assert_eq!(exp, *stmt)
    }

    #[test]
    fn parse_while() {
        let stmt = parse_stmt("while true { 2; }");

        let e = lit_int(1, 14, 2);
        let s = estmt(1, 14, e);
        let b = bstmt(1, 12, vec![s]);
        let cond = lit_bool(1, 7, true);

        let exp = Statement::While(WhileStmt {
            position: Position::new(1, 1),
            condition: cond,
            block: b
        });

        assert_eq!(exp, *stmt);
    }

    #[test]
    fn parse_loop() {
        let stmt = parse_stmt("loop { 1; }");

        let e = lit_int(1, 8, 1);
        let s = estmt(1, 8, e);
        let b = bstmt(1, 6, vec![s]);

        let exp = Statement::Loop(LoopStmt {
            position: Position::new(1, 1),
            block: b
        });

        assert_eq!(exp, *stmt);
    }

    #[test]
    fn parse_block() {
        let stmt = parse_stmt("{ 1; }");

        let e = lit_int(1, 3, 1);
        let s = estmt(1, 3, e);
        let exp = bstmt(1, 1, vec![s]);

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_block_with_multiple_stmts() {
        let stmt = parse_stmt("{ 1; 2; }");

        let e = lit_int(1, 3, 1);
        let s1 = estmt(1, 3, e);

        let e = lit_int(1, 6, 2);
        let s2 = estmt(1, 6, e);

        let exp = bstmt(1, 1, vec![s1, s2]);

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_break() {
        let stmt = parse_stmt("break;");
        let s = Statement::Break(BreakStmt {
            position: Position::new(1, 1),
        });

        assert_eq!(s, *stmt)
    }

    #[test]
    fn parse_continue() {
        let stmt = parse_stmt("continue;");
        let s = Statement::Continue(ContinueStmt {
            position: Position::new(1, 1),
        });

        assert_eq!(s, *stmt)
    }

    #[test]
    fn parse_return_value() {
        let stmt = parse_stmt("return 1;");

        let e = lit_int(1, 8, 1);
        let s = Statement::Return(ReturnStmt {
            position: Position::new(1, 1),
            expression: Some(e)
        });

        assert_eq!(s, *stmt);
    }

    #[test]
    fn parse_return() {
        let stmt = parse_stmt("return;");
        let s = Statement::Return(ReturnStmt {
            position: Position::new(1, 1),
            expression: None
        });

        assert_eq!(s, *stmt);
    }

    #[test]
    fn parse_else() {
        err_stmt("else", ErrorCode::MisplacedElse, 1, 1);
    }

    #[test]
    fn parse_type_basic() {
        assert_eq!(Type::Basic("int".to_string()), parse_type("int"));
        assert_eq!(Type::Basic("string".to_string()), parse_type("string"));
    }

    #[test]
    fn parse_type_slice() {
        let t = Type::Basic("int".to_string());
        assert_eq!(Type::Slice(box t), parse_type("[int]"));

        let t = Type::Basic("string".to_string());
        assert_eq!(Type::Slice(box t), parse_type("[string]"));
    }

    #[test]
    fn parse_type_generic() {
        assert_eq!(Type::Generic("Test".to_string(), Vec::new()), parse_type("Test<>"));

        let t = Type::Basic("int".to_string());
        assert_eq!(Type::Generic("Vec".to_string(), vec![t]), parse_type("Vec<int>"));

        let t1 = Type::Basic("int".to_string());
        let t2 = Type::Basic("string".to_string());
        assert_eq!(Type::Generic("Map".to_string(), vec![t1, t2]), parse_type("Map<int,string>"));
    }

    #[test]
    fn parse_type_ptr() {
        let t = Type::Basic("int".to_string());
        assert_eq!(Type::Ptr(box t), parse_type("*int"));

        let t = Type::Basic("string".to_string());
        assert_eq!(Type::Ptr(box t), parse_type("*string"));
    }

    #[test]
    fn parse_type_unit() {
        assert_eq!(Type::Tuple(Vec::new()), parse_type("()"));
    }

    #[test]
    fn parse_type_tuple_with_one_element() {
        let t = Type::Basic("string".to_string());
        assert_eq!(Type::Tuple(vec![t]), parse_type("(string)"));
    }

    #[test]
    fn parse_type_pair() {
        let t1 = Type::Basic("int".to_string());
        let t2 = Type::Basic("string".to_string());
        assert_eq!(Type::Tuple(vec![t1, t2]), parse_type("(int,string)"));
    }

    #[test]
    fn parse_file() {
        let parser = Parser::from_file("tests/abc.txt");

        assert!(parser.is_ok());
    }

    #[test]
    fn parse_non_existing_file() {
        let parser = Parser::from_file("tests/non_existing.txt");

        assert!(parser.is_err());
    }
}
