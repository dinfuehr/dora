use std::mem;
use std::io::Error;

use ast::Ast;
use ast::BinOp;
use ast::Elem::{self, ElemFunction};
use ast::Expr;
use ast::Expr::*;
use ast::Function;
use ast::NodeId;
use ast::Param;
use ast::Stmt::{self, StmtBlock, StmtBreak, StmtContinue, StmtExpr,
    StmtIf, StmtLoop, StmtReturn, StmtVar, StmtWhile};
use ast::Type::{self, TypeUnit, TypeBasic};
use ast::TypeBasicType;
use ast::TypeUnitType;
use ast::UnOp;

use error::ParseError;
use error::ErrorCode;

use interner::Interner;
use interner::Name;

use lexer::Lexer;
use lexer::token::{TokenType,Token};
use lexer::position::Position;
use lexer::reader::{CodeReader,FileReader};

#[cfg(test)]
use lexer::reader::StrReader;

pub struct Parser<T: CodeReader> {
    lexer: Lexer<T>,
    token: Token,
    interner: Interner,

    next_id: NodeId,
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
type StmtResult = Result<Box<Stmt>,ParseError>;

impl<T: CodeReader> Parser<T> {
    pub fn new( lexer: Lexer<T> ) -> Parser<T> {
        let token = Token::new(TokenType::End, Position::new(1,1));
        let parser = Parser {
            lexer: lexer,
            token: token,
            interner: Interner::new(),
            next_id: NodeId(1),
        };

        parser
    }

    pub fn generate_id(&mut self) -> NodeId {
        let ret = self.next_id;
        self.next_id = NodeId(ret.0+1);

        ret
    }

    pub fn parse(&mut self) -> Result<Ast, ParseError> {
        try!(self.init());
        let mut elements = vec![];

        while !self.token.is_eof() {
            let el = try!(self.parse_top_level_element());
            elements.push(el);
        }

        let interner = mem::replace(&mut self.interner, Interner::new());

        Ok(Ast::new(elements, interner))
    }

    fn init(&mut self) -> Result<(), ParseError> {
        try!(self.read_token());

        Ok(())
    }

    fn parse_top_level_element(&mut self) -> Result<Elem, ParseError> {
        let pos = self.token.position;

        match self.token.token_type {
            TokenType::Fn => {
                let fct = try!(self.parse_function());
                Ok(ElemFunction(fct))
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

        let params = try!(self.parse_function_params());
        let return_type = try!(self.parse_function_type());
        let block = try!(self.parse_block());

        Ok(Function {
            id: self.generate_id(),
            name: ident,
            pos: pos,
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
            id: self.generate_id(),
            name: name,
            pos: pos,
            data_type: data_type,
        })
    }

    fn parse_function_type(&mut self) -> Result<Type, ParseError> {
        if self.token.is(TokenType::Arrow) {
            try!(self.read_token());
            let ty = try!(self.parse_type());

            Ok(ty)
        } else {
            Ok(TypeUnit(TypeUnitType {
                id: self.generate_id(),
                pos: None
            }))
        }
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.token.token_type {
            TokenType::Identifier => {
                let token = try!(self.read_token());
                let interned = self.interner.intern(token.value);

                Ok(TypeBasic(TypeBasicType {
                    id: self.generate_id(),
                    pos: token.position,
                    name: interned,
                }))
            }


            TokenType::LParen => {
                let token = try!(self.read_token());
                try!(self.expect_token(TokenType::RParen));

                Ok(TypeUnit(TypeUnitType {
                    id: self.generate_id(),
                    pos: Some(token.position)
                }))
            }

            _ => Err(ParseError {
                position: self.token.position,
                code: ErrorCode::ExpectedType,
                message: "type expected".to_string()
            }),
        }
    }

    fn parse_statement(&mut self) -> StmtResult {
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

    fn parse_var(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Var)).position;
        let ident = try!(self.expect_identifier());
        let data_type = try!(self.parse_var_type());
        let expr = try!(self.parse_var_assignment());

        try!(self.expect_semicolon());

        Ok(box Stmt::create_var(self.generate_id(), pos, ident, data_type, expr))
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

    fn parse_block(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::LBrace)).position;
        let mut stmts = vec![];

        while !self.token.is(TokenType::RBrace) && !self.token.is_eof() {
            let stmt = try!(self.parse_statement());
            stmts.push(stmt);
        }

        try!(self.expect_token(TokenType::RBrace));

        Ok(box Stmt::create_block(self.generate_id(), pos, stmts))
    }

    fn parse_if(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::If)).position;
        let cond = try!(self.parse_expression());

        let then_block = try!(self.parse_block());
        let mut else_block = None;

        if self.token.is(TokenType::Else) {
            try!(self.read_token());
            else_block = Some(try!(self.parse_block()));
        }

        Ok(box Stmt::create_if(self.generate_id(), pos, cond, then_block, else_block))
    }

    fn parse_while(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::While)).position;
        let expr = try!(self.parse_expression());

        let block = try!(self.parse_block());

        Ok(box Stmt::create_while(self.generate_id(), pos, expr, block))
    }

    fn parse_loop(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Loop)).position;
        let block = try!(self.parse_block());

        Ok(box Stmt::create_loop(self.generate_id(), pos, block))
    }

    fn parse_break(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Break)).position;
        try!(self.expect_semicolon());

        Ok(box Stmt::create_break(self.generate_id(), pos))
    }

    fn parse_continue(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Continue)).position;
        try!(self.expect_semicolon());

        Ok(box Stmt::create_continue(self.generate_id(), pos))
    }

    fn parse_return(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Return)).position;
        let expr = if self.token.is(TokenType::Semicolon) {
            None
        } else {
            let expr = try!(self.parse_expression());
            Some(expr)
        };

        try!(self.expect_semicolon());

        Ok(box Stmt::create_return(self.generate_id(), pos, expr))
    }

    fn parse_expression_statement(&mut self) -> StmtResult {
        let pos = self.token.position;
        let expr = try!(self.parse_expression());
        try!(self.expect_semicolon());

        Ok(box Stmt::create_expr(self.generate_id(), pos, expr))
    }

    fn parse_expression(&mut self) -> ExprResult {
        self.parse_expression_l0()
    }

    fn parse_expression_l0(&mut self) -> ExprResult {
        let left = try!(self.parse_expression_l1());

        if self.token.is(TokenType::Eq) {
            let tok = try!(self.read_token());
            let right = try!(self.parse_expression_l0());

            Ok(box Expr::create_assign(self.generate_id(), tok.position, left, right))
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
            left = box Expr::create_bin(self.generate_id(),
                tok.position, op, left, right);
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
            left = box Expr::create_bin(self.generate_id(), tok.position, op, left, right);
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
            left = box Expr::create_bin(self.generate_id(), tok.position, op, left, right);
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
            left = box Expr::create_bin(self.generate_id(), tok.position, op, left, right);
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
            Ok(box Expr::create_un(self.generate_id(), tok.position, op, expr))
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
            Ok(num) => Ok(box Expr::create_lit_int(self.generate_id(), tok.position, num)),
            _ => Err(ParseError {
                position: tok.position,
                message: format!("number {} does not fit into range", tok),
                code: ErrorCode::NumberOverflow
            })
        }
    }

    fn parse_string(&mut self) -> ExprResult {
        let string = try!(self.read_token());

        Ok(box Expr::create_lit_str(self.generate_id(), string.position, string.value))
    }

    fn parse_identifier(&mut self) -> ExprResult {
        let pos = self.token.position;
        let ident = try!(self.expect_identifier());

        Ok(box Expr::create_ident(self.generate_id(), pos, ident))
    }

    fn parse_bool_literal(&mut self) -> ExprResult {
        let tok = try!(self.read_token());
        let value = tok.is(TokenType::True);

        Ok(box Expr::create_lit_bool(self.generate_id(), tok.position, value))
    }

    fn expect_identifier(&mut self) -> Result<Name, ParseError> {
        if self.token.token_type == TokenType::Identifier {
            let ident = try!(self.read_token());
            let interned = self.interner.intern(ident.value);

            Ok(interned)
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
    use ast::Ast;
    use ast::BinOp;
    use ast::Expr::{self, ExprAssign, ExprBin, ExprIdent,
        ExprLitBool, ExprLitInt, ExprLitStr, ExprUn};
    use ast::NodeId;
    use ast::Param;
    use ast::Stmt::{self, StmtBlock, StmtBreak, StmtContinue, StmtExpr,
        StmtIf, StmtLoop, StmtReturn, StmtVar, StmtWhile};
    use ast::Type::{self, TypeUnit, TypeBasic};
    use ast::UnOp;

    use interner::Name;

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

    fn parse_stmt(code: &'static str) -> Box<Stmt> {
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

    fn parse(code: &'static str) -> Ast {
        Parser::from_str(code).parse().unwrap()
    }

    fn pos(line: u32, col: u32) -> Position {
        Position::new(line, col)
    }

    fn bstmt(id: NodeId, line: u32, col: u32, stmts: Vec<Box<Stmt>>) -> Box<Stmt> {
        box Stmt::create_block(id, Position::new(line, col), stmts)
    }

    fn estmt(id: NodeId, line: u32, col: u32, expr: Box<Expr>) -> Box<Stmt> {
        box Stmt::create_expr(id, Position::new(line, col), expr)
    }

    fn ident(id: NodeId, line: u32, col: u32, value: Name) -> Box<Expr> {
        box Expr::create_ident(id, Position::new(line, col), value)
    }

    fn lit_str(id: NodeId, line: u32, col: u32, value: String) -> Box<Expr> {
        box Expr::create_lit_str(id, Position::new(line, col), value)
    }

    fn lit_bool(id: NodeId, line: u32, col: u32, value: bool) -> Box<Expr> {
        box Expr::create_lit_bool(id, Position::new(line, col), value)
    }

    fn lit_int(id: NodeId, line: u32, col: u32, value: i32) -> Box<Expr> {
        box Expr::create_lit_int(id, Position::new(line, col), value)
    }

    #[test]
    fn parse_ident() {
        let expr = parse_expr("a");

        let ident = expr.to_ident().unwrap();
        assert_eq!(Name(0), ident.name);
    }

    #[test]
    fn parse_number() {
        let expr = parse_expr("10");

        let lit = expr.to_lit_int().unwrap();
        assert_eq!(10, lit.value);
    }

    #[test]
    fn parse_string() {
        let expr = parse_expr("\"abc\"");

        let lit = expr.to_lit_str().unwrap();
        assert_eq!("abc", &lit.value);
    }

    #[test]
    fn parse_true() {
        let expr = parse_expr("true");

        let lit = expr.to_lit_bool().unwrap();
        assert_eq!(true, lit.value);
    }

    #[test]
    fn parse_false() {
        let expr = parse_expr("true");

        let lit = expr.to_lit_bool().unwrap();
        assert_eq!(true, lit.value);
    }

    #[test]
    fn parse_neg() {
        let expr = parse_expr("-1");

        let un = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, un.op);

        assert!(un.opnd.is_lit_int());
    }

    #[test]
    fn parse_double_neg() {
        let expr = parse_expr("-(-3)");

        let neg1 = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg1.op);

        let neg2 = neg1.opnd.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg2.op);

        assert!(neg2.opnd.is_lit_int());
    }

    #[test]
    fn parse_double_neg_without_parentheses() {
        err_expr("- -2", ErrorCode::UnknownFactor, 1, 3);
    }

    #[test]
    fn parse_unary_plus() {
        let expr = parse_expr("+2");

        let add = expr.to_un().unwrap();
        assert_eq!(UnOp::Plus, add.op);

        assert!(add.opnd.is_lit_int());
    }

    #[test]
    fn parse_double_unary_plus_without_parentheses() {
        err_expr("+ +4", ErrorCode::UnknownFactor, 1, 3);
    }

    #[test]
    fn parse_double_unary_plus() {
        let expr = parse_expr("+(+9)");

        let add1 = expr.to_un().unwrap();
        assert_eq!(UnOp::Plus, add1.op);

        let add2 = add1.opnd.to_un().unwrap();
        assert_eq!(UnOp::Plus, add2.op);
        assert!(add2.opnd.is_lit_int());
    }

    #[test]
    fn parse_mul() {
        let expr = parse_expr("6*3");

        let mul = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul.op);
        assert_eq!(6, mul.lhs.to_lit_int().unwrap().value);
        assert_eq!(3, mul.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_multiple_muls() {
        let expr = parse_expr("6*3*4");

        let mul1 = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul1.op);

        let mul2 = mul1.lhs.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul2.op);
        assert_eq!(6, mul2.lhs.to_lit_int().unwrap().value);
        assert_eq!(3, mul2.rhs.to_lit_int().unwrap().value);

        assert_eq!(4, mul1.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_div() {
        let expr = parse_expr("4/5");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Div, div.op);
        assert_eq!(4, div.lhs.to_lit_int().unwrap().value);
        assert_eq!(5, div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_mod() {
        let expr = parse_expr("2%15");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mod, div.op);
        assert_eq!(2, div.lhs.to_lit_int().unwrap().value);
        assert_eq!(15, div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_l3_add() {
        let a = lit_int(NodeId(1), 1, 1, 2);
        let b = lit_int(NodeId(2), 1, 3, 3);
        let exp = Expr::create_bin(NodeId(3), pos(1, 2), BinOp::Add, a, b);
        assert_eq!(exp, *parse_expr("2+3"));
    }

    #[test]
    fn parse_l3_sub() {
        let a = lit_int(NodeId(1), 1, 1, 1);
        let b = lit_int(NodeId(2), 1, 3, 2);
        let exp = Expr::create_bin(NodeId(3), pos(1, 2), BinOp::Sub, a, b);
        assert_eq!(exp, *parse_expr("1-2"));
    }

    #[test]
    fn parse_l2_lt() {
        let a = lit_int(NodeId(1), 1, 1, 1);
        let b = lit_int(NodeId(2), 1, 3, 2);
        let exp = Expr::create_bin(NodeId(3), pos(1, 2), BinOp::Lt, a, b);
        assert_eq!(exp, *parse_expr("1<2"));
    }

    #[test]
    fn parse_l2_le() {
        let a = lit_int(NodeId(1), 1, 1, 1);
        let b = lit_int(NodeId(2), 1, 4, 2);
        let exp = Expr::create_bin(NodeId(3), pos(1, 2), BinOp::Le, a, b);
        assert_eq!(exp, *parse_expr("1<=2"));
    }

    #[test]
    fn parse_l2_gt() {
        let a = lit_int(NodeId(1), 1, 1, 1);
        let b = lit_int(NodeId(2), 1, 3, 2);
        let exp = Expr::create_bin(NodeId(3), pos(1, 2), BinOp::Gt, a, b);
        assert_eq!(exp, *parse_expr("1>2"));
    }

    #[test]
    fn parse_l2_ge() {
        let a = lit_int(NodeId(1), 1, 1, 1);
        let b = lit_int(NodeId(2), 1, 4, 2);
        let exp = Expr::create_bin(NodeId(3), pos(1, 2), BinOp::Ge, a, b);
        assert_eq!(exp, *parse_expr("1>=2"));
    }

    #[test]
    fn parse_l1_eq() {
        let a = lit_int(NodeId(1), 1, 1, 1);
        let b = lit_int(NodeId(2), 1, 4, 2);
        let exp = Expr::create_bin(NodeId(3), pos(1, 2), BinOp::Eq, a, b);
        assert_eq!(exp, *parse_expr("1==2"));
    }

    #[test]
    fn parse_l1_ne() {
        let a = lit_int(NodeId(1), 1, 1, 1);
        let b = lit_int(NodeId(2), 1, 4, 2);
        let exp = Expr::create_bin(NodeId(3), pos(1, 2), BinOp::Ne, a, b);
        assert_eq!(exp, *parse_expr("1!=2"));
    }

    #[test]
    fn parse_assign() {
        let a = ident(NodeId(1), 1, 1, Name(0));
        let b = lit_int(NodeId(2), 1, 3, 4);
        let exp = Expr::create_assign(NodeId(3), pos(1, 2), a, b);

        assert_eq!(exp, *parse_expr("a=4"));
    }

    #[test]
    fn parse_function() {
        let prog = parse("fn b() { }");
        let fct = prog.function("b").unwrap();

        assert_eq!(Name(0), fct.name);
        assert_eq!(0, fct.params.len());
        assert_eq!(Type::create_unit_implicit(NodeId(1)), fct.return_type);
        assert_eq!(Position::new(1, 1), fct.pos);
    }

    #[test]
    fn parse_function_with_single_param() {
        let p1 = parse("fn f(a:int) { }");
        let f1 = p1.function("f").unwrap();

        let p2 = parse("fn f(a:int,) { }");
        let f2 = p2.function("f").unwrap();

        assert_eq!(f1.params, f2.params);

        let param = Param {
            id: NodeId(2),
            name: Name(1),
            pos: Position::new(1, 6),
            data_type: Type::create_basic(NodeId(1), pos(1,8), Name(2)),
        };

        assert_eq!(vec![param], f1.params);
    }

    #[test]
    fn parse_function_with_multiple_params() {
        let p1 = parse("fn f(a:int, b:str) { }");
        let f1 = p1.function("f").unwrap();

        let p2 = parse("fn f(a:int, b:str,) { }");
        let f2 = p2.function("f").unwrap();

        assert_eq!(f1.params, f2.params);

        let p1 = Param {
            id: NodeId(2),
            name: Name(1),
            pos: Position::new(1, 6),
            data_type: Type::create_basic(NodeId(1), pos(1, 8), Name(2)),
        };

        let p2 = Param {
            id: NodeId(4),
            name: Name(3),
            pos: Position::new(1, 13),
            data_type: Type::create_basic(NodeId(3), pos(1, 15), Name(4)),
        };

        assert_eq!(vec![p1, p2], f1.params);
    }

    #[test]
    fn parse_var_without_type() {
        let lit = lit_int(NodeId(1), 1, 9, 1);
        let var = Stmt::create_var(
            NodeId(2),
            pos(1, 1),
            Name(0),
            None,
            Some(lit)
        );

        let stmt = parse_stmt("var a = 1;");

        assert_eq!(var, *stmt);
    }

    #[test]
    fn parse_var_with_type() {
        let var = Stmt::create_var(
            NodeId(3),
            pos(1, 1),
            Name(0),
            Some(Type::create_basic(NodeId(1), pos(1, 9), Name(1))),
            Some(lit_int(NodeId(2), 1, 15, 1))
        );

        let stmt = parse_stmt("var x : int = 1;");

        assert_eq!(var, *stmt);
    }

    #[test]
    fn parse_var_with_type_but_without_assignment() {
        let var = Stmt::create_var(
            NodeId(2),
            pos(1, 1),
            Name(0),
            Some(Type::create_basic(NodeId(1), pos(1, 9), Name(1))),
            None
        );

        let stmt = parse_stmt("var x : int;");

        assert_eq!(var, *stmt);
    }

    #[test]
    fn parse_var_without_type_and_assignment() {
        let var = Stmt::create_var(
            NodeId(1),
            pos(1, 1),
            Name(0),
            None,
            None
        );

        let stmt = parse_stmt("var x;");

        assert_eq!(var, *stmt);
    }

    #[test]
    fn parse_multiple_functions() {
        let prog = parse("fn f() { } fn g() { }");

        let f = prog.function("f").unwrap();
        assert_eq!(Name(0), f.name);
        assert_eq!(Position::new(1, 1), f.pos);

        let g = prog.function("g").unwrap();
        assert_eq!(Name(1), g.name);
        assert_eq!(Position::new(1, 12), g.pos);
    }

    #[test]
    fn parse_expr_stmt() {
        let stmt = parse_stmt("1;");
        let lit = lit_int(NodeId(1), 1, 1, 1);
        let expr = estmt(NodeId(2), 1, 1, lit);

        assert_eq!(expr, stmt);

        err_stmt("1", ErrorCode::UnexpectedToken, 1, 2);
    }

    #[test]
    fn parse_if() {
        let e1 = lit_int(NodeId(2), 1, 11, 2);
        let s1 = estmt(NodeId(3), 1, 11, e1);
        let b1 = bstmt(NodeId(4), 1, 9, vec![s1]);

        let e2 = lit_int(NodeId(5), 1, 23, 3);
        let s2 = estmt(NodeId(6), 1, 23, e2);
        let b2 = bstmt(NodeId(7), 1, 21, vec![s2]);

        let cond = lit_bool(NodeId(1), 1, 4, true);

        let sif = Stmt::create_if(NodeId(8), pos(1, 1), cond, b1, Some(b2));

        let stmt = parse_stmt("if true { 2; } else { 3; }");
        assert_eq!(sif, *stmt)
    }

    #[test]
    fn parse_if_without_else() {
        let e1 = lit_int(NodeId(2), 1, 11, 2);
        let s1 = estmt(NodeId(3), 1, 11, e1);
        let b1 = bstmt(NodeId(4), 1, 9, vec![s1]);

        let cond = lit_bool(NodeId(1), 1, 4, true);

        let exp = Stmt::create_if(NodeId(5), pos(1, 1), cond, b1, None);
        let stmt = parse_stmt("if true { 2; }");
        assert_eq!(exp, *stmt)
    }

    #[test]
    fn parse_while() {
        let e = lit_int(NodeId(2), 1, 14, 2);
        let s = estmt(NodeId(3), 1, 14, e);
        let b = bstmt(NodeId(4), 1, 12, vec![s]);
        let cond = lit_bool(NodeId(1), 1, 7, true);

        let exp = Stmt::create_while(NodeId(5), pos(1, 1), cond, b);
        let stmt = parse_stmt("while true { 2; }");
        assert_eq!(exp, *stmt);
    }

    #[test]
    fn parse_loop() {
        let e = lit_int(NodeId(1), 1, 8, 1);
        let s = estmt(NodeId(2), 1, 8, e);
        let b = bstmt(NodeId(3), 1, 6, vec![s]);

        let exp = Stmt::create_loop(NodeId(4), pos(1, 1), b);
        let stmt = parse_stmt("loop { 1; }");
        assert_eq!(exp, *stmt);
    }

    #[test]
    fn parse_block() {
        let stmt = parse_stmt("{ 1; }");

        let e = lit_int(NodeId(1), 1, 3, 1);
        let s = estmt(NodeId(2), 1, 3, e);
        let exp = bstmt(NodeId(3), 1, 1, vec![s]);

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_block_with_multiple_stmts() {
        let stmt = parse_stmt("{ 1; 2; }");

        let e = lit_int(NodeId(1), 1, 3, 1);
        let s1 = estmt(NodeId(2), 1, 3, e);

        let e = lit_int(NodeId(3), 1, 6, 2);
        let s2 = estmt(NodeId(4), 1, 6, e);

        let exp = bstmt(NodeId(5), 1, 1, vec![s1, s2]);

        assert_eq!(exp, stmt);
    }

    #[test]
    fn parse_break() {
        let s = Stmt::create_break(NodeId(1), pos(1, 1));
        let stmt = parse_stmt("break;");

        assert_eq!(s, *stmt)
    }

    #[test]
    fn parse_continue() {
        let s = Stmt::create_continue(NodeId(1), pos(1, 1));
        let stmt = parse_stmt("continue;");

        assert_eq!(s, *stmt)
    }

    #[test]
    fn parse_return_value() {
        let e = lit_int(NodeId(1), 1, 8, 1);
        let s = Stmt::create_return(NodeId(2), pos(1, 1), Some(e));

        let stmt = parse_stmt("return 1;");
        assert_eq!(s, *stmt);
    }

    #[test]
    fn parse_return() {
        let s = Stmt::create_return(NodeId(1), pos(1, 1), None);
        let stmt = parse_stmt("return;");

        assert_eq!(s, *stmt);
    }

    #[test]
    fn parse_else() {
        err_stmt("else", ErrorCode::MisplacedElse, 1, 1);
    }

    #[test]
    fn parse_type_basic() {
        assert_eq!(Type::create_basic(NodeId(1), pos(1, 1), Name(0)), parse_type("int"));
        assert_eq!(Type::create_basic(NodeId(1), pos(1, 1), Name(0)), parse_type("string"));
    }

    #[test]
    fn parse_type_unit() {
        assert_eq!(Type::create_unit(NodeId(1), pos(1,1)), parse_type("()"));
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
