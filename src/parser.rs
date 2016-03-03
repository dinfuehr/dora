use std::mem;
use std::io::Error;

use ast::*;
use ast::Elem::*;

use error::*;

use interner::*;

use lexer::*;
use lexer::token::*;
use lexer::position::Position;
use lexer::reader::{CodeReader, FileReader};

#[cfg(test)]
use lexer::reader::StrReader;

pub struct Parser<'a, T: CodeReader> {
    lexer: Lexer<T>,
    token: Token,
    interner: &'a mut Interner,
    param_idx: u32,
    prop_idx: u32,
    in_class: bool,

    next_id: NodeId,
}

#[cfg(test)]
impl<'a> Parser<'a, StrReader> {
    pub fn from_str(code: &'static str, interner: &'a mut Interner) -> Parser<'a, StrReader> {
        Parser::new(Lexer::from_str(code), interner)
    }
}

impl<'a> Parser<'a, FileReader> {
    pub fn from_file(filename: &str, interner: &'a mut Interner) -> Result<Parser<'a, FileReader>,Error> {
        let reader = try!(Lexer::from_file(filename));

        Ok(Parser::new(reader, interner))
    }
}

type ExprResult = Result<Box<Expr>,ParseError>;
type StmtResult = Result<Box<Stmt>,ParseError>;

impl<'a, T: CodeReader> Parser<'a, T> {
    pub fn new(lexer: Lexer<T>, interner: &'a mut Interner) -> Parser<T> {
        let token = Token::new(TokenType::End, Position::new(1,1));
        let parser = Parser {
            lexer: lexer,
            token: token,
            interner: interner,
            param_idx: 0,
            prop_idx: 0,
            in_class: false,
            next_id: NodeId(1),
        };

        parser
    }

    fn generate_id(&mut self) -> NodeId {
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

        Ok(Ast::new(elements))
    }

    fn init(&mut self) -> Result<(), ParseError> {
        try!(self.read_token());

        Ok(())
    }

    fn parse_top_level_element(&mut self) -> Result<Elem, ParseError> {
        match self.token.token_type {
            TokenType::Fn => {
                let fct = try!(self.parse_function());
                Ok(ElemFunction(fct))
            }

            TokenType::Class => {
                let class = try!(self.parse_class());
                Ok(ElemClass(class))
            }

            _ => Err(ParseError {
                position: self.token.position,
                code: ErrorCode::ExpectedTopLevelElement,
                message: format!("top level element expected but got {}", self.token)
            })
        }
    }

    fn parse_class(&mut self) -> Result<Class, ParseError> {
        let pos = try!(self.expect_token(TokenType::Class)).position;
        let ident = try!(self.expect_identifier());

        let mut cls = Class {
            id: self.generate_id(),
            name: ident,
            pos: pos,
            ctor: None,
            props: Vec::new(),
            methods: Vec::new()
        };

        self.in_class = true;
        try!(self.parse_primary_ctor(&mut cls));
        try!(self.parse_class_body(&mut cls));

        let ctor = self.generate_ctor(&mut cls);
        cls.ctor = Some(ctor);

        self.in_class = false;

        Ok(cls)
    }

    fn parse_primary_ctor(&mut self, cls: &mut Class) -> Result<(), ParseError> {
        if !self.token.is(TokenType::LParen) {
            return Ok(());
        }

        try!(self.expect_token(TokenType::LParen));
        self.prop_idx = 0;

        let mut props = try!(self.parse_comma_list(TokenType::RParen, |p| {
            p.prop_idx += 1;

            p.parse_primary_ctor_param()
        }));

        cls.props.append(&mut props);

        Ok(())
    }

    fn generate_ctor(&mut self, cls: &mut Class) -> Function {
        let mut assignments : Vec<_> = cls.props.iter().map(|prop| {
            let this = self.build_this();
            let lhs = self.build_prop(this, prop.name);
            let rhs = self.build_ident(prop.name);
            let ass = self.build_assign(lhs, rhs);

            self.build_stmt_expr(ass)
        }).collect();

        let this = self.build_this();
        assignments.push(self.build_return(Some(this)));

        let params = cls.props.iter().enumerate().map(|(idx, prop)| {
            self.build_param(idx as u32, prop.name, prop.data_type.clone())
        }).collect();
        let id = self.generate_id();

        Function {
            id: id,
            pos: Position::new(1, 1),
            name: cls.name,
            method: true,
            params: params,
            return_type: Some(self.build_type(cls.name)),
            block: self.build_block(assignments)
        }
    }

    fn build_stmt_expr(&mut self, expr: Box<Expr>) -> Box<Stmt> {
        let id = self.generate_id();

        Box::new(Stmt::StmtExpr(StmtExprType {
            id: id,
            pos: Position::new(1, 1),
            expr: expr
        }))
    }

    fn build_param(&mut self, idx: u32, name: Name, ty: Type) -> Param {
        let id = self.generate_id();

        Param {
            id: id,
            idx: idx,
            name: name,
            mutable: false,
            pos: Position::new(1, 1),
            data_type: ty
        }
    }

    fn build_block(&mut self, stmts: Vec<Box<Stmt>>) -> Box<Stmt> {
        let id = self.generate_id();

        Box::new(Stmt::StmtBlock(StmtBlockType {
            id: id,
            pos: Position::new(1, 1),
            stmts: stmts
        }))
    }

    fn build_type(&mut self, name: Name) -> Type {
        let id = self.generate_id();

        Type::TypeBasic(TypeBasicType {
            id: id,
            pos: Position::new(1, 1),
            name: name
        })
    }

    fn build_ident(&mut self, name: Name) -> Box<Expr> {
        let id = self.generate_id();

        Box::new(Expr::ExprIdent(ExprIdentType {
            id: id,
            pos: Position::new(1, 1),
            name: name
        }))
    }

    fn build_return(&mut self, expr: Option<Box<Expr>>) -> Box<Stmt> {
        let id = self.generate_id();

        Box::new(Stmt::StmtReturn(StmtReturnType {
            id: id,
            pos: Position::new(1, 1),
            expr: expr,
        }))
    }

    fn build_this(&mut self) -> Box<Expr> {
        let id = self.generate_id();

        Box::new(Expr::ExprSelf(ExprSelfType {
            id: id,
            pos: Position::new(1, 1),
        }))
    }

    fn build_assign(&mut self, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        let id = self.generate_id();

        Box::new(Expr::ExprAssign(ExprAssignType {
            id: id,
            pos: Position::new(1, 1),
            lhs: lhs,
            rhs: rhs
        }))
    }

    fn build_prop(&mut self, object: Box<Expr>, name: Name) -> Box<Expr> {
        let id = self.generate_id();

        Box::new(Expr::ExprProp(ExprPropType {
            id: id,
            pos: Position::new(1, 1),
            object: object,
            name: name
        }))
    }

    fn parse_primary_ctor_param(&mut self) -> Result<Prop, ParseError> {
        let pos = self.token.position;
        let name = try!(self.expect_identifier());

        try!(self.expect_token(TokenType::Colon));
        let data_type = try!(self.parse_type());

        Ok(Prop {
            id: self.generate_id(),
            idx: self.prop_idx - 1,
            name: name,
            pos: pos,
            data_type: data_type,
        })
    }

    fn parse_class_body(&mut self, cls: &mut Class) -> Result<(), ParseError> {
        if !self.token.is(TokenType::LBrace) {
            return Ok(());
        }

        try!(self.read_token());

        while !self.token.is(TokenType::RBrace) {
            let fct = try!(self.parse_function());
            cls.methods.push(fct);
        }

        try!(self.read_token());
        Ok(())
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
            method: self.in_class,
            params: params,
            return_type: return_type,
            block: block,
        })
    }

    fn parse_function_params(&mut self) -> Result<Vec<Param>,ParseError> {
        try!(self.expect_token(TokenType::LParen));
        self.param_idx = 0;

        let params = try!(self.parse_comma_list(TokenType::RParen, |p| {
            p.param_idx += 1;

            p.parse_function_param()
        }));

        Ok(params)
    }

    fn parse_comma_list<F, R>(&mut self, stop: TokenType, parse: F) -> Result<Vec<R>, ParseError>
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
        let mutable = if self.token.is(TokenType::Mut) {
            try!(self.read_token());
            true
        } else {
            false
        };

        let name = try!(self.expect_identifier());

        try!(self.expect_token(TokenType::Colon));
        let data_type = try!(self.parse_type());

        Ok(Param {
            id: self.generate_id(),
            idx: self.param_idx - 1,
            name: name,
            pos: pos,
            mutable: mutable,
            data_type: data_type,
        })
    }

    fn parse_function_type(&mut self) -> Result<Option<Type>, ParseError> {
        if self.token.is(TokenType::Arrow) {
            try!(self.read_token());
            let ty = try!(self.parse_type());

            Ok(Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.token.token_type {
            TokenType::Identifier => {
                let token = try!(self.read_token());
                let name = self.interner.intern(&token.value);

                Ok(Type::create_basic(
                    self.generate_id(),
                    token.position,
                    name,
                ))
            }

            TokenType::LParen => {
                let token = try!(self.read_token());
                let subtypes = try!(self.parse_comma_list(TokenType::RParen, |p| {
                    let ty = try!(p.parse_type());

                    Ok(Box::new(ty))
                }));

                Ok(Type::create_tuple(
                    self.generate_id(),
                    token.position,
                    subtypes
                ))
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
            TokenType::Let => self.parse_let(),
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

    fn parse_let(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Let)).position;
        let mutable = if self.token.is(TokenType::Mut) {
            try!(self.read_token());

            true
        } else {
            false
        };

        let ident = try!(self.expect_identifier());
        let data_type = try!(self.parse_var_type());
        let expr = try!(self.parse_var_assignment());

        try!(self.expect_semicolon());

        Ok(Box::new(Stmt::create_let(self.generate_id(), pos, ident, mutable, data_type, expr)))
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

        Ok(Box::new(Stmt::create_block(self.generate_id(), pos, stmts)))
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

        Ok(Box::new(Stmt::create_if(self.generate_id(), pos, cond, then_block, else_block)))
    }

    fn parse_while(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::While)).position;
        let expr = try!(self.parse_expression());

        let block = try!(self.parse_block());

        Ok(Box::new(Stmt::create_while(self.generate_id(), pos, expr, block)))
    }

    fn parse_loop(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Loop)).position;
        let block = try!(self.parse_block());

        Ok(Box::new(Stmt::create_loop(self.generate_id(), pos, block)))
    }

    fn parse_break(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Break)).position;
        try!(self.expect_semicolon());

        Ok(Box::new(Stmt::create_break(self.generate_id(), pos)))
    }

    fn parse_continue(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Continue)).position;
        try!(self.expect_semicolon());

        Ok(Box::new(Stmt::create_continue(self.generate_id(), pos)))
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

        Ok(Box::new(Stmt::create_return(self.generate_id(), pos, expr)))
    }

    fn parse_expression_statement(&mut self) -> StmtResult {
        let pos = self.token.position;
        let expr = try!(self.parse_expression());
        try!(self.expect_semicolon());

        Ok(Box::new(Stmt::create_expr(self.generate_id(), pos, expr)))
    }

    fn parse_expression(&mut self) -> ExprResult {
        self.parse_expression_l0()
    }

    fn parse_expression_l0(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l1());

        while self.token.is(TokenType::Or) {
            let tok = try!(self.read_token());
            let op = BinOp::Or;

            let right = try!(self.parse_expression_l1());
            left = Box::new(Expr::create_bin(self.generate_id(),
                tok.position, op, left, right));
        }

        Ok(left)
    }

    fn parse_expression_l1(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l2());

        while self.token.is(TokenType::And) {
            let tok = try!(self.read_token());
            let op = BinOp::And;

            let right = try!(self.parse_expression_l2());
            left = Box::new(Expr::create_bin(self.generate_id(),
                tok.position, op, left, right));
        }

        Ok(left)
    }

    fn parse_expression_l2(&mut self) -> ExprResult {
        let left = try!(self.parse_expression_l3());

        if self.token.is(TokenType::Eq) {
            let tok = try!(self.read_token());
            let right = try!(self.parse_expression_l3());

            Ok(Box::new(Expr::create_assign(self.generate_id(), tok.position, left, right)))
        } else {
            Ok(left)
        }
    }

    fn parse_expression_l3(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l4());

        while self.token.is(TokenType::EqEq) || self.token.is(TokenType::Ne) {
            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::EqEq => CmpOp::Eq,
                _ => CmpOp::Ne
            };

            let right = try!(self.parse_expression_l4());
            left = Box::new(Expr::create_bin(self.generate_id(),
                tok.position, BinOp::Cmp(op), left, right));
        }

        Ok(left)
    }

    fn parse_expression_l4(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l5());

        while self.token.is(TokenType::Lt) || self.token.is(TokenType::Le) ||
                self.token.is(TokenType::Gt) || self.token.is(TokenType::Ge) {

            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::Lt => CmpOp::Lt,
                TokenType::Le => CmpOp::Le,
                TokenType::Gt => CmpOp::Gt,
                _ => CmpOp::Ge
            };

            let right = try!(self.parse_expression_l5());
            left = Box::new(Expr::create_bin(self.generate_id(), tok.position,
                BinOp::Cmp(op), left, right));
        }

        Ok(left)
    }

    fn parse_expression_l5(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l6());

        while self.token.is(TokenType::Is) || self.token.is(TokenType::IsNot) {

            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::Is => CmpOp::Is,
                _ => CmpOp::IsNot
            };

            let right = try!(self.parse_expression_l6());
            left = Box::new(Expr::create_bin(self.generate_id(), tok.position,
                BinOp::Cmp(op), left, right));
        }

        Ok(left)
    }

    fn parse_expression_l6(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l7());

        while self.token.is(TokenType::BitOr) || self.token.is(TokenType::BitAnd) ||
              self.token.is(TokenType::Caret) {
            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::BitOr => BinOp::BitOr,
                TokenType::BitAnd => BinOp::BitAnd,
                TokenType::Caret => BinOp::BitXor,
                _ => unreachable!()
            };

            let right = try!(self.parse_expression_l7());
            left = Box::new(Expr::create_bin(self.generate_id(), tok.position, op, left, right));
        }

        Ok(left)
    }

    fn parse_expression_l7(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l8());

        while self.token.is(TokenType::Add) || self.token.is(TokenType::Sub) {
            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::Add => BinOp::Add,
                _ => BinOp::Sub
            };

            let right = try!(self.parse_expression_l8());
            left = Box::new(Expr::create_bin(self.generate_id(), tok.position, op, left, right));
        }

        Ok(left)
    }

    fn parse_expression_l8(&mut self) -> ExprResult {
        let mut left = try!(self.parse_expression_l9());

        while self.token.is(TokenType::Mul) || self.token.is(TokenType::Div) ||
                self.token.is(TokenType::Mod) {
            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::Mul => BinOp::Mul,
                TokenType::Div => BinOp::Div,
                _ => BinOp::Mod
            };

            let right = try!(self.parse_expression_l9());
            left = Box::new(Expr::create_bin(self.generate_id(), tok.position, op, left, right));
        }

        Ok(left)
    }

    fn parse_expression_l9(&mut self) -> ExprResult {
        if self.token.is(TokenType::Add) || self.token.is(TokenType::Sub) ||
           self.token.is(TokenType::Not) || self.token.is(TokenType::Tilde) {
            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::Add => UnOp::Plus,
                TokenType::Sub => UnOp::Neg,
                TokenType::Not => UnOp::Not,
                TokenType::Tilde => UnOp::BitNot,
                _ => unreachable!()
            };

            let expr = try!(self.parse_expression_l10());
            Ok(Box::new(Expr::create_un(self.generate_id(), tok.position, op, expr)))
        } else {
            self.parse_expression_l10()
        }
    }

    fn parse_expression_l10(&mut self) -> ExprResult {
        let mut left = try!(self.parse_factor());

        while self.token.is(TokenType::Dot) {
            let tok = try!(self.read_token());
            let ident = try!(self.expect_identifier());

            left = if self.token.is(TokenType::LParen) {
                try!(self.parse_call(tok.position, Some(left), ident))

            } else {
                Box::new(Expr::create_prop(self.generate_id(), tok.position, left, ident))
            };
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> ExprResult {
        match self.token.token_type {
            TokenType::LParen => self.parse_parentheses(),
            TokenType::Number => self.parse_number(),
            TokenType::String => self.parse_string(),
            TokenType::Identifier => self.parse_identifier_or_call(),
            TokenType::True => self.parse_bool_literal(),
            TokenType::False => self.parse_bool_literal(),
            TokenType::This => self.parse_self(),
            _ => Err(ParseError {
                position: self.token.position,
                code: ErrorCode::UnknownFactor,
                message: format!("factor expected but got {}", self.token)
            })
        }
    }

    fn parse_identifier_or_call(&mut self) -> ExprResult {
        let pos = self.token.position;
        let ident = try!(self.expect_identifier());

        // is this a function call?
        if self.token.is(TokenType::LParen) {
            self.parse_call(pos, None, ident)

        // if not we have a simple variable
        } else {
            Ok(Box::new(Expr::create_ident(self.generate_id(), pos, ident)))
        }
    }

    fn parse_call(&mut self, pos: Position, object: Option<Box<Expr>>, ident: Name) -> ExprResult {
        try!(self.expect_token(TokenType::LParen));

        let mut args = try!(self.parse_comma_list(TokenType::RParen, |p| {
            p.parse_expression()
        }));

        let with_self = if let Some(this) = object {
            args.insert(0, this);

            true
        } else {
            false
        };

        Ok(Box::new(Expr::create_call(self.generate_id(), pos, ident, with_self, args)))
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
            Ok(num) => Ok(Box::new(Expr::create_lit_int(self.generate_id(), tok.position, num))),
            _ => Err(ParseError {
                position: tok.position,
                message: format!("number {} does not fit into range", tok),
                code: ErrorCode::NumberOverflow
            })
        }
    }

    fn parse_string(&mut self) -> ExprResult {
        let string = try!(self.read_token());

        Ok(Box::new(Expr::create_lit_str(self.generate_id(), string.position, string.value)))
    }

    fn parse_bool_literal(&mut self) -> ExprResult {
        let tok = try!(self.read_token());
        let value = tok.is(TokenType::True);

        Ok(Box::new(Expr::create_lit_bool(self.generate_id(), tok.position, value)))
    }

    fn parse_self(&mut self) -> ExprResult {
        let tok = try!(self.read_token());

        Ok(Box::new(Expr::create_this(self.generate_id(), tok.position)))
    }

    fn expect_identifier(&mut self) -> Result<Name, ParseError> {
        if self.token.token_type == TokenType::Identifier {
            let ident = try!(self.read_token());
            let interned = self.interner.intern(&ident.value);

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
    use ast::*;
    use interner::*;

    use error::ErrorCode;
    use lexer::position::Position;
    use parser::Parser;

    fn parse_expr(code: &'static str) -> (Box<Expr>, Interner) {
        let mut interner = Interner::new();
        let ast = {
            let mut parser = Parser::from_str(code, &mut interner);
            assert!(parser.init().is_ok(), true);

            parser.parse_expression().unwrap()
        };

        (ast, interner)
    }

    fn err_expr(code: &'static str, error_code: ErrorCode, line:u32, col:u32) {
        let err = {
            let mut interner = Interner::new();
            let mut parser = Parser::from_str(code, &mut interner);

            assert!(parser.init().is_ok(), true);
            parser.parse_expression().unwrap_err()
        };

        assert_eq!(error_code, err.code);
        assert_eq!(line, err.position.line);
        assert_eq!(col, err.position.column);
    }

    fn parse_stmt(code: &'static str) -> Box<Stmt> {
        let mut interner = Interner::new();
        let mut parser = Parser::from_str(code, &mut interner);
        assert!(parser.init().is_ok(), true);

        parser.parse_statement().unwrap()
    }

    fn err_stmt(code: &'static str, error_code: ErrorCode, line:u32, col:u32) {
        let err = {
            let mut interner = Interner::new();
            let mut parser = Parser::from_str(code, &mut interner);

            assert!(parser.init().is_ok(), true);
            parser.parse_statement().unwrap_err()
        };

        assert_eq!(error_code, err.code);
        assert_eq!(line, err.position.line);
        assert_eq!(col, err.position.column);
    }

    fn parse_type(code: &'static str) -> (Type, Interner) {
        let mut interner = Interner::new();
        let ty = {
            let mut parser = Parser::from_str(code, &mut interner);
            assert!(parser.init().is_ok(), true);

            parser.parse_type().unwrap()
        };

        (ty, interner)
    }

    fn parse(code: &'static str) -> (Ast, Interner) {
        let mut interner = Interner::new();
        let ast = Parser::from_str(code, &mut interner).parse().unwrap();

        (ast, interner)
    }

    #[test]
    fn parse_ident() {
        let (expr, interner) = parse_expr("a");

        let ident = expr.to_ident().unwrap();
        assert_eq!("a", *interner.str(ident.name));
    }

    #[test]
    fn parse_number() {
        let (expr, _) = parse_expr("10");

        let lit = expr.to_lit_int().unwrap();
        assert_eq!(10, lit.value);
    }

    #[test]
    fn parse_string() {
        let (expr, _) = parse_expr("\"abc\"");

        let lit = expr.to_lit_str().unwrap();
        assert_eq!("abc", &lit.value);
    }

    #[test]
    fn parse_true() {
        let (expr, _) = parse_expr("true");

        let lit = expr.to_lit_bool().unwrap();
        assert_eq!(true, lit.value);
    }

    #[test]
    fn parse_false() {
        let (expr, _) = parse_expr("true");

        let lit = expr.to_lit_bool().unwrap();
        assert_eq!(true, lit.value);
    }

    #[test]
    fn parse_prop() {
        use ast::dump::dump_expr;

        let (expr, interner) = parse_expr("obj.prop");
        let prop = expr.to_prop().unwrap();

        let ident = prop.object.to_ident().unwrap();
        assert_eq!("obj", *interner.str(ident.name));
        assert_eq!("prop", *interner.str(prop.name));
    }

    #[test]
    fn parse_prop_negated() {
        use ast::dump::dump_expr;

        let (expr, interner) = parse_expr("-obj.prop");
        assert!(expr.to_un().unwrap().opnd.is_prop());
    }

    #[test]
    fn parse_prop_non_ident() {
        err_expr("obj.12", ErrorCode::ExpectedIdentifier, 1, 5);
    }

    #[test]
    fn parse_self() {
        let (expr, _) = parse_expr("self");

        assert!(expr.is_this());
    }

    #[test]
    fn parse_neg() {
        let (expr, _) = parse_expr("-1");

        let un = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, un.op);

        assert!(un.opnd.is_lit_int());
    }

    #[test]
    fn parse_double_neg() {
        let (expr, _) = parse_expr("-(-3)");

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
        let (expr, _) = parse_expr("+2");

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
        let (expr, _) = parse_expr("+(+9)");

        let add1 = expr.to_un().unwrap();
        assert_eq!(UnOp::Plus, add1.op);

        let add2 = add1.opnd.to_un().unwrap();
        assert_eq!(UnOp::Plus, add2.op);
        assert!(add2.opnd.is_lit_int());
    }

    #[test]
    fn parse_mul() {
        let (expr, _) = parse_expr("6*3");

        let mul = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul.op);
        assert_eq!(6, mul.lhs.to_lit_int().unwrap().value);
        assert_eq!(3, mul.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_multiple_muls() {
        let (expr, _) = parse_expr("6*3*4");

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
        let (expr, _) = parse_expr("4/5");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Div, div.op);
        assert_eq!(4, div.lhs.to_lit_int().unwrap().value);
        assert_eq!(5, div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_mod() {
        let (expr, _) = parse_expr("2%15");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mod, div.op);
        assert_eq!(2, div.lhs.to_lit_int().unwrap().value);
        assert_eq!(15, div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add() {
        let (expr, _) = parse_expr("2+3");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Add, add.op);
        assert_eq!(2, add.lhs.to_lit_int().unwrap().value);
        assert_eq!(3, add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add_left_associativity() {
        let (expr, interner) = parse_expr("1+2+3");

        let add = expr.to_bin().unwrap();
        assert_eq!(3, add.rhs.to_lit_int().unwrap().value);

        let lhs = add.lhs.to_bin().unwrap();
        assert_eq!(1, lhs.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, lhs.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add_right_associativity_via_parens() {
        let (expr, interner) = parse_expr("1+(2+3)");

        let add = expr.to_bin().unwrap();
        assert_eq!(1, add.lhs.to_lit_int().unwrap().value);

        let rhs = add.rhs.to_bin().unwrap();
        assert_eq!(2, rhs.lhs.to_lit_int().unwrap().value);
        assert_eq!(3, rhs.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_sub() {
        let (expr, _) = parse_expr("1-2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Sub, add.op);
        assert_eq!(1, add.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_or() {
        let (expr, _) = parse_expr("1||2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Or, add.op);
        assert_eq!(1, add.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_and() {
        let (expr, _) = parse_expr("1&&2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::And, add.op);
        assert_eq!(1, add.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_or() {
        let (expr, _) = parse_expr("1|2");

        let or = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitOr, or.op);
        assert_eq!(1, or.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, or.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_and() {
        let (expr, _) = parse_expr("1&2");

        let and = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitAnd, and.op);
        assert_eq!(1, and.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, and.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_xor() {
        let (expr, _) = parse_expr("1^2");

        let xor = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitXor, xor.op);
        assert_eq!(1, xor.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, xor.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_lt() {
        let (expr, _) = parse_expr("1<2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Lt), cmp.op);
        assert_eq!(1, cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_le() {
        let (expr, _) = parse_expr("1<=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Le), cmp.op);
        assert_eq!(1, cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_gt() {
        let (expr, _) = parse_expr("1>2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Gt), cmp.op);
        assert_eq!(1, cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_ge() {
        let (expr, _) = parse_expr("1>=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Ge), cmp.op);
        assert_eq!(1, cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_eq() {
        let (expr, _) = parse_expr("1==2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Eq), cmp.op);
        assert_eq!(1, cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_ne() {
        let (expr, _) = parse_expr("1!=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Ne), cmp.op);
        assert_eq!(1, cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_is_not() {
        let (expr, _) = parse_expr("1!==2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::IsNot), cmp.op);
        assert_eq!(1, cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_is() {
        let (expr, _) = parse_expr("1===2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Is), cmp.op);
        assert_eq!(1, cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_assign() {
        let (expr, _) = parse_expr("a=4");

        let assign = expr.to_assign().unwrap();
        assert!(assign.lhs.is_ident());
        assert_eq!(4, assign.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_call_without_params() {
        let (expr, interner) = parse_expr("fname()");

        let call = expr.to_call().unwrap();
        assert_eq!("fname", *interner.str(call.name));
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_call_with_params() {
        let (expr, interner) = parse_expr("fname2(1,2,3)");

        let call = expr.to_call().unwrap();
        assert_eq!("fname2", *interner.str(call.name));
        assert_eq!(3, call.args.len());

        for i in 0..3 {
            let lit = call.args[i as usize].to_lit_int().unwrap();
            assert_eq!(i+1, lit.value);
        }
    }

    #[test]
    fn parse_function() {
        let (prog, interner) = parse("fn b() { }");
        let fct = prog.elements[0].to_function().unwrap();

        assert_eq!("b", *interner.str(fct.name));
        assert_eq!(0, fct.params.len());
        assert!(fct.return_type.is_none());
        assert_eq!(Position::new(1, 1), fct.pos);
    }

    #[test]
    fn parse_function_with_single_param() {
        let (p1, interner1) = parse("fn f(a:int) { }");
        let f1 = p1.elements[0].to_function().unwrap();

        let (p2, interner2) = parse("fn f(a:int,) { }");
        let f2 = p2.elements[0].to_function().unwrap();

        let p1 = &f1.params[0];
        let p2 = &f2.params[0];

        assert_eq!(NodeId(2), p1.id);
        assert_eq!(NodeId(2), p2.id);

        assert_eq!("a", *interner1.str(p1.name));
        assert_eq!("a", *interner1.str(p2.name));

        assert_eq!("int", *interner1.str(p1.data_type.to_basic().unwrap().name));
        assert_eq!("int", *interner1.str(p2.data_type.to_basic().unwrap().name));
    }

    #[test]
    fn parse_function_with_multiple_params() {
        let (p1, interner1) = parse("fn f(a:int, b:str) { }");
        let f1 = p1.elements[0].to_function().unwrap();

        let (p2, interner2) = parse("fn f(a:int, b:str,) { }");
        let f2 = p2.elements[0].to_function().unwrap();

        let p1a = &f1.params[0];
        let p1b = &f1.params[1];
        let p2a = &f2.params[0];
        let p2b = &f2.params[1];

        assert_eq!("a", *interner1.str(p1a.name));
        assert_eq!("a", *interner2.str(p2a.name));

        assert_eq!("b", *interner1.str(p1b.name));
        assert_eq!("b", *interner2.str(p2b.name));

        assert_eq!("int", *interner1.str(p1a.data_type.to_basic().unwrap().name));
        assert_eq!("int", *interner2.str(p2a.data_type.to_basic().unwrap().name));

        assert_eq!("str", *interner1.str(p1b.data_type.to_basic().unwrap().name));
        assert_eq!("str", *interner2.str(p2b.data_type.to_basic().unwrap().name));
    }

    #[test]
    fn parse_let_without_type() {
        let stmt = parse_stmt("let a = 1;");
        let var = stmt.to_let().unwrap();

        assert!(var.data_type.is_none());
        assert!(var.expr.as_ref().unwrap().is_lit_int());
    }

    #[test]
    fn parse_let_with_type() {
        let stmt = parse_stmt("let x : int = 1;");
        let var = stmt.to_let().unwrap();

        assert!(var.data_type.is_some());
        assert!(var.expr.as_ref().unwrap().is_lit_int());
    }

    #[test]
    fn parse_let_with_type_but_without_assignment() {
        let stmt = parse_stmt("let x : int;");
        let var = stmt.to_let().unwrap();

        assert!(var.data_type.is_some());
        assert!(var.expr.is_none());
    }

    #[test]
    fn parse_let_without_type_and_assignment() {
        let stmt = parse_stmt("let x;");
        let var = stmt.to_let().unwrap();

        assert!(var.data_type.is_none());
        assert!(var.expr.is_none());
        assert_eq!(false, var.mutable);
    }

    #[test]
    fn parse_let_mut() {
        let stmt = parse_stmt("let mut x;");
        let var = stmt.to_let().unwrap();

        assert_eq!(true, var.mutable);
    }

    #[test]
    fn parse_multiple_functions() {
        let (prog, interner) = parse("fn f() { } fn g() { }");

        let f = prog.elements[0].to_function().unwrap();
        assert_eq!("f", *interner.str(f.name));
        assert_eq!(false, f.method);
        assert_eq!(Position::new(1, 1), f.pos);

        let g = prog.elements[1].to_function().unwrap();
        assert_eq!("g", *interner.str(g.name));
        assert_eq!(false, g.method);
        assert_eq!(Position::new(1, 12), g.pos);
    }

    #[test]
    fn parse_expr_stmt() {
        let stmt = parse_stmt("1;");
        let expr = stmt.to_expr().unwrap();

        assert!(expr.expr.is_lit_int());
    }

    #[test]
    fn parse_expr_stmt_without_semicolon() {
        err_stmt("1", ErrorCode::UnexpectedToken, 1, 2);
    }

    #[test]
    fn parse_if() {
        let stmt = parse_stmt("if true { 2; } else { 3; }");
        let ifstmt = stmt.to_if().unwrap();

        assert!(ifstmt.cond.is_lit_bool());
        assert!(ifstmt.else_block.is_some());
    }

    #[test]
    fn parse_if_without_else() {
        let stmt = parse_stmt("if true { 2; }");
        let ifstmt = stmt.to_if().unwrap();

        assert!(ifstmt.cond.is_lit_bool());
        assert!(ifstmt.else_block.is_none());
    }

    #[test]
    fn parse_while() {
        let stmt = parse_stmt("while true { 2; }");
        let whilestmt = stmt.to_while().unwrap();

        assert!(whilestmt.cond.is_lit_bool());

        let block = whilestmt.block.to_block().unwrap();
        assert_eq!(1, block.stmts.len());
    }

    #[test]
    fn parse_loop() {
        let stmt = parse_stmt("loop { 1; }");
        let block = &stmt.to_loop().unwrap().block;

        assert_eq!(1, block.to_block().unwrap().stmts.len());
    }

    #[test]
    fn parse_empty_block() {
        let stmt = parse_stmt("{}");
        let block = stmt.to_block().unwrap();

        assert_eq!(0, block.stmts.len());
    }

    #[test]
    fn parse_block_with_one_stmt() {
        let stmt = parse_stmt("{ 1; }");
        let block = stmt.to_block().unwrap();

        assert_eq!(1, block.stmts.len());

        let expr = &block.stmts[0].to_expr().unwrap().expr;
        assert_eq!(1, expr.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_block_with_multiple_stmts() {
        let stmt = parse_stmt("{ 1; 2; }");
        let block = stmt.to_block().unwrap();

        assert_eq!(2, block.stmts.len());

        let expr = &block.stmts[0].to_expr().unwrap().expr;
        assert_eq!(1, expr.to_lit_int().unwrap().value);

        let expr = &block.stmts[1].to_expr().unwrap().expr;
        assert_eq!(2, expr.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_break() {
        let stmt = parse_stmt("break;");

        assert!(stmt.is_break());
    }

    #[test]
    fn parse_continue() {
        let stmt = parse_stmt("continue;");

        assert!(stmt.is_continue());
    }

    #[test]
    fn parse_return_value() {
        let stmt = parse_stmt("return 1;");
        let ret = stmt.to_return().unwrap();

        assert_eq!(1, ret.expr.as_ref().unwrap().to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_return() {
        let stmt = parse_stmt("return;");
        let ret = stmt.to_return().unwrap();

        assert!(ret.expr.is_none());
    }

    #[test]
    fn parse_else() {
        err_stmt("else", ErrorCode::MisplacedElse, 1, 1);
    }

    #[test]
    fn parse_type_basic() {
        let (ty, interner) = parse_type("bla");
        let basic = ty.to_basic().unwrap();

        assert_eq!("bla", *interner.str(basic.name));
    }

    #[test]
    fn parse_type_unit() {
        let (ty, _) = parse_type("()");
        let ty = ty.to_tuple().unwrap();

        assert!(ty.subtypes.is_empty());
    }

    #[test]
    fn parse_type_unit_with_one_type() {
        let (ty, interner) = parse_type("(c)");

        let subtypes = &ty.to_tuple().unwrap().subtypes;
        assert_eq!(1, subtypes.len());

        let ty = subtypes[0].to_basic().unwrap();
        assert_eq!("c", *interner.str(ty.name));
    }

    #[test]
    fn parse_type_unit_with_two_types() {
        let (ty, interner) = parse_type("(a, b)");

        let subtypes = &ty.to_tuple().unwrap().subtypes;
        assert_eq!(2, subtypes.len());

        let ty1 = subtypes[0].to_basic().unwrap();
        assert_eq!("a", *interner.str(ty1.name));

        let ty2 = subtypes[1].to_basic().unwrap();
        assert_eq!("b", *interner.str(ty2.name));
    }

    #[test]
    fn parse_method() {
        let (prog, interner) = parse("class Foo {
            fn zero() -> int { return 0; }
            fn id(a: Str) -> Str { return a; }
        }");

        let cls = prog.elements[0].to_class().unwrap();
        assert_eq!(0, cls.props.len());
        assert_eq!(2, cls.methods.len());

        let mtd1 = &cls.methods[0];
        assert_eq!("zero", *interner.str(mtd1.name));
        assert_eq!(0, mtd1.params.len());
        assert_eq!(true, mtd1.method);
        let rt1 = mtd1.return_type.as_ref().unwrap().to_basic().unwrap().name;
        assert_eq!("int", *interner.str(rt1));

        let mtd2 = &cls.methods[1];
        assert_eq!("id", *interner.str(mtd2.name));
        assert_eq!(1, mtd2.params.len());
        assert_eq!(true, mtd2.method);
        let rt2 = mtd2.return_type.as_ref().unwrap().to_basic().unwrap().name;
        assert_eq!("Str", *interner.str(rt2));
    }

    #[test]
    fn parse_class() {
        let (prog, interner) = parse("class Foo");
        let class = prog.elements[0].to_class().unwrap();

        assert_eq!(0, class.props.len());
        assert_eq!(Position::new(1, 1), class.pos);
        assert_eq!("Foo", *interner.str(class.name));

        let ctor = &class.ctor.as_ref().unwrap();
        assert_eq!(0, ctor.params.len());
    }

    #[test]
    fn parse_class_with_parens_but_no_params() {
        let (prog, interner) = parse("class Foo()");
        let class = prog.elements[0].to_class().unwrap();

        assert_eq!(0, class.props.len());
        assert_eq!(Position::new(1, 1), class.pos);
        assert_eq!("Foo", *interner.str(class.name));

        let ctor = &class.ctor.as_ref().unwrap();
        assert_eq!(0, ctor.params.len());
    }

    #[test]
    fn parse_class_with_param() {
        let (prog, interner) = parse("class Foo(a: int)");
        let class = prog.elements[0].to_class().unwrap();

        assert_eq!(1, class.props.len());

        let ctor = &class.ctor.as_ref().unwrap();
        assert_eq!(1, ctor.params.len());
    }

    #[test]
    fn parse_class_with_params() {
        let (prog, interner) = parse("class Foo(a: int, b: int)");
        let class = prog.elements[0].to_class().unwrap();

        assert_eq!(2, class.props.len());
    }

    #[test]
    fn parse_method_invocation() {
        let (expr, interner) = parse_expr("a.foo()");
        let call = expr.to_call().unwrap();
        assert_eq!(true, call.with_self);
        assert_eq!(1, call.args.len());

        let (expr, interner) = parse_expr("a.foo(1)");
        let call = expr.to_call().unwrap();
        assert_eq!(2, call.args.len());

        let (expr, interner) = parse_expr("a.foo(1,2)");
        let call = expr.to_call().unwrap();
        assert_eq!(3, call.args.len());
    }
}
