use std::cell::RefCell;
use std::mem;
use std::io::Error;

use ast::*;
use ast::Elem::*;
use ctxt::CtorType;
use error::msg::*;

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
    ast: &'a mut Ast,
    param_idx: u32,
    field_idx: u32,
    in_class: bool,

    next_id: NodeId,
}

#[cfg(test)]
impl<'a> Parser<'a, StrReader> {
    pub fn from_str(code: &'static str, ast: &'a mut Ast,
                    interner: &'a mut Interner) -> Parser<'a, StrReader> {
        Parser::new(Lexer::from_str(code), ast, interner)
    }
}

impl<'a> Parser<'a, FileReader> {
    pub fn from_file(filename: &str, ast: &'a mut Ast,
                     interner: &'a mut Interner) -> Result<Parser<'a, FileReader>,Error> {
        let reader = try!(Lexer::from_file(filename));

        Ok(Parser::new(reader, ast, interner))
    }
}

type ExprResult = Result<Box<Expr>, MsgWithPos>;
type StmtResult = Result<Box<Stmt>, MsgWithPos>;

impl<'a, T: CodeReader> Parser<'a, T> {
    pub fn new(lexer: Lexer<T>, ast: &'a mut Ast, interner: &'a mut Interner) -> Parser<'a, T> {
        let token = Token::new(TokenType::End, Position::new(1,1));
        let parser = Parser {
            lexer: lexer,
            token: token,
            interner: interner,
            param_idx: 0,
            field_idx: 0,
            in_class: false,
            ast: ast,
            next_id: NodeId(1),
        };

        parser
    }

    fn generate_id(&mut self) -> NodeId {
        self.ast.generate_id()
    }

    pub fn parse(&mut self) -> Result<(), MsgWithPos> {
        try!(self.init());
        let mut elements = vec![];

        while !self.token.is_eof() {
            let el = try!(self.parse_top_level_element());
            elements.push(el);
        }

        self.ast.files.push(File {
            path: self.lexer.filename().to_string(),
            elements: elements
        });

        Ok(())
    }

    fn init(&mut self) -> Result<(), MsgWithPos> {
        try!(self.read_token());

        Ok(())
    }

    fn parse_top_level_element(&mut self) -> Result<Elem, MsgWithPos> {
        let modifiers = try!(self.parse_modifiers());

        match self.token.token_type {
            TokenType::Fun => {
                try!(self.restrict_modifiers(&modifiers, &[Modifier::Internal]));
                let fct = try!(self.parse_function(&modifiers));
                Ok(ElemFunction(fct))
            }

            TokenType::Class => {
                try!(self.restrict_modifiers(&modifiers, &[Modifier::Open, Modifier::Internal]));
                let class = try!(self.parse_class(&modifiers));
                Ok(ElemClass(class))
            }

            _ => {
                let msg = Msg::ExpectedTopLevelElement(self.token.name());
                Err(MsgWithPos::new(self.token.position, msg))
            }
        }
    }

    fn parse_class(&mut self, modifiers: &Modifiers) -> Result<Class, MsgWithPos> {
        let has_open = modifiers.contains(Modifier::Open);
        let internal = modifiers.contains(Modifier::Internal);

        let pos = try!(self.expect_token(TokenType::Class)).position;
        let ident = try!(self.expect_identifier());

        let mut cls = Class {
            id: self.generate_id(),
            name: ident,
            pos: pos,
            has_open: has_open,
            internal: internal,
            primary_ctor: false,
            parent_class: None,
            ctors: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new()
        };

        self.in_class = true;
        let ctor_params = try!(self.parse_primary_ctor(&mut cls));

        cls.parent_class = if self.token.is(TokenType::Colon) {
            try!(self.read_token());

            let pos = self.token.position;
            let name = try!(self.expect_identifier());
            let params = try!(self.parse_parent_class_params());

            Some(ParentClass::new(name, pos, params))
        } else {
            None
        };

        try!(self.parse_class_body(&mut cls));

        // add initializers to all ctors only if no primary ctor was added
        if ctor_params.len() == 0 {
            self.add_field_initializers_to_ctors(&mut cls);
        }

        // do not generate ctors for internal classes
        // add ctor if either there are primary ctor params or no ctors exist yet
        if !cls.internal && (ctor_params.len() > 0 || cls.ctors.is_empty()) {
            let ctor = self.generate_primary_ctor(&mut cls, ctor_params);
            cls.ctors.push(ctor);
            cls.primary_ctor = true;
        }

        self.in_class = false;

        Ok(cls)
    }

    fn add_field_initializers_to_ctors(&mut self, cls: &mut Class) {
        for ctor in &mut cls.ctors {
            let mut inits = Vec::new();

            for field in &cls.fields {
                if let Some(ref expr) = field.expr {
                    let this = self.build_this();
                    let lhs = self.build_field(this, field.name);
                    let ass = self.build_assign(lhs, expr.clone());

                    inits.push(self.build_stmt_expr(ass));
                }
            }

            if inits.len() > 0 && ctor.block.is_some() {
                let block = mem::replace(&mut ctor.block, None);
                inits.push(block.unwrap());

                let block = self.build_block(inits);
                ctor.block = Some(block);
            }
        }
    }

    fn parse_parent_class_params(&mut self) -> Result<Vec<Box<Expr>>, MsgWithPos> {
        if !self.token.is(TokenType::LParen) {
            return Ok(Vec::new());
        }

        try!(self.expect_token(TokenType::LParen));

        let params = try!(self.parse_comma_list(TokenType::RParen, |p| {
            p.parse_expression()
        }));

        Ok(params)
    }

    fn parse_primary_ctor(&mut self, cls: &mut Class) -> Result<Vec<PrimaryCtorParam>, MsgWithPos> {
        if !self.token.is(TokenType::LParen) {
            return Ok(Vec::new());
        }

        try!(self.expect_token(TokenType::LParen));

        let params = try!(self.parse_comma_list(TokenType::RParen, |p| {
            p.parse_primary_ctor_param(cls)
        }));

        Ok(params)
    }

    fn parse_primary_ctor_param(&mut self, cls: &mut Class) -> Result<PrimaryCtorParam, MsgWithPos> {
        let field = self.token.is(TokenType::Var) || self.token.is(TokenType::Let);
        let reassignable = self.token.is(TokenType::Var);

        // consume var and let
        if field {
            try!(self.read_token());
        }

        let pos = self.token.position;
        let name = try!(self.expect_identifier());

        try!(self.expect_token(TokenType::Colon));
        let data_type = try!(self.parse_type());

        if field {
            cls.fields.push(Field {
                id: self.generate_id(),
                name: name,
                pos: pos,
                data_type: data_type.clone(),
                primary_ctor: true,
                expr: None,
                reassignable: reassignable,
            })
        }

        Ok(PrimaryCtorParam {
            name: name,
            pos: pos,
            data_type: data_type,
            field: field,
            reassignable: reassignable,
            ty: RefCell::new(None),
        })
    }

    fn parse_class_body(&mut self, cls: &mut Class) -> Result<(), MsgWithPos> {
        if !self.token.is(TokenType::LBrace) {
            return Ok(());
        }

        try!(self.read_token());

        while !self.token.is(TokenType::RBrace) {
            let modifiers = try!(self.parse_modifiers());

            match self.token.token_type {
                TokenType::Fun => {
                    let mods = &[Modifier::Internal, Modifier::Open,
                                 Modifier::Override, Modifier::Final];
                    try!(self.restrict_modifiers(&modifiers, mods));

                    let fct = try!(self.parse_function(&modifiers));
                    cls.methods.push(fct);
                }

                TokenType::Init => {
                    try!(self.ban_modifiers(&modifiers));

                    let ctor = try!(self.parse_ctor(cls));
                    cls.ctors.push(ctor);
                }

                TokenType::Var
                | TokenType::Let => {
                    try!(self.ban_modifiers(&modifiers));

                    let field = try!(self.parse_field());
                    cls.fields.push(field);
                }

                _ => {
                    return Err(MsgWithPos::new(self.token.position,
                                    Msg::ExpectedClassElement(self.token.name())))
                }
            }
        }

        try!(self.read_token());
        Ok(())
    }

    fn parse_modifiers(&mut self) -> Result<Modifiers, MsgWithPos> {
        let mut modifiers = Modifiers::new();

        loop {
            let modifier = match self.token.token_type {
                TokenType::Open => Modifier::Open,
                TokenType::Override => Modifier::Override,
                TokenType::Final => Modifier::Final,
                TokenType::Internal => Modifier::Internal,
                _ => { break; }
            };

            if modifiers.contains(modifier) {
                return Err(MsgWithPos::new(self.token.position,
                                Msg::RedundantModifier(self.token.name())));
            }

            let pos = try!(self.read_token()).position;
            modifiers.add(modifier, pos);
        }

        Ok(modifiers)
    }

    fn ban_modifiers(&mut self, modifiers: &Modifiers) -> Result<(), MsgWithPos> {
        self.restrict_modifiers(modifiers, &[])
    }

    fn restrict_modifiers(&mut self, modifiers: &Modifiers,
                                     restrict: &[Modifier]) -> Result<(), MsgWithPos> {
        for modifier in modifiers.iter() {
            if !restrict.contains(&modifier.value) {
                return Err(MsgWithPos::new(modifier.pos,
                                Msg::MisplacedModifier(modifier.value.name().into())));
            }
        }

        Ok(())
    }

    fn parse_ctor(&mut self, cls: &Class) -> Result<Function, MsgWithPos> {
        let pos = try!(self.expect_token(TokenType::Init)).position;
        let params = try!(self.parse_function_params());
        let delegation = try!(self.parse_delegation());
        let mut block = try!(self.parse_function_block());

        if let Some(delegation) = delegation {
            let expr = Expr::create_delegation(
                self.generate_id(),
                delegation.pos,
                delegation.ty,
                delegation.args
            );

            let stmt = self.build_stmt_expr(Box::new(expr));

            block = Some(self.build_block(
                vec![stmt, block.unwrap()]
            ));
        }

        Ok(Function {
            id: self.generate_id(),
            pos: pos,
            name: cls.name,
            method: true,
            has_open: false,
            has_override: false,
            has_final: false,
            internal: false,
            ctor: CtorType::Secondary,
            params: params,
            throws: false,
            return_type: None,
            block: block
        })
    }

    fn parse_delegation(&mut self) -> Result<Option<Delegation>, MsgWithPos> {
        if !self.token.is(TokenType::Colon) {
            return Ok(None);
        }

        try!(self.expect_token(TokenType::Colon));
        let pos = self.token.position;

        let ty = if self.token.is(TokenType::This) {
            DelegationType::This
        } else if self.token.is(TokenType::Super) {
            DelegationType::Super
        } else {
            let name = self.token.name();
            return Err(MsgWithPos::new(pos, Msg::ThisOrSuperExpected(name)));
        };

        try!(self.read_token());
        try!(self.expect_token(TokenType::LParen));

        let args = try!(self.parse_comma_list(TokenType::RParen, |p| {
            p.parse_expression()
        }));

        Ok(Some(Delegation {
            pos: pos,
            ty: ty,
            args: args
        }))
    }

    fn parse_field(&mut self) -> Result<Field, MsgWithPos> {
        let pos = self.token.position;
        let reassignable = if self.token.is(TokenType::Var) {
            try!(self.expect_token(TokenType::Var));

            true
        } else {
            try!(self.expect_token(TokenType::Let));

            false
        };

        let name = try!(self.expect_identifier());
        try!(self.expect_token(TokenType::Colon));
        let data_type = try!(self.parse_type());

        let expr = if self.token.is(TokenType::Eq) {
            try!(self.expect_token(TokenType::Eq));
            Some(try!(self.parse_expression()))

        } else {
            None
        };

        try!(self.expect_semicolon());

        Ok(Field {
            id: self.generate_id(),
            name: name,
            pos: pos,
            data_type: data_type,
            primary_ctor: false,
            expr: expr,
            reassignable: reassignable,
        })
    }

    fn parse_function(&mut self, modifiers: &Modifiers) -> Result<Function, MsgWithPos> {
        let pos = try!(self.expect_token(TokenType::Fun)).position;
        let ident = try!(self.expect_identifier());

        let params = try!(self.parse_function_params());
        let throws = try!(self.parse_throws());
        let return_type = try!(self.parse_function_type());
        let block = try!(self.parse_function_block());

        Ok(Function {
            id: self.generate_id(),
            name: ident,
            pos: pos,
            method: self.in_class,
            has_open: modifiers.contains(Modifier::Open),
            has_override: modifiers.contains(Modifier::Override),
            has_final: modifiers.contains(Modifier::Final),
            internal: modifiers.contains(Modifier::Internal),
            ctor: CtorType::None,
            params: params,
            throws: throws,
            return_type: return_type,
            block: block,
        })
    }

    fn parse_throws(&mut self) -> Result<bool, MsgWithPos> {
        if self.token.is(TokenType::Throws) {
            try!(self.read_token());

            return Ok(true);
        }

        Ok(false)
    }

    fn parse_function_params(&mut self) -> Result<Vec<Param>, MsgWithPos> {
        try!(self.expect_token(TokenType::LParen));
        self.param_idx = 0;

        let params = try!(self.parse_comma_list(TokenType::RParen, |p| {
            p.param_idx += 1;

            p.parse_function_param()
        }));

        Ok(params)
    }

    fn parse_comma_list<F, R>(&mut self, stop: TokenType, mut parse: F) -> Result<Vec<R>, MsgWithPos>
        where F: FnMut(&mut Parser<T>) -> Result<R, MsgWithPos> {
        let mut data = vec![];
        let mut comma = true;

        while !self.token.is(stop) && !self.token.is_eof() {
            if !comma {
                return Err(MsgWithPos::new(self.token.position,
                           Msg::ExpectedToken(TokenType::Comma.name().into(), self.token.name())));
            }

            let entry = try!(parse(self));
            data.push(entry);

            comma = self.token.is(TokenType::Comma);
            if comma { try!(self.read_token()); }
        }

        try!(self.expect_token(stop));

        Ok(data)
    }

    fn parse_function_param(&mut self) -> Result<Param, MsgWithPos> {
        let pos = self.token.position;

        let reassignable = if self.token.is(TokenType::Var) {
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
            reassignable: reassignable,
            name: name,
            pos: pos,
            data_type: data_type,
            info: RefCell::new(None),
        })
    }

    fn parse_function_type(&mut self) -> Result<Option<Type>, MsgWithPos> {
        if self.token.is(TokenType::Arrow) {
            try!(self.read_token());
            let ty = try!(self.parse_type());

            Ok(Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_function_block(&mut self) -> Result<Option<Box<Stmt>>, MsgWithPos> {
        if self.token.is(TokenType::Semicolon) {
            try!(self.read_token());

            Ok(None)
        } else {
            let block = try!(self.parse_block());

            Ok(Some(block))
        }
    }

    fn parse_type(&mut self) -> Result<Type, MsgWithPos> {
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

            _ => Err(MsgWithPos::new(self.token.position,
                     Msg::ExpectedType(self.token.name()))),
        }
    }

    fn parse_statement(&mut self) -> StmtResult {
        match self.token.token_type {
            TokenType::Let
                | TokenType::Var => self.parse_var(),
            TokenType::LBrace => self.parse_block(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Loop => self.parse_loop(),
            TokenType::Break => self.parse_break(),
            TokenType::Continue => self.parse_continue(),
            TokenType::Return => self.parse_return(),
            TokenType::Else => Err(MsgWithPos::new(self.token.position, Msg::MisplacedElse)),
            TokenType::Throw => self.parse_throw(),
            TokenType::Try => self.parse_try(),
            _ => self.parse_expression_statement()
        }
    }

    fn parse_throw(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Throw)).position;
        let expr = try!(self.parse_expression());

        try!(self.expect_semicolon());

        Ok(Box::new(Stmt::create_throw(self.generate_id(), pos, expr)))
    }

    fn parse_try(&mut self) -> StmtResult {
        let pos = try!(self.expect_token(TokenType::Try)).position;
        let try_block = try!(self.parse_block());
        let mut catch_blocks = Vec::new();

        while self.token.is(TokenType::Catch) {
            catch_blocks.push(try!(self.parse_catch()));
        }

        let finally_block = if self.token.is(TokenType::Finally) {
            Some(try!(self.parse_finally()))
        } else {
            None
        };

        Ok(Box::new(Stmt::create_try(self.generate_id(), pos,
            try_block, catch_blocks, finally_block)))
    }

    fn parse_catch(&mut self) -> Result<CatchBlock, MsgWithPos> {
        let pos = try!(self.expect_token(TokenType::Catch)).position;
        let name = try!(self.expect_identifier());
        try!(self.expect_token(TokenType::Colon));
        let data_type = try!(self.parse_type());
        let block = try!(self.parse_block());

        Ok(CatchBlock::new(name, pos, data_type, block))
    }

    fn parse_finally(&mut self) -> Result<FinallyBlock, MsgWithPos> {
        try!(self.expect_token(TokenType::Finally));
        let block = try!(self.parse_block());

        Ok(FinallyBlock::new(block))
    }

    fn parse_var(&mut self) -> StmtResult {
        let reassignable = if self.token.is(TokenType::Let) {
            false
        } else if self.token.is(TokenType::Var) {
            true
        } else {
            panic!("let or var expected")
        };

        let pos = try!(self.read_token()).position;
        let ident = try!(self.expect_identifier());
        let data_type = try!(self.parse_var_type());
        let expr = try!(self.parse_var_assignment());

        try!(self.expect_semicolon());

        Ok(Box::new(Stmt::create_var(self.generate_id(), pos, ident,
                                     reassignable, data_type, expr)))
    }

    fn parse_var_type(&mut self) -> Result<Option<Type>, MsgWithPos> {
        if self.token.is(TokenType::Colon) {
            try!(self.read_token());

            Ok(Some(try!(self.parse_type())))
        } else {
            Ok(None)
        }
    }

    fn parse_var_assignment(&mut self) -> Result<Option<Box<Expr>>, MsgWithPos> {
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

        while self.token.is(TokenType::EqEqEq) || self.token.is(TokenType::NeEqEq) {

            let tok = try!(self.read_token());
            let op = match tok.token_type {
                TokenType::EqEqEq => CmpOp::Is,
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
        let mut left = try!(self.parse_expression_l10());

        while self.token.is(TokenType::Is) || self.token.is(TokenType::As) {
            let is = self.token.is(TokenType::Is);

            let tok = try!(self.read_token());
            let right = Box::new(try!(self.parse_type()));
            let id = self.generate_id();

            let expr = Expr::create_conv(id, tok.position, left, right, is);
            left = Box::new(expr);
        }

        Ok(left)
    }

    fn parse_expression_l10(&mut self) -> ExprResult {
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

            let expr = try!(self.parse_expression_l11());
            Ok(Box::new(Expr::create_un(self.generate_id(), tok.position, op, expr)))
        } else {
            self.parse_expression_l11()
        }
    }

    fn parse_expression_l11(&mut self) -> ExprResult {
        let mut left = try!(self.parse_factor());

        while self.token.is(TokenType::Dot) || self.token.is(TokenType::LBracket) {
            left = if self.token.is(TokenType::Dot) {
                let tok = try!(self.read_token());
                let ident = try!(self.expect_identifier());

                if self.token.is(TokenType::LParen) {
                    try!(self.parse_call(tok.position, Some(left), ident))

                } else {
                    Box::new(Expr::create_field(self.generate_id(), tok.position, left, ident))
                }

            } else {
                let tok = try!(self.read_token());
                let index = try!(self.parse_expression());
                try!(self.expect_token(TokenType::RBracket));

                Box::new(Expr::create_array(self.generate_id(), tok.position, left, index))
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
            TokenType::Nil => self.parse_nil(),
            TokenType::This => self.parse_this(),
            TokenType::Super => self.parse_super(),
            _ => Err(MsgWithPos::new(self.token.position,
                     Msg::ExpectedFactor(self.token.name().clone())))
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

        let args = try!(self.parse_comma_list(TokenType::RParen, |p| {
            p.parse_expression()
        }));

        Ok(Box::new(Expr::create_call(self.generate_id(), pos, ident, object, args)))
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
            _ => Err(MsgWithPos::new(tok.position, Msg::NumberOverflow(tok.name().clone())))
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

    fn parse_this(&mut self) -> ExprResult {
        let tok = try!(self.read_token());

        Ok(Box::new(Expr::create_this(self.generate_id(), tok.position)))
    }

    fn parse_super(&mut self) -> ExprResult {
        let tok = try!(self.read_token());

        Ok(Box::new(Expr::create_super(self.generate_id(), tok.position)))
    }

    fn parse_nil(&mut self) -> ExprResult {
        let tok = try!(self.read_token());

        Ok(Box::new(Expr::create_nil(self.generate_id(), tok.position)))
    }

    fn expect_identifier(&mut self) -> Result<Name, MsgWithPos> {
        if self.token.token_type == TokenType::Identifier {
            let ident = try!(self.read_token());
            let interned = self.interner.intern(&ident.value);

            Ok(interned)
        } else {
            Err(MsgWithPos::new(self.token.position,
                Msg::ExpectedIdentifier(self.token.name())))
        }
    }

    fn expect_semicolon(&mut self) -> Result<Token,MsgWithPos> {
        self.expect_token(TokenType::Semicolon)
    }

    fn expect_token(&mut self, token_type: TokenType) -> Result<Token, MsgWithPos> {
        if self.token.token_type == token_type {
            let token = try!(self.read_token());

            Ok(token)
        } else {
            Err(MsgWithPos::new(self.token.position,
                Msg::ExpectedToken(token_type.name().into(), self.token.name())))
        }
    }

    fn read_token(&mut self) -> Result<Token, MsgWithPos> {
        let tok = try!(self.lexer.read_token());

        Ok(mem::replace(&mut self.token, tok))
    }

    fn generate_primary_ctor(&mut self, cls: &mut Class,
                             ctor_params: Vec<PrimaryCtorParam>) -> Function {
        let delegation = if let Some(ref parent_class) = cls.parent_class {
            let expr = Expr::create_delegation(
                self.generate_id(),
                parent_class.pos,
                DelegationType::Super,
                parent_class.params.clone()
            );

            vec![self.build_stmt_expr(Box::new(expr))]
        } else {
            Vec::new()
        };

        let param_assignments: Vec<Box<Stmt>> = ctor_params.iter()
                                                    .filter(|param| param.field)
                                                    .map(|param| {
            let this = self.build_this();
            let lhs = self.build_field(this, param.name);
            let rhs = self.build_ident(param.name);
            let ass = self.build_assign(lhs, rhs);

            self.build_stmt_expr(ass)
        }).collect();

        let field_assignments: Vec<Box<Stmt>> = cls.fields.iter()
                  .filter(|field| field.expr.is_some())
                  .map(|field| {
            let this = self.build_this();
            let lhs = self.build_field(this, field.name);
            let ass = self.build_assign(lhs, field.expr.as_ref().unwrap().clone());

            self.build_stmt_expr(ass)
        }).collect();

        let assignments = delegation.into_iter()
                            .chain(param_assignments.into_iter())
                            .chain(field_assignments.into_iter()).collect();

        let params = ctor_params.iter().enumerate().map(|(idx, field)| {
            self.build_param(idx as u32, field.name, field.data_type.clone())
        }).collect();

        Function {
            id: self.generate_id(),
            pos: Position::new(1, 1),
            name: cls.name,
            method: true,
            has_open: false,
            has_override: false,
            has_final: false,
            internal: false,
            ctor: CtorType::Primary,
            params: params,
            throws: false,
            return_type: None,
            block: Some(self.build_block(assignments))
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
            reassignable: false,
            pos: Position::new(1, 1),
            data_type: ty,
            info: RefCell::new(None),
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
            name: name,
            info: RefCell::new(None),
            ty: RefCell::new(None),
        }))
    }

    fn build_this(&mut self) -> Box<Expr> {
        let id = self.generate_id();

        Box::new(Expr::ExprSelf(ExprSelfType {
            id: id,
            pos: Position::new(1, 1),
            ty: RefCell::new(None),
        }))
    }

    fn build_assign(&mut self, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        let id = self.generate_id();

        Box::new(Expr::ExprAssign(ExprAssignType {
            id: id,
            pos: Position::new(1, 1),
            lhs: lhs,
            rhs: rhs,
            ty: RefCell::new(None),
        }))
    }

    fn build_field(&mut self, object: Box<Expr>, name: Name) -> Box<Expr> {
        let id = self.generate_id();

        Box::new(Expr::ExprField(ExprFieldType {
            id: id,
            pos: Position::new(1, 1),
            object: object,
            name: name,
            info: RefCell::new(None),
            ty: RefCell::new(None),
        }))
    }
}

#[derive(Clone, Debug)]
struct Delegation {
    pub pos: Position,
    pub ty: DelegationType,
    pub args: Vec<Box<Expr>>,
}

#[cfg(test)]
mod tests {
    use ast::*;
    use interner::*;

    use error::msg::{Msg, MsgWithPos};
    use lexer::position::Position;
    use parser::Parser;

    fn parse_expr(code: &'static str) -> (Box<Expr>, Interner) {
        let mut interner = Interner::new();
        let mut ast = Ast::new();

        let expr = {
            let mut parser = Parser::from_str(code, &mut ast, &mut interner);
            assert!(parser.init().is_ok(), true);

            parser.parse_expression().unwrap()
        };

        (expr, interner)
    }

    fn err_expr(code: &'static str, msg: Msg, line:u32, col:u32) {
        let err = {
            let mut interner = Interner::new();
            let mut ast = Ast::new();
            let mut parser = Parser::from_str(code, &mut ast, &mut interner);

            assert!(parser.init().is_ok(), true);
            parser.parse_expression().unwrap_err()
        };

        assert_eq!(msg, err.msg);
        assert_eq!(line, err.pos.line);
        assert_eq!(col, err.pos.column);
    }

    fn parse_stmt(code: &'static str) -> Box<Stmt> {
        let mut interner = Interner::new();
        let mut ast = Ast::new();
        let mut parser = Parser::from_str(code, &mut ast, &mut interner);
        assert!(parser.init().is_ok(), true);

        parser.parse_statement().unwrap()
    }

    fn err_stmt(code: &'static str, msg: Msg, line:u32, col:u32) {
        let err = {
            let mut interner = Interner::new();
            let mut ast = Ast::new();
            let mut parser = Parser::from_str(code, &mut ast, &mut interner);

            assert!(parser.init().is_ok(), true);
            parser.parse_statement().unwrap_err()
        };

        assert_eq!(msg, err.msg);
        assert_eq!(line, err.pos.line);
        assert_eq!(col, err.pos.column);
    }

    fn parse_type(code: &'static str) -> (Type, Interner) {
        let mut interner = Interner::new();
        let ty = {
            let mut ast = Ast::new();
            let mut parser = Parser::from_str(code, &mut ast, &mut interner);
            assert!(parser.init().is_ok(), true);

            parser.parse_type().unwrap()
        };

        (ty, interner)
    }

    fn parse(code: &'static str) -> (Ast, Interner) {
        let mut interner = Interner::new();
        let mut ast = Ast::new();

        Parser::from_str(code, &mut ast, &mut interner).parse().unwrap();

        (ast, interner)
    }

    fn parse_err(code: &'static str) -> MsgWithPos {
        let mut interner = Interner::new();
        let mut ast = Ast::new();

        Parser::from_str(code, &mut ast, &mut interner).parse().unwrap_err()
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
    fn parse_field_access() {
        let (expr, interner) = parse_expr("obj.field");
        let field = expr.to_field().unwrap();

        let ident = field.object.to_ident().unwrap();
        assert_eq!("obj", *interner.str(ident.name));
        assert_eq!("field", *interner.str(field.name));
    }

    #[test]
    fn parse_field_negated() {
        let (expr, _) = parse_expr("-obj.field");
        assert!(expr.to_un().unwrap().opnd.is_field());
    }

    #[test]
    fn parse_field_non_ident() {
        err_expr("obj.12", Msg::ExpectedIdentifier("12".into()), 1, 5);
    }

    #[test]
    fn parse_self() {
        let (expr, _) = parse_expr("self");

        assert!(expr.is_this());
    }

    #[test]
    fn parse_nil() {
        let (expr, _) = parse_expr("nil");

        assert!(expr.is_nil());
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
        err_expr("- -2", Msg::ExpectedFactor("-".into()), 1, 3);
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
        err_expr("+ +4", Msg::ExpectedFactor("+".into()), 1, 3);
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
        let (expr, _) = parse_expr("1+2+3");

        let add = expr.to_bin().unwrap();
        assert_eq!(3, add.rhs.to_lit_int().unwrap().value);

        let lhs = add.lhs.to_bin().unwrap();
        assert_eq!(1, lhs.lhs.to_lit_int().unwrap().value);
        assert_eq!(2, lhs.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add_right_associativity_via_parens() {
        let (expr, _) = parse_expr("1+(2+3)");

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
        let (prog, interner) = parse("fun b() { }");
        let fct = prog.fct0();

        assert_eq!("b", *interner.str(fct.name));
        assert_eq!(0, fct.params.len());
        assert!(fct.return_type.is_none());
        assert_eq!(Position::new(1, 1), fct.pos);
    }

    #[test]
    fn parse_function_with_single_param() {
        let (p1, interner1) = parse("fun f(a:int) { }");
        let f1 = p1.fct0();

        let (p2, interner2) = parse("fun f(a:int,) { }");
        let f2 = p2.fct0();

        let p1 = &f1.params[0];
        let p2 = &f2.params[0];

        assert_eq!(NodeId(2), p1.id);
        assert_eq!(NodeId(2), p2.id);

        assert_eq!("a", *interner1.str(p1.name));
        assert_eq!("a", *interner2.str(p2.name));

        assert_eq!("int", *interner1.str(p1.data_type.to_basic().unwrap().name));
        assert_eq!("int", *interner2.str(p2.data_type.to_basic().unwrap().name));
    }

    #[test]
    fn parse_function_with_multiple_params() {
        let (p1, interner1) = parse("fun f(a:int, b:str) { }");
        let f1 = p1.fct0();

        let (p2, interner2) = parse("fun f(a:int, b:str,) { }");
        let f2 = p2.fct0();

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
        let var = stmt.to_var().unwrap();

        assert_eq!(false, var.reassignable);
        assert!(var.data_type.is_none());
        assert!(var.expr.as_ref().unwrap().is_lit_int());
    }

    #[test]
    fn parse_var_without_type() {
        let stmt = parse_stmt("var a = 1;");
        let var = stmt.to_var().unwrap();

        assert_eq!(true, var.reassignable);
        assert!(var.data_type.is_none());
        assert!(var.expr.as_ref().unwrap().is_lit_int());
    }

    #[test]
    fn parse_let_with_type() {
        let stmt = parse_stmt("let x : int = 1;");
        let var = stmt.to_var().unwrap();

        assert_eq!(false, var.reassignable);
        assert!(var.data_type.is_some());
        assert!(var.expr.as_ref().unwrap().is_lit_int());
    }

    #[test]
    fn parse_var_with_type() {
        let stmt = parse_stmt("var x : int = 1;");
        let var = stmt.to_var().unwrap();

        assert_eq!(true, var.reassignable);
        assert!(var.data_type.is_some());
        assert!(var.expr.as_ref().unwrap().is_lit_int());
    }

    #[test]
    fn parse_let_with_type_but_without_assignment() {
        let stmt = parse_stmt("let x : int;");
        let var = stmt.to_var().unwrap();

        assert_eq!(false, var.reassignable);
        assert!(var.data_type.is_some());
        assert!(var.expr.is_none());
    }

    #[test]
    fn parse_var_with_type_but_without_assignment() {
        let stmt = parse_stmt("var x : int;");
        let var = stmt.to_var().unwrap();

        assert_eq!(true, var.reassignable);
        assert!(var.data_type.is_some());
        assert!(var.expr.is_none());
    }

    #[test]
    fn parse_let_without_type_and_assignment() {
        let stmt = parse_stmt("let x;");
        let var = stmt.to_var().unwrap();

        assert_eq!(false, var.reassignable);
        assert!(var.data_type.is_none());
        assert!(var.expr.is_none());
    }

    #[test]
    fn parse_var_without_type_and_assignment() {
        let stmt = parse_stmt("var x;");
        let var = stmt.to_var().unwrap();

        assert_eq!(true, var.reassignable);
        assert!(var.data_type.is_none());
        assert!(var.expr.is_none());
    }

    #[test]
    fn parse_multiple_functions() {
        let (prog, interner) = parse("fun f() { } fun g() { }");

        let f = prog.fct0();
        assert_eq!("f", *interner.str(f.name));
        assert_eq!(false, f.method);
        assert_eq!(Position::new(1, 1), f.pos);

        let g = prog.fct(1);
        assert_eq!("g", *interner.str(g.name));
        assert_eq!(false, g.method);
        assert_eq!(Position::new(1, 13), g.pos);
    }

    #[test]
    fn parse_expr_stmt() {
        let stmt = parse_stmt("1;");
        let expr = stmt.to_expr().unwrap();

        assert!(expr.expr.is_lit_int());
    }

    #[test]
    fn parse_expr_stmt_without_semicolon() {
        err_stmt("1", Msg::ExpectedToken(";".into(), "<<EOF>>".into()), 1, 2);
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
        err_stmt("else", Msg::MisplacedElse, 1, 1);
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
            fun zero() -> int { return 0; }
            fun id(a: Str) -> Str { return a; }
        }");

        let cls = prog.cls0();
        assert_eq!(0, cls.fields.len());
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
        let class = prog.cls0();

        assert_eq!(0, class.fields.len());
        assert_eq!(false, class.has_open);
        assert_eq!(Position::new(1, 1), class.pos);
        assert_eq!("Foo", *interner.str(class.name));
    }

    #[test]
    fn parse_class_with_parens_but_no_params() {
        let (prog, interner) = parse("open class Foo()");
        let class = prog.cls0();

        assert_eq!(0, class.fields.len());
        assert_eq!(true, class.has_open);
        assert_eq!(Position::new(1, 6), class.pos);
        assert_eq!("Foo", *interner.str(class.name));
    }

    #[test]
    fn parse_class_with_param() {
        let (prog, _) = parse("class Foo(a: int)");
        let class = prog.cls0();

        assert_eq!(0, class.fields.len());
        assert_eq!(1, class.ctors.len());
        assert_eq!(1, class.ctors[0].params.len());
        assert_eq!(false, class.ctors[0].params[0].reassignable);
    }

    #[test]
    fn parse_class_with_param_var() {
        let (prog, _) = parse("class Foo(var a: int)");
        let class = prog.cls0();

        assert_eq!(1, class.fields.len());
        assert_eq!(true, class.fields[0].reassignable);
        assert_eq!(1, class.ctors.len());
        assert_eq!(1, class.ctors[0].params.len());
    }

    #[test]
    fn parse_class_with_param_let() {
        let (prog, _) = parse("class Foo(let a: int)");
        let class = prog.cls0();

        assert_eq!(1, class.fields.len());
        assert_eq!(false, class.fields[0].reassignable);
        assert_eq!(1, class.ctors.len());
        assert_eq!(1, class.ctors[0].params.len());
        assert_eq!(false, class.ctors[0].params[0].reassignable);
    }

    #[test]
    fn parse_class_with_params() {
        let (prog, _) = parse("class Foo(a: int, b: int)");
        let class = prog.cls0();

        assert_eq!(0, class.fields.len());
        assert_eq!(2, class.ctors[0].params.len());
    }

    #[test]
    fn parse_class_with_parent_class() {
        let (prog, interner) = parse("class Foo : Bar");
        let class = prog.cls0();

        assert_eq!("Bar", interner.str(class.parent_class.as_ref().unwrap().name).to_string());
    }

    #[test]
    fn parse_class_with_open() {
        let (prog, _) = parse("open class Foo");
        let class = prog.cls0();

        assert_eq!(true, class.has_open);
    }

    #[test]
    fn parse_method_invocation() {
        let (expr, _) = parse_expr("a.foo()");
        let call = expr.to_call().unwrap();
        assert_eq!(true, call.object.is_some());
        assert_eq!(0, call.args.len());

        let (expr, _) = parse_expr("a.foo(1)");
        let call = expr.to_call().unwrap();
        assert_eq!(true, call.object.is_some());
        assert_eq!(1, call.args.len());

        let (expr, _) = parse_expr("a.foo(1,2)");
        let call = expr.to_call().unwrap();
        assert_eq!(true, call.object.is_some());
        assert_eq!(2, call.args.len());
    }

    #[test]
    fn parse_array_index() {
        let (expr, interner) = parse_expr("a[b]");
        let expr = expr.to_array().unwrap();
        assert_eq!("a", *interner.str(expr.object.to_ident().unwrap().name));
        assert_eq!("b", *interner.str(expr.index.to_ident().unwrap().name));
    }

    #[test]
    fn parse_function_without_throws() {
        let (prog, _) = parse("fun f(a: int) {}");
        let fct = prog.fct0();
        assert!(!fct.throws);
    }

    #[test]
    fn parse_function_throws() {
        let (prog, _) = parse("fun f(a: int) throws {}");
        let fct = prog.fct0();
        assert!(fct.throws);
    }

    #[test]
    fn parse_function_throws_with_return_type() {
        let (prog, _) = parse("fun f(a: int) throws -> int { return 0; }");
        let fct = prog.fct0();
        assert!(fct.throws);
    }

    #[test]
    fn parse_throw() {
        let stmt = parse_stmt("throw 1;");
        let throw = stmt.to_throw().unwrap();

        assert!(throw.expr.is_lit_int());
    }

    #[test]
    fn parse_try() {
        let stmt = parse_stmt("try { 1; } catch e: Str { 2; }
                                          catch e: IntArray { 3; } finally { 4; }");
        let try = stmt.to_try().unwrap();

        assert_eq!(2, try.catch_blocks.len());
        assert!(try.finally_block.is_some());
    }

    #[test]
    fn parse_try_without_catch() {
        let stmt = parse_stmt("try { 1; }");
        let try = stmt.to_try().unwrap();

        assert_eq!(0, try.catch_blocks.len());
        assert!(try.finally_block.is_none());
    }

    #[test]
    fn parse_field() {
        let (prog, interner) = parse("class A { var f1: int; let f2: int = 0; }");
        let cls = prog.cls0();

        let f1 = &cls.fields[0];
        assert_eq!("f1", &interner.str(f1.name).to_string());
        assert_eq!(true, f1.reassignable);

        let f2 = &cls.fields[1];
        assert_eq!("f2", &interner.str(f2.name).to_string());
        assert_eq!(false, f2.reassignable);
    }

    #[test]
    fn parse_open_method() {
        let (prog, _) = parse("class A { open fun f() {} fun g() {} }");
        let cls = prog.cls0();

        let m1 = &cls.methods[0];
        assert_eq!(true, m1.has_open);

        let m2 = &cls.methods[1];
        assert_eq!(false, m2.has_open);
    }

    #[test]
    fn parse_override_method() {
        let (prog, _) = parse("class A { fun f() {}
                                                override fun g() {}
                                                open fun h() {} }");
        let cls = prog.cls0();

        let m1 = &cls.methods[0];
        assert_eq!(false, m1.has_override);
        assert_eq!(false, m1.has_open);

        let m2 = &cls.methods[1];
        assert_eq!(true, m2.has_override);
        assert_eq!(false, m2.has_open);

        let m3 = &cls.methods[2];
        assert_eq!(false, m3.has_override);
        assert_eq!(true, m3.has_open);
    }

    #[test]
    fn parse_parent_class_params() {
        let (prog, _) = parse("class A: B(1, 2)");
        let cls = prog.cls0();

        let parent_class = cls.parent_class.as_ref().unwrap();
        assert_eq!(2, parent_class.params.len());
    }

    #[test]
    fn parse_final_method() {
        let (prog, _) = parse("open class A { final override fun g() {} }");
        let cls = prog.cls0();

        let m1 = &cls.methods[0];
        assert_eq!(true, m1.has_override);
        assert_eq!(false, m1.has_open);
        assert_eq!(true, m1.has_final);
    }

    #[test]
    fn parse_is_expr() {
        let (expr, _) = parse_expr("a is Str");
        let expr = expr.to_conv().unwrap();
        assert_eq!(true, expr.object.is_ident());
        assert_eq!(true, expr.is);
    }

    #[test]
    fn parse_as_expr() {
        let (expr, _) = parse_expr("a as Str");
        let expr = expr.to_conv().unwrap();
        assert_eq!(true, expr.object.is_ident());
        assert_eq!(false, expr.is);
    }

    #[test]
    fn parse_internal() {
        let (prog, _) = parse("internal fun foo();");
        let fct = prog.fct0();
        assert!(fct.internal);
    }

    #[test]
    fn parse_function_without_body() {
        let (prog, _) = parse("fun foo();");
        let fct = prog.fct0();
        assert!(fct.block.is_none());
    }

    #[test]
    fn parse_internal_class() {
        let (prog, _) = parse("internal class Foo {}");
        let cls = prog.cls0();
        assert!(cls.internal);
    }

    #[test]
    fn parse_ctor() {
        let (prog, _) = parse("class X { init() {} }");
        let cls = prog.cls0();
        assert_eq!(1, cls.ctors.len());
        assert_eq!(0, cls.ctors[0].params.len());
    }

    #[test]
    fn parse_ctor_with_params() {
        let (prog, _) = parse("class X { init(a: int, b: int) {} }");
        let cls = prog.cls0();
        assert_eq!(1, cls.ctors.len());
        assert_eq!(2, cls.ctors[0].params.len());
    }

    #[test]
    fn parse_ctor_with_this_delegation() {
        let (prog, _) = parse("class X(a: int) { init(a: int, b: int): self(a) {} }");
        let cls = prog.cls0();
        assert_eq!(2, cls.ctors.len());

        let block = cls.ctors[0].block.as_ref().unwrap().to_block().unwrap();
        let delegation = block.stmts[0].to_expr().unwrap().expr.to_delegation().unwrap();

        assert_eq!(DelegationType::This, delegation.ty);
        assert_eq!(1, delegation.args.len());
    }

    #[test]
    fn parse_ctor_with_super_delegation() {
        let (prog, _) = parse("class Y(a: int)
                               class X: Y { init(a: int, b: int): super(a) {} }");
        let cls = prog.cls(1);
        assert_eq!(1, cls.ctors.len());

        let block = cls.ctors[0].block.as_ref().unwrap().to_block().unwrap();
        let delegation = block.stmts[0].to_expr().unwrap().expr.to_delegation().unwrap();

        assert_eq!(DelegationType::Super, delegation.ty);
        assert_eq!(1, delegation.args.len());
    }
}
