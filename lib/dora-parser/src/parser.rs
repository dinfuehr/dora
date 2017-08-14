use std::cell::RefCell;
use std::mem;

use ast::*;
use ast::Elem::*;
use builder::Builder;
use error::msg::*;

use interner::*;

use lexer::*;
use lexer::token::*;
use lexer::position::Position;
use lexer::reader::Reader;

pub struct Parser<'a> {
    lexer: Lexer,
    token: Token,
    id_generator: &'a NodeIdGenerator,
    interner: &'a mut Interner,
    ast: &'a mut Ast,
    param_idx: u32,
    field_idx: u32,
    in_class: bool,

    next_id: NodeId,
}

type ExprResult = Result<Box<Expr>, MsgWithPos>;
type StmtResult = Result<Box<Stmt>, MsgWithPos>;

impl<'a> Parser<'a> {
    pub fn new(reader: Reader,
               id_generator: &'a NodeIdGenerator,
               ast: &'a mut Ast,
               interner: &'a mut Interner)
               -> Parser<'a> {
        let token = Token::new(TokenKind::End, Position::new(1, 1));
        let lexer = Lexer::new(reader);

        let parser = Parser {
            lexer: lexer,
            token: token,
            id_generator: id_generator,
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
        self.id_generator.next()
    }

    pub fn parse(&mut self) -> Result<(), MsgWithPos> {
        self.init()?;
        let mut elements = vec![];

        while !self.token.is_eof() {
            self.parse_top_level_element(&mut elements)?;
        }

        self.ast
            .files
            .push(File {
                      path: self.lexer.filename().to_string(),
                      elements: elements,
                  });

        Ok(())
    }

    fn init(&mut self) -> Result<(), MsgWithPos> {
        self.advance_token()?;

        Ok(())
    }

    fn parse_top_level_element(&mut self, elements: &mut Vec<Elem>) -> Result<(), MsgWithPos> {
        let modifiers = self.parse_modifiers()?;

        match self.token.kind {
            TokenKind::Fun => {
                self.restrict_modifiers(&modifiers, &[Modifier::Internal])?;
                let fct = self.parse_function(&modifiers)?;
                elements.push(ElemFunction(fct));
            }

            TokenKind::Class => {
                self.restrict_modifiers(&modifiers,
                                        &[Modifier::Abstract, Modifier::Open, Modifier::Internal])?;
                let class = self.parse_class(&modifiers)?;
                elements.push(ElemClass(class));
            }

            TokenKind::Struct => {
                self.ban_modifiers(&modifiers)?;
                let struc = self.parse_struct()?;
                elements.push(ElemStruct(struc))
            }

            TokenKind::Trait => {
                self.ban_modifiers(&modifiers)?;
                let xtrait = self.parse_trait()?;
                elements.push(ElemTrait(xtrait));
            }

            TokenKind::Impl => {
                self.ban_modifiers(&modifiers)?;
                let ximpl = self.parse_impl()?;
                elements.push(ElemImpl(ximpl));
            }

            TokenKind::Let | TokenKind::Var => {
                self.ban_modifiers(&modifiers)?;
                self.parse_global(elements)?;
            }

            TokenKind::Const => {
                self.ban_modifiers(&modifiers)?;
                let xconst = self.parse_const()?;
                elements.push(ElemConst(xconst));
            }

            _ => {
                let msg = Msg::ExpectedTopLevelElement(self.token.name());
                return Err(MsgWithPos::new(self.token.position, msg));
            }
        }

        Ok(())
    }

    fn parse_const(&mut self) -> Result<Const, MsgWithPos> {
        let pos = self.expect_token(TokenKind::Const)?.position;
        let name = self.expect_identifier()?;
        self.expect_token(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        self.expect_token(TokenKind::Eq)?;
        let expr = self.parse_expression()?;
        self.expect_semicolon()?;

        Ok(Const {
               id: self.generate_id(),
               pos: pos,
               name: name,
               data_type: ty,
               expr: expr,
           })
    }

    fn parse_impl(&mut self) -> Result<Impl, MsgWithPos> {
        let pos = self.expect_token(TokenKind::Impl)?.position;
        let trait_name = self.expect_identifier()?;
        self.expect_token(TokenKind::For)?;
        let class_name = self.expect_identifier()?;

        self.expect_token(TokenKind::LBrace)?;

        let mut methods = Vec::new();

        while !self.token.is(TokenKind::RBrace) {
            let modifiers = self.parse_modifiers()?;
            let mods = &[Modifier::Static, Modifier::Internal];
            self.restrict_modifiers(&modifiers, mods)?;

            methods.push(self.parse_function(&modifiers)?);
        }

        self.expect_token(TokenKind::RBrace)?;

        Ok(Impl {
               id: self.generate_id(),
               trait_name: trait_name,
               class_name: class_name,
               pos: pos,
               methods: methods,
           })
    }

    fn parse_global(&mut self, elements: &mut Vec<Elem>) -> Result<(), MsgWithPos> {
        let pos = self.token.position;
        let reassignable = self.token.is(TokenKind::Var);

        self.advance_token()?;
        let name = self.expect_identifier()?;

        self.expect_token(TokenKind::Colon)?;
        let data_type = self.parse_type()?;

        let expr = if self.token.is(TokenKind::Eq) {
            self.advance_token()?;

            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect_semicolon()?;

        let builder = Builder::new(self.id_generator);

        let global = Global {
            id: self.generate_id(),
            name: name,
            pos: pos,
            data_type: data_type,
            reassignable: reassignable,
            expr: expr,
        };

        elements.push(ElemGlobal(global));

        Ok(())
    }

    fn parse_trait(&mut self) -> Result<Trait, MsgWithPos> {
        let pos = self.expect_token(TokenKind::Trait)?.position;
        let ident = self.expect_identifier()?;

        self.expect_token(TokenKind::LBrace)?;

        let mut methods = Vec::new();

        while !self.token.is(TokenKind::RBrace) {
            let modifiers = self.parse_modifiers()?;
            let mods = &[Modifier::Static];
            self.restrict_modifiers(&modifiers, mods)?;

            methods.push(self.parse_function(&modifiers)?);
        }

        self.expect_token(TokenKind::RBrace)?;

        Ok(Trait {
               id: self.generate_id(),
               name: ident,
               pos: pos,
               methods: methods,
           })
    }

    fn parse_struct(&mut self) -> Result<Struct, MsgWithPos> {
        let pos = self.expect_token(TokenKind::Struct)?.position;
        let ident = self.expect_identifier()?;

        self.expect_token(TokenKind::LBrace)?;
        let fields = self.parse_comma_list(TokenKind::RBrace, |p| p.parse_struct_field())?;

        Ok(Struct {
               id: self.generate_id(),
               name: ident,
               pos: pos,
               fields: fields,
           })
    }

    fn parse_struct_field(&mut self) -> Result<StructField, MsgWithPos> {
        let pos = self.token.position;
        let ident = self.expect_identifier()?;

        self.expect_token(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        Ok(StructField {
               id: self.generate_id(),
               name: ident,
               pos: pos,
               data_type: ty,
           })
    }

    fn parse_class(&mut self, modifiers: &Modifiers) -> Result<Class, MsgWithPos> {
        let has_open = modifiers.contains(Modifier::Open);
        let internal = modifiers.contains(Modifier::Internal);
        let is_abstract = modifiers.contains(Modifier::Abstract);

        let pos = self.expect_token(TokenKind::Class)?.position;
        let ident = self.expect_identifier()?;
        let type_params = self.parse_type_params()?;

        let mut cls = Class {
            id: self.generate_id(),
            name: ident,
            pos: pos,
            has_open: has_open,
            internal: internal,
            is_abstract: is_abstract,
            primary_ctor: false,
            parent_class: None,
            ctors: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            type_params: type_params,
        };

        self.in_class = true;
        let ctor_params = self.parse_primary_ctor(&mut cls)?;

        cls.parent_class = if self.token.is(TokenKind::Colon) {
            self.advance_token()?;

            let pos = self.token.position;
            let name = self.expect_identifier()?;
            let params = self.parse_parent_class_params()?;

            Some(ParentClass::new(name, pos, params))
        } else {
            None
        };

        self.parse_class_body(&mut cls)?;

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

    fn parse_type_params(&mut self) -> Result<Option<Vec<TypeParam>>, MsgWithPos> {
        if self.token.is(TokenKind::Lt) {
            self.advance_token()?;
            let params = self.parse_comma_list(TokenKind::Gt, |p| p.parse_type_param())?;

            Ok(Some(params))
        } else {
            Ok(None)
        }
    }

    fn parse_type_param(&mut self) -> Result<TypeParam, MsgWithPos> {
        let pos = self.token.position;
        let name = self.expect_identifier()?;

        let bounds = if self.token.is(TokenKind::Colon) {
            self.advance_token()?;

            let mut bounds = Vec::new();

            loop {
                bounds.push(self.parse_type()?);

                if self.token.is(TokenKind::Add) {
                    self.advance_token()?;
                } else {
                    break;
                }
            }

            bounds
        } else {
            Vec::new()
        };

        Ok(TypeParam {
               name: name,
               pos: pos,
               bounds: bounds,
           })
    }

    fn add_field_initializers_to_ctors(&mut self, cls: &mut Class) {
        let builder = Builder::new(self.id_generator);

        for ctor in &mut cls.ctors {
            let mut block = builder.build_block();

            for field in &cls.fields {
                if let Some(ref expr) = field.expr {
                    let this = builder.build_this();
                    let lhs = builder.build_field(this, field.name);
                    let ass = builder.build_assign(lhs, expr.clone());

                    block.add_expr(ass);
                }
            }

            if block.len() > 0 && ctor.block.is_some() {
                let old_block = mem::replace(&mut ctor.block, None);
                block.add_stmt(old_block.unwrap());
                ctor.block = Some(block.build());
            }
        }
    }

    fn parse_parent_class_params(&mut self) -> Result<Vec<Box<Expr>>, MsgWithPos> {
        if !self.token.is(TokenKind::LParen) {
            return Ok(Vec::new());
        }

        self.expect_token(TokenKind::LParen)?;

        let params = self.parse_comma_list(TokenKind::RParen, |p| p.parse_expression())?;

        Ok(params)
    }

    fn parse_primary_ctor(&mut self, cls: &mut Class) -> Result<Vec<PrimaryCtorParam>, MsgWithPos> {
        if !self.token.is(TokenKind::LParen) {
            return Ok(Vec::new());
        }

        self.expect_token(TokenKind::LParen)?;

        let params = self.parse_comma_list(TokenKind::RParen, |p| p.parse_primary_ctor_param(cls))?;

        Ok(params)
    }

    fn parse_primary_ctor_param(&mut self,
                                cls: &mut Class)
                                -> Result<PrimaryCtorParam, MsgWithPos> {
        let field = self.token.is(TokenKind::Var) || self.token.is(TokenKind::Let);
        let reassignable = self.token.is(TokenKind::Var);

        // consume var and let
        if field {
            self.advance_token()?;
        }

        let pos = self.token.position;
        let name = self.expect_identifier()?;

        self.expect_token(TokenKind::Colon)?;
        let data_type = self.parse_type()?;

        if field {
            cls.fields
                .push(Field {
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
           })
    }

    fn parse_class_body(&mut self, cls: &mut Class) -> Result<(), MsgWithPos> {
        if !self.token.is(TokenKind::LBrace) {
            return Ok(());
        }

        self.advance_token()?;

        while !self.token.is(TokenKind::RBrace) {
            let modifiers = self.parse_modifiers()?;

            match self.token.kind {
                TokenKind::Fun => {
                    let mods = &[Modifier::Abstract,
                                 Modifier::Internal,
                                 Modifier::Open,
                                 Modifier::Override,
                                 Modifier::Final,
                                 Modifier::Pub,
                                 Modifier::Static];
                    self.restrict_modifiers(&modifiers, mods)?;

                    let fct = self.parse_function(&modifiers)?;
                    cls.methods.push(fct);
                }

                TokenKind::Init => {
                    let mods = &[Modifier::Internal];
                    self.restrict_modifiers(&modifiers, mods)?;

                    let ctor = self.parse_ctor(cls, &modifiers)?;
                    cls.ctors.push(ctor);
                }

                TokenKind::Var | TokenKind::Let => {
                    self.ban_modifiers(&modifiers)?;

                    let field = self.parse_field()?;
                    cls.fields.push(field);
                }

                _ => {
                    return Err(MsgWithPos::new(self.token.position,
                                               Msg::ExpectedClassElement(self.token.name())))
                }
            }
        }

        self.advance_token()?;
        Ok(())
    }

    fn parse_modifiers(&mut self) -> Result<Modifiers, MsgWithPos> {
        let mut modifiers = Modifiers::new();

        loop {
            let modifier = match self.token.kind {
                TokenKind::Abstract => Modifier::Abstract,
                TokenKind::Open => Modifier::Open,
                TokenKind::Override => Modifier::Override,
                TokenKind::Final => Modifier::Final,
                TokenKind::Internal => Modifier::Internal,
                TokenKind::Pub => Modifier::Pub,
                TokenKind::Static => Modifier::Static,
                _ => {
                    break;
                }
            };

            if modifiers.contains(modifier) {
                return Err(MsgWithPos::new(self.token.position,
                                           Msg::RedundantModifier(self.token.name())));
            }

            let pos = self.advance_token()?.position;
            modifiers.add(modifier, pos);
        }

        Ok(modifiers)
    }

    fn ban_modifiers(&mut self, modifiers: &Modifiers) -> Result<(), MsgWithPos> {
        self.restrict_modifiers(modifiers, &[])
    }

    fn restrict_modifiers(&mut self,
                          modifiers: &Modifiers,
                          restrict: &[Modifier])
                          -> Result<(), MsgWithPos> {
        for modifier in modifiers.iter() {
            if !restrict.contains(&modifier.value) {
                return Err(MsgWithPos::new(modifier.pos,
                                           Msg::MisplacedModifier(modifier.value.name().into())));
            }
        }

        Ok(())
    }

    fn parse_ctor(&mut self, cls: &Class, modifiers: &Modifiers) -> Result<Function, MsgWithPos> {
        let pos = self.expect_token(TokenKind::Init)?.position;
        let params = self.parse_function_params()?;
        let delegation = self.parse_delegation()?;
        let mut block = self.parse_function_block()?;
        let builder = Builder::new(self.id_generator);

        if let Some(delegation) = delegation {
            let expr = Expr::create_delegation(self.generate_id(),
                                               delegation.pos,
                                               delegation.ty,
                                               delegation.args);

            let mut bblock = builder.build_block();
            bblock.add_expr(Box::new(expr));
            bblock.add_stmt(block.unwrap());
            block = Some(bblock.build());
        }

        Ok(Function {
               id: self.generate_id(),
               pos: pos,
               name: cls.name,
               method: true,
               has_open: false,
               has_override: false,
               has_final: false,
               is_pub: true,
               is_static: false,
               is_abstract: false,
               internal: modifiers.contains(Modifier::Internal),
               ctor: CtorType::Secondary,
               params: params,
               throws: false,
               return_type: None,
               block: block,
               type_params: None,
           })
    }

    fn parse_delegation(&mut self) -> Result<Option<Delegation>, MsgWithPos> {
        if !self.token.is(TokenKind::Colon) {
            return Ok(None);
        }

        self.expect_token(TokenKind::Colon)?;
        let pos = self.token.position;

        let ty = if self.token.is(TokenKind::This) {
            DelegationType::This
        } else if self.token.is(TokenKind::Super) {
            DelegationType::Super
        } else {
            let name = self.token.name();
            return Err(MsgWithPos::new(pos, Msg::ThisOrSuperExpected(name)));
        };

        self.advance_token()?;
        self.expect_token(TokenKind::LParen)?;

        let args = self.parse_comma_list(TokenKind::RParen, |p| p.parse_expression())?;

        Ok(Some(Delegation {
                    pos: pos,
                    ty: ty,
                    args: args,
                }))
    }

    fn parse_field(&mut self) -> Result<Field, MsgWithPos> {
        let pos = self.token.position;
        let reassignable = if self.token.is(TokenKind::Var) {
            self.expect_token(TokenKind::Var)?;

            true
        } else {
            self.expect_token(TokenKind::Let)?;

            false
        };

        let name = self.expect_identifier()?;
        self.expect_token(TokenKind::Colon)?;
        let data_type = self.parse_type()?;

        let expr = if self.token.is(TokenKind::Eq) {
            self.expect_token(TokenKind::Eq)?;
            Some(self.parse_expression()?)

        } else {
            None
        };

        self.expect_semicolon()?;

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
        let pos = self.expect_token(TokenKind::Fun)?.position;
        let ident = self.expect_identifier()?;

        let type_params = self.parse_type_params()?;
        let params = self.parse_function_params()?;
        let throws = self.parse_throws()?;
        let return_type = self.parse_function_type()?;
        let block = self.parse_function_block()?;

        Ok(Function {
               id: self.generate_id(),
               name: ident,
               pos: pos,
               method: self.in_class,
               has_open: modifiers.contains(Modifier::Open),
               has_override: modifiers.contains(Modifier::Override),
               has_final: modifiers.contains(Modifier::Final),
               is_pub: modifiers.contains(Modifier::Pub),
               is_static: modifiers.contains(Modifier::Static),
               internal: modifiers.contains(Modifier::Internal),
               is_abstract: modifiers.contains(Modifier::Abstract),
               ctor: CtorType::None,
               params: params,
               throws: throws,
               return_type: return_type,
               block: block,
               type_params: type_params,
           })
    }

    fn parse_throws(&mut self) -> Result<bool, MsgWithPos> {
        if self.token.is(TokenKind::Throws) {
            self.advance_token()?;

            return Ok(true);
        }

        Ok(false)
    }

    fn parse_function_params(&mut self) -> Result<Vec<Param>, MsgWithPos> {
        self.expect_token(TokenKind::LParen)?;
        self.param_idx = 0;

        let params = self.parse_comma_list(TokenKind::RParen, |p| {
                p.param_idx += 1;

                p.parse_function_param()
            })?;

        Ok(params)
    }

    fn parse_comma_list<F, R>(&mut self,
                              stop: TokenKind,
                              mut parse: F)
                              -> Result<Vec<R>, MsgWithPos>
        where F: FnMut(&mut Parser) -> Result<R, MsgWithPos>
    {
        let mut data = vec![];
        let mut comma = true;

        while !self.token.is(stop.clone()) && !self.token.is_eof() {
            if !comma {
                return Err(MsgWithPos::new(self.token.position,
                                           Msg::ExpectedToken(TokenKind::Comma.name().into(),
                                                              self.token.name())));
            }

            let entry = parse(self)?;
            data.push(entry);

            comma = self.token.is(TokenKind::Comma);
            if comma {
                self.advance_token()?;
            }
        }

        self.expect_token(stop)?;

        Ok(data)
    }

    fn parse_function_param(&mut self) -> Result<Param, MsgWithPos> {
        let pos = self.token.position;

        let reassignable = if self.token.is(TokenKind::Var) {
            self.advance_token()?;

            true
        } else {
            false
        };

        let name = self.expect_identifier()?;

        self.expect_token(TokenKind::Colon)?;
        let data_type = self.parse_type()?;

        Ok(Param {
               id: self.generate_id(),
               idx: self.param_idx - 1,
               reassignable: reassignable,
               name: name,
               pos: pos,
               data_type: data_type,
           })
    }

    fn parse_function_type(&mut self) -> Result<Option<Type>, MsgWithPos> {
        if self.token.is(TokenKind::Arrow) {
            self.advance_token()?;
            let ty = self.parse_type()?;

            Ok(Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_function_block(&mut self) -> Result<Option<Box<Stmt>>, MsgWithPos> {
        if self.token.is(TokenKind::Semicolon) {
            self.advance_token()?;

            Ok(None)
        } else {
            let block = self.parse_block()?;

            Ok(Some(block))
        }
    }

    fn parse_type(&mut self) -> Result<Type, MsgWithPos> {
        match self.token.kind {
            TokenKind::CapitalThis => {
                let pos = self.token.position;
                self.advance_token()?;
                Ok(Type::create_self(self.generate_id(), pos))
            }

            TokenKind::Identifier(_) => {
                let pos = self.token.position;
                let name = self.expect_identifier()?;

                let params = if self.token.is(TokenKind::Lt) {
                    self.advance_token()?;
                    self.parse_comma_list(TokenKind::Gt, |p| Ok(Box::new(p.parse_type()?)))?
                } else {
                    Vec::new()
                };

                Ok(Type::create_basic(self.generate_id(), pos, name, params))
            }

            TokenKind::LParen => {
                let token = self.advance_token()?;
                let subtypes = self.parse_comma_list(TokenKind::RParen, |p| {
                        let ty = p.parse_type()?;

                        Ok(Box::new(ty))
                    })?;

                if self.token.is(TokenKind::Arrow) {
                    self.advance_token()?;
                    let ret = Box::new(self.parse_type()?);

                    Ok(Type::create_fct(self.generate_id(), token.position, subtypes, ret))
                } else {
                    Ok(Type::create_tuple(self.generate_id(), token.position, subtypes))
                }
            }

            _ => Err(MsgWithPos::new(self.token.position, Msg::ExpectedType(self.token.name()))),
        }
    }

    fn parse_statement(&mut self) -> StmtResult {
        match self.token.kind {
            TokenKind::Let | TokenKind::Var => self.parse_var(),
            TokenKind::LBrace => self.parse_block(),
            TokenKind::If => self.parse_if(),
            TokenKind::While => self.parse_while(),
            TokenKind::Loop => self.parse_loop(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Else => Err(MsgWithPos::new(self.token.position, Msg::MisplacedElse)),
            TokenKind::Throw => self.parse_throw(),
            TokenKind::Defer => self.parse_defer(),
            TokenKind::Do => self.parse_do(),
            TokenKind::Spawn => self.parse_spawn(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_spawn(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::Spawn)?.position;
        let expr = self.parse_expression()?;

        self.expect_semicolon()?;

        Ok(Box::new(Stmt::create_spawn(self.generate_id(), pos, expr)))
    }

    fn parse_throw(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::Throw)?.position;
        let expr = self.parse_expression()?;

        self.expect_semicolon()?;

        Ok(Box::new(Stmt::create_throw(self.generate_id(), pos, expr)))
    }

    fn parse_defer(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::Defer)?.position;
        let expr = self.parse_expression()?;

        self.expect_semicolon()?;

        Ok(Box::new(Stmt::create_defer(self.generate_id(), pos, expr)))
    }

    fn parse_do(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::Do)?.position;
        let try_block = self.parse_block()?;
        let mut catch_blocks = Vec::new();

        while self.token.is(TokenKind::Catch) {
            catch_blocks.push(self.parse_catch()?);
        }

        let finally_block = if self.token.is(TokenKind::Finally) {
            Some(self.parse_finally()?)
        } else {
            None
        };

        Ok(Box::new(Stmt::create_do(self.generate_id(),
                                    pos,
                                    try_block,
                                    catch_blocks,
                                    finally_block)))
    }

    fn parse_catch(&mut self) -> Result<CatchBlock, MsgWithPos> {
        let id = self.generate_id();
        let pos = self.expect_token(TokenKind::Catch)?.position;
        let name = self.expect_identifier()?;
        self.expect_token(TokenKind::Colon)?;
        let data_type = self.parse_type()?;
        let block = self.parse_block()?;

        Ok(CatchBlock::new(id, name, pos, data_type, block))
    }

    fn parse_finally(&mut self) -> Result<FinallyBlock, MsgWithPos> {
        self.expect_token(TokenKind::Finally)?;
        let block = self.parse_block()?;

        Ok(FinallyBlock::new(block))
    }

    fn parse_var(&mut self) -> StmtResult {
        let reassignable = if self.token.is(TokenKind::Let) {
            false
        } else if self.token.is(TokenKind::Var) {
            true
        } else {
            panic!("let or var expected")
        };

        let pos = self.advance_token()?.position;
        let ident = self.expect_identifier()?;
        let data_type = self.parse_var_type()?;
        let expr = self.parse_var_assignment()?;

        self.expect_semicolon()?;

        Ok(Box::new(Stmt::create_var(self.generate_id(),
                                     pos,
                                     ident,
                                     reassignable,
                                     data_type,
                                     expr)))
    }

    fn parse_var_type(&mut self) -> Result<Option<Type>, MsgWithPos> {
        if self.token.is(TokenKind::Colon) {
            self.advance_token()?;

            Ok(Some(self.parse_type()?))
        } else {
            Ok(None)
        }
    }

    fn parse_var_assignment(&mut self) -> Result<Option<Box<Expr>>, MsgWithPos> {
        if self.token.is(TokenKind::Eq) {
            self.expect_token(TokenKind::Eq)?;
            let expr = self.parse_expression()?;

            Ok(Some(expr))
        } else {
            Ok(None)
        }
    }

    fn parse_block(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::LBrace)?.position;
        let mut stmts = vec![];

        while !self.token.is(TokenKind::RBrace) && !self.token.is_eof() {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        self.expect_token(TokenKind::RBrace)?;

        Ok(Box::new(Stmt::create_block(self.generate_id(), pos, stmts)))
    }

    fn parse_if(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::If)?.position;

        let mut opts = ExprParsingOpts::new();
        opts.parse_struct_lit(false);
        let cond = self.parse_expression_with_opts(&opts)?;

        let then_block = self.parse_block()?;

        let else_block = if self.token.is(TokenKind::Else) {
            self.advance_token()?;

            if self.token.is(TokenKind::If) {
                let if_block = self.parse_if()?;
                let block = Stmt::create_block(self.generate_id(), if_block.pos(), vec![if_block]);

                Some(Box::new(block))
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };

        Ok(Box::new(Stmt::create_if(self.generate_id(), pos, cond, then_block, else_block)))
    }

    fn parse_while(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::While)?.position;

        let mut opts = ExprParsingOpts::new();
        opts.parse_struct_lit(false);
        let expr = self.parse_expression_with_opts(&opts)?;

        let block = self.parse_block()?;

        Ok(Box::new(Stmt::create_while(self.generate_id(), pos, expr, block)))
    }

    fn parse_loop(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::Loop)?.position;
        let block = self.parse_block()?;

        Ok(Box::new(Stmt::create_loop(self.generate_id(), pos, block)))
    }

    fn parse_break(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::Break)?.position;
        self.expect_semicolon()?;

        Ok(Box::new(Stmt::create_break(self.generate_id(), pos)))
    }

    fn parse_continue(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::Continue)?.position;
        self.expect_semicolon()?;

        Ok(Box::new(Stmt::create_continue(self.generate_id(), pos)))
    }

    fn parse_return(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::Return)?.position;
        let expr = if self.token.is(TokenKind::Semicolon) {
            None
        } else {
            let expr = self.parse_expression()?;
            Some(expr)
        };

        self.expect_semicolon()?;

        Ok(Box::new(Stmt::create_return(self.generate_id(), pos, expr)))
    }

    fn parse_expression_statement(&mut self) -> StmtResult {
        let pos = self.token.position;
        let expr = self.parse_expression()?;
        self.expect_semicolon()?;

        Ok(Box::new(Stmt::create_expr(self.generate_id(), pos, expr)))
    }

    fn parse_expression(&mut self) -> ExprResult {
        let opts = ExprParsingOpts::new();
        self.parse_binary(0, &opts)
    }

    fn parse_expression_with_opts(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        self.parse_binary(0, opts)
    }

    fn parse_binary(&mut self, precedence: u32, opts: &ExprParsingOpts) -> ExprResult {
        let mut left = self.parse_unary(opts)?;

        loop {
            let right_precedence = match self.token.kind {
                TokenKind::Or => 1,
                TokenKind::And => 2,
                TokenKind::Eq => 3,
                TokenKind::EqEq | TokenKind::Ne | TokenKind::Lt | TokenKind::Le |
                TokenKind::Gt | TokenKind::Ge => 4,
                TokenKind::EqEqEq | TokenKind::NeEqEq => 5,
                TokenKind::BitOr | TokenKind::BitAnd | TokenKind::Caret => 6,
                TokenKind::LtLt | TokenKind::GtGt | TokenKind::GtGtGt => 7,
                TokenKind::Add | TokenKind::Sub => 8,
                TokenKind::Mul | TokenKind::Div | TokenKind::Mod => 9,
                TokenKind::Is | TokenKind::As => 10,
                _ => {
                    return Ok(left);
                }
            };

            if precedence >= right_precedence {
                return Ok(left);
            }

            let tok = self.advance_token()?;

            left = match tok.kind {
                TokenKind::Is | TokenKind::As => {
                    let is = tok.is(TokenKind::Is);

                    let right = Box::new(self.parse_type()?);
                    let expr = Expr::create_conv(self.generate_id(), tok.position, left, right, is);

                    Box::new(expr)
                }

                _ => {
                    let right = self.parse_binary(right_precedence, opts)?;
                    self.create_binary(tok, left, right)
                }
            };
        }
    }

    fn parse_unary(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        match self.token.kind {
            TokenKind::Add | TokenKind::Sub | TokenKind::Not => {

                let tok = self.advance_token()?;
                let op = match tok.kind {
                    TokenKind::Add => UnOp::Plus,
                    TokenKind::Sub => UnOp::Neg,
                    TokenKind::Not => UnOp::Not,
                    _ => unreachable!(),
                };

                let expr = self.parse_primary(opts)?;
                Ok(Box::new(Expr::create_un(self.generate_id(), tok.position, op, expr)))
            }

            _ => self.parse_primary(opts),
        }
    }

    fn parse_primary(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        let mut left = self.parse_factor(opts)?;

        loop {
            left = match self.token.kind {
                TokenKind::Dot => {
                    let tok = self.advance_token()?;
                    let ident = self.expect_identifier()?;

                    let type_params = if self.token.is(TokenKind::Sep) {
                        self.advance_token()?;
                        self.expect_token(TokenKind::Lt)?;

                        Some(self.parse_comma_list(TokenKind::Gt, |p| p.parse_type())?)
                    } else {
                        None
                    };

                    if self.token.is(TokenKind::LParen) {
                        self.parse_call(tok.position, Some(left), Path::new(ident), type_params)?

                    } else {
                        assert!(type_params.is_none());
                        Box::new(Expr::create_field(self.generate_id(), tok.position, left, ident))
                    }
                }

                TokenKind::LBracket => {
                    let tok = self.advance_token()?;
                    let index = self.parse_expression()?;
                    self.expect_token(TokenKind::RBracket)?;

                    Box::new(Expr::create_array(self.generate_id(), tok.position, left, index))
                }

                _ => {
                    return Ok(left);
                }
            }
        }
    }

    fn create_binary(&mut self, tok: Token, left: Box<Expr>, right: Box<Expr>) -> Box<Expr> {
        let op = match tok.kind {
            TokenKind::Eq => {
                return Box::new(Expr::create_assign(self.generate_id(), tok.position, left, right));
            }

            TokenKind::Or => BinOp::Or,
            TokenKind::And => BinOp::And,
            TokenKind::EqEq => BinOp::Cmp(CmpOp::Eq),
            TokenKind::Ne => BinOp::Cmp(CmpOp::Ne),
            TokenKind::Lt => BinOp::Cmp(CmpOp::Lt),
            TokenKind::Le => BinOp::Cmp(CmpOp::Le),
            TokenKind::Gt => BinOp::Cmp(CmpOp::Gt),
            TokenKind::Ge => BinOp::Cmp(CmpOp::Ge),
            TokenKind::EqEqEq => BinOp::Cmp(CmpOp::Is),
            TokenKind::NeEqEq => BinOp::Cmp(CmpOp::IsNot),
            TokenKind::BitOr => BinOp::BitOr,
            TokenKind::BitAnd => BinOp::BitAnd,
            TokenKind::Caret => BinOp::BitXor,
            TokenKind::Add => BinOp::Add,
            TokenKind::Sub => BinOp::Sub,
            TokenKind::Mul => BinOp::Mul,
            TokenKind::Div => BinOp::Div,
            TokenKind::Mod => BinOp::Mod,
            TokenKind::LtLt => BinOp::ShiftL,
            TokenKind::GtGt => BinOp::ShiftR,
            TokenKind::GtGtGt => BinOp::UnShiftR,
            _ => panic!("unimplemented token {:?}", tok),
        };

        Box::new(Expr::create_bin(self.generate_id(), tok.position, op, left, right))
    }

    fn parse_factor(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        match self.token.kind {
            TokenKind::LParen => self.parse_parentheses(),
            TokenKind::LitChar(_) => self.parse_lit_char(),
            TokenKind::LitInt(_, _, _) => self.parse_lit_int(),
            TokenKind::LitFloat(_, _) => self.parse_lit_float(),
            TokenKind::String(_) => self.parse_string(),
            TokenKind::Identifier(_) => self.parse_identifier_or_call(opts),
            TokenKind::True => self.parse_bool_literal(),
            TokenKind::False => self.parse_bool_literal(),
            TokenKind::Nil => self.parse_nil(),
            TokenKind::This => self.parse_this(),
            TokenKind::Super => self.parse_super(),
            TokenKind::Try => self.parse_try(),
            TokenKind::TryForce | TokenKind::TryOpt => self.parse_try_op(),
            TokenKind::BitOr | TokenKind::Or => self.parse_lambda(),
            _ => {
                Err(MsgWithPos::new(self.token.position,
                                    Msg::ExpectedFactor(self.token.name().clone())))
            }
        }
    }

    fn parse_identifier_or_call(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        let pos = self.token.position;
        let mut path = vec![self.expect_identifier()?];
        let mut type_params = None;

        while self.token.is(TokenKind::Sep) {
            self.advance_token()?;

            if self.token.is(TokenKind::Lt) {
                self.expect_token(TokenKind::Lt)?;

                let params = self.parse_comma_list(TokenKind::Gt, |p| p.parse_type())?;
                type_params = Some(params);
                break;

            } else {
                let ident = self.expect_identifier()?;
                path.push(ident);
            }
        }

        // is this a function call?
        if self.token.is(TokenKind::LParen) {
            self.parse_call(pos, None, Path { path: path }, type_params)

        } else if self.token.is(TokenKind::LBrace) && opts.parse_struct_lit {
            assert!(type_params.is_none());
            self.parse_lit_struct(pos, Path { path: path })

            // if not we have a simple identifier
        } else {
            assert_eq!(1, path.len());
            let name = path[0];
            Ok(Box::new(Expr::create_ident(self.generate_id(), pos, name, type_params)))
        }
    }

    fn parse_lit_struct(&mut self, pos: Position, path: Path) -> ExprResult {
        self.expect_token(TokenKind::LBrace)?;
        let args = self.parse_comma_list(TokenKind::RBrace, |p| p.parse_lit_struct_arg())?;

        Ok(Box::new(Expr::create_lit_struct(self.generate_id(), pos, path, args)))
    }

    fn parse_lit_struct_arg(&mut self) -> Result<StructArg, MsgWithPos> {
        let pos = self.token.position;
        let name = self.expect_identifier()?;

        self.expect_token(TokenKind::Colon)?;

        let expr = self.parse_expression()?;

        Ok(StructArg {
               id: self.generate_id(),
               pos: pos,
               name: name,
               expr: expr,
           })
    }

    fn parse_call(&mut self,
                  pos: Position,
                  object: Option<Box<Expr>>,
                  path: Path,
                  type_params: Option<Vec<Type>>)
                  -> ExprResult {
        self.expect_token(TokenKind::LParen)?;

        let args = self.parse_comma_list(TokenKind::RParen, |p| p.parse_expression())?;

        Ok(Box::new(Expr::create_call(self.generate_id(), pos, path, object, args, type_params)))
    }

    fn parse_parentheses(&mut self) -> ExprResult {
        self.advance_token()?;
        let exp = self.parse_expression()?;
        self.expect_token(TokenKind::RParen)?;

        Ok(exp)
    }

    fn parse_try_op(&mut self) -> ExprResult {
        let tok = self.advance_token()?;
        let exp = self.parse_expression()?;

        let mode = if tok.is(TokenKind::TryForce) {
            TryMode::Force
        } else {
            TryMode::Opt
        };

        Ok(Box::new(Expr::create_try(self.generate_id(), tok.position, exp, mode)))
    }

    fn parse_try(&mut self) -> ExprResult {
        let pos = self.expect_token(TokenKind::Try)?.position;
        let exp = self.parse_expression()?;

        let mode = if self.token.is(TokenKind::Else) {
            self.advance_token()?;
            let alt_exp = self.parse_expression()?;

            TryMode::Else(alt_exp)
        } else {
            TryMode::Normal
        };

        Ok(Box::new(Expr::create_try(self.generate_id(), pos, exp, mode)))
    }

    fn parse_lit_char(&mut self) -> ExprResult {
        let tok = self.advance_token()?;
        let pos = tok.position;

        if let TokenKind::LitChar(val) = tok.kind {
            Ok(Box::new(Expr::create_lit_char(self.generate_id(), pos, val)))

        } else {
            unreachable!();
        }
    }

    fn parse_lit_int(&mut self) -> ExprResult {
        let tok = self.advance_token()?;
        let pos = tok.position;

        if let TokenKind::LitInt(value, base, suffix) = tok.kind {
            let filtered = value
                .chars()
                .filter(|&ch| ch != '_')
                .collect::<String>();
            let parsed = u64::from_str_radix(&filtered, base.num());

            match parsed {
                Ok(num) => {
                    let expr = Expr::create_lit_int(self.generate_id(), pos, num, base, suffix);
                    Ok(Box::new(expr))
                }

                _ => {
                    let bits = match suffix {
                        IntSuffix::Byte => "byte",
                        IntSuffix::Int => "int",
                        IntSuffix::Long => "long",
                    };

                    Err(MsgWithPos::new(pos, Msg::NumberOverflow(bits.into())))
                }
            }
        } else {
            unreachable!();
        }
    }

    fn parse_lit_float(&mut self) -> ExprResult {
        let tok = self.advance_token()?;
        let pos = tok.position;

        if let TokenKind::LitFloat(value, suffix) = tok.kind {
            let filtered = value
                .chars()
                .filter(|&ch| ch != '_')
                .collect::<String>();
            let parsed = filtered.parse::<f64>();

            if let Ok(num) = parsed {
                let expr = Expr::create_lit_float(self.generate_id(), pos, num, suffix);
                return Ok(Box::new(expr));

            }
        }

        unreachable!()
    }

    fn parse_string(&mut self) -> ExprResult {
        let string = self.advance_token()?;

        if let TokenKind::String(value) = string.kind {
            Ok(Box::new(Expr::create_lit_str(self.generate_id(), string.position, value)))
        } else {
            unreachable!();
        }
    }

    fn parse_bool_literal(&mut self) -> ExprResult {
        let tok = self.advance_token()?;
        let value = tok.is(TokenKind::True);

        Ok(Box::new(Expr::create_lit_bool(self.generate_id(), tok.position, value)))
    }

    fn parse_this(&mut self) -> ExprResult {
        let tok = self.advance_token()?;

        Ok(Box::new(Expr::create_this(self.generate_id(), tok.position)))
    }

    fn parse_super(&mut self) -> ExprResult {
        let tok = self.advance_token()?;

        Ok(Box::new(Expr::create_super(self.generate_id(), tok.position)))
    }

    fn parse_nil(&mut self) -> ExprResult {
        let tok = self.advance_token()?;

        Ok(Box::new(Expr::create_nil(self.generate_id(), tok.position)))
    }

    fn parse_lambda(&mut self) -> ExprResult {
        let tok = self.advance_token()?;

        let params = if tok.kind == TokenKind::Or {
            // nothing to do
            Vec::new()

        } else {
            self.param_idx = 0;
            self.parse_comma_list(TokenKind::BitOr, |p| {
                    p.param_idx += 1;
                    p.parse_function_param()
                })?
        };

        let ret = if self.token.is(TokenKind::Arrow) {
            self.advance_token()?;
            Some(Box::new(self.parse_type()?))

        } else {
            None
        };

        let block = self.parse_block()?;

        Ok(Box::new(Expr::create_lambda(self.generate_id(), tok.position, params, ret, block)))
    }

    fn expect_identifier(&mut self) -> Result<Name, MsgWithPos> {
        let tok = self.advance_token()?;

        if let TokenKind::Identifier(ref value) = tok.kind {
            let interned = self.interner.intern(value);

            Ok(interned)

        } else {
            Err(MsgWithPos::new(tok.position, Msg::ExpectedIdentifier(tok.name())))
        }
    }

    fn expect_semicolon(&mut self) -> Result<Token, MsgWithPos> {
        self.expect_token(TokenKind::Semicolon)
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<Token, MsgWithPos> {
        if self.token.kind == kind {
            let token = self.advance_token()?;

            Ok(token)
        } else {
            Err(MsgWithPos::new(self.token.position,
                                Msg::ExpectedToken(kind.name().into(), self.token.name())))
        }
    }

    fn advance_token(&mut self) -> Result<Token, MsgWithPos> {
        let tok = self.lexer.read_token()?;

        Ok(mem::replace(&mut self.token, tok))
    }

    fn generate_primary_ctor(&mut self,
                             cls: &mut Class,
                             ctor_params: Vec<PrimaryCtorParam>)
                             -> Function {
        let builder = Builder::new(self.id_generator);
        let mut block = builder.build_block();

        if let Some(ref parent_class) = cls.parent_class {
            let expr = Expr::create_delegation(self.generate_id(),
                                               parent_class.pos,
                                               DelegationType::Super,
                                               parent_class.params.clone());

            block.add_expr(Box::new(expr));
        }

        for param in ctor_params.iter().filter(|param| param.field) {
            let this = builder.build_this();
            let lhs = builder.build_field(this, param.name);
            let rhs = builder.build_ident(param.name);
            let ass = builder.build_assign(lhs, rhs);

            block.add_expr(ass);
        }

        for field in cls.fields.iter().filter(|field| field.expr.is_some()) {
            let this = builder.build_this();
            let lhs = builder.build_field(this, field.name);
            let ass = builder.build_assign(lhs, field.expr.as_ref().unwrap().clone());

            block.add_expr(ass);
        }

        let mut fct = builder.build_fct(cls.name);

        for field in &ctor_params {
            fct.add_param(field.name, field.data_type.clone());
        }

        fct.is_method(true)
            .is_public(true)
            .ctor(CtorType::Primary)
            .block(block.build());

        fct.build()
    }
}

struct ExprParsingOpts {
    parse_struct_lit: bool,
}

impl ExprParsingOpts {
    pub fn new() -> ExprParsingOpts {
        ExprParsingOpts { parse_struct_lit: true }
    }

    pub fn parse_struct_lit(&mut self, val: bool) -> &mut ExprParsingOpts {
        self.parse_struct_lit = val;
        self
    }
}

#[derive(Clone, Debug)]
struct Delegation {
    pub pos: Position,
    pub ty: DelegationType,
    pub args: Vec<Box<Expr>>,
}

#[derive(Debug)]
pub struct NodeIdGenerator {
    value: RefCell<usize>,
}

impl NodeIdGenerator {
    pub fn new() -> NodeIdGenerator {
        NodeIdGenerator { value: RefCell::new(1) }
    }

    pub fn next(&self) -> NodeId {
        let value = *self.value.borrow();
        *self.value.borrow_mut() += 1;

        NodeId(value)
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
    use interner::*;

    use error::msg::{Msg, MsgWithPos};
    use lexer::position::Position;
    use lexer::reader::Reader;
    use parser::{NodeIdGenerator, Parser};

    fn parse_expr(code: &'static str) -> (Box<Expr>, Interner) {
        let id_generator = NodeIdGenerator::new();
        let mut interner = Interner::new();
        let mut ast = Ast::new();

        let expr = {
            let reader = Reader::from_string(code);
            let mut parser = Parser::new(reader, &id_generator, &mut ast, &mut interner);
            assert!(parser.init().is_ok(), true);

            parser.parse_expression().unwrap()
        };

        (expr, interner)
    }

    fn err_expr(code: &'static str, msg: Msg, line: u32, col: u32) {
        let err = {
            let id_generator = NodeIdGenerator::new();
            let mut interner = Interner::new();
            let mut ast = Ast::new();
            let reader = Reader::from_string(code);
            let mut parser = Parser::new(reader, &id_generator, &mut ast, &mut interner);

            assert!(parser.init().is_ok(), true);
            parser.parse_expression().unwrap_err()
        };

        assert_eq!(msg, err.msg);
        assert_eq!(line, err.pos.line);
        assert_eq!(col, err.pos.column);
    }

    fn parse_stmt(code: &'static str) -> Box<Stmt> {
        let id_generator = NodeIdGenerator::new();
        let mut interner = Interner::new();
        let mut ast = Ast::new();
        let reader = Reader::from_string(code);
        let mut parser = Parser::new(reader, &id_generator, &mut ast, &mut interner);
        assert!(parser.init().is_ok(), true);

        parser.parse_statement().unwrap()
    }

    fn err_stmt(code: &'static str, msg: Msg, line: u32, col: u32) {
        let err = {
            let id_generator = NodeIdGenerator::new();
            let mut interner = Interner::new();
            let mut ast = Ast::new();
            let reader = Reader::from_string(code);
            let mut parser = Parser::new(reader, &id_generator, &mut ast, &mut interner);

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
            let id_generator = NodeIdGenerator::new();
            let mut ast = Ast::new();
            let reader = Reader::from_string(code);
            let mut parser = Parser::new(reader, &id_generator, &mut ast, &mut interner);
            assert!(parser.init().is_ok(), true);

            parser.parse_type().unwrap()
        };

        (ty, interner)
    }

    fn parse(code: &'static str) -> (Ast, Interner) {
        let id_generator = NodeIdGenerator::new();
        let mut interner = Interner::new();
        let mut ast = Ast::new();

        let reader = Reader::from_string(code);
        Parser::new(reader, &id_generator, &mut ast, &mut interner)
            .parse()
            .unwrap();

        (ast, interner)
    }

    fn parse_err(code: &'static str) -> MsgWithPos {
        let id_generator = NodeIdGenerator::new();
        let mut interner = Interner::new();
        let mut ast = Ast::new();

        let reader = Reader::from_string(code);
        Parser::new(reader, &id_generator, &mut ast, &mut interner)
            .parse()
            .unwrap_err()
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
    fn parse_number_with_underscore() {
        let (expr, _) = parse_expr("1____0");

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
    fn parse_shift_right() {
        let (expr, _) = parse_expr("a>>4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::ShiftR, bin.op);
    }

    #[test]
    fn parse_unsigned_shift_right() {
        let (expr, _) = parse_expr("a>>>4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::UnShiftR, bin.op);
    }

    #[test]
    fn parse_left() {
        let (expr, _) = parse_expr("a<<4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::ShiftL, bin.op);
    }

    #[test]
    fn parse_call_without_params() {
        let (expr, interner) = parse_expr("fname()");

        let call = expr.to_call().unwrap();
        assert_eq!("fname", *interner.str(call.path.name()));
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_call_with_params() {
        let (expr, interner) = parse_expr("fname2(1,2,3)");

        let call = expr.to_call().unwrap();
        assert_eq!("fname2", *interner.str(call.path.name()));
        assert_eq!(3, call.args.len());

        for i in 0..3 {
            let lit = call.args[i as usize].to_lit_int().unwrap();
            assert_eq!(i + 1, lit.value);
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

        assert_eq!("int",
                   *interner1.str(p1a.data_type.to_basic().unwrap().name));
        assert_eq!("int",
                   *interner2.str(p2a.data_type.to_basic().unwrap().name));

        assert_eq!("str",
                   *interner1.str(p1b.data_type.to_basic().unwrap().name));
        assert_eq!("str",
                   *interner2.str(p2b.data_type.to_basic().unwrap().name));
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

        assert_eq!(1,
                   ret.expr
                       .as_ref()
                       .unwrap()
                       .to_lit_int()
                       .unwrap()
                       .value);
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

        assert_eq!(0, basic.params.len());
        assert_eq!("bla", *interner.str(basic.name));
    }

    #[test]
    fn parse_type_basic_with_params() {
        let (ty, interner) = parse_type("Foo<A, B>");
        let basic = ty.to_basic().unwrap();

        assert_eq!(2, basic.params.len());
        assert_eq!("Foo", *interner.str(basic.name));
        assert_eq!("A", *interner.str(basic.params[0].to_basic().unwrap().name));
        assert_eq!("B", *interner.str(basic.params[1].to_basic().unwrap().name));
    }

    #[test]
    fn parse_type_fct_no_params() {
        let (ty, _) = parse_type("() -> ()");
        let fct = ty.to_fct().unwrap();

        assert_eq!(0, fct.params.len());
        assert!(fct.ret.is_unit());
    }

    #[test]
    fn parse_type_fct_one_param() {
        let (ty, interner) = parse_type("(A) -> B");
        let fct = ty.to_fct().unwrap();

        assert_eq!(1, fct.params.len());
        assert_eq!("A", *interner.str(fct.params[0].to_basic().unwrap().name));
        assert_eq!("B", *interner.str(fct.ret.to_basic().unwrap().name));
    }

    #[test]
    fn parse_type_fct_two_params() {
        let (ty, interner) = parse_type("(A, B) -> C");
        let fct = ty.to_fct().unwrap();

        assert_eq!(2, fct.params.len());
        assert_eq!("A", *interner.str(fct.params[0].to_basic().unwrap().name));
        assert_eq!("B", *interner.str(fct.params[1].to_basic().unwrap().name));
        assert_eq!("C", *interner.str(fct.ret.to_basic().unwrap().name));
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
        let rt1 = mtd1.return_type
            .as_ref()
            .unwrap()
            .to_basic()
            .unwrap()
            .name;
        assert_eq!("int", *interner.str(rt1));

        let mtd2 = &cls.methods[1];
        assert_eq!("id", *interner.str(mtd2.name));
        assert_eq!(1, mtd2.params.len());
        assert_eq!(true, mtd2.method);
        let rt2 = mtd2.return_type
            .as_ref()
            .unwrap()
            .to_basic()
            .unwrap()
            .name;
        assert_eq!("Str", *interner.str(rt2));
    }

    #[test]
    fn parse_abstract_method() {
        let (prog, _) = parse("class Foo {
            abstract fun zero();
            fun foo();
        }");

        let cls = prog.cls0();
        assert_eq!(0, cls.fields.len());
        assert_eq!(2, cls.methods.len());

        let mtd1 = &cls.methods[0];
        assert_eq!(true, mtd1.is_abstract);

        let mtd2 = &cls.methods[1];
        assert_eq!(false, mtd2.is_abstract);
    }

    #[test]
    fn parse_class() {
        let (prog, interner) = parse("class Foo");
        let class = prog.cls0();

        assert_eq!(0, class.fields.len());
        assert_eq!(false, class.has_open);
        assert_eq!(false, class.is_abstract);
        assert_eq!(Position::new(1, 1), class.pos);
        assert_eq!("Foo", *interner.str(class.name));
    }

    #[test]
    fn parse_class_abstract() {
        let (prog, interner) = parse("abstract class Foo");
        let class = prog.cls0();

        assert_eq!(true, class.is_abstract);
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

        assert_eq!("Bar",
                   interner
                       .str(class.parent_class.as_ref().unwrap().name)
                       .to_string());
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
    fn parse_defer() {
        let stmt = parse_stmt("defer foo();");
        let defer = stmt.to_defer().unwrap();

        assert!(defer.expr.is_call());
    }

    #[test]
    fn parse_do() {
        let stmt = parse_stmt("do { 1; } catch e: Str { 2; }
                                          catch e: IntArray { 3; } finally { 4; }");
        let try = stmt.to_do().unwrap();

        assert_eq!(2, try.catch_blocks.len());
        assert!(try.finally_block.is_some());
    }

    #[test]
    fn parse_do_without_catch() {
        let stmt = parse_stmt("do { 1; }");
        let try = stmt.to_do().unwrap();

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

        let block = cls.ctors[0]
            .block
            .as_ref()
            .unwrap()
            .to_block()
            .unwrap();
        let delegation = block.stmts[0]
            .to_expr()
            .unwrap()
            .expr
            .to_delegation()
            .unwrap();

        assert_eq!(DelegationType::This, delegation.ty);
        assert_eq!(1, delegation.args.len());
    }

    #[test]
    fn parse_ctor_with_super_delegation() {
        let (prog, _) = parse("class Y(a: int)
                               class X: Y { init(a: int, b: int): super(a) {} }");
        let cls = prog.cls(1);
        assert_eq!(1, cls.ctors.len());

        let block = cls.ctors[0]
            .block
            .as_ref()
            .unwrap()
            .to_block()
            .unwrap();
        let delegation = block.stmts[0]
            .to_expr()
            .unwrap()
            .expr
            .to_delegation()
            .unwrap();

        assert_eq!(DelegationType::Super, delegation.ty);
        assert_eq!(1, delegation.args.len());
    }

    #[test]
    fn parse_try_function() {
        let (expr, _) = parse_expr("try foo()");
        let try = expr.to_try().unwrap();
        let call = try.expr.to_call().unwrap();

        assert!(try.mode.is_normal());
        assert!(call.object.is_none());
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_try_method() {
        let (expr, _) = parse_expr("try obj.foo()");
        let try = expr.to_try().unwrap();
        let call = try.expr.to_call().unwrap();

        assert!(try.mode.is_normal());
        assert!(call.object.is_some());
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_try_expr() {
        // although `try 1` does not make sense it should parse correctly
        let (expr, _) = parse_expr("try 1");
        let try = expr.to_try().unwrap();

        assert!(try.mode.is_normal());
        assert!(try.expr.is_lit_int());
    }

    #[test]
    fn parse_try_with_else() {
        let (expr, _) = parse_expr("try foo() else 2");
        let try = expr.to_try().unwrap();

        assert!(try.mode.is_else());
    }

    #[test]
    fn parse_try_force() {
        let (expr, _) = parse_expr("try! foo()");
        let try = expr.to_try().unwrap();

        assert!(try.mode.is_force());
    }

    #[test]
    fn parse_try_opt() {
        let (expr, _) = parse_expr("try? foo()");
        let try = expr.to_try().unwrap();

        assert!(try.mode.is_opt());
    }

    #[test]
    fn parse_struct_empty() {
        let (prog, interner) = parse("struct Foo {}");
        let struc = prog.struct0();
        assert_eq!(0, struc.fields.len());
        assert_eq!("Foo", *interner.str(struc.name));
    }

    #[test]
    fn parse_struct_one_field() {
        let (prog, interner) = parse("struct Bar {
            f1: Foo1,
        }");
        let struc = prog.struct0();
        assert_eq!(1, struc.fields.len());
        assert_eq!("Bar", *interner.str(struc.name));

        let f1 = &struc.fields[0];
        assert_eq!("f1", *interner.str(f1.name));
    }

    #[test]
    fn parse_struct_multiple_fields() {
        let (prog, interner) = parse("struct FooBar {
            fa: Foo1,
            fb: Foo2,
        }");
        let struc = prog.struct0();
        assert_eq!(2, struc.fields.len());
        assert_eq!("FooBar", *interner.str(struc.name));

        let f1 = &struc.fields[0];
        assert_eq!("fa", *interner.str(f1.name));

        let f2 = &struc.fields[1];
        assert_eq!("fb", *interner.str(f2.name));
    }

    #[test]
    fn parse_struct_lit() {
        let (expr, _) = parse_expr("Foo { a: 1, b: 2 }");
        assert!(expr.is_lit_struct());
    }

    #[test]
    fn parse_struct_lit_while() {
        let stmt = parse_stmt("while i < n { }");
        let while_stmt = stmt.to_while().unwrap();
        let bin = while_stmt.cond.to_bin().unwrap();

        assert!(bin.lhs.is_ident());
        assert!(bin.rhs.is_ident());
    }

    #[test]
    fn parse_struct_lit_if() {
        let stmt = parse_stmt("if i < n { }");
        let ifstmt = stmt.to_if().unwrap();
        let bin = ifstmt.cond.to_bin().unwrap();

        assert!(bin.lhs.is_ident());
        assert!(bin.rhs.is_ident());
    }

    #[test]
    fn parse_lit_float() {
        let (expr, _) = parse_expr("1.2");

        let lit = expr.to_lit_float().unwrap();

        assert_eq!(1.2, lit.value);
    }

    #[test]
    fn parse_class_type_params() {
        let (prog, interner) = parse("class Foo<T>");
        let cls = prog.cls0();

        let type_params = cls.type_params.as_ref().unwrap();
        assert_eq!(1, type_params.len());
        assert_eq!("T", *interner.str(type_params[0].name));

        let (prog, interner) = parse("class Foo<X>");
        let cls = prog.cls0();

        let type_params = cls.type_params.as_ref().unwrap();
        assert_eq!(1, type_params.len());
        assert_eq!("X", *interner.str(type_params[0].name));
    }

    #[test]
    fn parse_multiple_class_type_params() {
        let (prog, interner) = parse("class Foo<A, B>");
        let cls = prog.cls0();

        let type_params = cls.type_params.as_ref().unwrap();
        assert_eq!(2, type_params.len());
        assert_eq!("A", *interner.str(type_params[0].name));
        assert_eq!("B", *interner.str(type_params[1].name));
    }

    #[test]
    fn parse_empty_trait() {
        let (prog, interner) = parse("trait Foo { }");
        let xtrait = prog.trait0();

        assert_eq!("Foo", *interner.str(xtrait.name));
        assert_eq!(0, xtrait.methods.len());
    }

    #[test]
    fn parse_trait_with_function() {
        let (prog, interner) = parse("trait Foo { fun empty(); }");
        let xtrait = prog.trait0();

        assert_eq!("Foo", *interner.str(xtrait.name));
        assert_eq!(1, xtrait.methods.len());
        assert_eq!(false, xtrait.methods[0].is_static);
    }

    #[test]
    fn parse_trait_with_static_function() {
        let (prog, interner) = parse("trait Foo { static fun empty(); }");
        let xtrait = prog.trait0();

        assert_eq!("Foo", *interner.str(xtrait.name));
        assert_eq!(1, xtrait.methods.len());
        assert_eq!(true, xtrait.methods[0].is_static);
    }

    #[test]
    fn parse_empty_impl() {
        let (prog, interner) = parse("impl Foo for A {}");
        let ximpl = prog.impl0();

        assert_eq!("Foo", *interner.str(ximpl.trait_name));
        assert_eq!("A", *interner.str(ximpl.class_name));
        assert_eq!(0, ximpl.methods.len());
    }

    #[test]
    fn parse_impl_with_function() {
        let (prog, interner) = parse("impl Bar for B { fun foo(); }");
        let ximpl = prog.impl0();

        assert_eq!("Bar", *interner.str(ximpl.trait_name));
        assert_eq!("B", *interner.str(ximpl.class_name));
        assert_eq!(1, ximpl.methods.len());
        assert_eq!(false, ximpl.methods[0].is_static);
    }

    #[test]
    fn parse_impl_with_static_function() {
        let (prog, interner) = parse("impl Bar for B { static fun foo(); }");
        let ximpl = prog.impl0();

        assert_eq!("Bar", *interner.str(ximpl.trait_name));
        assert_eq!("B", *interner.str(ximpl.class_name));
        assert_eq!(1, ximpl.methods.len());
        assert_eq!(true, ximpl.methods[0].is_static);
    }

    #[test]
    fn parse_global_var() {
        let (prog, interner) = parse("var a: int = 0;");
        let global = prog.global0();

        assert_eq!("a", *interner.str(global.name));
        assert_eq!(true, global.reassignable);
    }

    #[test]
    fn parse_global_let() {
        let (prog, interner) = parse("let b: int = 0;");
        let global = prog.global0();

        assert_eq!("b", *interner.str(global.name));
        assert_eq!(false, global.reassignable);
    }

    #[test]
    fn parse_lit_char() {
        let (expr, _) = parse_expr("'a'");
        let lit = expr.to_lit_char().unwrap();

        assert_eq!('a', lit.value);
    }

    #[test]
    fn parse_spawn() {
        let stmt = parse_stmt("spawn 1;");
        let spawn = stmt.to_spawn().unwrap();
        assert!(spawn.expr.is_lit_int());
    }

    #[test]
    fn parse_fct_call_with_type_param() {
        let (expr, _) = parse_expr("Array::<int>()");
        let ident = expr.to_call().unwrap();

        assert_eq!(1, ident.type_params.as_ref().unwrap().len());

        let (expr, _) = parse_expr("Foo::<int, long>()");
        let ident = expr.to_call().unwrap();

        assert_eq!(2, ident.type_params.as_ref().unwrap().len());

        let (expr, _) = parse_expr("Bar::<>()");
        let ident = expr.to_call().unwrap();

        assert_eq!(0, ident.type_params.as_ref().unwrap().len());

        let (expr, _) = parse_expr("Vec()");
        let ident = expr.to_call().unwrap();

        assert!(ident.type_params.is_none());
    }

    #[test]
    fn parse_static_method() {
        let (prog, _) = parse("class A {
                static fun test() {}
              }");
        let cls = prog.cls0();
        assert_eq!(1, cls.methods.len());

        let mtd = &cls.methods[0];
        assert!(mtd.is_static);
    }

    #[test]
    fn parse_call_with_path() {
        let (expr, interner) = parse_expr("Foo::get()");
        let call = expr.to_call().unwrap();

        assert_eq!(2, call.path.len());
        assert_eq!("Foo", *interner.str(call.path[0]));
        assert_eq!("get", *interner.str(call.path[1]));
    }

    #[test]
    fn parse_fct_with_type_params() {
        let (prog, _) = parse("fun f<T>() {}");
        let fct = prog.fct0();

        assert_eq!(1, fct.type_params.as_ref().unwrap().len());
    }

    #[test]
    fn parse_const() {
        let (prog, interner) = parse("const x: int = 0;");
        let xconst = prog.const0();

        assert_eq!("x", *interner.str(xconst.name));
    }

    #[test]
    fn parse_generic_with_bound() {
        let (prog, _) = parse("class A<T: Foo>");
        let cls = prog.cls0();

        let type_param = &cls.type_params.as_ref().unwrap()[0];
        assert_eq!(1, type_param.bounds.len());
    }

    #[test]
    fn parse_generic_with_multiple_bounds() {
        let (prog, _) = parse("class A<T: Foo + Bar>");
        let cls = prog.cls0();

        let type_param = &cls.type_params.as_ref().unwrap()[0];
        assert_eq!(2, type_param.bounds.len());
    }

    #[test]
    fn parse_lambda_no_params_no_return_value() {
        let (expr, _) = parse_expr("|| {}");
        let lambda = expr.to_lambda().unwrap();

        assert!(lambda.ret.is_none());
    }

    #[test]
    fn parse_lambda_no_params_unit_as_return_value() {
        let (expr, _) = parse_expr("|| -> () {}");
        let lambda = expr.to_lambda().unwrap();
        let ret = lambda.ret.as_ref().unwrap();

        assert!(ret.is_unit());
    }

    #[test]
    fn parse_lambda_no_params_with_return_value() {
        let (expr, interner) = parse_expr("|| -> A {}");
        let lambda = expr.to_lambda().unwrap();
        let ret = lambda.ret.as_ref().unwrap();
        let basic = ret.to_basic().unwrap();

        assert_eq!("A", *interner.str(basic.name));
    }

    #[test]
    fn parse_lambda_with_one_param() {
        let (expr, interner) = parse_expr("|a: A| -> B {}");
        let lambda = expr.to_lambda().unwrap();

        assert_eq!(1, lambda.params.len());

        let param = &lambda.params[0];
        assert_eq!("a", *interner.str(param.name));
        let basic = param.data_type.to_basic().unwrap();
        assert_eq!("A", *interner.str(basic.name));

        let ret = lambda.ret.as_ref().unwrap();
        let basic = ret.to_basic().unwrap();

        assert_eq!("B", *interner.str(basic.name));
    }

    #[test]
    fn parse_lambda_with_two_params() {
        let (expr, interner) = parse_expr("|a: A, b: B| -> C {}");
        let lambda = expr.to_lambda().unwrap();

        assert_eq!(2, lambda.params.len());

        let param = &lambda.params[0];
        assert_eq!("a", *interner.str(param.name));
        let basic = param.data_type.to_basic().unwrap();
        assert_eq!("A", *interner.str(basic.name));

        let param = &lambda.params[1];
        assert_eq!("b", *interner.str(param.name));
        let basic = param.data_type.to_basic().unwrap();
        assert_eq!("B", *interner.str(basic.name));

        let ret = lambda.ret.as_ref().unwrap();
        let basic = ret.to_basic().unwrap();

        assert_eq!("C", *interner.str(basic.name));
    }
}
