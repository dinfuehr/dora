use std::cell::RefCell;
use std::mem;
use std::sync::Arc;

use crate::ast;
use crate::ast::*;
use crate::error::{ParseError, ParseErrorWithLocation};

use crate::interner::*;

use crate::token::EXPRESSION_FIRST;
use crate::{lex, Span, Token, TokenKind, TokenSet};

pub struct Parser<'a> {
    tokens: Vec<Token>,
    token: Token,
    token_idx: usize,
    id_generator: NodeIdGenerator,
    interner: &'a mut Interner,
    content: Arc<String>,
    errors: Vec<ParseErrorWithLocation>,
}

enum StmtOrExpr {
    Stmt(Stmt),
    Expr(Expr),
}

impl<'a> Parser<'a> {
    pub fn from_string(code: &'static str, interner: &'a mut Interner) -> Parser<'a> {
        let content = Arc::new(String::from(code));
        Parser::common_init(content, interner)
    }

    pub fn from_shared_string(content: Arc<String>, interner: &'a mut Interner) -> Parser<'a> {
        Parser::common_init(content, interner)
    }

    fn common_init(content: Arc<String>, interner: &'a mut Interner) -> Parser<'a> {
        let (tokens, errors) = lex(&*content);

        let mut parser = Parser {
            token: Token::new(TokenKind::EOF, Span::invalid()),
            tokens,
            token_idx: 0,
            id_generator: NodeIdGenerator::new(),
            interner,
            content,
            errors,
        };

        parser.setup_token();

        parser
    }

    fn generate_id(&mut self) -> NodeId {
        self.id_generator.next()
    }

    pub fn parse(mut self) -> (ast::File, NodeIdGenerator, Vec<ParseErrorWithLocation>) {
        let ast_file = self.parse_top_level();
        (ast_file, self.id_generator, self.errors)
    }

    fn parse_top_level(&mut self) -> ast::File {
        let mut elements = vec![];

        while !self.is_eof() {
            elements.push(self.parse_top_level_element());
        }

        ast::File { elements }
    }

    fn parse_top_level_element(&mut self) -> Elem {
        let modifiers = self.parse_annotation_usages();

        match self.current() {
            TokenKind::FN => {
                self.restrict_modifiers(
                    &modifiers,
                    &[
                        Annotation::Internal,
                        Annotation::OptimizeImmediately,
                        Annotation::Test,
                        Annotation::Pub,
                    ],
                );
                let fct = self.parse_function(&modifiers);
                Arc::new(ElemData::Function(fct))
            }

            TokenKind::CLASS => {
                self.restrict_modifiers(&modifiers, &[Annotation::Internal, Annotation::Pub]);
                let class = self.parse_class(&modifiers);
                Arc::new(ElemData::Class(class))
            }

            TokenKind::STRUCT => {
                self.restrict_modifiers(&modifiers, &[Annotation::Pub, Annotation::Internal]);
                let struc = self.parse_struct(&modifiers);
                Arc::new(ElemData::Struct(struc))
            }

            TokenKind::TRAIT => {
                self.restrict_modifiers(&modifiers, &[Annotation::Pub]);
                let trait_ = self.parse_trait(&modifiers);
                Arc::new(ElemData::Trait(trait_))
            }

            TokenKind::IMPL => {
                self.ban_modifiers(&modifiers);
                let impl_ = self.parse_impl();
                Arc::new(ElemData::Impl(impl_))
            }

            TokenKind::ALIAS => {
                self.restrict_modifiers(&modifiers, &[Annotation::Pub]);
                let alias = self.parse_alias(&modifiers);
                Arc::new(ElemData::Alias(alias))
            }

            TokenKind::LET => {
                self.restrict_modifiers(&modifiers, &[Annotation::Pub]);
                let global = self.parse_global(&modifiers);
                Arc::new(ElemData::Global(global))
            }

            TokenKind::CONST => {
                self.restrict_modifiers(&modifiers, &[Annotation::Pub]);
                let const_ = self.parse_const(&modifiers);
                Arc::new(ElemData::Const(const_))
            }

            TokenKind::ENUM => {
                self.restrict_modifiers(&modifiers, &[Annotation::Pub]);
                let enum_ = self.parse_enum(&modifiers);
                Arc::new(ElemData::Enum(enum_))
            }

            TokenKind::MOD => {
                self.restrict_modifiers(&modifiers, &[Annotation::Pub]);
                let module = self.parse_module(&modifiers);
                Arc::new(ElemData::Module(module))
            }

            TokenKind::USE => {
                self.restrict_modifiers(&modifiers, &[Annotation::Pub]);
                let use_stmt = self.parse_use();
                Arc::new(ElemData::Use(use_stmt))
            }

            TokenKind::EXTERN => {
                self.ban_modifiers(&modifiers);
                let extern_stmt = self.parse_extern();
                Arc::new(ElemData::Extern(extern_stmt))
            }

            _ => {
                let span = self.token.span;
                let msg = ParseError::ExpectedTopLevelDeclaration;
                self.error_and_advance(msg);
                Arc::new(ElemData::Error {
                    id: self.id_generator.next(),
                    span,
                })
            }
        }
    }

    fn parse_extern(&mut self) -> Arc<ExternPackage> {
        let start = self.token.span.start();

        self.expect(TokenKind::EXTERN);
        self.eat(TokenKind::PACKAGE);
        let name = self.eat_identifier();
        let identifier = if self.is(TokenKind::AS) {
            self.eat(TokenKind::AS);
            self.eat_identifier()
        } else {
            None
        };

        let span = self.span_from(start);

        Arc::new(ExternPackage {
            id: self.generate_id(),
            span,
            name,
            identifier,
        })
    }

    fn parse_use(&mut self) -> Arc<Use> {
        self.expect(TokenKind::USE);
        let use_declaration = self.parse_use_inner();
        self.eat_semicolon();

        use_declaration
    }

    fn parse_use_inner(&mut self) -> Arc<Use> {
        let start = self.token.span.start();
        let mut path = Vec::new();
        let mut allow_brace = false;

        loop {
            if self.is(TokenKind::L_BRACE) {
                allow_brace = true;
                break;
            }

            let component = self.parse_use_path_component();
            path.push(component);

            if self.is(TokenKind::COLON_COLON) {
                self.eat(TokenKind::COLON_COLON);
            } else {
                break;
            }
        }

        let target = if allow_brace && self.is(TokenKind::L_BRACE) {
            self.parse_use_brace()
        } else if self.is(TokenKind::AS) {
            UseTargetDescriptor::As(self.parse_use_as())
        } else {
            UseTargetDescriptor::Default
        };

        let span = self.span_from(start);

        Arc::new(Use {
            id: self.generate_id(),
            span,
            common_path: path,
            target,
        })
    }

    fn parse_use_as(&mut self) -> UseTargetName {
        self.eat(TokenKind::AS);

        let start = self.token.span.start();

        let name = if self.is(TokenKind::UNDERSCORE) {
            self.eat(TokenKind::UNDERSCORE);
            None
        } else {
            self.eat_identifier()
        };

        let span = self.span_from(start);
        UseTargetName { span, name }
    }

    fn parse_use_path_component(&mut self) -> UsePathComponent {
        let start = self.token.span.start();

        let value = if self.is(TokenKind::THIS) {
            self.eat(TokenKind::THIS);
            UsePathComponentValue::This
        } else if self.is(TokenKind::PACKAGE) {
            self.eat(TokenKind::PACKAGE);
            UsePathComponentValue::Package
        } else if self.is(TokenKind::SUPER) {
            self.eat(TokenKind::SUPER);
            UsePathComponentValue::Super
        } else {
            let name = self.eat_identifier();
            if let Some(name) = name {
                UsePathComponentValue::Name(name)
            } else {
                UsePathComponentValue::Error
            }
        };

        let span = self.span_from(start);

        UsePathComponent { span, value }
    }

    fn parse_use_brace(&mut self) -> UseTargetDescriptor {
        let start = self.token.span.start();
        self.eat(TokenKind::L_BRACE);

        let targets = self.parse_list(TokenKind::COMMA, TokenKind::R_BRACE, |p| {
            p.parse_use_inner()
        });

        let span = self.span_from(start);

        UseTargetDescriptor::Group(UseTargetGroup { span, targets })
    }

    fn parse_enum(&mut self, modifiers: &Modifiers) -> Arc<Enum> {
        let start = self.token.span.start();
        self.expect(TokenKind::ENUM);
        let name = self.eat_identifier();
        let type_params = self.parse_type_params();

        self.eat(TokenKind::L_BRACE);
        let variants = self.parse_list(TokenKind::COMMA, TokenKind::R_BRACE, |p| {
            p.parse_enum_variant()
        });
        let span = self.span_from(start);

        Arc::new(Enum {
            id: self.generate_id(),
            span,
            name,
            type_params,
            variants,
            visibility: Visibility::from_modifiers(modifiers),
        })
    }

    fn parse_module(&mut self, modifiers: &Modifiers) -> Arc<Module> {
        let start = self.token.span.start();
        self.expect(TokenKind::MOD);
        let name = self.eat_identifier();

        let elements = if self.is(TokenKind::L_BRACE) {
            self.eat(TokenKind::L_BRACE);

            let mut elements = Vec::new();

            while !self.is(TokenKind::R_BRACE) && !self.is_eof() {
                elements.push(self.parse_top_level_element());
            }

            self.eat(TokenKind::R_BRACE);
            Some(elements)
        } else {
            self.eat(TokenKind::SEMICOLON);
            None
        };

        let span = self.span_from(start);

        Arc::new(Module {
            id: self.generate_id(),
            span,
            name,
            elements,
            visibility: Visibility::from_modifiers(modifiers),
        })
    }

    fn parse_enum_variant(&mut self) -> EnumVariant {
        let start = self.token.span.start();
        let name = self.eat_identifier();

        let types = if self.is(TokenKind::L_PAREN) {
            self.advance_token();
            Some(self.parse_list(TokenKind::COMMA, TokenKind::R_PAREN, |p| p.parse_type()))
        } else {
            None
        };

        let span = self.span_from(start);

        EnumVariant {
            id: self.generate_id(),
            span,
            name,
            types,
        }
    }

    fn parse_const(&mut self, modifiers: &Modifiers) -> Arc<Const> {
        let start = self.token.span.start();
        self.expect(TokenKind::CONST);
        let name = self.eat_identifier();
        self.eat(TokenKind::COLON);
        let ty = self.parse_type();
        self.eat(TokenKind::EQ);
        let expr = self.parse_expression();
        self.eat_semicolon();
        let span = self.span_from(start);

        Arc::new(Const {
            id: self.generate_id(),
            span,
            name,
            data_type: ty,
            expr,
            visibility: Visibility::from_modifiers(modifiers),
        })
    }

    fn parse_impl(&mut self) -> Arc<Impl> {
        let start = self.token.span.start();
        self.expect(TokenKind::IMPL);
        let type_params = self.parse_type_params();

        let type_name = self.parse_type();

        let (class_type, trait_type) = if self.is(TokenKind::FOR) {
            self.advance_token();
            let class_type = self.parse_type();

            (class_type, Some(type_name))
        } else {
            (type_name, None)
        };

        self.eat(TokenKind::L_BRACE);

        let mut methods = Vec::new();

        while !self.is(TokenKind::R_BRACE) {
            let modifiers = self.parse_annotation_usages();
            let mods = &[Annotation::Static, Annotation::Internal, Annotation::Pub];
            self.restrict_modifiers(&modifiers, mods);

            let method = self.parse_function(&modifiers);
            methods.push(method);
        }

        self.eat(TokenKind::R_BRACE);
        let span = self.span_from(start);

        Arc::new(Impl {
            id: self.generate_id(),
            span,
            type_params,
            trait_type,
            extended_type: class_type,
            methods,
        })
    }

    fn parse_global(&mut self, modifiers: &Modifiers) -> Arc<Global> {
        let start = self.token.span.start();
        self.expect(TokenKind::LET);

        let mutable = if self.is(TokenKind::MUT) {
            self.expect(TokenKind::MUT);
            true
        } else {
            false
        };

        let name = self.eat_identifier();

        self.eat(TokenKind::COLON);
        let data_type = self.parse_type();

        let expr = if self.is(TokenKind::EQ) {
            self.advance_token();
            Some(self.parse_expression())
        } else {
            None
        };

        self.eat_semicolon();
        let span = self.span_from(start);

        Arc::new(Global {
            id: self.generate_id(),
            name,
            span,
            data_type,
            mutable,
            visibility: Visibility::from_modifiers(modifiers),
            initial_value: expr.clone(),
        })
    }

    fn parse_trait(&mut self, modifiers: &Modifiers) -> Arc<Trait> {
        let start = self.token.span.start();
        self.expect(TokenKind::TRAIT);
        let name = self.eat_identifier();
        let type_params = self.parse_type_params();

        self.eat(TokenKind::L_BRACE);

        let mut methods = Vec::new();

        while !self.is(TokenKind::R_BRACE) && !self.is_eof() {
            let modifiers = self.parse_annotation_usages();
            let mods = &[Annotation::Static];
            self.restrict_modifiers(&modifiers, mods);

            let method = self.parse_function(&modifiers);
            methods.push(method);
        }

        self.eat(TokenKind::R_BRACE);
        let span = self.span_from(start);

        Arc::new(Trait {
            id: self.generate_id(),
            name,
            type_params,
            span,
            methods,
            visibility: Visibility::from_modifiers(modifiers),
        })
    }

    fn parse_struct(&mut self, modifiers: &Modifiers) -> Arc<Struct> {
        let start = self.token.span.start();
        self.expect(TokenKind::STRUCT);
        let ident = self.eat_identifier();
        let type_params = self.parse_type_params();

        let fields = if self.is(TokenKind::L_PAREN) {
            self.eat(TokenKind::L_PAREN);
            self.parse_list(TokenKind::COMMA, TokenKind::R_PAREN, |p| {
                p.parse_struct_field()
            })
        } else if self.is(TokenKind::L_BRACE) {
            self.eat(TokenKind::L_BRACE);
            self.parse_list(TokenKind::COMMA, TokenKind::R_BRACE, |p| {
                p.parse_struct_field()
            })
        } else {
            Vec::new()
        };

        let span = self.span_from(start);

        Arc::new(Struct {
            id: self.generate_id(),
            name: ident,
            span,
            fields,
            visibility: Visibility::from_modifiers(modifiers),
            internal: modifiers.contains(Annotation::Internal),
            type_params,
        })
    }

    fn parse_struct_field(&mut self) -> StructField {
        let start = self.token.span.start();

        let modifiers = self.parse_annotation_usages();
        let mods = &[Annotation::Pub];
        self.restrict_modifiers(&modifiers, mods);

        let ident = self.eat_identifier();

        self.eat(TokenKind::COLON);
        let ty = self.parse_type();
        let span = self.span_from(start);

        StructField {
            id: self.generate_id(),
            name: ident,
            span,
            data_type: ty,
            visibility: Visibility::from_modifiers(&modifiers),
        }
    }

    fn parse_class(&mut self, modifiers: &Modifiers) -> Arc<Class> {
        let start = self.token.span.start();
        self.expect(TokenKind::CLASS);

        let name = self.eat_identifier();
        let type_params = self.parse_type_params();

        let fields = if self.is(TokenKind::L_PAREN) {
            self.expect(TokenKind::L_PAREN);
            self.parse_list(TokenKind::COMMA, TokenKind::R_PAREN, |p| {
                p.parse_class_field()
            })
        } else if self.is(TokenKind::L_BRACE) {
            self.expect(TokenKind::L_BRACE);
            self.parse_list(TokenKind::COMMA, TokenKind::R_BRACE, |p| {
                p.parse_class_field()
            })
        } else {
            Vec::new()
        };

        let span = self.span_from(start);

        Arc::new(Class {
            id: self.generate_id(),
            span,
            name,
            internal: modifiers.contains(Annotation::Internal),
            visibility: Visibility::from_modifiers(modifiers),
            fields,
            type_params,
        })
    }

    fn parse_class_field(&mut self) -> Field {
        let start = self.token.span.start();

        let modifiers = self.parse_annotation_usages();
        let mods = &[Annotation::Pub];
        self.restrict_modifiers(&modifiers, mods);

        let name = self.eat_identifier();

        self.eat(TokenKind::COLON);
        let data_type = self.parse_type();
        let span = self.span_from(start);

        Field {
            id: self.generate_id(),
            name,
            span,
            data_type,
            primary_ctor: false,
            expr: None,
            mutable: true,
            visibility: Visibility::from_modifiers(&modifiers),
        }
    }

    fn parse_alias(&mut self, modifiers: &Modifiers) -> Arc<Alias> {
        let start = self.token.span.start();
        self.expect(TokenKind::ALIAS);
        let name = self.eat_identifier();
        self.eat(TokenKind::EQ);
        let ty = self.parse_type();
        self.eat_semicolon();
        let span = self.span_from(start);

        Arc::new(Alias {
            id: self.generate_id(),
            name,
            span,
            ty,
            visibility: Visibility::from_modifiers(modifiers),
        })
    }

    fn parse_type_params(&mut self) -> Option<Vec<TypeParam>> {
        if self.is(TokenKind::L_BRACKET) {
            self.advance_token();
            let params = self.parse_list(TokenKind::COMMA, TokenKind::R_BRACKET, |p| {
                p.parse_type_param()
            });

            Some(params)
        } else {
            None
        }
    }

    fn parse_type_param(&mut self) -> TypeParam {
        let start = self.token.span.start();
        let name = self.eat_identifier();

        let bounds = if self.is(TokenKind::COLON) {
            self.advance_token();

            let mut bounds = Vec::new();

            loop {
                bounds.push(self.parse_type());

                if self.is(TokenKind::ADD) {
                    self.advance_token();
                } else {
                    break;
                }
            }

            bounds
        } else {
            Vec::new()
        };

        let span = self.span_from(start);

        TypeParam { name, span, bounds }
    }

    fn parse_annotation_usages(&mut self) -> Modifiers {
        let mut modifiers = Modifiers::new();
        loop {
            let modifier = self.parse_annotation_usage();

            if modifier.is_none() {
                break;
            }

            let modifier = modifier.unwrap();

            if modifiers.contains(modifier) {
                self.report_error(ParseError::RedundantAnnotation(modifier.name().into()));
                continue;
            }

            modifiers.add(modifier, self.token.span);
        }

        modifiers
    }

    fn parse_annotation_usage(&mut self) -> Option<Annotation> {
        if self.is(TokenKind::PUB) {
            self.advance_token();
            Some(Annotation::Pub)
        } else if self.is(TokenKind::STATIC) {
            self.advance_token();
            Some(Annotation::Static)
        } else {
            if !self.is(TokenKind::AT) {
                return None;
            }
            self.advance_token();

            if self.is(TokenKind::PUB) {
                self.advance_token();
                return Some(Annotation::Pub);
            } else if self.is(TokenKind::STATIC) {
                self.advance_token();
                return Some(Annotation::Static);
            }

            let name = self.eat_identifier();
            let annotation = if let Some(name) = &name {
                match self.interner.str(name.name).as_str() {
                    "internal" => Annotation::Internal,
                    "pub" => Annotation::Pub,
                    "static" => Annotation::Static,
                    "Test" => Annotation::Test,
                    "optimizeImmediately" => Annotation::OptimizeImmediately,
                    annotation => {
                        self.report_error(ParseError::UnknownAnnotation(annotation.into()));
                        Annotation::Error
                    }
                }
            } else {
                Annotation::Error
            };

            Some(annotation)
        }
    }

    fn ban_modifiers(&mut self, modifiers: &Modifiers) {
        self.restrict_modifiers(modifiers, &[]);
    }

    fn restrict_modifiers(&mut self, modifiers: &Modifiers, restrict: &[Annotation]) {
        for modifier in modifiers.iter() {
            if !restrict.contains(&modifier.value) {
                self.report_error_at(
                    ParseError::MisplacedAnnotation(modifier.value.name().into()),
                    modifier.span,
                );
            }
        }
    }

    fn parse_function(&mut self, modifiers: &Modifiers) -> Arc<Function> {
        let start = self.token.span.start();
        self.expect(TokenKind::FN);
        let name = self.eat_identifier();
        let type_params = self.parse_type_params();
        let params = self.parse_function_params();
        let return_type = self.parse_function_type();
        let block = self.parse_function_block();
        let span = self.span_from(start);

        Arc::new(Function {
            id: self.generate_id(),
            kind: FunctionKind::Function,
            name,
            span,
            is_optimize_immediately: modifiers.contains(Annotation::OptimizeImmediately),
            visibility: Visibility::from_modifiers(modifiers),
            is_static: modifiers.contains(Annotation::Static),
            internal: modifiers.contains(Annotation::Internal),
            is_constructor: false,
            is_test: modifiers.contains(Annotation::Test),
            params,
            return_type,
            block,
            type_params,
        })
    }

    fn parse_function_params(&mut self) -> Vec<Param> {
        if self.is(TokenKind::L_PAREN) {
            self.expect(TokenKind::L_PAREN);

            self.parse_list(TokenKind::COMMA, TokenKind::R_PAREN, |p| {
                p.parse_function_param()
            })
        } else {
            assert!(!self.eat(TokenKind::L_PAREN));
            Vec::new()
        }
    }

    fn parse_list<F, R>(&mut self, sep: TokenKind, stop: TokenKind, mut parse: F) -> Vec<R>
    where
        F: FnMut(&mut Parser) -> R,
    {
        let mut data = vec![];
        let mut comma = true;

        while !self.is(stop.clone()) && !self.is_eof() {
            if !comma {
                self.report_error(ParseError::ExpectedToken(
                    sep.name().into(),
                    self.token.name(),
                ));

                break;
            }

            let entry = parse(self);
            data.push(entry);

            comma = self.is(sep.clone());
            if comma {
                self.advance_token();
            }
        }

        self.eat(stop);

        data
    }

    fn parse_function_param(&mut self) -> Param {
        let start = self.token.span.start();

        let mutable = if self.is(TokenKind::MUT) {
            self.expect(TokenKind::MUT);
            true
        } else {
            false
        };

        let name = self.eat_identifier();

        self.eat(TokenKind::COLON);

        let data_type = self.parse_type();

        let variadic = if self.is(TokenKind::DOT_DOT_DOT) {
            self.advance_token();
            true
        } else {
            false
        };

        let span = self.span_from(start);

        Param {
            id: self.generate_id(),
            variadic,
            name,
            span,
            mutable,
            data_type,
        }
    }

    fn parse_function_type(&mut self) -> Option<Type> {
        if self.is(TokenKind::COLON) {
            self.expect(TokenKind::COLON);
            let ty = self.parse_type();

            Some(ty)
        } else {
            None
        }
    }

    fn parse_function_block(&mut self) -> Option<Expr> {
        if self.is(TokenKind::SEMICOLON) {
            self.expect(TokenKind::SEMICOLON);

            None
        } else {
            let block = self.parse_block();
            Some(block)
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.current() {
            TokenKind::CAPITAL_THIS => {
                let span = self.token.span;
                self.advance_token();
                Arc::new(TypeData::create_self(self.generate_id(), span))
            }

            TokenKind::IDENTIFIER => {
                let start = self.token.span.start();
                let path = self.parse_path();

                let params = if self.is(TokenKind::L_BRACKET) {
                    self.expect(TokenKind::L_BRACKET);
                    self.parse_list(TokenKind::COMMA, TokenKind::R_BRACKET, |p| p.parse_type())
                } else {
                    Vec::new()
                };

                let span = self.span_from(start);
                Arc::new(TypeData::create_basic(
                    self.generate_id(),
                    span,
                    path,
                    params,
                ))
            }

            TokenKind::L_PAREN => {
                let start = self.token.span.start();
                self.expect(TokenKind::L_PAREN);
                let subtypes =
                    self.parse_list(TokenKind::COMMA, TokenKind::R_PAREN, |p| p.parse_type());

                if self.is(TokenKind::COLON) {
                    self.expect(TokenKind::COLON);
                    let ret = self.parse_type();
                    let span = self.span_from(start);

                    Arc::new(TypeData::create_fct(
                        self.generate_id(),
                        span,
                        subtypes,
                        Some(ret),
                    ))
                } else {
                    let span = self.span_from(start);
                    Arc::new(TypeData::create_tuple(self.generate_id(), span, subtypes))
                }
            }

            _ => {
                let span = self.token.span;
                self.report_error(ParseError::ExpectedType(self.token.name()));
                Arc::new(TypeData::Error {
                    id: self.generate_id(),
                    span,
                })
            }
        }
    }

    fn parse_path(&mut self) -> Path {
        let start = self.token.span.start();
        let mut names = Vec::new();
        assert!(self.token.is_identifier());
        let name = self.eat_identifier();
        if let Some(name) = name {
            names.push(name);
        }

        while self.is(TokenKind::COLON_COLON) {
            self.advance_token();
            let name = self.eat_identifier();
            if let Some(name) = name {
                names.push(name);
            } else {
                break;
            }
        }

        let span = self.span_from(start);

        Arc::new(PathData {
            id: self.generate_id(),
            span,
            names,
        })
    }

    #[cfg(test)]
    fn parse_statement(&mut self) -> Stmt {
        let stmt_or_expr = self.parse_statement_or_expression();

        match stmt_or_expr {
            StmtOrExpr::Stmt(stmt) => stmt,
            StmtOrExpr::Expr(expr) => {
                if expr.needs_semicolon() {
                    self.eat_semicolon();
                }

                Arc::new(StmtData::create_expr(self.generate_id(), expr.span(), expr))
            }
        }
    }

    fn parse_let(&mut self) -> Stmt {
        let start = self.token.span.start();

        self.expect(TokenKind::LET);
        let pattern = self.parse_let_pattern();
        let data_type = self.parse_var_type();
        let expr = self.parse_var_assignment();

        self.eat_semicolon();
        let span = self.span_from(start);

        Arc::new(StmtData::create_let(
            self.generate_id(),
            span,
            pattern,
            data_type,
            expr,
        ))
    }

    fn parse_let_pattern(&mut self) -> Box<LetPattern> {
        if self.is(TokenKind::L_PAREN) {
            let start = self.token.span.start();
            self.expect(TokenKind::L_PAREN);

            let parts = self.parse_list(TokenKind::COMMA, TokenKind::R_PAREN, |p| {
                p.parse_let_pattern()
            });

            let span = self.span_from(start);

            Box::new(LetPattern::Tuple(LetTupleType {
                id: self.generate_id(),
                span,
                parts,
            }))
        } else if self.is(TokenKind::UNDERSCORE) {
            let span = self.token.span;
            self.expect(TokenKind::UNDERSCORE);

            Box::new(LetPattern::Underscore(LetUnderscoreType {
                id: self.generate_id(),
                span,
            }))
        } else {
            let start = self.token.span.start();
            let mutable = if self.is(TokenKind::MUT) {
                self.advance_token();
                true
            } else {
                false
            };
            let name = self.eat_identifier();
            let span = self.span_from(start);

            Box::new(LetPattern::Ident(LetIdentType {
                id: self.generate_id(),
                span,
                mutable,
                name,
            }))
        }
    }

    fn parse_var_type(&mut self) -> Option<Type> {
        if self.is(TokenKind::COLON) {
            self.expect(TokenKind::COLON);
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn parse_var_assignment(&mut self) -> Option<Expr> {
        if self.is(TokenKind::EQ) {
            self.expect(TokenKind::EQ);
            let expr = self.parse_expression();

            Some(expr)
        } else {
            None
        }
    }

    fn parse_block_stmt(&mut self) -> Stmt {
        let block = self.parse_block();
        Arc::new(StmtData::create_expr(
            self.generate_id(),
            block.span(),
            block,
        ))
    }

    fn parse_block(&mut self) -> Expr {
        let start = self.token.span.start();
        self.expect(TokenKind::L_BRACE);
        let mut stmts = vec![];
        let mut expr = None;

        while !self.is(TokenKind::R_BRACE) && !self.is_eof() {
            let stmt_or_expr = self.parse_statement_or_expression();

            match stmt_or_expr {
                StmtOrExpr::Stmt(stmt) => stmts.push(stmt),
                StmtOrExpr::Expr(curr_expr) => {
                    if curr_expr.needs_semicolon() {
                        expr = Some(curr_expr);
                        break;
                    } else if !self.is(TokenKind::R_BRACE) {
                        stmts.push(Arc::new(StmtData::create_expr(
                            self.generate_id(),
                            curr_expr.span(),
                            curr_expr,
                        )));
                    } else {
                        expr = Some(curr_expr);
                    }
                }
            }
        }

        self.eat(TokenKind::R_BRACE);
        let span = self.span_from(start);

        Arc::new(ExprData::create_block(
            self.generate_id(),
            span,
            stmts,
            expr,
        ))
    }

    fn parse_statement_or_expression(&mut self) -> StmtOrExpr {
        match self.current() {
            TokenKind::LET => StmtOrExpr::Stmt(self.parse_let()),
            TokenKind::WHILE => StmtOrExpr::Stmt(self.parse_while()),
            TokenKind::BREAK => StmtOrExpr::Stmt(self.parse_break()),
            TokenKind::CONTINUE => StmtOrExpr::Stmt(self.parse_continue()),
            TokenKind::RETURN => StmtOrExpr::Stmt(self.parse_return()),
            TokenKind::ELSE => {
                let span = self.token.span;
                self.report_error(ParseError::MisplacedElse);
                StmtOrExpr::Expr(Arc::new(ExprData::Error {
                    id: self.generate_id(),
                    span,
                }))
            }
            TokenKind::FOR => StmtOrExpr::Stmt(self.parse_for()),
            _ => {
                let expr = self.parse_expression();

                if self.is(TokenKind::SEMICOLON) {
                    self.eat(TokenKind::SEMICOLON);
                    let span = self.span_from(expr.span().start());

                    StmtOrExpr::Stmt(Arc::new(StmtData::create_expr(
                        self.generate_id(),
                        span,
                        expr,
                    )))
                } else {
                    StmtOrExpr::Expr(expr)
                }
            }
        }
    }

    fn parse_if(&mut self) -> Expr {
        let start = self.token.span.start();
        self.expect(TokenKind::IF);

        let cond = self.parse_expression();

        let then_block = self.parse_block();

        let else_block = if self.is(TokenKind::ELSE) {
            self.advance_token();

            if self.is(TokenKind::IF) {
                Some(self.parse_if())
            } else {
                Some(self.parse_block())
            }
        } else {
            None
        };

        let span = self.span_from(start);

        Arc::new(ExprData::create_if(
            self.generate_id(),
            span,
            cond,
            then_block,
            else_block,
        ))
    }

    fn parse_match(&mut self) -> Expr {
        let start = self.token.span.start();
        self.expect(TokenKind::MATCH);

        let expr = self.parse_expression();
        let mut cases = Vec::new();
        let mut comma = true;

        self.eat(TokenKind::L_BRACE);

        while !self.is(TokenKind::R_BRACE) && !self.is_eof() {
            if !comma {
                self.report_error(ParseError::ExpectedToken(
                    TokenKind::COMMA.name().into(),
                    self.token.name(),
                ));
                break;
            }

            let case = self.parse_match_case();
            cases.push(case);

            comma = self.is(TokenKind::COMMA);

            if comma {
                self.advance_token();
            }
        }

        self.eat(TokenKind::R_BRACE);
        let span = self.span_from(start);

        Arc::new(ExprData::create_match(
            self.generate_id(),
            span,
            expr,
            cases,
        ))
    }

    fn parse_match_case(&mut self) -> MatchCaseType {
        let start = self.token.span.start();
        let mut patterns = Vec::new();
        patterns.push(self.parse_match_pattern());

        while self.is(TokenKind::OR) {
            self.advance_token();
            patterns.push(self.parse_match_pattern());
        }

        self.eat(TokenKind::DOUBLE_ARROW);

        let value = self.parse_expression();
        let span = self.span_from(start);

        MatchCaseType {
            id: self.generate_id(),
            span,
            patterns,
            value,
        }
    }

    fn parse_match_pattern(&mut self) -> MatchPattern {
        let start = self.token.span.start();

        let data = if self.is(TokenKind::UNDERSCORE) {
            self.eat(TokenKind::UNDERSCORE);
            MatchPatternData::Underscore
        } else {
            let path = self.parse_path();

            let params = if self.is(TokenKind::L_PAREN) {
                self.eat(TokenKind::L_PAREN);
                let params = self.parse_list(TokenKind::COMMA, TokenKind::R_PAREN, |this| {
                    this.parse_match_pattern_param()
                });

                Some(params)
            } else {
                None
            };

            MatchPatternData::Ident(MatchPatternIdent { path, params })
        };

        let span = self.span_from(start);

        MatchPattern {
            id: self.generate_id(),
            span,
            data,
        }
    }

    fn parse_match_pattern_param(&mut self) -> MatchPatternParam {
        let start = self.token.span.start();

        let (mutable, name) = if self.is(TokenKind::UNDERSCORE) {
            self.expect(TokenKind::UNDERSCORE);

            (false, None)
        } else {
            let mutable = if self.is(TokenKind::MUT) {
                self.eat(TokenKind::MUT);
                true
            } else {
                false
            };

            let ident = self.eat_identifier();

            (mutable, ident)
        };

        let span = self.span_from(start);

        MatchPatternParam {
            id: self.generate_id(),
            span,
            mutable,
            name,
        }
    }

    fn parse_for(&mut self) -> Stmt {
        let start = self.token.span.start();
        self.expect(TokenKind::FOR);
        let pattern = self.parse_let_pattern();
        self.eat(TokenKind::IN);
        let expr = self.parse_expression();
        let block = self.parse_block_stmt();
        let span = self.span_from(start);

        Arc::new(StmtData::create_for(
            self.generate_id(),
            span,
            pattern,
            expr,
            block,
        ))
    }

    fn parse_while(&mut self) -> Stmt {
        let start = self.token.span.start();
        self.expect(TokenKind::WHILE);
        let expr = self.parse_expression();
        let block = self.parse_block_stmt();
        let span = self.span_from(start);

        Arc::new(StmtData::create_while(
            self.generate_id(),
            span,
            expr,
            block,
        ))
    }

    fn parse_break(&mut self) -> Stmt {
        let start = self.token.span.start();
        self.expect(TokenKind::BREAK);
        self.eat_semicolon();
        let span = self.span_from(start);

        Arc::new(StmtData::create_break(self.generate_id(), span))
    }

    fn parse_continue(&mut self) -> Stmt {
        let start = self.token.span.start();
        self.expect(TokenKind::CONTINUE);
        self.eat_semicolon();
        let span = self.span_from(start);

        Arc::new(StmtData::create_continue(self.generate_id(), span))
    }

    fn parse_return(&mut self) -> Stmt {
        let start = self.token.span.start();
        self.expect(TokenKind::RETURN);
        let expr = if self.is(TokenKind::SEMICOLON) {
            None
        } else {
            let expr = self.parse_expression();
            Some(expr)
        };

        self.eat_semicolon();
        let span = self.span_from(start);

        Arc::new(StmtData::create_return(self.generate_id(), span, expr))
    }

    fn parse_expression(&mut self) -> Expr {
        if !self.is_set(EXPRESSION_FIRST) {
            self.report_error(ParseError::ExpectedExpression);
            return Arc::new(ExprData::Error {
                id: self.generate_id(),
                span: self.token.span,
            });
        }

        match self.current() {
            TokenKind::L_BRACE => self.parse_block(),
            TokenKind::IF => self.parse_if(),
            TokenKind::MATCH => self.parse_match(),
            _ => self.parse_binary(0),
        }
    }

    fn parse_binary(&mut self, precedence: u32) -> Expr {
        let start = self.token.span.start();
        let mut left = self.parse_unary();

        loop {
            let right_precedence = match self.current() {
                TokenKind::EQ => 1,
                TokenKind::OR_OR => 2,
                TokenKind::AND_AND => 3,
                TokenKind::EQ_EQ
                | TokenKind::NOT_EQ
                | TokenKind::LT
                | TokenKind::LE
                | TokenKind::GT
                | TokenKind::GE
                | TokenKind::EQ_EQ_EQ
                | TokenKind::NOT_EQ_EQ => 4,
                TokenKind::ADD | TokenKind::SUB | TokenKind::OR | TokenKind::CARET => 5,
                TokenKind::MUL
                | TokenKind::DIV
                | TokenKind::MODULO
                | TokenKind::AND
                | TokenKind::LT_LT
                | TokenKind::GT_GT
                | TokenKind::GT_GT_GT => 6,
                TokenKind::AS => 7,
                _ => {
                    return left;
                }
            };

            if precedence >= right_precedence {
                return left;
            }

            let tok = self.advance_token();

            left = match tok.kind {
                TokenKind::AS => {
                    let right = self.parse_type();
                    let span = self.span_from(start);
                    let expr = ExprData::create_conv(self.generate_id(), span, left, right);

                    Arc::new(expr)
                }

                _ => {
                    let right = self.parse_binary(right_precedence);
                    self.create_binary(tok, start, left, right)
                }
            };
        }
    }

    fn parse_unary(&mut self) -> Expr {
        match self.current() {
            TokenKind::ADD | TokenKind::SUB | TokenKind::NOT => {
                let start = self.token.span.start();
                let tok = self.advance_token();
                let op = match tok.kind {
                    TokenKind::ADD => UnOp::Plus,
                    TokenKind::SUB => UnOp::Neg,
                    TokenKind::NOT => UnOp::Not,
                    _ => unreachable!(),
                };

                let expr = self.parse_primary();
                let span = self.span_from(start);
                Arc::new(ExprData::create_un(self.generate_id(), span, op, expr))
            }

            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Expr {
        let start = self.token.span.start();
        let mut left = self.parse_factor();

        loop {
            left = match self.current() {
                TokenKind::DOT => {
                    let op_span = self.advance_token().span;
                    let rhs = self.parse_factor();
                    let span = self.span_from(start);

                    Arc::new(ExprData::create_dot(
                        self.generate_id(),
                        span,
                        op_span,
                        left,
                        rhs,
                    ))
                }

                TokenKind::L_PAREN => {
                    self.advance_token();
                    let args = self.parse_list(TokenKind::COMMA, TokenKind::R_PAREN, |p| {
                        p.parse_expression()
                    });
                    let span = self.span_from(start);

                    Arc::new(ExprData::create_call(self.generate_id(), span, left, args))
                }

                TokenKind::L_BRACKET => {
                    let op_span = self.advance_token().span;
                    let types =
                        self.parse_list(TokenKind::COMMA, TokenKind::R_BRACKET, |p| p.parse_type());
                    let span = self.span_from(start);

                    Arc::new(ExprData::create_type_param(
                        self.generate_id(),
                        span,
                        op_span,
                        left,
                        types,
                    ))
                }

                TokenKind::COLON_COLON => {
                    let op_span = self.advance_token().span;
                    let rhs = self.parse_factor();
                    let span = self.span_from(start);

                    Arc::new(ExprData::create_path(
                        self.generate_id(),
                        span,
                        op_span,
                        left,
                        rhs,
                    ))
                }

                _ => {
                    return left;
                }
            }
        }
    }

    fn create_binary(&mut self, tok: Token, start: u32, left: Expr, right: Expr) -> Expr {
        let op = match tok.kind {
            TokenKind::EQ => BinOp::Assign,
            TokenKind::OR_OR => BinOp::Or,
            TokenKind::AND_AND => BinOp::And,
            TokenKind::EQ_EQ => BinOp::Cmp(CmpOp::Eq),
            TokenKind::NOT_EQ => BinOp::Cmp(CmpOp::Ne),
            TokenKind::LT => BinOp::Cmp(CmpOp::Lt),
            TokenKind::LE => BinOp::Cmp(CmpOp::Le),
            TokenKind::GT => BinOp::Cmp(CmpOp::Gt),
            TokenKind::GE => BinOp::Cmp(CmpOp::Ge),
            TokenKind::EQ_EQ_EQ => BinOp::Cmp(CmpOp::Is),
            TokenKind::NOT_EQ_EQ => BinOp::Cmp(CmpOp::IsNot),
            TokenKind::OR => BinOp::BitOr,
            TokenKind::AND => BinOp::BitAnd,
            TokenKind::CARET => BinOp::BitXor,
            TokenKind::ADD => BinOp::Add,
            TokenKind::SUB => BinOp::Sub,
            TokenKind::MUL => BinOp::Mul,
            TokenKind::DIV => BinOp::Div,
            TokenKind::MODULO => BinOp::Mod,
            TokenKind::LT_LT => BinOp::ShiftL,
            TokenKind::GT_GT => BinOp::ArithShiftR,
            TokenKind::GT_GT_GT => BinOp::LogicalShiftR,
            _ => panic!("unimplemented token {:?}", tok),
        };

        let span = self.span_from(start);

        Arc::new(ExprData::create_bin(
            self.generate_id(),
            span,
            op,
            left,
            right,
        ))
    }

    fn parse_factor(&mut self) -> Expr {
        let span = self.token.span;
        match self.current() {
            TokenKind::L_PAREN => self.parse_parentheses(),
            TokenKind::L_BRACE => self.parse_block(),
            TokenKind::IF => self.parse_if(),
            TokenKind::CHAR_LITERAL => self.parse_lit_char(),
            TokenKind::INT_LITERAL => self.parse_lit_int(),
            TokenKind::FLOAT_LITERAL => self.parse_lit_float(),
            TokenKind::STRING_LITERAL | TokenKind::TEMPLATE_LITERAL => self.parse_string(),
            TokenKind::IDENTIFIER => self.parse_identifier(),
            TokenKind::TRUE => self.parse_bool_literal(),
            TokenKind::FALSE => self.parse_bool_literal(),
            TokenKind::THIS => self.parse_this(),
            TokenKind::OR | TokenKind::OR_OR => self.parse_lambda(),
            _ => {
                self.report_error(ParseError::ExpectedFactor(self.token.name().clone()));
                Arc::new(ExprData::Error {
                    id: self.generate_id(),
                    span,
                })
            }
        }
    }

    fn parse_identifier(&mut self) -> Expr {
        let ident = self.eat_identifier().expect("identifier expected");

        Arc::new(ExprData::create_ident(
            self.generate_id(),
            ident.span,
            ident.name,
        ))
    }

    fn parse_parentheses(&mut self) -> Expr {
        let start = self.token.span.start();
        self.eat(TokenKind::L_PAREN);

        if self.is(TokenKind::R_PAREN) {
            self.advance_token();
            let span = self.span_from(start);
            return Arc::new(ExprData::create_tuple(self.generate_id(), span, Vec::new()));
        }

        let expr = self.parse_expression();

        if self.current() == TokenKind::COMMA {
            let mut values = vec![expr];
            let span;

            loop {
                self.eat(TokenKind::COMMA);

                if self.current() == TokenKind::R_PAREN {
                    self.advance_token();
                    span = self.span_from(start);
                    break;
                }

                let expr = self.parse_expression();
                values.push(expr);

                if self.current() == TokenKind::R_PAREN {
                    self.advance_token();
                    span = self.span_from(start);
                    break;
                }
            }

            Arc::new(ExprData::create_tuple(self.generate_id(), span, values))
        } else {
            self.eat(TokenKind::R_PAREN);
            let span = self.span_from(start);

            Arc::new(ExprData::create_paren(self.generate_id(), span, expr))
        }
    }

    fn parse_lit_char(&mut self) -> Expr {
        let span = self.token.span;
        let tok = self.advance_token();
        let value = self.source_span(span);

        if let TokenKind::CHAR_LITERAL = tok.kind {
            Arc::new(ExprData::create_lit_char(self.generate_id(), span, value))
        } else {
            unreachable!();
        }
    }

    fn parse_lit_int(&mut self) -> Expr {
        assert_eq!(self.current(), TokenKind::INT_LITERAL);
        let span = self.token.span;
        let value = self.source_span(span);
        self.advance_token();

        let expr = ExprData::create_lit_int(self.generate_id(), span, value);
        Arc::new(expr)
    }

    fn parse_lit_float(&mut self) -> Expr {
        assert_eq!(self.current(), TokenKind::FLOAT_LITERAL);
        let span = self.token.span;
        self.advance_token();

        let value = self.source_span(span);

        let expr = ExprData::create_lit_float(self.generate_id(), span, value);
        Arc::new(expr)
    }

    fn parse_string(&mut self) -> Expr {
        let span = self.token.span;
        let string = self.advance_token();
        let value = self.source_span(span);

        match string.kind {
            TokenKind::STRING_LITERAL => {
                Arc::new(ExprData::create_lit_str(self.generate_id(), span, value))
            }

            TokenKind::TEMPLATE_LITERAL => {
                let start = self.token.span.start();
                let mut parts: Vec<Expr> = Vec::new();
                parts.push(Arc::new(ExprData::create_lit_str(
                    self.generate_id(),
                    span,
                    value,
                )));

                loop {
                    let expr = self.parse_expression();
                    parts.push(expr);

                    let span = self.token.span;
                    let value = self.source_span(span);

                    let (value, finished) = match self.current() {
                        TokenKind::STRING_LITERAL => (value, true),
                        TokenKind::TEMPLATE_LITERAL => (value, false),
                        _ => {
                            self.report_error(ParseError::UnclosedStringTemplate);
                            (String::new(), true)
                        }
                    };

                    parts.push(Arc::new(ExprData::create_lit_str(
                        self.generate_id(),
                        span,
                        value,
                    )));

                    self.advance_token();

                    if finished {
                        break;
                    }
                }

                let span = self.span_from(start);

                Arc::new(ExprData::create_template(self.generate_id(), span, parts))
            }

            _ => unreachable!(),
        }
    }

    fn parse_bool_literal(&mut self) -> Expr {
        let span = self.token.span;
        let tok = self.advance_token();
        let value = tok.is(TokenKind::TRUE);

        Arc::new(ExprData::create_lit_bool(self.generate_id(), span, value))
    }

    fn parse_this(&mut self) -> Expr {
        let span = self.token.span;
        self.advance_token();

        Arc::new(ExprData::create_this(self.generate_id(), span))
    }

    fn parse_lambda(&mut self) -> Expr {
        let start = self.token.span.start();
        let tok = self.advance_token();

        let params = if tok.kind == TokenKind::OR_OR {
            // nothing to do
            Vec::new()
        } else {
            self.parse_list(TokenKind::COMMA, TokenKind::OR, |p| {
                p.parse_function_param()
            })
        };

        let return_type = if self.is(TokenKind::COLON) {
            self.advance_token();
            Some(self.parse_type())
        } else {
            None
        };

        let block = self.parse_block();

        let span = self.span_from(start);

        let name = self.interner.intern("closure");

        let function = Arc::new(Function {
            id: self.generate_id(),
            kind: FunctionKind::Lambda,
            name: Some(Arc::new(IdentData { span: span, name })),
            span,
            is_optimize_immediately: false,
            visibility: Visibility::Default,
            is_static: false,
            internal: false,
            is_constructor: false,
            is_test: false,
            params,
            return_type,
            block: Some(block),
            type_params: None,
        });

        Arc::new(ExprData::create_lambda(function))
    }

    fn expect(&mut self, kind: TokenKind) {
        assert!(self.eat(kind));
    }

    fn eat_identifier(&mut self) -> Option<Ident> {
        let token = self.advance_token();

        if let TokenKind::IDENTIFIER = token.kind {
            let span = token.span;
            let value = self.source_span(token.span);
            let name = self.interner.intern(&value);

            Some(Arc::new(IdentData { span, name }))
        } else {
            self.report_error_at(ParseError::ExpectedIdentifier(token.name()), token.span);
            None
        }
    }

    fn eat_semicolon(&mut self) -> bool {
        self.eat(TokenKind::SEMICOLON)
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.current() == kind {
            self.advance_token();
            true
        } else {
            self.report_error(ParseError::ExpectedToken(
                kind.name().into(),
                self.token.name(),
            ));
            false
        }
    }

    fn report_error(&mut self, msg: ParseError) {
        self.report_error_at(msg, self.token.span);
    }

    fn report_error_at(&mut self, msg: ParseError, span: Span) {
        self.errors.push(ParseErrorWithLocation::new(span, msg));
    }

    fn error_and_advance(&mut self, msg: ParseError) {
        self.errors
            .push(ParseErrorWithLocation::new(self.token.span, msg));
        self.advance_token();
    }

    fn advance_token(&mut self) -> Token {
        if self.token_idx < self.tokens.len() {
            self.token_idx += 1;
        }

        self.setup_token()
    }

    fn current(&self) -> TokenKind {
        if self.token_idx < self.tokens.len() {
            self.tokens[self.token_idx].kind
        } else {
            TokenKind::EOF
        }
    }

    fn is(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    fn is_set(&self, set: TokenSet) -> bool {
        set.contains(self.current())
    }

    fn is_eof(&self) -> bool {
        self.current() == TokenKind::EOF
    }

    fn setup_token(&mut self) -> Token {
        let token = if self.token_idx == self.tokens.len() {
            let span = Span::new(self.content.len().try_into().expect("overflow"), 0);
            Token::new(TokenKind::EOF, span)
        } else {
            self.tokens[self.token_idx].clone()
        };

        mem::replace(&mut self.token, token)
    }

    fn source_span(&self, span: Span) -> String {
        let start = span.start() as usize;
        let end = span.end() as usize;
        String::from(&self.content[start..end])
    }

    fn span_from(&self, start: u32) -> Span {
        let last_token_end = self.tokens[self.token_idx - 1].span.end();
        Span::new(start, last_token_end - start)
    }
}

#[derive(Debug)]
pub struct NodeIdGenerator {
    value: RefCell<usize>,
}

impl NodeIdGenerator {
    pub fn new() -> NodeIdGenerator {
        NodeIdGenerator {
            value: RefCell::new(1),
        }
    }

    pub fn next(&self) -> NodeId {
        let value = *self.value.borrow();
        *self.value.borrow_mut() += 1;

        NodeId(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::interner::*;

    use crate::error::ParseError;
    use crate::parser::Parser;
    use crate::{compute_line_column, compute_line_starts};

    fn parse_expr(code: &'static str) -> (Expr, Interner) {
        let mut interner = Interner::new();

        let expr = {
            let mut parser = Parser::from_string(code, &mut interner);

            let result = parser.parse_expression();
            assert!(parser.errors.is_empty());

            result
        };

        (expr, interner)
    }

    fn err_expr(code: &'static str, msg: ParseError, line: u32, col: u32) {
        let mut interner = Interner::new();
        let mut parser = Parser::from_string(code, &mut interner);

        let _expr = parser.parse_expression();

        let errors = parser.errors;
        assert_eq!(errors.len(), 1);
        let err = &errors[0];

        assert_eq!(msg, err.error);

        let line_starts = compute_line_starts(code);
        let (computed_line, computed_column) = compute_line_column(&line_starts, err.span.start());
        assert_eq!(line, computed_line);
        assert_eq!(col, computed_column);
    }

    fn parse_stmt(code: &'static str) -> Stmt {
        let mut interner = Interner::new();
        let mut parser = Parser::from_string(code, &mut interner);
        let result = parser.parse_statement();
        assert!(parser.errors.is_empty());
        result
    }

    fn err_stmt(code: &'static str, msg: ParseError, line: u32, col: u32) {
        let mut interner = Interner::new();
        let mut parser = Parser::from_string(code, &mut interner);

        let _ = parser.parse_statement();

        let errors = parser.errors.clone();
        assert_eq!(errors.len(), 1);
        let err = &errors[0];

        assert_eq!(msg, err.error);
        println!("error = {}", err.span);
        let line_starts = compute_line_starts(code);
        let (computed_line, computed_column) = compute_line_column(&line_starts, err.span.start());
        assert_eq!(line, computed_line);
        assert_eq!(col, computed_column);
    }

    fn parse_type(code: &'static str) -> (Type, Interner) {
        let mut interner = Interner::new();
        let ty = {
            let mut parser = Parser::from_string(code, &mut interner);
            parser.parse_type()
        };

        (ty, interner)
    }

    fn parse(code: &'static str) -> (File, Interner) {
        let mut interner = Interner::new();

        let (file, _id_generator, errors) = Parser::from_string(code, &mut interner).parse();
        assert!(errors.is_empty());

        (file, interner)
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
        assert_eq!(String::from("10"), lit.value);
    }

    #[test]
    fn parse_number_with_underscore() {
        let (expr, _) = parse_expr("1____0");

        let lit = expr.to_lit_int().unwrap();
        assert_eq!(String::from("1____0"), lit.value);
    }

    #[test]
    fn parse_string() {
        let (expr, _) = parse_expr("\"abc\"");

        let lit = expr.to_lit_str().unwrap();
        assert_eq!("\"abc\"", &lit.value);
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
        let dot = expr.to_dot().unwrap();

        let ident = dot.lhs.to_ident().unwrap();
        assert_eq!("obj", *interner.str(ident.name));

        let ident = dot.rhs.to_ident().unwrap();
        assert_eq!("field", *interner.str(ident.name));
    }

    #[test]
    fn parse_field_negated() {
        let (expr, _) = parse_expr("-obj.field");
        assert!(expr.to_un().unwrap().opnd.is_dot());
    }

    #[test]
    fn parse_field_non_ident() {
        let (expr, interner) = parse_expr("bar.12");
        let dot = expr.to_dot().unwrap();

        let ident = dot.lhs.to_ident().unwrap();
        assert_eq!("bar", *interner.str(ident.name));

        assert_eq!(String::from("12"), dot.rhs.to_lit_int().unwrap().value);
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
    fn parse_neg_twice() {
        let (expr, _) = parse_expr("-(-3)");

        let neg1 = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg1.op);

        let neg2 = neg1.opnd.to_paren().unwrap().expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg2.op);

        assert!(neg2.opnd.is_lit_int());
    }

    #[test]
    fn parse_neg_twice_without_parentheses() {
        err_expr("- -2", ParseError::ExpectedFactor("-".into()), 1, 3);
    }

    #[test]
    fn parse_unary_plus() {
        let (expr, _) = parse_expr("+2");

        let add = expr.to_un().unwrap();
        assert_eq!(UnOp::Plus, add.op);

        assert!(add.opnd.is_lit_int());
    }

    #[test]
    fn parse_unary_plus_twice_without_parentheses() {
        err_expr("+ +4", ParseError::ExpectedFactor("+".into()), 1, 3);
    }

    #[test]
    fn parse_unary_plus_twice() {
        let (expr, _) = parse_expr("+(+9)");

        let add1 = expr.to_un().unwrap();
        assert_eq!(UnOp::Plus, add1.op);

        let add2 = add1.opnd.to_paren().unwrap().expr.to_un().unwrap();
        assert_eq!(UnOp::Plus, add2.op);
        assert!(add2.opnd.is_lit_int());
    }

    #[test]
    fn parse_mul() {
        let (expr, _) = parse_expr("6*3");

        let mul = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul.op);
        assert_eq!(String::from("6"), mul.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), mul.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_multiple_muls() {
        let (expr, _) = parse_expr("6*3*4");

        let mul1 = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul1.op);

        let mul2 = mul1.lhs.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul2.op);
        assert_eq!(String::from("6"), mul2.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), mul2.rhs.to_lit_int().unwrap().value);

        assert_eq!(String::from("4"), mul1.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_div() {
        let (expr, _) = parse_expr("4/5");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Div, div.op);
        assert_eq!(String::from("4"), div.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("5"), div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_mod() {
        let (expr, _) = parse_expr("2%15");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mod, div.op);
        assert_eq!(String::from("2"), div.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("15"), div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add() {
        let (expr, _) = parse_expr("2+3");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Add, add.op);
        assert_eq!(String::from("2"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add_left_associativity() {
        let (expr, _) = parse_expr("1+2+3");

        let add = expr.to_bin().unwrap();
        assert_eq!(String::from("3"), add.rhs.to_lit_int().unwrap().value);

        let lhs = add.lhs.to_bin().unwrap();
        assert_eq!(String::from("1"), lhs.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), lhs.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add_right_associativity_via_parens() {
        let (expr, _) = parse_expr("1+(2+3)");

        let add = expr.to_bin().unwrap();
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);

        let rhs = add.rhs.to_paren().unwrap().expr.to_bin().unwrap();
        assert_eq!(String::from("2"), rhs.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), rhs.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_sub() {
        let (expr, _) = parse_expr("1-2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Sub, add.op);
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_or() {
        let (expr, _) = parse_expr("1||2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Or, add.op);
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_and() {
        let (expr, _) = parse_expr("1&&2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::And, add.op);
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_or() {
        let (expr, _) = parse_expr("1|2");

        let or = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitOr, or.op);
        assert_eq!(String::from("1"), or.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), or.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_and() {
        let (expr, _) = parse_expr("1&2");

        let and = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitAnd, and.op);
        assert_eq!(String::from("1"), and.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), and.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_xor() {
        let (expr, _) = parse_expr("1^2");

        let xor = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitXor, xor.op);
        assert_eq!(String::from("1"), xor.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), xor.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_lt() {
        let (expr, _) = parse_expr("1<2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Lt), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_le() {
        let (expr, _) = parse_expr("1<=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Le), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_gt() {
        let (expr, _) = parse_expr("1>2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Gt), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_ge() {
        let (expr, _) = parse_expr("1>=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Ge), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_eq() {
        let (expr, _) = parse_expr("1==2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Eq), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_ne() {
        let (expr, _) = parse_expr("1!=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Ne), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_is_not() {
        let (expr, _) = parse_expr("1!==2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::IsNot), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_is() {
        let (expr, _) = parse_expr("1===2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Is), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_assign() {
        let (expr, _) = parse_expr("a=4");

        let assign = expr.to_bin().unwrap();
        assert!(assign.lhs.is_ident());
        assert_eq!(BinOp::Assign, assign.op);
        assert_eq!(String::from("4"), assign.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_shift_right() {
        let (expr, _) = parse_expr("a>>4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::ArithShiftR, bin.op);
    }

    #[test]
    fn parse_unsigned_shift_right() {
        let (expr, _) = parse_expr("a>>>4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::LogicalShiftR, bin.op);
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
        assert_eq!("fname", *interner.str(call.callee.to_ident().unwrap().name));
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_call_with_params() {
        let (expr, interner) = parse_expr("fname2(1,2,3)");

        let call = expr.to_call().unwrap();
        assert_eq!(
            "fname2",
            *interner.str(call.callee.to_ident().unwrap().name)
        );
        assert_eq!(3, call.args.len());
    }

    #[test]
    fn parse_function() {
        let (prog, interner) = parse("fn b() { }");
        let fct = prog.fct0();

        assert_eq!("b", *interner.str(fct.name.as_ref().unwrap().name));
        assert_eq!(0, fct.params.len());
        assert!(fct.return_type.is_none());
    }

    #[test]
    fn parse_function_with_single_param() {
        let (p1, interner1) = parse("fn f(a:int) { }");
        let f1 = p1.fct0();

        let (p2, interner2) = parse("fn f(a:int,) { }");
        let f2 = p2.fct0();

        assert_eq!(f1.params.len(), 1);
        assert_eq!(f2.params.len(), 1);

        let p1 = &f1.params[0];
        let p2 = &f2.params[0];

        assert_eq!("a", *interner1.str(p1.name.as_ref().unwrap().name));
        assert_eq!("a", *interner2.str(p2.name.as_ref().unwrap().name));

        assert_eq!(
            "int",
            *interner1.str(p1.data_type.to_basic().unwrap().name())
        );
        assert_eq!(
            "int",
            *interner2.str(p2.data_type.to_basic().unwrap().name())
        );
    }

    #[test]
    fn parse_function_with_multiple_params() {
        let (p1, interner1) = parse("fn f(a:int, b:str) { }");
        let f1 = p1.fct0();

        let (p2, interner2) = parse("fn f(a:int, b:str,) { }");
        let f2 = p2.fct0();

        let p1a = &f1.params[0];
        let p1b = &f1.params[1];
        let p2a = &f2.params[0];
        let p2b = &f2.params[1];

        assert_eq!("a", *interner1.str(p1a.name.as_ref().unwrap().name));
        assert_eq!("a", *interner2.str(p2a.name.as_ref().unwrap().name));

        assert_eq!("b", *interner1.str(p1b.name.as_ref().unwrap().name));
        assert_eq!("b", *interner2.str(p2b.name.as_ref().unwrap().name));

        assert_eq!(
            "int",
            *interner1.str(p1a.data_type.to_basic().unwrap().name())
        );
        assert_eq!(
            "int",
            *interner2.str(p2a.data_type.to_basic().unwrap().name())
        );

        assert_eq!(
            "str",
            *interner1.str(p1b.data_type.to_basic().unwrap().name())
        );
        assert_eq!(
            "str",
            *interner2.str(p2b.data_type.to_basic().unwrap().name())
        );
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
    fn parse_let_underscore() {
        let stmt = parse_stmt("let _ = 1;");
        let let_decl = stmt.to_let().unwrap();

        assert!(let_decl.pattern.is_underscore());
    }

    #[test]
    fn parse_let_tuple() {
        let stmt = parse_stmt("let (mut a, b, (c, d)) = 1;");
        let let_decl = stmt.to_let().unwrap();

        assert!(let_decl.pattern.is_tuple());
        let tuple = let_decl.pattern.to_tuple().unwrap();
        let first = tuple.parts.first().unwrap();
        assert!(first.is_ident());
        assert!(first.to_ident().unwrap().mutable);
        assert!(tuple.parts.last().unwrap().is_tuple());
    }

    #[test]
    fn parse_let_ident() {
        let stmt = parse_stmt("let x = 1;");
        let let_decl = stmt.to_let().unwrap();

        assert!(let_decl.pattern.is_ident());
    }

    #[test]
    fn parse_let_ident_mut() {
        let stmt = parse_stmt("let mut x = 1;");
        let let_decl = stmt.to_let().unwrap();

        assert!(let_decl.pattern.is_ident());
        assert!(let_decl.pattern.to_ident().unwrap().mutable);
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
    }

    #[test]
    fn parse_multiple_functions() {
        let (prog, interner) = parse("fn f() { } fn g() { }");

        let f = prog.fct0();
        assert_eq!("f", *interner.str(f.name.as_ref().unwrap().name));

        let g = prog.fct(1);
        assert_eq!("g", *interner.str(g.name.as_ref().unwrap().name));
    }

    #[test]
    fn parse_expr_stmt() {
        let stmt = parse_stmt("1;");
        let expr = stmt.to_expr().unwrap();

        assert!(expr.expr.is_lit_int());
    }

    #[test]
    fn parse_expr_stmt_without_semicolon() {
        err_stmt(
            "1",
            ParseError::ExpectedToken(";".into(), "<<EOF>>".into()),
            1,
            2,
        );
    }

    #[test]
    fn parse_if() {
        let (expr, _) = parse_expr("if true { 2; } else { 3; }");
        let ifexpr = expr.to_if().unwrap();

        assert!(ifexpr.cond.is_lit_bool());
        assert!(ifexpr.else_block.is_some());
    }

    #[test]
    fn parse_if_without_else() {
        let (expr, _) = parse_expr("if true { 2; }");
        let ifexpr = expr.to_if().unwrap();

        assert!(ifexpr.cond.is_lit_bool());
        assert!(ifexpr.else_block.is_none());
    }

    #[test]
    fn parse_while() {
        let stmt = parse_stmt("while true { 2; }");
        let whilestmt = stmt.to_while().unwrap();

        assert!(whilestmt.cond.is_lit_bool());
        assert!(whilestmt.block.is_expr());
    }

    #[test]
    fn parse_empty_block() {
        let (expr, _) = parse_expr("{}");
        let block = expr.to_block().unwrap();

        assert_eq!(0, block.stmts.len());
    }

    #[test]
    fn parse_block_with_one_stmt() {
        let (expr, _) = parse_expr("{ 1; 2 }");
        let block = expr.to_block().unwrap();

        assert_eq!(1, block.stmts.len());

        let expr = &block.stmts[0].to_expr().unwrap().expr;
        assert_eq!(String::from("1"), expr.to_lit_int().unwrap().value);

        assert_eq!(
            String::from("2"),
            block.expr.as_ref().unwrap().to_lit_int().unwrap().value
        );
    }

    #[test]
    fn parse_block_with_multiple_stmts() {
        let (expr, _) = parse_expr("{ 1; 2; }");
        let block = expr.to_block().unwrap();

        assert_eq!(2, block.stmts.len());

        let expr = &block.stmts[0].to_expr().unwrap().expr;
        assert_eq!(String::from("1"), expr.to_lit_int().unwrap().value);

        let expr = &block.stmts[1].to_expr().unwrap().expr;
        assert_eq!(String::from("2"), expr.to_lit_int().unwrap().value);

        assert!(block.expr.is_none());
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

        assert_eq!(
            String::from("1"),
            ret.expr.as_ref().unwrap().to_lit_int().unwrap().value
        );
    }

    #[test]
    fn parse_return() {
        let stmt = parse_stmt("return;");
        let ret = stmt.to_return().unwrap();

        assert!(ret.expr.is_none());
    }

    #[test]
    fn parse_type_basic() {
        let (ty, interner) = parse_type("bla");
        let basic = ty.to_basic().unwrap();

        assert_eq!(0, basic.params.len());
        assert_eq!("bla", *interner.str(basic.name()));
    }

    #[test]
    fn parse_type_basic_mod() {
        let (ty, interner) = parse_type("foo::bla");
        let basic = ty.to_basic().unwrap();

        assert_eq!(0, basic.params.len());
        assert_eq!(2, basic.path.names.len());
        assert_eq!("foo", *interner.str(basic.path.names[0].name));
        assert_eq!("bla", *interner.str(basic.path.names[1].name));
    }

    #[test]
    fn parse_type_basic_with_params() {
        let (ty, interner) = parse_type("Foo[A, B]");
        let basic = ty.to_basic().unwrap();

        assert_eq!(2, basic.params.len());
        assert_eq!("Foo", *interner.str(basic.name()));
        assert_eq!(
            "A",
            *interner.str(basic.params[0].to_basic().unwrap().name())
        );
        assert_eq!(
            "B",
            *interner.str(basic.params[1].to_basic().unwrap().name())
        );
    }

    #[test]
    fn parse_type_lambda_no_params() {
        let (ty, _) = parse_type("(): ()");
        let fct = ty.to_fct().unwrap();

        assert_eq!(0, fct.params.len());
        assert!(fct.ret.as_ref().unwrap().is_unit());
    }

    #[test]
    fn parse_type_lambda_one_param() {
        let (ty, interner) = parse_type("(A): B");
        let fct = ty.to_fct().unwrap();

        assert_eq!(1, fct.params.len());
        assert_eq!("A", *interner.str(fct.params[0].to_basic().unwrap().name()));
        assert_eq!(
            "B",
            *interner.str(fct.ret.as_ref().unwrap().to_basic().unwrap().name())
        );
    }

    #[test]
    fn parse_type_lambda_two_params() {
        let (ty, interner) = parse_type("(A, B): C");
        let fct = ty.to_fct().unwrap();

        assert_eq!(2, fct.params.len());
        assert_eq!("A", *interner.str(fct.params[0].to_basic().unwrap().name()));
        assert_eq!("B", *interner.str(fct.params[1].to_basic().unwrap().name()));
        assert_eq!(
            "C",
            *interner.str(fct.ret.as_ref().unwrap().to_basic().unwrap().name())
        );
    }

    #[test]
    fn parse_type_unit() {
        let (ty, _) = parse_type("()");
        let ty = ty.to_tuple().unwrap();

        assert!(ty.subtypes.is_empty());
    }

    #[test]
    fn parse_type_tuple_with_one_type() {
        let (ty, interner) = parse_type("(c)");

        let subtypes = &ty.to_tuple().unwrap().subtypes;
        assert_eq!(1, subtypes.len());

        let ty = subtypes[0].to_basic().unwrap();
        assert_eq!("c", *interner.str(ty.name()));
    }

    #[test]
    fn parse_type_tuple_with_two_types() {
        let (ty, interner) = parse_type("(a, b)");

        let subtypes = &ty.to_tuple().unwrap().subtypes;
        assert_eq!(2, subtypes.len());

        let ty1 = subtypes[0].to_basic().unwrap();
        assert_eq!("a", *interner.str(ty1.name()));

        let ty2 = subtypes[1].to_basic().unwrap();
        assert_eq!("b", *interner.str(ty2.name()));
    }

    #[test]
    fn parse_class_with_param() {
        let (prog, _) = parse("class Foo(a: int)");
        let class = prog.cls0();
        assert_eq!(1, class.fields.len());
    }

    #[test]
    fn parse_class_with_param_var() {
        let (prog, _) = parse("class Foo(a: int)");
        let class = prog.cls0();

        assert_eq!(1, class.fields.len());
        assert_eq!(true, class.fields[0].mutable);
    }

    #[test]
    fn parse_class_with_params() {
        let (prog, _) = parse("class Foo(a: int, b: int)");
        let class = prog.cls0();

        assert_eq!(2, class.fields.len());
    }

    #[test]
    fn parse_class() {
        let (prog, _) = parse("class Foo { a: Int64, b: Bool }");
        let class = prog.cls0();
        assert_eq!(class.fields.len(), 2);

        let (prog, _) = parse("class Foo(a: Int64, b: Bool)");
        let class = prog.cls0();
        assert_eq!(class.fields.len(), 2);

        let (prog, _) = parse("class Foo");
        let class = prog.cls0();
        assert!(class.fields.is_empty());
    }

    #[test]
    fn parse_method_invocation() {
        let (expr, _) = parse_expr("a.foo()");
        let call = expr.to_call().unwrap();
        assert!(call.callee.is_dot());
        assert_eq!(0, call.args.len());

        let (expr, _) = parse_expr("a.foo(1)");
        let call = expr.to_call().unwrap();
        assert!(call.callee.is_dot());
        assert_eq!(1, call.args.len());

        let (expr, _) = parse_expr("a.foo(1,2)");
        let call = expr.to_call().unwrap();
        assert!(call.callee.is_dot());
        assert_eq!(2, call.args.len());
    }

    #[test]
    fn parse_array_index() {
        let (expr, interner) = parse_expr("a(b)");
        let call = expr.to_call().unwrap();
        assert_eq!("a", *interner.str(call.callee.to_ident().unwrap().name));
        assert_eq!(1, call.args.len());
        assert_eq!("b", *interner.str(call.args[0].to_ident().unwrap().name));
    }

    #[test]
    fn parse_field() {
        let (prog, interner) = parse("class A { f1: int, f2: int }");
        let cls = prog.cls0();

        let f1 = &cls.fields[0];
        assert_eq!(
            "f1",
            &interner.str(f1.name.as_ref().unwrap().name).to_string()
        );
        assert_eq!(true, f1.mutable);

        let f2 = &cls.fields[1];
        assert_eq!(
            "f2",
            &interner.str(f2.name.as_ref().unwrap().name).to_string()
        );
        assert_eq!(true, f2.mutable);
    }

    #[test]
    fn parse_as_expr() {
        let (expr, _) = parse_expr("a as String");
        let expr = expr.to_conv().unwrap();
        assert_eq!(true, expr.object.is_ident());
    }

    #[test]
    fn parse_internal() {
        let (prog, _) = parse("@internal fn foo();");
        let fct = prog.fct0();
        assert!(fct.internal);
    }

    #[test]
    fn parse_function_without_body() {
        let (prog, _) = parse("fn foo();");
        let fct = prog.fct0();
        assert!(fct.block.is_none());
    }

    #[test]
    fn parse_struct_empty() {
        let (prog, interner) = parse("struct Foo {}");
        let struc = prog.struct0();
        assert_eq!(0, struc.fields.len());
        assert_eq!("Foo", *interner.str(struc.name.as_ref().unwrap().name));
    }

    #[test]
    fn parse_struct_one_field() {
        let (prog, interner) = parse(
            "struct Bar {
            f1: Foo1,
        }",
        );
        let struc = prog.struct0();
        assert_eq!(1, struc.fields.len());
        assert_eq!("Bar", *interner.str(struc.name.as_ref().unwrap().name));

        let f1 = &struc.fields[0];
        assert_eq!("f1", *interner.str(f1.name.as_ref().unwrap().name));
    }

    #[test]
    fn parse_struct_multiple_fields() {
        let (prog, interner) = parse(
            "struct FooBar {
            fa: Foo1,
            fb: Foo2,
        }",
        );
        let struc = prog.struct0();
        assert_eq!(2, struc.fields.len());
        assert_eq!("FooBar", *interner.str(struc.name.as_ref().unwrap().name));

        let f1 = &struc.fields[0];
        assert_eq!("fa", *interner.str(f1.name.as_ref().unwrap().name));

        let f2 = &struc.fields[1];
        assert_eq!("fb", *interner.str(f2.name.as_ref().unwrap().name));
    }

    #[test]
    fn parse_struct_with_type_params() {
        let (prog, interner) = parse(
            "struct Bar[T1, T2] {
            f1: T1, f2: T2,
        }",
        );
        let struct_ = prog.struct0();
        assert_eq!(2, struct_.fields.len());
        assert_eq!("Bar", *interner.str(struct_.name.as_ref().unwrap().name));

        assert_eq!(2, struct_.type_params.as_ref().unwrap().len());
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
        let (expr, _) = parse_expr("if i < n { }");
        let ifexpr = expr.to_if().unwrap();
        let bin = ifexpr.cond.to_bin().unwrap();

        assert!(bin.lhs.is_ident());
        assert!(bin.rhs.is_ident());
    }

    #[test]
    fn parse_lit_float() {
        let (expr, _) = parse_expr("1.2");

        let lit = expr.to_lit_float().unwrap();

        assert_eq!("1.2", lit.value);
    }

    #[test]
    fn parse_template() {
        let (expr, _) = parse_expr("\"a${1}b${2}c\"");
        let tmpl = expr.to_template().unwrap();
        assert_eq!(tmpl.parts.len(), 5);

        assert_eq!(
            "\"a${".to_string(),
            tmpl.parts[0].to_lit_str().unwrap().value
        );
        assert_eq!(String::from("1"), tmpl.parts[1].to_lit_int().unwrap().value);
        assert_eq!(
            "}b${".to_string(),
            tmpl.parts[2].to_lit_str().unwrap().value
        );
        assert_eq!(String::from("2"), tmpl.parts[3].to_lit_int().unwrap().value);
        assert_eq!(
            "}c\"".to_string(),
            tmpl.parts[4].to_lit_str().unwrap().value
        );

        let (expr, _) = parse_expr("\"a\\${1}b\"");
        assert!(expr.is_lit_str());
    }

    #[test]
    fn parse_class_type_params() {
        let (prog, interner) = parse("class Foo[T]");
        let cls = prog.cls0();

        let type_params = cls.type_params.as_ref().unwrap();
        assert_eq!(1, type_params.len());
        assert_eq!(
            "T",
            *interner.str(type_params[0].name.as_ref().unwrap().name)
        );

        let (prog, interner) = parse("class Foo[X]");
        let cls = prog.cls0();

        let type_params = cls.type_params.as_ref().unwrap();
        assert_eq!(1, type_params.len());
        assert_eq!(
            "X",
            *interner.str(type_params[0].name.as_ref().unwrap().name)
        );
    }

    #[test]
    fn parse_multiple_class_type_params() {
        let (prog, interner) = parse("class Foo[A, B]");
        let cls = prog.cls0();

        let type_params = cls.type_params.as_ref().unwrap();
        assert_eq!(2, type_params.len());
        assert_eq!(
            "A",
            *interner.str(type_params[0].name.as_ref().unwrap().name)
        );
        assert_eq!(
            "B",
            *interner.str(type_params[1].name.as_ref().unwrap().name)
        );
    }

    #[test]
    fn parse_empty_trait() {
        let (prog, interner) = parse("trait Foo { }");
        let trait_ = prog.trait0();

        assert_eq!("Foo", *interner.str(trait_.name.as_ref().unwrap().name));
        assert_eq!(0, trait_.methods.len());
    }

    #[test]
    fn parse_trait_with_function() {
        let (prog, interner) = parse("trait Foo { fn empty(); }");
        let trait_ = prog.trait0();

        assert_eq!("Foo", *interner.str(trait_.name.as_ref().unwrap().name));
        assert_eq!(1, trait_.methods.len());
        assert_eq!(false, trait_.methods[0].is_static);
    }

    #[test]
    fn parse_trait_with_static_function() {
        let (prog, interner) = parse("trait Foo { @static fn empty(); }");
        let trait_ = prog.trait0();

        assert_eq!("Foo", *interner.str(trait_.name.as_ref().unwrap().name));
        assert_eq!(1, trait_.methods.len());
        assert_eq!(true, trait_.methods[0].is_static);
    }

    #[test]
    fn parse_empty_impl() {
        let (prog, interner) = parse("impl Foo for A {}");
        let impl_ = prog.impl0();

        assert_eq!(
            "Foo",
            impl_.trait_type.as_ref().unwrap().to_string(&interner)
        );
        assert_eq!("A", impl_.extended_type.to_string(&interner));
        assert_eq!(0, impl_.methods.len());
    }

    #[test]
    fn parse_impl_with_function() {
        let (prog, interner) = parse("impl Bar for B { fn foo(); }");
        let impl_ = prog.impl0();

        assert_eq!(
            "Bar",
            impl_.trait_type.as_ref().unwrap().to_string(&interner)
        );
        assert_eq!("B", impl_.extended_type.to_string(&interner));
        assert_eq!(1, impl_.methods.len());
        assert_eq!(false, impl_.methods[0].is_static);
    }

    #[test]
    fn parse_impl_with_static_function() {
        let (prog, interner) = parse("impl Bar for B { @static fn foo(); }");
        let impl_ = prog.impl0();

        assert_eq!(
            "Bar",
            impl_.trait_type.as_ref().unwrap().to_string(&interner)
        );
        assert_eq!("B", impl_.extended_type.to_string(&interner));
        assert_eq!(1, impl_.methods.len());
        assert_eq!(true, impl_.methods[0].is_static);
    }

    #[test]
    fn parse_global_let() {
        let (prog, interner) = parse("let b: int = 0;");
        let global = prog.global0();

        assert_eq!("b", *interner.str(global.name.as_ref().unwrap().name));
        assert_eq!(false, global.mutable);
    }

    #[test]
    fn parse_lit_char() {
        let (expr, _) = parse_expr("'a'");
        let lit = expr.to_lit_char().unwrap();

        assert_eq!("'a'", lit.value);
    }

    #[test]
    fn parse_fct_call_with_type_param() {
        let (expr, _) = parse_expr("Array[Int]()");
        let call = expr.to_call().unwrap();
        let type_params = call.callee.to_type_param().unwrap();

        assert_eq!(1, type_params.args.len());

        let (expr, _) = parse_expr("Foo[Int, Long]()");
        let call = expr.to_call().unwrap();
        let type_params = call.callee.to_type_param().unwrap();

        assert_eq!(2, type_params.args.len());

        let (expr, _) = parse_expr("Bar[]()");
        let call = expr.to_call().unwrap();
        let type_params = call.callee.to_type_param().unwrap();

        assert_eq!(0, type_params.args.len());

        let (expr, _) = parse_expr("Vec()");
        let call = expr.to_call().unwrap();

        assert!(call.callee.is_ident());
    }

    #[test]
    fn parse_call_with_path() {
        let (expr, _) = parse_expr("Foo::get()");
        let call = expr.to_call().unwrap();

        assert!(call.callee.is_path());
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_fct_with_type_params() {
        let (prog, _) = parse("fn f[T]() {}");
        let fct = prog.fct0();

        assert_eq!(1, fct.type_params.as_ref().unwrap().len());
    }

    #[test]
    fn parse_const() {
        let (prog, interner) = parse("const x: int = 0;");
        let const_ = prog.const0();

        assert_eq!("x", *interner.str(const_.name.as_ref().unwrap().name));
    }

    #[test]
    fn parse_generic_with_bound() {
        let (prog, _) = parse("class A[T: Foo]");
        let cls = prog.cls0();

        let type_param = &cls.type_params.as_ref().unwrap()[0];
        assert_eq!(1, type_param.bounds.len());
    }

    #[test]
    fn parse_generic_with_multiple_bounds() {
        let (prog, _) = parse("class A[T: Foo + Bar]");
        let cls = prog.cls0();

        let type_param = &cls.type_params.as_ref().unwrap()[0];
        assert_eq!(2, type_param.bounds.len());
    }

    #[test]
    fn parse_lambda_no_params_no_return_value() {
        let (expr, _) = parse_expr("|| {}");
        let lambda = expr.to_lambda().unwrap();

        assert!(lambda.return_type.is_none());
    }

    #[test]
    fn parse_lambda_no_params_unit_as_return_value() {
        let (expr, _) = parse_expr("|| : () {}");
        let lambda = expr.to_lambda().unwrap();
        let ret = lambda.return_type.as_ref().unwrap();

        assert!(ret.is_unit());
    }

    #[test]
    fn parse_lambda_no_params_with_return_value() {
        let (expr, interner) = parse_expr("||: A {}");
        let lambda = expr.to_lambda().unwrap();
        let ret = lambda.return_type.as_ref().unwrap();
        let basic = ret.to_basic().unwrap();

        assert_eq!("A", *interner.str(basic.name()));
    }

    #[test]
    fn parse_lambda_with_one_param() {
        let (expr, interner) = parse_expr("|a: A|: B {}");
        let lambda = expr.to_lambda().unwrap();

        assert_eq!(1, lambda.params.len());

        let param = &lambda.params[0];
        assert_eq!("a", *interner.str(param.name.as_ref().unwrap().name));
        let basic = param.data_type.to_basic().unwrap();
        assert_eq!("A", *interner.str(basic.name()));

        let ret = lambda.return_type.as_ref().unwrap();
        let basic = ret.to_basic().unwrap();

        assert_eq!("B", *interner.str(basic.name()));
    }

    #[test]
    fn parse_lambda_with_two_params() {
        let (expr, interner) = parse_expr("|a: A, b: B|: C {}");
        let lambda = expr.to_lambda().unwrap();

        assert_eq!(2, lambda.params.len());

        let param = &lambda.params[0];
        assert_eq!("a", *interner.str(param.name.as_ref().unwrap().name));
        let basic = param.data_type.to_basic().unwrap();
        assert_eq!("A", *interner.str(basic.name()));

        let param = &lambda.params[1];
        assert_eq!("b", *interner.str(param.name.as_ref().unwrap().name));
        let basic = param.data_type.to_basic().unwrap();
        assert_eq!("B", *interner.str(basic.name()));

        let ret = lambda.return_type.as_ref().unwrap();
        let basic = ret.to_basic().unwrap();

        assert_eq!("C", *interner.str(basic.name()));
    }

    #[test]
    fn parse_for() {
        let stmt = parse_stmt("for i in a+b {}");
        assert!(stmt.is_for());
    }

    #[test]
    fn parse_new_call_ident() {
        let (expr, _interner) = parse_expr("i");
        assert!(expr.is_ident());
    }

    #[test]
    fn parse_new_call_path() {
        let (expr, _interner) = parse_expr("Foo::bar");
        let path = expr.to_path().unwrap();
        assert!(path.lhs.is_ident());
        assert!(path.rhs.is_ident());
    }

    #[test]
    fn parse_new_call_call() {
        let (expr, _interner) = parse_expr("foo(1,2)");
        let call = expr.to_call().unwrap();
        assert!(call.callee.is_ident());
        assert_eq!(call.args.len(), 2);
    }

    #[test]
    fn parse_block() {
        let (expr, _) = parse_expr("{1}");
        assert!(expr.to_block().unwrap().expr.as_ref().unwrap().is_lit_int());

        let (expr, _) = parse_expr("({}) + 1");
        assert!(expr.is_bin());

        let (expr, _) = parse_expr("1 + {}");
        assert!(expr.is_bin());
    }

    #[test]
    fn parse_tuple() {
        let (expr, _) = parse_expr("(1,)");
        assert_eq!(expr.to_tuple().unwrap().values.len(), 1);

        let (expr, _) = parse_expr("(1)");
        assert!(expr.is_paren());

        let (expr, _) = parse_expr("(1,2,3)");
        assert_eq!(expr.to_tuple().unwrap().values.len(), 3);

        let (expr, _) = parse_expr("(1,2,3,4,)");
        assert_eq!(expr.to_tuple().unwrap().values.len(), 4);
    }

    #[test]
    fn parse_enum() {
        let (prog, _) = parse("enum Foo { A, B, C }");
        let enum_ = prog.enum0();
        assert_eq!(enum_.variants.len(), 3);
    }

    #[test]
    fn parse_enum_with_type_params() {
        let (prog, _) = parse("enum MyOption[T] { None, Some(T), }");
        let enum_ = prog.enum0();
        assert_eq!(enum_.variants.len(), 2);
        assert!(enum_.variants[0].types.is_none());
        assert_eq!(enum_.variants[1].types.as_ref().unwrap().len(), 1);
    }

    #[test]
    fn parse_alias() {
        let (prog, _) = parse("alias NewType = Int;");
        let _alias = prog.alias0();
    }

    #[test]
    fn parse_module() {
        let (prog, _) = parse("mod foo { fn bar() {} fn baz() {} }");
        let module = prog.module0();
        let elements = module.elements.as_ref().unwrap();
        assert_eq!(elements.len(), 2);
        assert!(elements[0].to_function().is_some());
        assert!(elements[1].to_function().is_some());
    }

    #[test]
    fn parse_mod_without_body() {
        let (prog, _) = parse("mod foo;");
        let module = prog.module0();
        assert!(module.elements.is_none());
    }

    #[test]
    fn parse_match() {
        parse_expr("match x { }");
        parse_expr("match x { A(x, b) => 1, B => 2 }");
        parse_expr("match x { A(x, b) => 1, B | C => 2 }");
    }
}
