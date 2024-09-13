use std::sync::Arc;

use crate::ast;
use crate::ast::*;
use crate::error::{ParseError, ParseErrorWithLocation};

use crate::green::{GreenTreeBuilder, Marker};
use crate::token::{
    ELEM_FIRST, EMPTY, ENUM_VARIANT_ARGUMENT_RS, ENUM_VARIANT_RS, EXPRESSION_FIRST, FIELD_FIRST,
    MODIFIER_FIRST, PARAM_LIST_RS, PATTERN_FIRST, PATTERN_RS, TYPE_PARAM_RS, USE_PATH_ATOM_FIRST,
    USE_PATH_FIRST,
};
use crate::TokenKind::*;
use crate::{lex, Span, TokenKind, TokenSet};

pub struct Parser {
    tokens: Vec<TokenKind>,
    token_widths: Vec<u32>,
    token_idx: usize,
    next_node_id: usize,
    content: Arc<String>,
    errors: Vec<ParseErrorWithLocation>,
    nodes: Vec<(usize, u32)>,
    offset: u32,
    builder: GreenTreeBuilder,
}

enum StmtOrExpr {
    Stmt(Stmt),
    Expr(Expr),
}

impl Parser {
    pub fn from_string(code: &'static str) -> Parser {
        let content = Arc::new(String::from(code));
        Parser::common_init(content)
    }

    pub fn from_shared_string(content: Arc<String>) -> Parser {
        Parser::common_init(content)
    }

    fn common_init(content: Arc<String>) -> Parser {
        let result = lex(&*content);

        Parser {
            tokens: result.tokens,
            token_widths: result.widths,
            token_idx: 0,
            next_node_id: 0,
            offset: 0,
            content,
            errors: result.errors,
            nodes: Vec::new(),
            builder: GreenTreeBuilder::new(),
        }
    }

    fn new_node_id(&mut self) -> NodeId {
        let value = self.next_node_id;
        self.next_node_id += 1;
        NodeId(value)
    }

    pub fn parse(mut self) -> (Arc<ast::File>, Vec<ParseErrorWithLocation>) {
        let ast_file = self.parse_file();
        assert!(self.nodes.is_empty());

        let tree = self.builder.create_tree();
        assert_eq!(tree.len(), self.content.len() as u32);

        (Arc::new(ast_file), self.errors)
    }

    fn parse_file(&mut self) -> ast::File {
        self.builder.start_node();
        self.skip_trivia();
        let mut elements = vec![];

        while !self.is_eof() {
            elements.push(self.parse_element());
        }

        let green = self.builder.finish_node(SOURCE_FILE);
        ast::File { green, elements }
    }

    fn parse_element(&mut self) -> Elem {
        self.builder.start_node();
        let modifiers = self.parse_modifiers();
        match self.current() {
            FN_KW => {
                let fct = self.parse_function(modifiers);
                Arc::new(ElemData::Function(fct))
            }

            CLASS_KW => {
                let class = self.parse_class(modifiers);
                Arc::new(ElemData::Class(class))
            }

            STRUCT_KW => {
                let struc = self.parse_struct(modifiers);
                Arc::new(ElemData::Struct(struc))
            }

            TRAIT_KW => {
                let trait_ = self.parse_trait(modifiers);
                Arc::new(ElemData::Trait(trait_))
            }

            IMPL_KW => {
                let impl_ = self.parse_impl(modifiers);
                Arc::new(ElemData::Impl(impl_))
            }

            LET_KW => {
                let global = self.parse_global(modifiers);
                Arc::new(ElemData::Global(global))
            }

            CONST_KW => {
                let const_ = self.parse_const(modifiers);
                Arc::new(ElemData::Const(const_))
            }

            ENUM_KW => {
                let enum_ = self.parse_enum(modifiers);
                Arc::new(ElemData::Enum(enum_))
            }

            MOD_KW => {
                let module = self.parse_module(modifiers);
                Arc::new(ElemData::Module(module))
            }

            USE_KW => {
                let use_stmt = self.parse_use(modifiers);
                Arc::new(ElemData::Use(use_stmt))
            }

            EXTERN_KW => {
                let extern_stmt = self.parse_extern(modifiers);
                Arc::new(ElemData::Extern(extern_stmt))
            }

            TYPE_KW => {
                let type_alias = self.parse_type_alias(modifiers);
                Arc::new(ElemData::TypeAlias(type_alias))
            }

            _ => {
                assert!(!ELEM_FIRST.contains(self.current()));
                let span = self.current_span();
                self.report_error_at(ParseError::ExpectedElement, span);
                self.advance();
                self.builder.finish_node(ERROR);

                Arc::new(ElemData::Error {
                    id: self.new_node_id(),
                    span,
                })
            }
        }
    }

    fn parse_extern(&mut self, modifiers: Option<ModifierList>) -> Arc<ExternPackage> {
        self.start_node();

        self.assert(EXTERN_KW);
        self.expect(PACKAGE_KW);
        let name = self.expect_identifier();
        let identifier = if self.eat(AS_KW) {
            self.expect_identifier()
        } else {
            None
        };

        let green = self.builder.finish_node(EXTERN);

        Arc::new(ExternPackage {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            modifiers,
            name,
            identifier,
        })
    }

    fn parse_use(&mut self, modifiers: Option<ModifierList>) -> Arc<Use> {
        self.start_node();
        self.assert(USE_KW);
        let path = self.parse_use_path();
        self.expect(SEMICOLON);

        let green = self.builder.finish_node(USE);

        Arc::new(Use {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            modifiers,
            path,
        })
    }

    fn parse_use_path(&mut self) -> Arc<UsePath> {
        self.start_node();
        self.builder.start_node();
        let mut path = Vec::new();

        let target = if self.is_set(USE_PATH_ATOM_FIRST) {
            path.push(self.parse_use_atom());

            while self.is(COLON_COLON) && self.nth_is_set(1, USE_PATH_ATOM_FIRST) {
                self.advance();
                path.push(self.parse_use_atom());
            }

            if self.is(COLON_COLON) {
                self.advance();

                if self.is(L_BRACE) {
                    UsePathDescriptor::Group(self.parse_use_group())
                } else {
                    self.report_error(ParseError::ExpectedUsePath);
                    UsePathDescriptor::Error
                }
            } else if self.is(AS_KW) {
                UsePathDescriptor::As(self.parse_use_as())
            } else {
                UsePathDescriptor::Default
            }
        } else if self.is(L_BRACE) {
            UsePathDescriptor::Group(self.parse_use_group())
        } else {
            self.report_error(ParseError::ExpectedUsePath);
            UsePathDescriptor::Error
        };

        let green = self.builder.finish_node(USE_PATH);

        Arc::new(UsePath {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            path,
            target,
        })
    }

    fn parse_use_as(&mut self) -> UseTargetName {
        self.start_node();
        self.builder.start_node();
        self.assert(AS_KW);

        let name = if self.eat(UNDERSCORE) {
            None
        } else {
            self.expect_identifier()
        };

        let green = self.builder.finish_node(USE_RENAME);

        UseTargetName {
            green,
            span: self.finish_node(),
            name,
        }
    }

    fn parse_use_atom(&mut self) -> UseAtom {
        assert!(self.is_set(USE_PATH_ATOM_FIRST));
        self.start_node();
        self.builder.start_node();

        let value = if self.eat(SELF_KW) {
            UsePathComponentValue::This
        } else if self.eat(PACKAGE_KW) {
            UsePathComponentValue::Package
        } else if self.eat(SUPER_KW) {
            UsePathComponentValue::Super
        } else {
            let name = self.expect_identifier();
            if let Some(name) = name {
                UsePathComponentValue::Name(name)
            } else {
                UsePathComponentValue::Error
            }
        };

        let green = self.builder.finish_node(USE_COMPONENT);

        UseAtom {
            green,
            span: self.finish_node(),
            value,
        }
    }

    fn parse_use_group(&mut self) -> Arc<UseGroup> {
        self.start_node();
        self.builder.start_node();

        let targets = self.parse_list(
            L_BRACE,
            COMMA,
            R_BRACE,
            ELEM_FIRST,
            ParseError::ExpectedUsePath,
            USE_GROUP,
            |p| {
                if p.is_set(USE_PATH_FIRST) {
                    Some(p.parse_use_path())
                } else {
                    None
                }
            },
        );

        let green = self.builder.finish_node(USE_GROUP);

        Arc::new(UseGroup {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            targets,
        })
    }

    fn parse_enum(&mut self, modifiers: Option<ModifierList>) -> Arc<Enum> {
        self.start_node();
        self.assert(ENUM_KW);
        let name = self.expect_identifier();
        let type_params = self.parse_type_params();
        let where_bounds = self.parse_where();

        let variants = if self.is(L_BRACE) {
            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ENUM_VARIANT_RS,
                ParseError::ExpectedEnumVariant,
                ENUM_VARIANT_LIST,
                |p| {
                    if p.is(IDENTIFIER) {
                        Some(p.parse_enum_variant())
                    } else {
                        None
                    }
                },
            )
        } else {
            self.report_error(ParseError::ExpectedEnumVariants);
            Vec::new()
        };
        let green = self.builder.finish_node(ENUM);

        Arc::new(Enum {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            modifiers: modifiers.clone(),
            name,
            type_params,
            variants,
            where_bounds,
        })
    }

    fn parse_module(&mut self, modifiers: Option<ModifierList>) -> Arc<Module> {
        self.start_node();
        self.assert(MOD_KW);
        let name = self.expect_identifier();

        let elements = if self.eat(L_BRACE) {
            let mut elements = Vec::new();

            while !self.is(R_BRACE) && !self.is_eof() {
                elements.push(self.parse_element());
            }

            self.expect(R_BRACE);
            Some(elements)
        } else {
            self.expect(SEMICOLON);
            None
        };

        let green = self.builder.finish_node(MODULE);

        Arc::new(Module {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            modifiers: modifiers.clone(),
            name,
            elements,
        })
    }

    fn parse_enum_variant(&mut self) -> EnumVariant {
        self.start_node();
        self.builder.start_node();
        let name = self.expect_identifier();

        let types = if self.is(L_PAREN) {
            Some(self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ENUM_VARIANT_ARGUMENT_RS,
                ParseError::ExpectedType,
                ENUM_VARIANT_ARGUMENT_LIST,
                |p| p.parse_type_wrapper(),
            ))
        } else {
            None
        };

        let green = self.builder.finish_node(ENUM_VARIANT);

        EnumVariant {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            name,
            types,
        }
    }

    fn parse_const(&mut self, modifiers: Option<ModifierList>) -> Arc<Const> {
        self.start_node();
        self.assert(CONST_KW);
        let name = self.expect_identifier();
        self.expect(COLON);
        let ty = self.parse_type();
        self.expect(EQ);
        let expr = self.parse_expression();
        self.expect(SEMICOLON);

        let green = self.builder.finish_node(CONST);

        Arc::new(Const {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            modifiers: modifiers.clone(),
            name,
            data_type: ty,
            expr,
        })
    }

    fn parse_impl(&mut self, modifiers: Option<ModifierList>) -> Arc<Impl> {
        let start = self.current_span().start();
        self.start_node();
        self.assert(IMPL_KW);
        let type_params = self.parse_type_params();

        let type_name = self.parse_type();

        let (class_type, trait_type) = if self.eat(FOR_KW) {
            let class_type = self.parse_type();

            (class_type, Some(type_name))
        } else {
            (type_name, None)
        };

        let where_bounds = self.parse_where();
        let declaration_span = self.span_from(start);

        self.expect(L_BRACE);

        let mut methods = Vec::new();

        while !self.is(R_BRACE) && !self.is_eof() {
            methods.push(self.parse_element());
        }

        self.expect(R_BRACE);

        let green = self.builder.finish_node(IMPL);

        Arc::new(Impl {
            id: self.new_node_id(),
            declaration_span,
            span: self.finish_node(),
            green,
            modifiers,
            type_params,
            trait_type,
            extended_type: class_type,
            where_bounds,
            methods,
        })
    }

    fn parse_global(&mut self, modifiers: Option<ModifierList>) -> Arc<Global> {
        self.start_node();
        self.assert(LET_KW);

        let mutable = self.eat(MUT_KW);
        let name = self.expect_identifier();

        self.expect(COLON);
        let data_type = self.parse_type();

        let expr = if self.eat(EQ) {
            Some(self.parse_expression())
        } else {
            None
        };

        self.expect(SEMICOLON);

        let green = self.builder.finish_node(GLOBAL);

        Arc::new(Global {
            id: self.new_node_id(),
            name,
            green,
            modifiers: modifiers.clone(),
            span: self.finish_node(),
            data_type,
            mutable,
            initial_value: expr.clone(),
        })
    }

    fn parse_trait(&mut self, modifiers: Option<ModifierList>) -> Arc<Trait> {
        self.start_node();
        self.assert(TRAIT_KW);
        let name = self.expect_identifier();
        let type_params = self.parse_type_params();
        let where_bounds = self.parse_where();

        self.expect(L_BRACE);

        let mut methods = Vec::new();

        while !self.is(R_BRACE) && !self.is_eof() {
            methods.push(self.parse_element());
        }

        self.expect(R_BRACE);
        let green = self.builder.finish_node(TRAIT);

        Arc::new(Trait {
            id: self.new_node_id(),
            name,
            green,
            modifiers: modifiers.clone(),
            type_params,
            where_bounds,
            span: self.finish_node(),
            methods,
        })
    }

    fn parse_type_alias(&mut self, modifiers: Option<ModifierList>) -> Arc<TypeAlias> {
        self.start_node();
        self.assert(TYPE_KW);
        let name = self.expect_identifier();
        let bounds = if self.eat(COLON) {
            self.parse_type_bounds()
        } else {
            Vec::new()
        };
        let ty = if self.eat(EQ) {
            Some(self.parse_type())
        } else {
            None
        };
        self.expect(SEMICOLON);

        let green = self.builder.finish_node(TYPE_ALIAS);

        Arc::new(TypeAlias {
            id: self.new_node_id(),
            green,
            span: self.finish_node(),
            modifiers,
            name,
            bounds,
            ty,
        })
    }

    fn parse_struct(&mut self, modifiers: Option<ModifierList>) -> Arc<Struct> {
        self.start_node();
        self.assert(STRUCT_KW);
        let ident = self.expect_identifier();
        let type_params = self.parse_type_params();
        let where_bounds = self.parse_where();

        let fields = if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ELEM_FIRST,
                ParseError::ExpectedField,
                LIST,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        Some(p.parse_struct_field())
                    } else {
                        None
                    }
                },
            )
        } else if self.is(L_BRACE) {
            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ELEM_FIRST,
                ParseError::ExpectedField,
                LIST,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        Some(p.parse_struct_field())
                    } else {
                        None
                    }
                },
            )
        } else {
            Vec::new()
        };

        let green = self.builder.finish_node(STRUCT);

        Arc::new(Struct {
            id: self.new_node_id(),
            name: ident,
            green,
            modifiers: modifiers.clone(),
            span: self.finish_node(),
            fields,
            type_params,
            where_bounds,
        })
    }

    fn parse_struct_field(&mut self) -> StructField {
        self.start_node();
        self.builder.start_node();

        let modifiers = self.parse_modifiers();

        let ident = self.expect_identifier();

        self.expect(COLON);
        let ty = self.parse_type();

        let green = self.builder.finish_node(STRUCT_FIELD);

        StructField {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            modifiers,
            name: ident,
            data_type: ty,
        }
    }

    fn parse_class(&mut self, modifiers: Option<ModifierList>) -> Arc<Class> {
        self.start_node();
        self.assert(CLASS_KW);

        let name = self.expect_identifier();
        let type_params = self.parse_type_params();
        let where_bounds = self.parse_where();

        let fields = if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ELEM_FIRST,
                ParseError::ExpectedField,
                LIST,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        Some(p.parse_class_field())
                    } else {
                        None
                    }
                },
            )
        } else if self.is(L_BRACE) {
            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ELEM_FIRST,
                ParseError::ExpectedField,
                LIST,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        Some(p.parse_class_field())
                    } else {
                        None
                    }
                },
            )
        } else {
            Vec::new()
        };

        let green = self.builder.finish_node(CLASS);

        Arc::new(Class {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            modifiers: modifiers.clone(),
            name,
            fields,
            type_params,
            where_bounds,
        })
    }

    fn parse_class_field(&mut self) -> Field {
        self.start_node();
        self.builder.start_node();

        let modifiers = self.parse_modifiers();

        let name = self.expect_identifier();

        self.expect(COLON);
        let data_type = self.parse_type();

        let green = self.builder.finish_node(CLASS_FIELD);

        Field {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            modifiers,
            name,
            data_type,
            primary_ctor: false,
            expr: None,
            mutable: true,
        }
    }

    fn parse_type_params(&mut self) -> Option<TypeParams> {
        if self.is(L_BRACKET) {
            self.builder.start_node();
            self.start_node();
            let params = self.parse_list(
                L_BRACKET,
                COMMA,
                R_BRACKET,
                TYPE_PARAM_RS,
                ParseError::ExpectedTypeParam,
                TYPE_PARAMS,
                |p| p.parse_type_param_wrapper(),
            );
            self.builder.finish_node(TYPE_PARAMS);

            Some(TypeParams {
                span: self.finish_node(),
                params,
            })
        } else {
            None
        }
    }

    fn parse_type_param_wrapper(&mut self) -> Option<TypeParam> {
        if self.is(IDENTIFIER) {
            Some(self.parse_type_param())
        } else {
            None
        }
    }

    fn parse_type_param(&mut self) -> TypeParam {
        self.start_node();
        self.builder.start_node();
        let name = self.expect_identifier();

        let bounds = if self.eat(COLON) {
            self.parse_type_bounds()
        } else {
            Vec::new()
        };

        self.builder.finish_node(TYPE_PARAM);

        TypeParam {
            name,
            span: self.finish_node(),
            bounds,
        }
    }

    fn parse_type_bounds(&mut self) -> Vec<Type> {
        let mut bounds = Vec::new();

        loop {
            bounds.push(self.parse_type());

            if !self.eat(ADD) {
                break;
            }
        }

        bounds
    }

    fn parse_modifiers(&mut self) -> Option<ModifierList> {
        if self.is_set(MODIFIER_FIRST) {
            self.start_node();
            let marker = self.builder.create_marker();
            let mut modifiers: Vec<Modifier> = Vec::new();

            while self.is_set(MODIFIER_FIRST) {
                modifiers.push(self.parse_modifier());
            }

            assert!(!modifiers.is_empty());
            let green = self.builder.finish_node_starting_at(MODIFIERS, marker);

            Some(ModifierList {
                id: self.new_node_id(),
                span: self.finish_node(),
                green,
                modifiers,
            })
        } else {
            None
        }
    }

    fn parse_modifier(&mut self) -> Modifier {
        self.start_node();
        let m = self.builder.create_marker();

        if self.eat(PUB_KW) {
            // done
        } else if self.eat(STATIC_KW) {
            // done
        } else {
            self.assert(AT);
            self.expect_identifier();
        }

        let green = self.builder.finish_node_starting_at(MODIFIER, m);

        Modifier {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
        }
    }

    fn parse_function(&mut self, modifiers: Option<ModifierList>) -> Arc<Function> {
        let start = self.current_span().start();
        self.start_node();
        self.assert(FN_KW);
        let name = self.expect_identifier();
        let type_params = self.parse_type_params();
        let params = self.parse_function_params();
        let return_type = self.parse_function_type();
        let where_bounds = self.parse_where();
        let declaration_span = self.span_from(start);
        let block = self.parse_function_block();

        let green = self.builder.finish_node(FN);

        Arc::new(Function {
            id: self.new_node_id(),
            kind: FunctionKind::Function,
            modifiers: modifiers.clone(),
            name,
            declaration_span,
            span: self.finish_node(),
            params,
            return_type,
            block,
            type_params,
            where_bounds,
            green,
        })
    }

    fn parse_function_params(&mut self) -> Vec<Param> {
        if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                PARAM_LIST_RS,
                ParseError::ExpectedParam,
                PARAM_LIST,
                |p| p.parse_function_param_wrapper(),
            )
        } else {
            self.report_error(ParseError::ExpectedParams);
            Vec::new()
        }
    }

    fn parse_list<F, R>(
        &mut self,
        start: TokenKind,
        sep: TokenKind,
        stop: TokenKind,
        recovery_set: TokenSet,
        msg: ParseError,
        node: TokenKind,
        mut parse: F,
    ) -> Vec<R>
    where
        F: FnMut(&mut Parser) -> Option<R>,
    {
        let mut data = vec![];
        self.builder.start_node();
        self.assert(start);

        while !self.is(stop.clone()) && !self.is_eof() {
            let pos_before_element = self.token_idx;
            let entry = parse(self);

            match entry {
                Some(entry) => {
                    // Callback needs to at least advance by one token, otherwise
                    // we might loop forever here.
                    assert!(self.token_idx > pos_before_element);
                    data.push(entry)
                }

                None => {
                    if self.is_set(recovery_set) {
                        break;
                    }

                    self.report_error(msg.clone());
                    self.advance();
                }
            }

            if !self.is(stop.clone()) {
                self.expect(sep);
            }
        }

        self.expect(stop);
        self.builder.finish_node(node);

        data
    }

    fn parse_function_param_wrapper(&mut self) -> Option<Param> {
        if self.is(MUT_KW) || self.is(IDENTIFIER) {
            Some(self.parse_function_param())
        } else {
            None
        }
    }

    fn parse_function_param(&mut self) -> Param {
        self.start_node();
        let mutable = self.eat(MUT_KW);
        let name = self.expect_identifier();

        self.expect(COLON);

        let data_type = self.parse_type();

        let variadic = self.eat(DOT_DOT_DOT);

        Param {
            id: self.new_node_id(),
            variadic,
            name,
            span: self.finish_node(),
            mutable,
            data_type,
        }
    }

    fn parse_function_type(&mut self) -> Option<Type> {
        if self.eat(COLON) {
            let ty = self.parse_type();

            Some(ty)
        } else {
            None
        }
    }

    fn parse_where(&mut self) -> Option<WhereBounds> {
        if self.eat(WHERE_KW) {
            self.start_node();
            self.builder.start_node();

            let mut clauses = Vec::new();

            loop {
                clauses.push(self.parse_where_clause());

                if !self.eat(COMMA) {
                    break;
                }
            }

            let green = self.builder.finish_node(WHERE_CLAUSES);

            Some(Arc::new(WhereBoundsData {
                id: self.new_node_id(),
                span: self.finish_node(),
                green,
                clauses,
            }))
        } else {
            None
        }
    }

    fn parse_where_clause(&mut self) -> WhereClause {
        self.start_node();
        self.builder.start_node();
        let ty = self.parse_type();
        self.expect(COLON);
        let mut bounds = Vec::new();

        loop {
            bounds.push(self.parse_type());

            if !self.eat(ADD) {
                break;
            }
        }

        let green = self.builder.finish_node(WHERE_CLAUSE);

        Arc::new(WhereBoundData {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            ty,
            bounds,
        })
    }

    fn parse_function_block(&mut self) -> Option<Expr> {
        if self.eat(SEMICOLON) {
            None
        } else {
            let block = self.parse_block();
            Some(block)
        }
    }

    fn parse_type_wrapper(&mut self) -> Option<Type> {
        if self.is(UPCASE_SELF_KW) || self.is(IDENTIFIER) || self.is(L_PAREN) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn parse_type(&mut self) -> Type {
        self.builder.start_node();
        match self.current() {
            UPCASE_SELF_KW => {
                let span = self.current_span();
                self.assert(UPCASE_SELF_KW);
                let green = self.builder.finish_node(SELF_TYPE);
                Arc::new(TypeData::create_self(self.new_node_id(), span, green))
            }

            IDENTIFIER => {
                self.start_node();
                let path = self.parse_path();

                let params = if self.is(L_BRACKET) {
                    self.parse_list(
                        L_BRACKET,
                        COMMA,
                        R_BRACKET,
                        TYPE_PARAM_RS,
                        ParseError::ExpectedType,
                        LIST,
                        |p| p.parse_type_wrapper(),
                    )
                } else {
                    Vec::new()
                };

                let green = self.builder.finish_node(REGULAR_TYPE);

                Arc::new(TypeData::create_basic(
                    self.new_node_id(),
                    self.finish_node(),
                    green,
                    path,
                    params,
                ))
            }

            L_PAREN => {
                self.start_node();
                let subtypes = self.parse_list(
                    L_PAREN,
                    COMMA,
                    R_PAREN,
                    TYPE_PARAM_RS,
                    ParseError::ExpectedType,
                    LIST,
                    |p| p.parse_type_wrapper(),
                );

                if self.eat(COLON) {
                    let ret = self.parse_type();

                    let green = self.builder.finish_node(LAMBDA_TYPE);

                    Arc::new(TypeData::create_fct(
                        self.new_node_id(),
                        self.finish_node(),
                        green,
                        subtypes,
                        Some(ret),
                    ))
                } else {
                    let green = self.builder.finish_node(TUPLE_TYPE);

                    Arc::new(TypeData::create_tuple(
                        self.new_node_id(),
                        self.finish_node(),
                        green,
                        subtypes,
                    ))
                }
            }

            _ => {
                let span = self.current_span();
                self.report_error(ParseError::ExpectedType);
                self.builder.abandon_node();
                Arc::new(TypeData::Error {
                    id: self.new_node_id(),
                    span,
                })
            }
        }
    }

    fn parse_path(&mut self) -> Path {
        self.start_node();
        let mut names = Vec::new();
        let name = self.expect_identifier();
        if let Some(name) = name {
            names.push(name);
        } else {
            // Advance by token to avoid infinite loop in `parse_match`.
            self.advance();
        }

        while self.eat(COLON_COLON) {
            let name = self.expect_identifier();
            if let Some(name) = name {
                names.push(name);
            } else {
                break;
            }
        }

        Arc::new(PathData {
            id: self.new_node_id(),
            span: self.finish_node(),
            names,
        })
    }

    fn parse_let(&mut self) -> Stmt {
        self.start_node();

        self.assert(LET_KW);
        let pattern = self.parse_pattern();
        let data_type = self.parse_var_type();
        let expr = self.parse_var_assignment();

        self.expect(SEMICOLON);

        Arc::new(StmtData::create_let(
            self.new_node_id(),
            self.finish_node(),
            pattern,
            data_type,
            expr,
        ))
    }

    fn parse_var_type(&mut self) -> Option<Type> {
        if self.eat(COLON) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn parse_var_assignment(&mut self) -> Option<Expr> {
        if self.eat(EQ) {
            let expr = self.parse_expression();

            Some(expr)
        } else {
            None
        }
    }

    fn parse_block(&mut self) -> Expr {
        self.start_node();
        self.builder.start_node();
        let mut stmts = vec![];
        let mut expr = None;

        if self.expect(L_BRACE) {
            while !self.is(R_BRACE) && !self.is_eof() {
                let stmt_or_expr = self.parse_statement_or_expression();

                match stmt_or_expr {
                    StmtOrExpr::Stmt(stmt) => stmts.push(stmt),
                    StmtOrExpr::Expr(curr_expr) => {
                        if curr_expr.needs_semicolon() {
                            expr = Some(curr_expr);
                            break;
                        } else if !self.is(R_BRACE) {
                            stmts.push(Arc::new(StmtData::create_expr(
                                self.new_node_id(),
                                curr_expr.span(),
                                curr_expr,
                            )));
                        } else {
                            expr = Some(curr_expr);
                        }
                    }
                }
            }

            self.expect(R_BRACE);
        }

        let green = self.builder.finish_node(BLOCK_EXPR);

        Arc::new(ExprData::create_block(
            self.new_node_id(),
            self.finish_node(),
            green,
            stmts,
            expr,
        ))
    }

    fn parse_statement_or_expression(&mut self) -> StmtOrExpr {
        match self.current() {
            LET_KW => StmtOrExpr::Stmt(self.parse_let()),
            _ => {
                let expr = self.parse_expression();

                if self.eat(SEMICOLON) {
                    let span = self.span_from(expr.span().start());

                    StmtOrExpr::Stmt(Arc::new(StmtData::create_expr(
                        self.new_node_id(),
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
        self.start_node();
        self.builder.start_node();
        self.assert(IF_KW);

        let cond = self.parse_expression();

        let then_block = self.parse_block();

        let else_block = if self.eat(ELSE_KW) {
            if self.is(IF_KW) {
                Some(self.parse_if())
            } else {
                Some(self.parse_block())
            }
        } else {
            None
        };

        let green = self.builder.finish_node(IF_EXPR);

        Arc::new(ExprData::create_if(
            self.new_node_id(),
            self.finish_node(),
            green,
            cond,
            then_block,
            else_block,
        ))
    }

    fn parse_match(&mut self) -> Expr {
        self.start_node();
        self.builder.start_node();
        self.assert(MATCH_KW);

        let expr = self.parse_expression();
        let mut cases = Vec::new();

        self.expect(L_BRACE);

        while !self.is(R_BRACE) && !self.is_eof() {
            let case = self.parse_match_case();
            let is_block = case.value.is_block();
            cases.push(case);

            if !self.is(R_BRACE) {
                if is_block {
                    self.eat(COMMA);
                } else {
                    self.expect(COMMA);
                }
            }
        }

        self.expect(R_BRACE);
        let green = self.builder.finish_node(MATCH_EXPR);

        Arc::new(ExprData::create_match(
            self.new_node_id(),
            self.finish_node(),
            green,
            expr,
            cases,
        ))
    }

    fn parse_match_case(&mut self) -> MatchCaseType {
        self.start_node();
        let pattern = self.parse_pattern();

        self.expect(DOUBLE_ARROW);

        let value = self.parse_expression();

        MatchCaseType {
            id: self.new_node_id(),
            span: self.finish_node(),
            pattern,
            value,
        }
    }

    fn parse_pattern(&mut self) -> Arc<Pattern> {
        self.start_node();

        let first = self.parse_pattern_alt();
        let mut alts = vec![first];

        while self.eat(OR) {
            alts.push(self.parse_pattern_alt());
        }

        Arc::new(Pattern {
            id: self.new_node_id(),
            span: self.finish_node(),
            alts,
        })
    }

    fn parse_pattern_alt(&mut self) -> Arc<PatternAlt> {
        self.start_node();

        if self.eat(UNDERSCORE) {
            Arc::new(PatternAlt::Underscore(PatternUnderscore {
                id: self.new_node_id(),
                span: self.finish_node(),
            }))
        } else if self.eat(DOT_DOT) {
            Arc::new(PatternAlt::Rest(PatternRest {
                id: self.new_node_id(),
                span: self.finish_node(),
            }))
        } else if self.is(TRUE) || self.is(FALSE) {
            let expr = self.parse_lit_bool();
            Arc::new(PatternAlt::LitBool(PatternLit {
                id: self.new_node_id(),
                span: self.finish_node(),
                expr,
            }))
        } else if self.is(L_PAREN) {
            let params = self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                PATTERN_RS,
                ParseError::ExpectedPattern,
                PATTERN_LIST,
                |p| {
                    if p.is_set(PATTERN_FIRST) {
                        Some(p.parse_pattern())
                    } else {
                        None
                    }
                },
            );

            Arc::new(PatternAlt::Tuple(PatternTuple {
                id: self.new_node_id(),
                span: self.finish_node(),
                params,
            }))
        } else if self.is(STRING_LITERAL) {
            let expr = self.parse_string();
            Arc::new(PatternAlt::LitString(PatternLit {
                id: self.new_node_id(),
                span: self.finish_node(),
                expr,
            }))
        } else if self.is_pair(MUT_KW, IDENTIFIER) {
            self.assert(MUT_KW);
            let name = self.expect_identifier().expect("identifier expected");
            Arc::new(PatternAlt::Ident(PatternIdent {
                id: self.new_node_id(),
                span: self.finish_node(),
                mutable: true,
                name,
            }))
        } else if self.is(IDENTIFIER) {
            if !self.nth_is(1, COLON_COLON) && !self.nth_is(1, L_PAREN) {
                let name = self.expect_identifier().expect("identifier expected");
                return Arc::new(PatternAlt::Ident(PatternIdent {
                    id: self.new_node_id(),
                    span: self.finish_node(),
                    mutable: false,
                    name,
                }));
            }

            let path = self.parse_path();

            let params = if self.is(L_PAREN) {
                let params = self.parse_list(
                    L_PAREN,
                    COMMA,
                    R_PAREN,
                    PATTERN_RS,
                    ParseError::ExpectedPattern,
                    PATTERN_LIST,
                    |p| {
                        if p.is_set(PATTERN_FIRST) {
                            Some(p.parse_pattern())
                        } else {
                            None
                        }
                    },
                );

                Some(params)
            } else {
                None
            };

            Arc::new(PatternAlt::ClassOrStructOrEnum(
                PatternClassOrStructOrEnum {
                    id: self.new_node_id(),
                    span: self.finish_node(),
                    path,
                    params,
                },
            ))
        } else {
            unreachable!()
        }
    }

    fn parse_for(&mut self) -> Expr {
        self.start_node();
        self.builder.start_node();
        self.assert(FOR_KW);
        let pattern = self.parse_pattern();
        self.expect(IN_KW);
        let expr = self.parse_expression();
        let block = self.parse_block();
        let green = self.builder.finish_node(FOR_EXPR);

        Arc::new(ExprData::create_for(
            self.new_node_id(),
            self.finish_node(),
            green,
            pattern,
            expr,
            block,
        ))
    }

    fn parse_while(&mut self) -> Expr {
        self.start_node();
        self.builder.start_node();
        self.assert(WHILE_KW);
        let expr = self.parse_expression();
        let block = self.parse_block();
        let green = self.builder.finish_node(WHILE_EXPR);

        Arc::new(ExprData::create_while(
            self.new_node_id(),
            self.finish_node(),
            green,
            expr,
            block,
        ))
    }

    fn parse_break(&mut self) -> Expr {
        self.start_node();
        self.builder.start_node();
        self.assert(BREAK_KW);
        let green = self.builder.finish_node(BREAK_EXPR);

        Arc::new(ExprData::create_break(
            self.new_node_id(),
            self.finish_node(),
            green,
        ))
    }

    fn parse_continue(&mut self) -> Expr {
        self.start_node();
        self.builder.start_node();
        self.assert(CONTINUE_KW);
        let green = self.builder.finish_node(CONTINUE_EXPR);

        Arc::new(ExprData::create_continue(
            self.new_node_id(),
            self.finish_node(),
            green,
        ))
    }

    fn parse_return(&mut self) -> Expr {
        self.start_node();
        self.builder.start_node();
        self.assert(RETURN_KW);
        let expr = if self.is(SEMICOLON) {
            None
        } else {
            let expr = self.parse_expression();
            Some(expr)
        };

        let green = self.builder.finish_node(RETURN_EXPR);

        Arc::new(ExprData::create_return(
            self.new_node_id(),
            self.finish_node(),
            green,
            expr,
        ))
    }

    fn parse_expression(&mut self) -> Expr {
        self.parse_binary_expr(0)
    }

    fn parse_binary_expr(&mut self, precedence: u32) -> Expr {
        if !self.is_set(EXPRESSION_FIRST) {
            self.report_error(ParseError::ExpectedExpression);
            return Arc::new(ExprData::Error {
                id: self.new_node_id(),
                span: self.current_span(),
            });
        }

        let start = self.current_span().start();
        let marker = self.builder.create_marker();
        let mut left = self.parse_unary_expr();

        loop {
            let right_precedence = match self.current() {
                EQ => 1,
                OR_OR => 2,
                AND_AND => 3,
                EQ_EQ | NOT_EQ | LT | LE | GT | GE | EQ_EQ_EQ | NOT_EQ_EQ => 4,
                ADD | SUB | OR | CARET => 5,
                MUL | DIV | MODULO | AND | LT_LT | GT_GT | GT_GT_GT => 6,
                AS_KW | IS_KW => 7,
                _ => {
                    return left;
                }
            };

            if precedence >= right_precedence {
                return left;
            }

            let kind = self.current();
            self.advance();

            left = match kind {
                AS_KW => {
                    let right = self.parse_type();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(CONV_EXPR, marker.clone());

                    let expr = ExprData::create_conv(self.new_node_id(), span, left, right);

                    Arc::new(expr)
                }

                IS_KW => {
                    let right = self.parse_pattern();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(IS_EXPR, marker.clone());

                    let expr = ExprData::create_is(self.new_node_id(), span, left, right);

                    Arc::new(expr)
                }

                _ => {
                    let right = self.parse_binary_expr(right_precedence);
                    self.builder
                        .finish_node_starting_at(BINARY_EXPR, marker.clone());
                    self.create_binary(kind, start, left, right)
                }
            };
        }
    }

    fn parse_unary_expr(&mut self) -> Expr {
        match self.current() {
            SUB | NOT => {
                self.start_node();
                self.builder.start_node();
                let kind = self.current();
                self.advance();
                let op = match kind {
                    SUB => UnOp::Neg,
                    NOT => UnOp::Not,
                    _ => unreachable!(),
                };

                let expr = self.parse_postfix_expr();
                let green = self.builder.finish_node(UNARY_EXPR);
                Arc::new(ExprData::create_un(
                    self.new_node_id(),
                    self.finish_node(),
                    green,
                    op,
                    expr,
                ))
            }

            _ => self.parse_postfix_expr(),
        }
    }

    fn parse_postfix_expr(&mut self) -> Expr {
        let start = self.current_span().start();
        let marker = self.builder.create_marker();
        let mut left = self.parse_factor();

        loop {
            left = match self.current() {
                DOT => {
                    let op_span = self.current_span();
                    self.assert(DOT);
                    let rhs = self.parse_factor();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(POSTFIX_EXPR, marker.clone());

                    Arc::new(ExprData::create_dot(
                        self.new_node_id(),
                        span,
                        op_span,
                        left,
                        rhs,
                    ))
                }

                L_PAREN => self.parse_call(start, marker.clone(), left),

                L_BRACKET => {
                    let op_span = self.current_span();
                    let types = self.parse_list(
                        L_BRACKET,
                        COMMA,
                        R_BRACKET,
                        TYPE_PARAM_RS,
                        ParseError::ExpectedType,
                        TYPE_LIST,
                        |p| p.parse_type_wrapper(),
                    );
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(POSTFIX_EXPR, marker.clone());

                    Arc::new(ExprData::create_type_param(
                        self.new_node_id(),
                        span,
                        op_span,
                        left,
                        types,
                    ))
                }

                COLON_COLON => {
                    let op_span = self.current_span();
                    self.assert(COLON_COLON);
                    let rhs = self.parse_factor();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(POSTFIX_EXPR, marker.clone());

                    Arc::new(ExprData::create_path(
                        self.new_node_id(),
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

    fn parse_call(&mut self, start: u32, marker: Marker, left: Expr) -> Expr {
        let args = self.parse_list(
            L_PAREN,
            COMMA,
            R_PAREN,
            EMPTY,
            ParseError::ExpectedExpression,
            ARG_LIST,
            |p| {
                if p.is_set(EXPRESSION_FIRST) {
                    Some(p.parse_expression())
                } else {
                    None
                }
            },
        );
        let span = self.span_from(start);

        self.builder
            .finish_node_starting_at(POSTFIX_EXPR, marker.clone());

        Arc::new(ExprData::create_call(self.new_node_id(), span, left, args))
    }

    fn create_binary(&mut self, kind: TokenKind, start: u32, left: Expr, right: Expr) -> Expr {
        let op = match kind {
            EQ => BinOp::Assign,
            OR_OR => BinOp::Or,
            AND_AND => BinOp::And,
            EQ_EQ => BinOp::Cmp(CmpOp::Eq),
            NOT_EQ => BinOp::Cmp(CmpOp::Ne),
            LT => BinOp::Cmp(CmpOp::Lt),
            LE => BinOp::Cmp(CmpOp::Le),
            GT => BinOp::Cmp(CmpOp::Gt),
            GE => BinOp::Cmp(CmpOp::Ge),
            EQ_EQ_EQ => BinOp::Cmp(CmpOp::Is),
            NOT_EQ_EQ => BinOp::Cmp(CmpOp::IsNot),
            OR => BinOp::BitOr,
            AND => BinOp::BitAnd,
            CARET => BinOp::BitXor,
            ADD => BinOp::Add,
            SUB => BinOp::Sub,
            MUL => BinOp::Mul,
            DIV => BinOp::Div,
            MODULO => BinOp::Mod,
            LT_LT => BinOp::ShiftL,
            GT_GT => BinOp::ArithShiftR,
            GT_GT_GT => BinOp::LogicalShiftR,
            _ => panic!("unimplemented token {:?}", kind),
        };

        let span = self.span_from(start);

        Arc::new(ExprData::create_bin(
            self.new_node_id(),
            span,
            op,
            left,
            right,
        ))
    }

    fn parse_factor(&mut self) -> Expr {
        let span = self.current_span();
        match self.current() {
            L_PAREN => self.parse_parentheses(),
            L_BRACE => self.parse_block(),
            IF_KW => self.parse_if(),
            CHAR_LITERAL => self.parse_lit_char(),
            INT_LITERAL => self.parse_lit_int(),
            FLOAT_LITERAL => self.parse_lit_float(),
            STRING_LITERAL => self.parse_string(),
            TEMPLATE_LITERAL => self.parse_template(),
            IDENTIFIER => self.parse_identifier(),
            TRUE => self.parse_lit_bool(),
            FALSE => self.parse_lit_bool(),
            SELF_KW => self.parse_this(),
            OR | OR_OR => self.parse_lambda(),
            FOR_KW => self.parse_for(),
            WHILE_KW => self.parse_while(),
            BREAK_KW => self.parse_break(),
            CONTINUE_KW => self.parse_continue(),
            RETURN_KW => self.parse_return(),
            MATCH_KW => self.parse_match(),
            _ => {
                self.report_error(ParseError::ExpectedFactor);
                Arc::new(ExprData::Error {
                    id: self.new_node_id(),
                    span,
                })
            }
        }
    }

    fn parse_identifier(&mut self) -> Expr {
        self.builder.start_node();
        let ident = self.expect_identifier().expect("identifier expected");
        let green = self.builder.finish_node(IDENT_EXPR);
        Arc::new(ExprData::create_ident(
            self.new_node_id(),
            ident.span,
            green,
            ident.name_as_string.clone(),
        ))
    }

    fn parse_parentheses(&mut self) -> Expr {
        self.start_node();
        self.builder.start_node();
        self.assert(L_PAREN);

        if self.eat(R_PAREN) {
            let green = self.builder.finish_node(TUPLE_EXPR);
            return Arc::new(ExprData::create_tuple(
                self.new_node_id(),
                self.finish_node(),
                green,
                Vec::new(),
            ));
        }

        let expr = self.parse_expression();

        if self.current() == COMMA {
            let mut values = vec![expr];

            loop {
                self.expect(COMMA);

                if self.eat(R_PAREN) {
                    break;
                }

                if !self.is_set(EXPRESSION_FIRST) {
                    break;
                }

                let expr = self.parse_expression();
                values.push(expr);

                if self.eat(R_PAREN) {
                    break;
                }
            }

            let green = self.builder.finish_node(TUPLE_EXPR);

            Arc::new(ExprData::create_tuple(
                self.new_node_id(),
                self.finish_node(),
                green,
                values,
            ))
        } else {
            let green = self.builder.finish_node(PAREN_EXPR);

            self.expect(R_PAREN);
            Arc::new(ExprData::create_paren(
                self.new_node_id(),
                self.finish_node(),
                green,
                expr,
            ))
        }
    }

    fn parse_lit_char(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();
        self.assert(CHAR_LITERAL);
        let value = self.source_span(span);

        let green = self.builder.finish_node(CHAR_LIT_EXPR);
        Arc::new(ExprData::create_lit_char(
            self.new_node_id(),
            span,
            green,
            value,
        ))
    }

    fn parse_lit_int(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();
        self.assert(INT_LITERAL);
        let value = self.source_span(span);

        let green = self.builder.finish_node(INT_LIT_EXPR);
        Arc::new(ExprData::create_lit_int(
            self.new_node_id(),
            span,
            green,
            value,
        ))
    }

    fn parse_lit_float(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();
        self.assert(FLOAT_LITERAL);
        let value = self.source_span(span);

        let green = self.builder.finish_node(FLOAT_LIT_EXPR);
        Arc::new(ExprData::create_lit_float(
            self.new_node_id(),
            span,
            green,
            value,
        ))
    }

    fn parse_template(&mut self) -> Expr {
        self.builder.start_node();
        let span = self.current_span();
        let start = span.start();

        self.builder.start_node();
        self.assert(TEMPLATE_LITERAL);
        let value = self.source_span(span);
        let green = self.builder.finish_node(STRING_LIT_EXPR);

        let mut parts: Vec<Expr> = Vec::new();
        parts.push(Arc::new(ExprData::create_lit_str(
            self.new_node_id(),
            span,
            green,
            value,
        )));

        let mut done = false;

        while !done {
            parts.push(self.parse_expression());

            let span = self.current_span();

            if !self.is(TEMPLATE_LITERAL) {
                done = true;
            }

            if !self.is(TEMPLATE_LITERAL) && !self.is(TEMPLATE_END_LITERAL) {
                self.report_error(ParseError::UnclosedStringTemplate);
                break;
            }

            self.builder.start_node();
            let value = self.source_span(span);
            self.advance();
            let green = self.builder.finish_node(STRING_LIT_EXPR);

            parts.push(Arc::new(ExprData::create_lit_str(
                self.new_node_id(),
                span,
                green,
                value,
            )));
        }

        let span = self.span_from(start);

        let green = self.builder.finish_node(TEMPLATE_EXPR);
        Arc::new(ExprData::create_template(
            self.new_node_id(),
            span,
            green,
            parts,
        ))
    }

    fn parse_string(&mut self) -> Expr {
        let span = self.current_span();
        self.builder.start_node();
        self.assert(STRING_LITERAL);

        let value = self.source_span(span);
        let green = self.builder.finish_node(STRING_LIT_EXPR);
        Arc::new(ExprData::create_lit_str(
            self.new_node_id(),
            span,
            green,
            value,
        ))
    }

    fn parse_lit_bool(&mut self) -> Expr {
        self.builder.start_node();
        let span = self.current_span();
        let kind = self.current();
        self.assert(kind);
        let value = kind == TRUE;
        self.builder.finish_node(BOOL_LIT_EXPR);

        Arc::new(ExprData::create_lit_bool(self.new_node_id(), span, value))
    }

    fn parse_this(&mut self) -> Expr {
        self.builder.start_node();
        let span = self.current_span();
        self.assert(SELF_KW);
        let green = self.builder.finish_node(THIS_EXPR);

        Arc::new(ExprData::create_this(self.new_node_id(), span, green))
    }

    fn parse_lambda(&mut self) -> Expr {
        let start = self.current_span().start();
        self.start_node();
        self.builder.start_node();

        let params = if self.eat(OR_OR) {
            // nothing to do
            Vec::new()
        } else {
            assert!(self.is(OR));
            self.parse_list(
                OR,
                COMMA,
                OR,
                PARAM_LIST_RS,
                ParseError::ExpectedParam,
                PARAM_LIST,
                |p| p.parse_function_param_wrapper(),
            )
        };

        let return_type = if self.eat(COLON) {
            Some(self.parse_type())
        } else {
            None
        };

        let declaration_span = self.span_from(start);

        let block = self.parse_block();
        let green = self.builder.finish_node(LAMBDA_EXPR);

        let function = Arc::new(Function {
            id: self.new_node_id(),
            kind: FunctionKind::Lambda,
            modifiers: None,
            name: None,
            declaration_span,
            span: self.finish_node(),
            params,
            return_type,
            block: Some(block),
            type_params: None,
            where_bounds: None,
            green,
        });

        Arc::new(ExprData::create_lambda(function))
    }

    fn assert(&mut self, kind: TokenKind) {
        assert!(self.eat(kind));
    }

    fn expect_identifier(&mut self) -> Option<Ident> {
        let span = self.current_span();

        if self.is(IDENTIFIER) {
            self.assert(IDENTIFIER);
            let value = self.source_span(span);

            Some(Arc::new(IdentData {
                span,
                name_as_string: value,
            }))
        } else {
            self.report_error_at(ParseError::ExpectedIdentifier, span);
            None
        }
    }

    fn expect(&mut self, kind: TokenKind) -> bool {
        debug_assert!(token_name(kind).is_some());

        if self.eat(kind) {
            true
        } else {
            let kind = token_name(kind).expect("missing name");
            self.report_error(ParseError::ExpectedToken(kind.into()));
            false
        }
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.current() == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    fn report_error(&mut self, msg: ParseError) {
        self.report_error_at(msg, self.current_span());
    }

    fn report_error_at(&mut self, msg: ParseError, span: Span) {
        self.errors.push(ParseErrorWithLocation::new(span, msg));
    }

    fn advance(&mut self) {
        self.raw_advance();
        self.skip_trivia();
    }

    fn skip_trivia(&mut self) {
        while self.current().is_trivia() {
            self.raw_advance();
        }
    }

    fn raw_advance(&mut self) {
        if self.token_idx < self.tokens.len() {
            let kind = self.current();
            let value = self.source_span(self.current_span());
            let len = self.token_widths[self.token_idx];
            self.offset += len;
            debug_assert!(kind <= EOF);
            self.builder.token(kind, value);
            self.token_idx += 1;
        }
    }

    fn current(&self) -> TokenKind {
        self.nth(0)
    }

    fn nth(&self, idx: usize) -> TokenKind {
        if self.token_idx + idx < self.tokens.len() {
            self.tokens[self.token_idx + idx]
        } else {
            EOF
        }
    }

    fn current_span(&self) -> Span {
        if self.token_idx < self.tokens.len() {
            let length = self.token_widths[self.token_idx];
            Span::new(self.offset, length)
        } else {
            Span::at(self.offset)
        }
    }

    fn is(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    fn is_pair(&self, fst: TokenKind, snd: TokenKind) -> bool {
        if !self.is(fst) {
            return false;
        }

        let mut idx = 1;

        loop {
            let curr = self.nth(idx);

            if curr.is_trivia() {
                idx += 1;
                continue;
            }

            return curr == snd;
        }
    }

    fn is_set(&self, set: TokenSet) -> bool {
        set.contains(self.current())
    }

    #[allow(unused)]
    fn nth_is(&self, idx: usize, kind: TokenKind) -> bool {
        self.nth(idx) == kind
    }

    fn nth_is_set(&self, idx: usize, set: TokenSet) -> bool {
        set.contains(self.nth(idx))
    }

    fn is_eof(&self) -> bool {
        self.current() == EOF
    }

    fn start_node(&mut self) {
        self.nodes.push((self.token_idx, self.offset));
    }

    fn finish_node(&mut self) -> Span {
        let (start_token, start_offset) = self.nodes.pop().expect("missing node start");

        let mut end_token = self.token_idx - 1;
        assert!(end_token < self.tokens.len());
        let mut end_offset = self.offset;

        while end_token > start_token {
            if !self.tokens[end_token].is_trivia() {
                break;
            }

            end_offset -= self.token_widths[end_token];
            end_token -= 1;
        }

        Span::new(start_offset, end_offset - start_offset)
    }

    fn source_span(&self, span: Span) -> String {
        let start = span.start() as usize;
        let end = span.end() as usize;
        String::from(&self.content[start..end])
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.offset - start)
    }
}

fn token_name(kind: TokenKind) -> Option<&'static str> {
    match kind {
        PACKAGE_KW => Some("package"),
        IN_KW => Some("in"),
        EQ => Some("="),
        COMMA => Some(","),
        SEMICOLON => Some(";"),
        DOT => Some("."),
        COLON => Some(":"),
        ARROW => Some("->"),
        DOUBLE_ARROW => Some("=>"),
        OR => Some("|"),
        L_PAREN => Some("("),
        R_PAREN => Some(")"),
        L_BRACKET => Some("["),
        R_BRACKET => Some("]"),
        L_BRACE => Some("{"),
        R_BRACE => Some("}"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use std::sync::Arc;

    use crate::error::ParseError;
    use crate::parser::Parser;
    use crate::{compute_line_column, compute_line_starts};

    fn parse_expr(code: &'static str) -> Expr {
        let mut parser = Parser::from_string(code);

        let result = parser.parse_expression();
        assert!(parser.errors.is_empty());

        result
    }

    fn err_expr(code: &'static str, msg: ParseError, line: u32, col: u32) {
        let mut parser = Parser::from_string(code);

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

    fn parse_let(code: &'static str) -> Stmt {
        let mut parser = Parser::from_string(code);
        let result = parser.parse_let();
        if !parser.errors.is_empty() {
            for err in &parser.errors {
                eprintln!(
                    "error at {}: {:?} / {}",
                    err.span,
                    err.error,
                    err.error.message()
                );
            }
        }
        assert!(parser.errors.is_empty());
        result
    }

    fn parse_type(code: &'static str) -> Type {
        let mut parser = Parser::from_string(code);
        parser.parse_type()
    }

    fn parse(code: &'static str) -> Arc<File> {
        let (file, errors) = Parser::from_string(code).parse();
        assert!(errors.is_empty());
        file
    }

    #[test]
    fn parse_ident() {
        let expr = parse_expr("a");
        let ident = expr.to_ident().unwrap();
        assert_eq!("a", ident.name);
    }

    #[test]
    fn parse_number() {
        let expr = parse_expr("10");

        let lit = expr.to_lit_int().unwrap();
        assert_eq!(String::from("10"), lit.value);
    }

    #[test]
    fn parse_number_with_underscore() {
        let expr = parse_expr("1____0");

        let lit = expr.to_lit_int().unwrap();
        assert_eq!(String::from("1____0"), lit.value);
    }

    #[test]
    fn parse_string() {
        let expr = parse_expr("\"abc\"");

        let lit = expr.to_lit_str().unwrap();
        assert_eq!("\"abc\"", &lit.value);
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
    fn parse_field_access() {
        let expr = parse_expr("obj.field");
        let dot = expr.to_dot().unwrap();

        let ident = dot.lhs.to_ident().unwrap();
        assert_eq!("obj", ident.name);

        let ident = dot.rhs.to_ident().unwrap();
        assert_eq!("field", ident.name);
    }

    #[test]
    fn parse_field_negated() {
        let expr = parse_expr("-obj.field");
        assert!(expr.to_un().unwrap().opnd.is_dot());
    }

    #[test]
    fn parse_field_non_ident() {
        let expr = parse_expr("bar.12");
        let dot = expr.to_dot().unwrap();

        let ident = dot.lhs.to_ident().unwrap();
        assert_eq!("bar", ident.name);

        assert_eq!(String::from("12"), dot.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_self() {
        let expr = parse_expr("self");

        assert!(expr.is_this());
    }

    #[test]
    fn parse_neg() {
        let expr = parse_expr("-1");

        let un = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, un.op);

        assert!(un.opnd.is_lit_int());
    }

    #[test]
    fn parse_neg_twice() {
        let expr = parse_expr("-(-3)");

        let neg1 = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg1.op);

        let neg2 = neg1.opnd.to_paren().unwrap().expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg2.op);

        assert!(neg2.opnd.is_lit_int());
    }

    #[test]
    fn parse_neg_twice_without_parentheses() {
        err_expr("- -2", ParseError::ExpectedFactor, 1, 3);
    }

    #[test]
    fn parse_mul() {
        let expr = parse_expr("6*3");

        let mul = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul.op);
        assert_eq!(String::from("6"), mul.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), mul.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_multiple_muls() {
        let expr = parse_expr("6*3*4");

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
        let expr = parse_expr("4/5");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Div, div.op);
        assert_eq!(String::from("4"), div.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("5"), div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_mod() {
        let expr = parse_expr("2%15");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mod, div.op);
        assert_eq!(String::from("2"), div.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("15"), div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add() {
        let expr = parse_expr("2+3");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Add, add.op);
        assert_eq!(String::from("2"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add_left_associativity() {
        let expr = parse_expr("1+2+3");

        let add = expr.to_bin().unwrap();
        assert_eq!(String::from("3"), add.rhs.to_lit_int().unwrap().value);

        let lhs = add.lhs.to_bin().unwrap();
        assert_eq!(String::from("1"), lhs.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), lhs.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add_right_associativity_via_parens() {
        let expr = parse_expr("1+(2+3)");

        let add = expr.to_bin().unwrap();
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);

        let rhs = add.rhs.to_paren().unwrap().expr.to_bin().unwrap();
        assert_eq!(String::from("2"), rhs.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), rhs.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_sub() {
        let expr = parse_expr("1-2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Sub, add.op);
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_or() {
        let expr = parse_expr("1||2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Or, add.op);
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_and() {
        let expr = parse_expr("1&&2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::And, add.op);
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_or() {
        let expr = parse_expr("1|2");

        let or = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitOr, or.op);
        assert_eq!(String::from("1"), or.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), or.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_and() {
        let expr = parse_expr("1&2");

        let and = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitAnd, and.op);
        assert_eq!(String::from("1"), and.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), and.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_xor() {
        let expr = parse_expr("1^2");

        let xor = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitXor, xor.op);
        assert_eq!(String::from("1"), xor.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), xor.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_lt() {
        let expr = parse_expr("1<2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Lt), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_le() {
        let expr = parse_expr("1<=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Le), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_gt() {
        let expr = parse_expr("1>2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Gt), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_ge() {
        let expr = parse_expr("1>=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Ge), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_eq() {
        let expr = parse_expr("1==2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Eq), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_ne() {
        let expr = parse_expr("1!=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Ne), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_identity_not() {
        let expr = parse_expr("1!==2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::IsNot), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_identity() {
        let expr = parse_expr("1===2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Is), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_assign() {
        let expr = parse_expr("a=4");

        let assign = expr.to_bin().unwrap();
        assert!(assign.lhs.is_ident());
        assert_eq!(BinOp::Assign, assign.op);
        assert_eq!(String::from("4"), assign.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_shift_right() {
        let expr = parse_expr("a>>4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::ArithShiftR, bin.op);
    }

    #[test]
    fn parse_unsigned_shift_right() {
        let expr = parse_expr("a>>>4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::LogicalShiftR, bin.op);
    }

    #[test]
    fn parse_left() {
        let expr = parse_expr("a<<4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::ShiftL, bin.op);
    }

    #[test]
    fn parse_call_without_params() {
        let expr = parse_expr("fname()");

        let call = expr.to_call().unwrap();
        assert_eq!("fname", call.callee.to_ident().unwrap().name);
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_call_with_params() {
        let expr = parse_expr("fname2(1,2,3)");

        let call = expr.to_call().unwrap();
        assert_eq!("fname2", call.callee.to_ident().unwrap().name);
        assert_eq!(3, call.args.len());
    }

    #[test]
    fn parse_function() {
        let prog = parse("fn b() { }");
        let fct = prog.fct0();

        assert_eq!("b", fct.name.as_ref().unwrap().name_as_string);
        assert_eq!(0, fct.params.len());
        assert!(fct.return_type.is_none());
    }

    #[test]
    fn parse_function_with_single_param() {
        let p1 = parse("fn f(a:int) { }");
        let f1 = p1.fct0();

        let p2 = parse("fn f(a:int,) { }");
        let f2 = p2.fct0();

        assert_eq!(f1.params.len(), 1);
        assert_eq!(f2.params.len(), 1);

        let p1 = &f1.params[0];
        let p2 = &f2.params[0];

        assert_eq!("a", p1.name.as_ref().unwrap().name_as_string);
        assert_eq!("a", p2.name.as_ref().unwrap().name_as_string);

        assert_eq!("int", p1.data_type.to_basic().unwrap().name());
        assert_eq!("int", p2.data_type.to_basic().unwrap().name());
    }

    #[test]
    fn parse_function_with_multiple_params() {
        let p1 = parse("fn f(a:int, b:str) { }");
        let f1 = p1.fct0();

        let p2 = parse("fn f(a:int, b:str,) { }");
        let f2 = p2.fct0();

        let p1a = &f1.params[0];
        let p1b = &f1.params[1];
        let p2a = &f2.params[0];
        let p2b = &f2.params[1];

        assert_eq!("a", p1a.name.as_ref().unwrap().name_as_string);
        assert_eq!("a", p2a.name.as_ref().unwrap().name_as_string);

        assert_eq!("b", p1b.name.as_ref().unwrap().name_as_string);
        assert_eq!("b", p2b.name.as_ref().unwrap().name_as_string);

        assert_eq!("int", p1a.data_type.to_basic().unwrap().name());
        assert_eq!("int", p2a.data_type.to_basic().unwrap().name());

        assert_eq!("str", p1b.data_type.to_basic().unwrap().name());
        assert_eq!("str", p2b.data_type.to_basic().unwrap().name());
    }

    #[test]
    fn parse_let_without_type() {
        let stmt = parse_let("let a = 1;");
        let var = stmt.to_let().unwrap();

        assert!(var.data_type.is_none());
        assert!(var.expr.as_ref().unwrap().is_lit_int());
    }

    #[test]
    fn parse_let_rest() {
        let stmt = parse_let("let .. = 1;");
        let var = stmt.to_let().unwrap();
        assert!(var.pattern.first_alt().unwrap().is_rest());

        assert!(var.data_type.is_none());
        assert!(var.expr.as_ref().unwrap().is_lit_int());
    }

    #[test]
    fn parse_let_with_type() {
        let stmt = parse_let("let x : int = 1;");
        let var = stmt.to_let().unwrap();

        assert!(var.data_type.is_some());
        assert!(var.expr.as_ref().unwrap().is_lit_int());
    }

    #[test]
    fn parse_let_underscore() {
        let stmt = parse_let("let _ = 1;");
        let let_decl = stmt.to_let().unwrap();

        let pattern = let_decl.pattern.first_alt().unwrap();
        assert!(pattern.is_underscore());
    }

    #[test]
    fn parse_let_tuple() {
        let stmt = parse_let("let (mut a, b, (c, d)) = 1;");
        let let_decl = stmt.to_let().unwrap();

        let pattern = let_decl.pattern.first_alt().unwrap();
        let tuple = pattern.to_tuple().unwrap();
        let first: &Arc<Pattern> = tuple.params.first().unwrap();
        let first = first.first_alt().unwrap();
        assert!(first.is_ident());
        assert!(first.to_ident().unwrap().mutable);
        let last = tuple.params.last().unwrap();
        let last = last.first_alt().unwrap();
        assert!(last.is_tuple());
    }

    #[test]
    fn parse_let_lit_bool() {
        let stmt = parse_let("let (a, true) = 1;");
        let let_decl = stmt.to_let().unwrap();

        let pattern = let_decl.pattern.first_alt().unwrap();
        let tuple = pattern.to_tuple().unwrap();
        assert!(tuple.params[0].first_alt().unwrap().is_ident());
        assert!(tuple.params[1].first_alt().unwrap().is_lit_bool());
    }

    #[test]
    fn parse_let_ident() {
        let stmt = parse_let("let x = 1;");
        let let_decl = stmt.to_let().unwrap();

        let pattern = let_decl.pattern.first_alt().unwrap();
        assert!(pattern.is_ident());
    }

    #[test]
    fn parse_let_ident_mut() {
        let stmt = parse_let("let mut x = 1;");
        let let_decl = stmt.to_let().unwrap();

        let pattern = let_decl.pattern.first_alt().unwrap();
        assert!(pattern.to_ident().unwrap().mutable);
    }

    #[test]
    fn parse_let_with_type_but_without_assignment() {
        let stmt = parse_let("let x : int;");
        let var = stmt.to_let().unwrap();

        assert!(var.data_type.is_some());
        assert!(var.expr.is_none());
    }

    #[test]
    fn parse_let_without_type_and_assignment() {
        let stmt = parse_let("let x;");
        let var = stmt.to_let().unwrap();

        assert!(var.data_type.is_none());
        assert!(var.expr.is_none());
    }

    #[test]
    fn parse_multiple_functions() {
        let prog = parse("fn f() { } fn g() { }");

        let f = prog.fct0();
        assert_eq!("f", f.name.as_ref().unwrap().name_as_string);

        let g = prog.fct(1);
        assert_eq!("g", g.name.as_ref().unwrap().name_as_string);
    }

    #[test]
    fn parse_if() {
        let expr = parse_expr("if true { 2; } else { 3; }");
        let ifexpr = expr.to_if().unwrap();

        assert!(ifexpr.cond.is_lit_bool());
        assert!(ifexpr.else_block.is_some());
    }

    #[test]
    fn parse_if_without_else() {
        let expr = parse_expr("if true { 2; }");
        let ifexpr = expr.to_if().unwrap();

        assert!(ifexpr.cond.is_lit_bool());
        assert!(ifexpr.else_block.is_none());
    }

    #[test]
    fn parse_while() {
        let expr = parse_expr("while true { 2; }");
        let whilestmt = expr.to_while().unwrap();

        assert!(whilestmt.cond.is_lit_bool());
        assert!(whilestmt.block.is_block());
    }

    #[test]
    fn parse_empty_block() {
        let expr = parse_expr("{}");
        let block = expr.to_block().unwrap();

        assert_eq!(0, block.stmts.len());
    }

    #[test]
    fn parse_block_with_one_stmt() {
        let expr = parse_expr("{ 1; 2 }");
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
        let expr = parse_expr("{ 1; 2; }");
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
        let expr = parse_expr("break;");
        assert!(expr.is_break());
    }

    #[test]
    fn parse_continue() {
        let expr = parse_expr("continue;");
        assert!(expr.is_continue());
    }

    #[test]
    fn parse_return_value() {
        let expr = parse_expr("return 1;");
        let ret = expr.to_return().unwrap();

        assert_eq!(
            String::from("1"),
            ret.expr.as_ref().unwrap().to_lit_int().unwrap().value
        );
    }

    #[test]
    fn parse_return() {
        let expr = parse_expr("return;");
        let ret = expr.to_return().unwrap();

        assert!(ret.expr.is_none());
    }

    #[test]
    fn parse_type_basic() {
        let ty = parse_type("bla");
        let basic = ty.to_basic().unwrap();

        assert_eq!(0, basic.params.len());
        assert_eq!("bla", basic.name());
    }

    #[test]
    fn parse_type_basic_mod() {
        let ty = parse_type("foo::bla");
        let basic = ty.to_basic().unwrap();

        assert_eq!(0, basic.params.len());
        assert_eq!(2, basic.path.names.len());
        assert_eq!("foo", basic.path.names[0].name_as_string);
        assert_eq!("bla", basic.path.names[1].name_as_string);
    }

    #[test]
    fn parse_type_basic_with_params() {
        let ty = parse_type("Foo[A, B]");
        let basic = ty.to_basic().unwrap();

        assert_eq!(2, basic.params.len());
        assert_eq!("Foo", basic.name());
        assert_eq!("A", basic.params[0].to_basic().unwrap().name());
        assert_eq!("B", basic.params[1].to_basic().unwrap().name());
    }

    #[test]
    fn parse_type_lambda_no_params() {
        let ty = parse_type("(): ()");
        let fct = ty.to_fct().unwrap();

        assert_eq!(0, fct.params.len());
        assert!(fct.ret.as_ref().unwrap().is_unit());
    }

    #[test]
    fn parse_type_lambda_one_param() {
        let ty = parse_type("(A): B");
        let fct = ty.to_fct().unwrap();

        assert_eq!(1, fct.params.len());
        assert_eq!("A", fct.params[0].to_basic().unwrap().name());
        assert_eq!("B", fct.ret.as_ref().unwrap().to_basic().unwrap().name());
    }

    #[test]
    fn parse_type_lambda_two_params() {
        let ty = parse_type("(A, B): C");
        let fct = ty.to_fct().unwrap();

        assert_eq!(2, fct.params.len());
        assert_eq!("A", fct.params[0].to_basic().unwrap().name());
        assert_eq!("B", fct.params[1].to_basic().unwrap().name());
        assert_eq!("C", fct.ret.as_ref().unwrap().to_basic().unwrap().name());
    }

    #[test]
    fn parse_type_unit() {
        let ty = parse_type("()");
        let ty = ty.to_tuple().unwrap();

        assert!(ty.subtypes.is_empty());
    }

    #[test]
    fn parse_type_tuple_with_one_type() {
        let ty = parse_type("(c)");

        let subtypes = &ty.to_tuple().unwrap().subtypes;
        assert_eq!(1, subtypes.len());

        let ty = subtypes[0].to_basic().unwrap();
        assert_eq!("c", ty.name());
    }

    #[test]
    fn parse_type_tuple_with_two_types() {
        let ty = parse_type("(a, b)");

        let subtypes = &ty.to_tuple().unwrap().subtypes;
        assert_eq!(2, subtypes.len());

        let ty1 = subtypes[0].to_basic().unwrap();
        assert_eq!("a", ty1.name());

        let ty2 = subtypes[1].to_basic().unwrap();
        assert_eq!("b", ty2.name());
    }

    #[test]
    fn parse_class_with_param() {
        let prog = parse("class Foo(a: int)");
        let class = prog.cls0();
        assert_eq!(1, class.fields.len());
    }

    #[test]
    fn parse_class_with_param_var() {
        let prog = parse("class Foo(a: int)");
        let class = prog.cls0();

        assert_eq!(1, class.fields.len());
        assert_eq!(true, class.fields[0].mutable);
    }

    #[test]
    fn parse_class_with_params() {
        let prog = parse("class Foo(a: int, b: int)");
        let class = prog.cls0();

        assert_eq!(2, class.fields.len());
    }

    #[test]
    fn parse_class() {
        let prog = parse("class Foo { a: Int64, b: Bool }");
        let class = prog.cls0();
        assert_eq!(class.fields.len(), 2);

        let prog = parse("class Foo(a: Int64, b: Bool)");
        let class = prog.cls0();
        assert_eq!(class.fields.len(), 2);

        let prog = parse("class Foo");
        let class = prog.cls0();
        assert!(class.fields.is_empty());
    }

    #[test]
    fn parse_method_invocation() {
        let expr = parse_expr("a.foo()");
        let call = expr.to_call().unwrap();
        assert!(call.callee.is_dot());
        assert_eq!(0, call.args.len());

        let expr = parse_expr("a.foo(1)");
        let call = expr.to_call().unwrap();
        assert!(call.callee.is_dot());
        assert_eq!(1, call.args.len());

        let expr = parse_expr("a.foo(1,2)");
        let call = expr.to_call().unwrap();
        assert!(call.callee.is_dot());
        assert_eq!(2, call.args.len());
    }

    #[test]
    fn parse_array_index() {
        let expr = parse_expr("a(b)");
        let call = expr.to_call().unwrap();
        assert_eq!("a", call.callee.to_ident().unwrap().name);
        assert_eq!(1, call.args.len());
        assert_eq!("b", call.args[0].to_ident().unwrap().name);
    }

    #[test]
    fn parse_field() {
        let prog = parse("class A { f1: int, f2: int }");
        let cls = prog.cls0();

        let f1 = &cls.fields[0];
        assert_eq!("f1", f1.name.as_ref().unwrap().name_as_string);
        assert_eq!(true, f1.mutable);

        let f2 = &cls.fields[1];
        assert_eq!("f2", f2.name.as_ref().unwrap().name_as_string);
        assert_eq!(true, f2.mutable);
    }

    #[test]
    fn parse_as_expr() {
        let expr = parse_expr("a as String");
        let expr = expr.to_conv().unwrap();
        assert_eq!(true, expr.object.is_ident());
    }

    #[test]
    fn parse_internal() {
        parse("@internal fn foo();");
    }

    #[test]
    fn parse_function_without_body() {
        let prog = parse("fn foo();");
        let fct = prog.fct0();
        assert!(fct.block.is_none());
    }

    #[test]
    fn parse_struct_empty() {
        let prog = parse("struct Foo {}");
        let struc = prog.struct0();
        assert_eq!(0, struc.fields.len());
        assert_eq!("Foo", struc.name.as_ref().unwrap().name_as_string);
    }

    #[test]
    fn parse_struct_one_field() {
        let prog = parse(
            "struct Bar {
            f1: Foo1,
        }",
        );
        let struc = prog.struct0();
        assert_eq!(1, struc.fields.len());
        assert_eq!("Bar", struc.name.as_ref().unwrap().name_as_string);

        let f1 = &struc.fields[0];
        assert_eq!("f1", f1.name.as_ref().unwrap().name_as_string);
    }

    #[test]
    fn parse_struct_multiple_fields() {
        let prog = parse(
            "struct FooBar {
            fa: Foo1,
            fb: Foo2,
        }",
        );
        let struc = prog.struct0();
        assert_eq!(2, struc.fields.len());
        assert_eq!("FooBar", struc.name.as_ref().unwrap().name_as_string);

        let f1 = &struc.fields[0];
        assert_eq!("fa", f1.name.as_ref().unwrap().name_as_string);

        let f2 = &struc.fields[1];
        assert_eq!("fb", f2.name.as_ref().unwrap().name_as_string);
    }

    #[test]
    fn parse_struct_with_type_params() {
        let prog = parse(
            "struct Bar[T1, T2] {
            f1: T1, f2: T2,
        }",
        );
        let struct_ = prog.struct0();
        assert_eq!(2, struct_.fields.len());
        assert_eq!("Bar", struct_.name.as_ref().unwrap().name_as_string);

        assert_eq!(2, struct_.type_params.as_ref().unwrap().params.len());
    }

    #[test]
    fn parse_struct_lit_while() {
        let expr = parse_expr("while i < n { }");
        let while_expr = expr.to_while().unwrap();
        let bin = while_expr.cond.to_bin().unwrap();

        assert!(bin.lhs.is_ident());
        assert!(bin.rhs.is_ident());
    }

    #[test]
    fn parse_struct_lit_if() {
        let expr = parse_expr("if i < n { }");
        let ifexpr = expr.to_if().unwrap();
        let bin = ifexpr.cond.to_bin().unwrap();

        assert!(bin.lhs.is_ident());
        assert!(bin.rhs.is_ident());
    }

    #[test]
    fn parse_lit_float() {
        let expr = parse_expr("1.2");

        let lit = expr.to_lit_float().unwrap();

        assert_eq!("1.2", lit.value);
    }

    #[test]
    fn parse_template() {
        let expr = parse_expr("\"a${1}b${2}c\"");
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

        let expr = parse_expr("\"a\\${1}b\"");
        assert!(expr.is_lit_str());
    }

    #[test]
    fn parse_class_type_params() {
        let prog = parse("class Foo[T]");
        let cls = prog.cls0();

        let type_params = cls.type_params.as_ref().unwrap();
        assert_eq!(1, type_params.params.len());
        assert_eq!(
            "T",
            type_params.params[0].name.as_ref().unwrap().name_as_string
        );

        let prog = parse("class Foo[X]");
        let cls = prog.cls0();

        let type_params = cls.type_params.as_ref().unwrap();
        assert_eq!(1, type_params.params.len());
        assert_eq!(
            "X",
            type_params.params[0].name.as_ref().unwrap().name_as_string
        );
    }

    #[test]
    fn parse_multiple_class_type_params() {
        let prog = parse("class Foo[A, B]");
        let cls = prog.cls0();

        let type_params = cls.type_params.as_ref().unwrap();
        assert_eq!(2, type_params.params.len());
        assert_eq!(
            "A",
            type_params.params[0].name.as_ref().unwrap().name_as_string
        );
        assert_eq!(
            "B",
            type_params.params[1].name.as_ref().unwrap().name_as_string
        );
    }

    #[test]
    fn parse_empty_trait() {
        let prog = parse("trait Foo { }");
        let trait_ = prog.trait0();

        assert_eq!("Foo", trait_.name.as_ref().unwrap().name_as_string);
        assert_eq!(0, trait_.methods.len());
    }

    #[test]
    fn parse_trait_with_function() {
        let prog = parse("trait Foo { fn empty(); }");
        let trait_ = prog.trait0();

        assert_eq!("Foo", trait_.name.as_ref().unwrap().name_as_string);
        assert_eq!(1, trait_.methods.len());
    }

    #[test]
    fn parse_trait_with_static_function() {
        let prog = parse("trait Foo { static fn empty(); }");
        let trait_ = prog.trait0();

        assert_eq!("Foo", trait_.name.as_ref().unwrap().name_as_string);
        assert_eq!(1, trait_.methods.len());
    }

    #[test]
    fn parse_empty_impl() {
        let prog = parse("impl Foo for A {}");
        let impl_ = prog.impl0();

        assert_eq!("Foo", impl_.trait_type.as_ref().unwrap().to_string());
        assert_eq!("A", impl_.extended_type.to_string());
        assert_eq!(0, impl_.methods.len());
    }

    #[test]
    fn parse_impl_with_function() {
        let prog = parse("impl Bar for B { fn foo(); }");
        let impl_ = prog.impl0();

        assert_eq!("Bar", impl_.trait_type.as_ref().unwrap().to_string());
        assert_eq!("B", impl_.extended_type.to_string());
        assert_eq!(1, impl_.methods.len());
    }

    #[test]
    fn parse_impl_with_static_function() {
        let prog = parse("impl Bar for B { static fn foo(); }");
        let impl_ = prog.impl0();

        assert_eq!("Bar", impl_.trait_type.as_ref().unwrap().to_string());
        assert_eq!("B", impl_.extended_type.to_string());
        assert_eq!(1, impl_.methods.len());
    }

    #[test]
    fn parse_global_let() {
        let prog = parse("let b: int = 0;");
        let global = prog.global0();

        assert_eq!("b", global.name.as_ref().unwrap().name_as_string);
        assert_eq!(false, global.mutable);
    }

    #[test]
    fn parse_lit_char() {
        let expr = parse_expr("'a'");
        let lit = expr.to_lit_char().unwrap();

        assert_eq!("'a'", lit.value);
    }

    #[test]
    fn parse_fct_call_with_type_param() {
        let expr = parse_expr("Array[Int]()");
        let call = expr.to_call().unwrap();
        let type_params = call.callee.to_type_param().unwrap();

        assert_eq!(1, type_params.args.len());

        let expr = parse_expr("Foo[Int, Long]()");
        let call = expr.to_call().unwrap();
        let type_params = call.callee.to_type_param().unwrap();

        assert_eq!(2, type_params.args.len());

        let expr = parse_expr("Bar[]()");
        let call = expr.to_call().unwrap();
        let type_params = call.callee.to_type_param().unwrap();

        assert_eq!(0, type_params.args.len());

        let expr = parse_expr("Vec()");
        let call = expr.to_call().unwrap();

        assert!(call.callee.is_ident());
    }

    #[test]
    fn parse_call_with_path() {
        let expr = parse_expr("Foo::get()");
        let call = expr.to_call().unwrap();

        assert!(call.callee.is_path());
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_fct_with_type_params() {
        let prog = parse("fn f[T]() {}");
        let fct = prog.fct0();

        assert_eq!(1, fct.type_params.as_ref().unwrap().params.len());
    }

    #[test]
    fn parse_const() {
        let prog = parse("const x: int = 0;");
        let const_ = prog.const0();

        assert_eq!("x", const_.name.as_ref().unwrap().name_as_string);
    }

    #[test]
    fn parse_generic_with_bound() {
        let prog = parse("class A[T: Foo]");
        let cls = prog.cls0();

        let type_param = &cls.type_params.as_ref().unwrap().params[0];
        assert_eq!(1, type_param.bounds.len());
    }

    #[test]
    fn parse_generic_with_multiple_bounds() {
        let prog = parse("class A[T: Foo + Bar]");
        let cls = prog.cls0();

        let type_param = &cls.type_params.as_ref().unwrap().params[0];
        assert_eq!(2, type_param.bounds.len());
    }

    #[test]
    fn parse_lambda_no_params_no_return_value() {
        let expr = parse_expr("|| {}");
        let lambda = expr.to_lambda().unwrap();

        assert!(lambda.return_type.is_none());
    }

    #[test]
    fn parse_lambda_no_params_unit_as_return_value() {
        let expr = parse_expr("|| : () {}");
        let lambda = expr.to_lambda().unwrap();
        let ret = lambda.return_type.as_ref().unwrap();

        assert!(ret.is_unit());
    }

    #[test]
    fn parse_lambda_no_params_with_return_value() {
        let expr = parse_expr("||: A {}");
        let lambda = expr.to_lambda().unwrap();
        let ret = lambda.return_type.as_ref().unwrap();
        let basic = ret.to_basic().unwrap();

        assert_eq!("A", basic.name());
    }

    #[test]
    fn parse_lambda_with_one_param() {
        let expr = parse_expr("|a: A|: B {}");
        let lambda = expr.to_lambda().unwrap();

        assert_eq!(1, lambda.params.len());

        let param = &lambda.params[0];
        assert_eq!("a", param.name.as_ref().unwrap().name_as_string);
        let basic = param.data_type.to_basic().unwrap();
        assert_eq!("A", basic.name());

        let ret = lambda.return_type.as_ref().unwrap();
        let basic = ret.to_basic().unwrap();

        assert_eq!("B", basic.name());
    }

    #[test]
    fn parse_lambda_with_two_params() {
        let expr = parse_expr("|a: A, b: B|: C {}");
        let lambda = expr.to_lambda().unwrap();

        assert_eq!(2, lambda.params.len());

        let param = &lambda.params[0];
        assert_eq!("a", param.name.as_ref().unwrap().name_as_string);
        let basic = param.data_type.to_basic().unwrap();
        assert_eq!("A", basic.name());

        let param = &lambda.params[1];
        assert_eq!("b", param.name.as_ref().unwrap().name_as_string);
        let basic = param.data_type.to_basic().unwrap();
        assert_eq!("B", basic.name());

        let ret = lambda.return_type.as_ref().unwrap();
        let basic = ret.to_basic().unwrap();

        assert_eq!("C", basic.name());
    }

    #[test]
    fn parse_for() {
        let expr = parse_expr("for i in a+b {}");
        assert!(expr.is_for());
    }

    #[test]
    fn parse_new_call_ident() {
        let expr = parse_expr("i");
        assert!(expr.is_ident());
    }

    #[test]
    fn parse_new_call_path() {
        let expr = parse_expr("Foo::bar");
        let path = expr.to_path().unwrap();
        assert!(path.lhs.is_ident());
        assert!(path.rhs.is_ident());
    }

    #[test]
    fn parse_new_call_call() {
        let expr = parse_expr("foo(1,2)");
        let call = expr.to_call().unwrap();
        assert!(call.callee.is_ident());
        assert_eq!(call.args.len(), 2);
    }

    #[test]
    fn parse_block() {
        let expr = parse_expr("{1}");
        assert!(expr.to_block().unwrap().expr.as_ref().unwrap().is_lit_int());

        let expr = parse_expr("({}) + 1");
        assert!(expr.is_bin());

        let expr = parse_expr("1 + {}");
        assert!(expr.is_bin());
    }

    #[test]
    fn parse_tuple() {
        let expr = parse_expr("(1,)");
        assert_eq!(expr.to_tuple().unwrap().values.len(), 1);

        let expr = parse_expr("(1)");
        assert!(expr.is_paren());

        let expr = parse_expr("(1,2,3)");
        assert_eq!(expr.to_tuple().unwrap().values.len(), 3);

        let expr = parse_expr("(1,2,3,4,)");
        assert_eq!(expr.to_tuple().unwrap().values.len(), 4);
    }

    #[test]
    fn parse_enum() {
        let prog = parse("enum Foo { A, B, C }");
        let enum_ = prog.enum0();
        assert_eq!(enum_.variants.len(), 3);
    }

    #[test]
    fn parse_enum_with_type_params() {
        let prog = parse("enum MyOption[T] { None, Some(T), }");
        let enum_ = prog.enum0();
        assert_eq!(enum_.variants.len(), 2);
        assert!(enum_.variants[0].types.is_none());
        assert_eq!(enum_.variants[1].types.as_ref().unwrap().len(), 1);
    }

    #[test]
    fn parse_module() {
        let prog = parse("mod foo { fn bar() {} fn baz() {} }");
        let module = prog.module0();
        let elements = module.elements.as_ref().unwrap();
        assert_eq!(elements.len(), 2);
        assert!(elements[0].to_function().is_some());
        assert!(elements[1].to_function().is_some());
    }

    #[test]
    fn parse_mod_without_body() {
        let prog = parse("mod foo;");
        let module = prog.module0();
        assert!(module.elements.is_none());
    }

    #[test]
    fn parse_match() {
        parse_expr("match x { }");
        parse_expr("match x { A(x, b) => 1, B => 2 }");
        parse_expr("match x { A(x, b) => 1, B | C => 2 }");
        parse_expr("match x { A(x, b) => { 1 } B | C => { 2 } }");
    }

    #[test]
    fn parse_type_alias_in_trait() {
        parse(
            "trait Foo {
            type MY_TYPE;
        }",
        );
    }

    #[test]
    fn parse_where_clauses() {
        parse(
            "
            fn f() where A: B {}
            struct F where A: B, C: D {}
            class F where A: B + C, D: E {}
            impl F for X where A: B + C + D, E: F {}
            trait F where A: B {}
            enum F where A: B + C { A, B }
        ",
        );
    }

    #[test]
    fn parse_is() {
        parse_expr("x is Foo");
        parse_expr("x is Foo::Bar");
        parse_expr("x is Foo::Bar(a, b, c)");
    }
}
