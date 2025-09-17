use std::sync::Arc;

use crate::ast;
use crate::ast::*;
use crate::error::{ParseError, ParseErrorWithLocation};

use crate::token::{
    ELEM_FIRST, EMPTY, ENUM_VARIANT_RS, EXPRESSION_FIRST, FIELD_FIRST, MODIFIER_FIRST,
    PARAM_LIST_RS, PATTERN_FIRST, PATTERN_RS, TYPE_FIRST, TYPE_PARAM_RS, UNNAMED_FIELD_FIRST,
    USE_PATH_ATOM_FIRST, USE_PATH_FIRST,
};
use crate::TokenKind::*;
use crate::{lex, Span, TokenKind, TokenSet};

#[cfg(test)]
mod tests;

pub struct Parser {
    tokens: Vec<TokenKind>,
    token_widths: Vec<u32>,
    token_idx: usize,
    next_node_id: usize,
    content: Arc<String>,
    errors: Vec<ParseErrorWithLocation>,
    nodes: Vec<(usize, u32)>,
    offset: u32,
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
        }
    }

    fn new_node_id(&mut self) -> NodeId {
        let value = self.next_node_id;
        self.next_node_id += 1;
        NodeId(value)
    }

    pub fn parse(mut self) -> (Arc<ast::File>, Vec<ParseErrorWithLocation>) {
        self.skip_trivia();
        let mut elements = vec![];

        while !self.is_eof() {
            elements.push(self.parse_element());
        }

        assert!(self.nodes.is_empty());

        (
            Arc::new(ast::File {
                content: self.content.clone(),
                elements,
            }),
            self.errors,
        )
    }

    fn parse_element(&mut self) -> Elem {
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
                let alias = self.parse_alias(modifiers);
                Arc::new(ElemData::Alias(alias))
            }

            _ => {
                assert!(!ELEM_FIRST.contains(self.current()));
                let span = self.current_span();
                self.report_error_at(ParseError::ExpectedElement, span);
                self.advance();

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
        self.expect(SEMICOLON);

        Arc::new(ExternPackage {
            id: self.new_node_id(),
            span: self.finish_node(),
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

        Arc::new(Use {
            id: self.new_node_id(),
            span: self.finish_node(),
            modifiers,
            path,
        })
    }

    fn parse_use_path(&mut self) -> Arc<UsePath> {
        self.start_node();
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

        Arc::new(UsePath {
            id: self.new_node_id(),
            span: self.finish_node(),
            path,
            target,
        })
    }

    fn parse_use_as(&mut self) -> UseTargetName {
        self.start_node();
        self.assert(AS_KW);

        let name = if self.eat(UNDERSCORE) {
            None
        } else {
            self.expect_identifier()
        };

        UseTargetName {
            span: self.finish_node(),
            name,
        }
    }

    fn parse_use_atom(&mut self) -> UseAtom {
        assert!(self.is_set(USE_PATH_ATOM_FIRST));
        self.start_node();

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

        UseAtom {
            span: self.finish_node(),
            value,
        }
    }

    fn parse_use_group(&mut self) -> Arc<UseGroup> {
        self.start_node();

        let targets = self.parse_list(
            L_BRACE,
            COMMA,
            R_BRACE,
            ELEM_FIRST,
            ParseError::ExpectedUsePath,
            |p| {
                if p.is_set(USE_PATH_FIRST) {
                    Some(p.parse_use_path())
                } else {
                    None
                }
            },
        );

        Arc::new(UseGroup {
            id: self.new_node_id(),
            span: self.finish_node(),
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

        Arc::new(Enum {
            id: self.new_node_id(),
            span: self.finish_node(),
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

        Arc::new(Module {
            id: self.new_node_id(),
            span: self.finish_node(),
            modifiers: modifiers.clone(),
            name,
            elements,
        })
    }

    fn parse_enum_variant(&mut self) -> EnumVariant {
        self.start_node();
        let name = self.expect_identifier();
        let field_name_style;

        let fields = if self.is(L_PAREN) {
            field_name_style = FieldNameStyle::Positional;

            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(UNNAMED_FIELD_FIRST) {
                        Some(p.parse_unnamed_field())
                    } else {
                        None
                    }
                },
            )
        } else if self.is(L_BRACE) {
            field_name_style = FieldNameStyle::Named;

            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        Some(p.parse_named_field())
                    } else {
                        None
                    }
                },
            )
        } else {
            field_name_style = FieldNameStyle::Positional;
            Vec::new()
        };

        EnumVariant {
            id: self.new_node_id(),
            span: self.finish_node(),
            name,
            field_name_style,
            fields,
        }
    }

    fn parse_const(&mut self, modifiers: Option<ModifierList>) -> Arc<Const> {
        self.start_node();
        self.assert(CONST_KW);
        let name = self.expect_identifier();
        self.expect(COLON);
        let ty = self.parse_type();
        self.expect(EQ);
        let expr = self.parse_expr();
        self.expect(SEMICOLON);

        Arc::new(Const {
            id: self.new_node_id(),
            span: self.finish_node(),
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

        Arc::new(Impl {
            id: self.new_node_id(),
            declaration_span,
            span: self.finish_node(),
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
            Some(self.parse_expr())
        } else {
            None
        };

        self.expect(SEMICOLON);

        Arc::new(Global {
            id: self.new_node_id(),
            name,
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
        let bounds = if self.eat(COLON) {
            self.parse_type_bounds()
        } else {
            Vec::new()
        };
        let where_bounds = self.parse_where();

        self.expect(L_BRACE);

        let mut methods = Vec::new();

        while !self.is(R_BRACE) && !self.is_eof() {
            methods.push(self.parse_element());
        }

        self.expect(R_BRACE);

        Arc::new(Trait {
            id: self.new_node_id(),
            name,
            modifiers: modifiers.clone(),
            type_params,
            bounds,
            where_bounds,
            span: self.finish_node(),
            methods,
        })
    }

    fn parse_alias(&mut self, modifiers: Option<ModifierList>) -> Arc<Alias> {
        self.start_node();
        self.assert(TYPE_KW);
        let name = self.expect_identifier();
        let type_params = self.parse_type_params();
        let pre_where_bounds = self.parse_where();
        let bounds = if self.eat(COLON) {
            self.parse_type_bounds()
        } else {
            Vec::new()
        };
        let (ty, post_where_bounds) = if self.eat(EQ) {
            let ty = self.parse_type();
            let post_where_bounds = self.parse_where();
            (Some(ty), post_where_bounds)
        } else {
            (None, None)
        };
        self.expect(SEMICOLON);

        Arc::new(Alias {
            id: self.new_node_id(),
            span: self.finish_node(),
            modifiers,
            name,
            type_params,
            pre_where_bounds,
            bounds,
            ty,
            post_where_bounds,
        })
    }

    fn parse_struct(&mut self, modifiers: Option<ModifierList>) -> Arc<Struct> {
        self.start_node();
        self.assert(STRUCT_KW);
        let ident = self.expect_identifier();
        let type_params = self.parse_type_params();
        let where_bounds = self.parse_where();
        let field_style;

        let fields = if self.is(L_PAREN) {
            field_style = FieldNameStyle::Positional;
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(UNNAMED_FIELD_FIRST) {
                        Some(p.parse_unnamed_field())
                    } else {
                        None
                    }
                },
            )
        } else if self.is(L_BRACE) {
            field_style = FieldNameStyle::Named;

            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        Some(p.parse_named_field())
                    } else {
                        None
                    }
                },
            )
        } else {
            field_style = FieldNameStyle::Positional;
            Vec::new()
        };

        Arc::new(Struct {
            id: self.new_node_id(),
            name: ident,
            modifiers: modifiers.clone(),
            span: self.finish_node(),
            fields,
            type_params,
            where_bounds,
            field_style,
        })
    }

    fn parse_named_field(&mut self) -> Arc<Field> {
        self.start_node();

        let modifiers = self.parse_modifiers();

        let ident = self.expect_identifier();

        self.expect(COLON);
        let ty = self.parse_type();

        Arc::new(Field {
            id: self.new_node_id(),
            span: self.finish_node(),
            modifiers,
            name: ident,
            data_type: ty,
        })
    }

    fn parse_unnamed_field(&mut self) -> Arc<Field> {
        self.start_node();

        let modifiers = self.parse_modifiers();
        let ty = self.parse_type();

        Arc::new(Field {
            id: self.new_node_id(),
            span: self.finish_node(),
            modifiers,
            name: None,
            data_type: ty,
        })
    }

    fn parse_class(&mut self, modifiers: Option<ModifierList>) -> Arc<Class> {
        self.start_node();
        self.assert(CLASS_KW);

        let name = self.expect_identifier();
        let type_params = self.parse_type_params();
        let where_bounds = self.parse_where();
        let field_name_style;

        let fields = if self.is(L_PAREN) {
            field_name_style = FieldNameStyle::Positional;

            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(UNNAMED_FIELD_FIRST) {
                        Some(p.parse_unnamed_field())
                    } else {
                        None
                    }
                },
            )
        } else if self.is(L_BRACE) {
            field_name_style = FieldNameStyle::Named;

            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        Some(p.parse_named_field())
                    } else {
                        None
                    }
                },
            )
        } else {
            field_name_style = FieldNameStyle::Positional;
            Vec::new()
        };

        Arc::new(Class {
            id: self.new_node_id(),
            span: self.finish_node(),
            modifiers: modifiers.clone(),
            name,
            fields,
            type_params,
            where_bounds,
            field_name_style,
        })
    }

    fn parse_type_params(&mut self) -> Option<TypeParams> {
        if self.is(L_BRACKET) {
            self.start_node();
            let params = self.parse_list(
                L_BRACKET,
                COMMA,
                R_BRACKET,
                TYPE_PARAM_RS,
                ParseError::ExpectedTypeParam,
                |p| p.parse_type_param_wrapper(),
            );

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
        let name = self.expect_identifier();

        let bounds = if self.eat(COLON) {
            self.parse_type_bounds()
        } else {
            Vec::new()
        };

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
            let mut modifiers: Vec<Modifier> = Vec::new();

            while self.is_set(MODIFIER_FIRST) {
                modifiers.push(self.parse_modifier());
            }

            assert!(!modifiers.is_empty());

            Some(ModifierList {
                id: self.new_node_id(),
                span: self.finish_node(),
                modifiers,
            })
        } else {
            None
        }
    }

    fn parse_modifier(&mut self) -> Modifier {
        self.start_node();

        let kind = self.current();
        let mut ident = None;

        if self.eat(PUB_KW) {
            // done
        } else if self.eat(STATIC_KW) {
            // done
        } else {
            self.assert(AT);
            ident = self.expect_identifier();
        }

        Modifier {
            id: self.new_node_id(),
            span: self.finish_node(),
            kind,
            ident,
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
        })
    }

    fn parse_function_params(&mut self) -> Vec<Arc<Param>> {
        if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                PARAM_LIST_RS,
                ParseError::ExpectedParam,
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
        mut parse: F,
    ) -> Vec<R>
    where
        F: FnMut(&mut Parser) -> Option<R>,
    {
        let mut data = vec![];
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

        data
    }

    fn parse_function_param_wrapper(&mut self) -> Option<Arc<Param>> {
        Some(self.parse_function_param())
    }

    fn parse_function_param(&mut self) -> Arc<Param> {
        self.start_node();
        let pattern = self.parse_pattern_alt();

        self.expect(COLON);

        let data_type = self.parse_type();

        let variadic = self.eat(DOT_DOT_DOT);

        Arc::new(Param {
            id: self.new_node_id(),
            span: self.finish_node(),
            pattern,
            data_type,
            variadic,
        })
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
        if self.is(WHERE_KW) {
            self.start_node();
            self.assert(WHERE_KW);

            let mut clauses = Vec::new();

            loop {
                clauses.push(self.parse_where_clause());

                if !self.eat(COMMA) {
                    break;
                }
            }

            Some(Arc::new(WhereBoundsData {
                id: self.new_node_id(),
                span: self.finish_node(),
                clauses,
            }))
        } else {
            None
        }
    }

    fn parse_where_clause(&mut self) -> WhereClause {
        self.start_node();
        let ty = self.parse_type();
        self.expect(COLON);
        let mut bounds = Vec::new();

        loop {
            bounds.push(self.parse_type());

            if !self.eat(ADD) {
                break;
            }
        }

        Arc::new(WhereBoundData {
            id: self.new_node_id(),
            span: self.finish_node(),
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
        if self.is_set(TYPE_FIRST) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.current() {
            IDENTIFIER | UPCASE_SELF_KW => {
                self.start_node();
                let path = self.parse_path();

                let params = if self.is(L_BRACKET) {
                    self.parse_list(
                        L_BRACKET,
                        COMMA,
                        R_BRACKET,
                        TYPE_PARAM_RS,
                        ParseError::ExpectedType,
                        |p| p.parse_type_argument(),
                    )
                } else {
                    Vec::new()
                };

                Arc::new(TypeData::create_regular(
                    self.new_node_id(),
                    self.finish_node(),
                    path,
                    params,
                ))
            }

            L_BRACKET => {
                self.start_node();
                self.assert(L_BRACKET);
                let ty = self.parse_type();
                self.expect(AS_KW);
                let trait_ty = self.parse_type();
                self.expect(R_BRACKET);
                self.expect(COLON_COLON);
                let name = self.expect_identifier();

                Arc::new(TypeData::create_qualified_path(
                    self.new_node_id(),
                    self.finish_node(),
                    ty,
                    trait_ty,
                    name,
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
                    |p| p.parse_type_wrapper(),
                );

                if self.eat(COLON) {
                    let ret = self.parse_type();

                    Arc::new(TypeData::create_fct(
                        self.new_node_id(),
                        self.finish_node(),
                        subtypes,
                        Some(ret),
                    ))
                } else {
                    Arc::new(TypeData::create_tuple(
                        self.new_node_id(),
                        self.finish_node(),
                        subtypes,
                    ))
                }
            }

            _ => {
                let span = self.current_span();
                self.report_error(ParseError::ExpectedType);
                Arc::new(TypeData::Error {
                    id: self.new_node_id(),
                    span,
                })
            }
        }
    }

    fn parse_type_argument(&mut self) -> Option<Arc<TypeArgument>> {
        self.start_node();

        if self.is2(IDENTIFIER, EQ) {
            let name = self.expect_identifier().expect("identifier expected");
            self.assert(EQ);
            let ty = self.parse_type();

            Some(Arc::new(TypeArgument {
                id: self.new_node_id(),
                span: self.finish_node(),
                name: Some(name),
                ty,
            }))
        } else if let Some(ty) = self.parse_type_wrapper() {
            Some(Arc::new(TypeArgument {
                id: self.new_node_id(),
                span: self.finish_node(),
                name: None,
                ty,
            }))
        } else {
            self.cancel_node();
            None
        }
    }

    fn parse_path(&mut self) -> Path {
        self.start_node();
        let mut segments = Vec::new();
        let segment = self.parse_path_segment();
        segments.push(segment);

        while self.eat(COLON_COLON) {
            let segment = self.parse_path_segment();
            segments.push(segment);
        }

        Arc::new(PathData {
            id: self.new_node_id(),
            span: self.finish_node(),
            segments,
        })
    }

    fn parse_path_segment(&mut self) -> PathSegment {
        if self.is(IDENTIFIER) {
            self.start_node();
            let name = self.expect_identifier().expect("ident expected");
            Arc::new(PathSegmentData::Ident(PathSegmentIdent {
                id: self.new_node_id(),
                span: self.finish_node(),
                name,
            }))
        } else if self.is(UPCASE_SELF_KW) {
            self.start_node();
            self.assert(UPCASE_SELF_KW);
            Arc::new(PathSegmentData::Self_(PathSegmentSelf {
                id: self.new_node_id(),
                span: self.finish_node(),
            }))
        } else {
            let span = self.current_span();
            Arc::new(PathSegmentData::Error {
                id: self.new_node_id(),
                span,
            })
        }
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
            let expr = self.parse_expr();

            Some(expr)
        } else {
            None
        }
    }

    fn parse_block(&mut self) -> Expr {
        self.start_node();
        let mut stmts = vec![];
        let mut expr = None;

        if self.expect(L_BRACE) {
            while !self.is(R_BRACE) && !self.is_eof() {
                let stmt_or_expr = self.parse_block_stmt();

                match stmt_or_expr {
                    StmtOrExpr::Stmt(stmt) => stmts.push(stmt),
                    StmtOrExpr::Expr(curr_expr) => expr = Some(curr_expr),
                }
            }

            self.expect(R_BRACE);
        }

        Arc::new(ExprData::create_block(
            self.new_node_id(),
            self.finish_node(),
            stmts,
            expr,
        ))
    }

    fn parse_block_stmt(&mut self) -> StmtOrExpr {
        match self.current() {
            LET_KW => StmtOrExpr::Stmt(self.parse_let()),
            _ => {
                if self.is_set(EXPRESSION_FIRST) {
                    let expr = self.parse_expr_stmt();

                    if self.is(R_BRACE) {
                        StmtOrExpr::Expr(expr)
                    } else {
                        if expr.is_blocklike() {
                            self.eat(SEMICOLON);
                        } else {
                            self.expect(SEMICOLON);
                        }

                        StmtOrExpr::Stmt(Arc::new(StmtData::create_expr(
                            self.new_node_id(),
                            expr.span(),
                            expr,
                        )))
                    }
                } else {
                    let span = self.current_span();
                    self.report_error(ParseError::ExpectedStatement);

                    if !self.is(R_BRACE) {
                        self.advance();
                    }

                    StmtOrExpr::Stmt(Arc::new(StmtData::create_expr(
                        self.new_node_id(),
                        span,
                        Arc::new(ExprData::Error {
                            id: self.new_node_id(),
                            span,
                        }),
                    )))
                }
            }
        }
    }

    fn parse_if(&mut self) -> Expr {
        self.start_node();
        self.assert(IF_KW);

        let cond = self.parse_expr();

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

        Arc::new(ExprData::create_if(
            self.new_node_id(),
            self.finish_node(),
            cond,
            then_block,
            else_block,
        ))
    }

    fn parse_match(&mut self) -> Expr {
        self.start_node();
        self.assert(MATCH_KW);

        let expr = self.parse_expr();
        let mut cases = Vec::new();

        self.expect(L_BRACE);

        while !self.is(R_BRACE) && !self.is_eof() {
            let case = self.parse_match_arm();
            let is_block = case.value.is_block();
            cases.push(case);

            if !self.is(R_BRACE) && !self.is_eof() {
                if is_block {
                    self.eat(COMMA);
                } else {
                    self.expect(COMMA);
                }
            }
        }

        self.expect(R_BRACE);

        Arc::new(ExprData::create_match(
            self.new_node_id(),
            self.finish_node(),
            expr,
            cases,
        ))
    }

    fn parse_match_arm(&mut self) -> Arc<MatchArmType> {
        self.start_node();
        let pattern = self.parse_pattern();

        let cond = if self.eat(IF_KW) {
            let expr = self.parse_expr();
            Some(expr)
        } else {
            None
        };

        self.expect(DOUBLE_ARROW);

        let value = self.parse_expr_stmt();

        Arc::new(MatchArmType {
            id: self.new_node_id(),
            span: self.finish_node(),
            pattern,
            cond,
            value,
        })
    }

    fn parse_pattern(&mut self) -> Arc<Pattern> {
        self.start_node();

        let pattern = self.parse_pattern_alt();

        if self.is(OR) {
            let mut alts = vec![pattern];

            while self.eat(OR) {
                alts.push(self.parse_pattern_alt());
            }

            Arc::new(Pattern::Alt(PatternAlt {
                id: self.new_node_id(),
                span: self.finish_node(),
                alts,
            }))
        } else {
            self.cancel_node();
            pattern
        }
    }

    fn parse_pattern_alt(&mut self) -> Arc<Pattern> {
        self.start_node();

        if self.eat(UNDERSCORE) {
            Arc::new(Pattern::Underscore(PatternUnderscore {
                id: self.new_node_id(),
                span: self.finish_node(),
            }))
        } else if self.eat(DOT_DOT) {
            Arc::new(Pattern::Rest(PatternRest {
                id: self.new_node_id(),
                span: self.finish_node(),
            }))
        } else if self.is(TRUE) || self.is(FALSE) {
            let expr = self.parse_lit_bool();
            Arc::new(Pattern::LitBool(PatternLit {
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
                |p| {
                    if p.is_set(PATTERN_FIRST) {
                        Some(p.parse_pattern())
                    } else {
                        None
                    }
                },
            );

            Arc::new(Pattern::Tuple(PatternTuple {
                id: self.new_node_id(),
                span: self.finish_node(),
                params,
            }))
        } else if self.is(CHAR_LITERAL) {
            let expr = self.parse_lit_char();
            Arc::new(Pattern::LitChar(PatternLit {
                id: self.new_node_id(),
                span: self.finish_node(),
                expr,
            }))
        } else if self.is(STRING_LITERAL) {
            let expr = self.parse_string();
            Arc::new(Pattern::LitString(PatternLit {
                id: self.new_node_id(),
                span: self.finish_node(),
                expr,
            }))
        } else if self.is(INT_LITERAL) || self.is2(SUB, INT_LITERAL) {
            let expr = self.parse_lit_int_minus();
            Arc::new(Pattern::LitInt(PatternLit {
                id: self.new_node_id(),
                span: self.finish_node(),
                expr,
            }))
        } else if self.is(FLOAT_LITERAL) || self.is2(SUB, FLOAT_LITERAL) {
            let expr = self.parse_lit_float_minus();
            Arc::new(Pattern::LitFloat(PatternLit {
                id: self.new_node_id(),
                span: self.finish_node(),
                expr,
            }))
        } else if self.is2(MUT_KW, IDENTIFIER) {
            self.assert(MUT_KW);
            let name = self.expect_identifier().expect("identifier expected");
            Arc::new(Pattern::Ident(PatternIdent {
                id: self.new_node_id(),
                span: self.finish_node(),
                mutable: true,
                name,
            }))
        } else if self.is(IDENTIFIER) {
            if !self.nth_is(1, COLON_COLON) && !self.nth_is(1, L_PAREN) {
                let name = self.expect_identifier().expect("identifier expected");
                return Arc::new(Pattern::Ident(PatternIdent {
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
                    |p| {
                        if p.is2(IDENTIFIER, EQ) {
                            p.start_node();
                            let ident = p.expect_identifier().expect("identifier expected");
                            p.assert(EQ);
                            let pattern = p.parse_pattern();

                            Some(Arc::new(PatternField {
                                id: p.new_node_id(),
                                span: p.finish_node(),
                                ident: Some(ident),
                                pattern,
                            }))
                        } else if p.is_set(PATTERN_FIRST) {
                            p.start_node();
                            let pattern = p.parse_pattern();

                            Some(Arc::new(PatternField {
                                id: p.new_node_id(),
                                span: p.finish_node(),
                                ident: None,
                                pattern,
                            }))
                        } else {
                            None
                        }
                    },
                );

                Some(params)
            } else {
                None
            };

            Arc::new(Pattern::ClassOrStructOrEnum(PatternClassOrStructOrEnum {
                id: self.new_node_id(),
                span: self.finish_node(),
                path,
                params,
            }))
        } else {
            self.report_error(ParseError::ExpectedPattern);
            self.advance();

            Arc::new(Pattern::Error(PatternError {
                id: self.new_node_id(),
                span: self.finish_node(),
            }))
        }
    }

    fn parse_for(&mut self) -> Expr {
        self.start_node();
        self.assert(FOR_KW);
        let pattern = self.parse_pattern();
        self.expect(IN_KW);
        let expr = self.parse_expr();
        let block = self.parse_block();

        Arc::new(ExprData::create_for(
            self.new_node_id(),
            self.finish_node(),
            pattern,
            expr,
            block,
        ))
    }

    fn parse_while(&mut self) -> Expr {
        self.start_node();
        self.assert(WHILE_KW);
        let expr = self.parse_expr();
        let block = self.parse_block();

        Arc::new(ExprData::create_while(
            self.new_node_id(),
            self.finish_node(),
            expr,
            block,
        ))
    }

    fn parse_break(&mut self) -> Expr {
        self.start_node();
        self.assert(BREAK_KW);

        Arc::new(ExprData::create_break(
            self.new_node_id(),
            self.finish_node(),
        ))
    }

    fn parse_continue(&mut self) -> Expr {
        self.start_node();
        self.assert(CONTINUE_KW);

        Arc::new(ExprData::create_continue(
            self.new_node_id(),
            self.finish_node(),
        ))
    }

    fn parse_return(&mut self) -> Expr {
        self.start_node();
        self.assert(RETURN_KW);
        let expr = if self.is_set(EXPRESSION_FIRST) {
            let expr = self.parse_expr();
            Some(expr)
        } else {
            None
        };

        Arc::new(ExprData::create_return(
            self.new_node_id(),
            self.finish_node(),
            expr,
        ))
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_expr_bp(0, false)
    }

    fn parse_expr_stmt(&mut self) -> Expr {
        self.parse_expr_bp(0, true)
    }

    fn parse_expr_bp(&mut self, min_bp: u32, prefer_stmt: bool) -> Expr {
        if !self.is_set(EXPRESSION_FIRST) {
            self.report_error(ParseError::ExpectedExpression);
            return Arc::new(ExprData::Error {
                id: self.new_node_id(),
                span: self.current_span(),
            });
        }

        let start = self.current_span().start();
        let mut left = self.parse_unary_expr(prefer_stmt);

        loop {
            let op = self.current();

            left = match op {
                AS_KW => {
                    self.assert(AS_KW);
                    let right = self.parse_type();
                    let span = self.span_from(start);

                    let expr = ExprData::create_conv(self.new_node_id(), span, left, right);

                    Arc::new(expr)
                }

                IS_KW => {
                    self.assert(IS_KW);
                    let right = self.parse_pattern();
                    let span = self.span_from(start);

                    let expr = ExprData::create_is(self.new_node_id(), span, left, right);

                    Arc::new(expr)
                }

                _ => left,
            };

            let op = self.current();
            let (l_bp, r_bp) = match op {
                EQ | ADD_EQ | SUB_EQ | MUL_EQ | DIV_EQ | MOD_EQ | OR_EQ | AND_EQ | CARET_EQ
                | LT_LT_EQ | GT_GT_EQ | GT_GT_GT_EQ => (1, 2),
                OR_OR => (2, 1),
                AND_AND => (3, 2),
                EQ_EQ | NOT_EQ | LT | LE | GT | GE | EQ_EQ_EQ | NOT_EQ_EQ => (4, 5),
                ADD | SUB | OR | CARET => (5, 6),
                MUL | DIV | MODULO | AND | LT_LT | GT_GT | GT_GT_GT => (6, 7),
                _ => {
                    return left;
                }
            };

            if l_bp < min_bp {
                return left;
            }

            self.advance();

            let right = self.parse_expr_bp(r_bp, prefer_stmt);
            left = self.create_binary(op, start, left, right);
        }
    }

    fn parse_unary_expr(&mut self, prefer_stmt: bool) -> Expr {
        match self.current() {
            SUB | NOT => {
                self.start_node();
                let kind = self.current();
                self.advance();
                let op = match kind {
                    SUB => UnOp::Neg,
                    NOT => UnOp::Not,
                    _ => unreachable!(),
                };

                let expr = self.parse_postfix_expr(prefer_stmt);
                Arc::new(ExprData::create_un(
                    self.new_node_id(),
                    self.finish_node(),
                    op,
                    expr,
                ))
            }

            _ => self.parse_postfix_expr(prefer_stmt),
        }
    }

    fn parse_postfix_expr(&mut self, prefer_stmt: bool) -> Expr {
        let start = self.current_span().start();
        let mut left = self.parse_factor();

        loop {
            left = match self.current() {
                DOT => {
                    let op_span = self.current_span();
                    self.assert(DOT);
                    let rhs = self.parse_factor();
                    let span = self.span_from(start);

                    Arc::new(ExprData::create_dot(
                        self.new_node_id(),
                        span,
                        op_span,
                        left,
                        rhs,
                    ))
                }

                L_PAREN if !(left.is_blocklike() && prefer_stmt) => self.parse_call(start, left),

                L_BRACKET => {
                    let op_span = self.current_span();
                    let types = self.parse_list(
                        L_BRACKET,
                        COMMA,
                        R_BRACKET,
                        TYPE_PARAM_RS,
                        ParseError::ExpectedType,
                        |p| p.parse_type_wrapper(),
                    );
                    let span = self.span_from(start);

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

    fn parse_call(&mut self, start: u32, left: Expr) -> Expr {
        let args = self.parse_list(
            L_PAREN,
            COMMA,
            R_PAREN,
            EMPTY,
            ParseError::ExpectedExpression,
            |p| {
                if p.is2(IDENTIFIER, EQ) {
                    let start = p.current_span().start();
                    let name = p.expect_identifier();
                    p.assert(EQ);
                    let expr = p.parse_expr();
                    let span = p.span_from(start);

                    Some(Arc::new(Argument {
                        id: p.new_node_id(),
                        name,
                        span,
                        expr,
                    }))
                } else if p.is_set(EXPRESSION_FIRST) {
                    let start = p.current_span().start();
                    let expr = p.parse_expr();
                    let span = p.span_from(start);

                    Some(Arc::new(Argument {
                        id: p.new_node_id(),
                        name: None,
                        span,
                        expr,
                    }))
                } else {
                    None
                }
            },
        );
        let span = self.span_from(start);

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
            OR_EQ => BinOp::BitOrAssign,
            AND => BinOp::BitAnd,
            AND_EQ => BinOp::BitAndAssign,
            CARET => BinOp::BitXor,
            CARET_EQ => BinOp::BitXorAssign,
            ADD => BinOp::Add,
            ADD_EQ => BinOp::AddAssign,
            SUB => BinOp::Sub,
            SUB_EQ => BinOp::SubAssign,
            MUL => BinOp::Mul,
            MUL_EQ => BinOp::MulAssign,
            DIV => BinOp::Div,
            DIV_EQ => BinOp::DivAssign,
            MODULO => BinOp::Mod,
            MOD_EQ => BinOp::ModAssign,
            LT_LT => BinOp::ShiftL,
            LT_LT_EQ => BinOp::ShiftLAssign,
            GT_GT => BinOp::ArithShiftR,
            GT_GT_EQ => BinOp::ArithShiftRAssign,
            GT_GT_GT => BinOp::LogicalShiftR,
            GT_GT_GT_EQ => BinOp::LogicalShiftRAssign,
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
        let ident = self.expect_identifier().expect("identifier expected");
        Arc::new(ExprData::create_ident(
            self.new_node_id(),
            ident.span,
            ident.name_as_string.clone(),
        ))
    }

    fn parse_parentheses(&mut self) -> Expr {
        self.start_node();
        self.assert(L_PAREN);

        if self.eat(R_PAREN) {
            return Arc::new(ExprData::create_tuple(
                self.new_node_id(),
                self.finish_node(),
                Vec::new(),
            ));
        }

        let expr = self.parse_expr();

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

                let expr = self.parse_expr();
                values.push(expr);

                if self.eat(R_PAREN) {
                    break;
                }
            }

            Arc::new(ExprData::create_tuple(
                self.new_node_id(),
                self.finish_node(),
                values,
            ))
        } else {
            self.expect(R_PAREN);
            Arc::new(ExprData::create_paren(
                self.new_node_id(),
                self.finish_node(),
                expr,
            ))
        }
    }

    fn parse_lit_char(&mut self) -> Expr {
        let span = self.current_span();
        self.assert(CHAR_LITERAL);
        let value = self.source_span(span);

        Arc::new(ExprData::create_lit_char(self.new_node_id(), span, value))
    }

    fn parse_lit_int(&mut self) -> Expr {
        let span = self.current_span();
        self.assert(INT_LITERAL);
        let value = self.source_span(span);

        Arc::new(ExprData::create_lit_int(self.new_node_id(), span, value))
    }

    fn parse_lit_int_minus(&mut self) -> Expr {
        self.parse_lit_with_minus(|p| p.parse_lit_int())
    }

    fn parse_lit_float_minus(&mut self) -> Expr {
        self.parse_lit_with_minus(|p| p.parse_lit_float())
    }

    fn parse_lit_with_minus<F: FnOnce(&mut Parser) -> Expr>(&mut self, fct: F) -> Expr {
        if self.is(SUB) {
            self.start_node();
            self.assert(SUB);

            let expr = fct(self);
            Arc::new(ExprData::create_un(
                self.new_node_id(),
                self.finish_node(),
                UnOp::Neg,
                expr,
            ))
        } else {
            fct(self)
        }
    }

    fn parse_lit_float(&mut self) -> Expr {
        let span = self.current_span();
        self.assert(FLOAT_LITERAL);
        let value = self.source_span(span);

        Arc::new(ExprData::create_lit_float(self.new_node_id(), span, value))
    }

    fn parse_template(&mut self) -> Expr {
        let span = self.current_span();
        let start = span.start();

        self.assert(TEMPLATE_LITERAL);
        let value = self.source_span(span);

        let mut parts: Vec<Expr> = Vec::new();
        parts.push(Arc::new(ExprData::create_lit_str(
            self.new_node_id(),
            span,
            value,
        )));

        let mut done = false;

        while !done {
            parts.push(self.parse_expr());

            let span = self.current_span();

            if !self.is(TEMPLATE_LITERAL) {
                done = true;
            }

            if !self.is(TEMPLATE_LITERAL) && !self.is(TEMPLATE_END_LITERAL) {
                self.report_error(ParseError::UnclosedStringTemplate);
                break;
            }

            let value = self.source_span(span);
            self.advance();

            parts.push(Arc::new(ExprData::create_lit_str(
                self.new_node_id(),
                span,
                value,
            )));
        }

        let span = self.span_from(start);

        Arc::new(ExprData::create_template(self.new_node_id(), span, parts))
    }

    fn parse_string(&mut self) -> Expr {
        let span = self.current_span();
        self.assert(STRING_LITERAL);

        let value = self.source_span(span);
        Arc::new(ExprData::create_lit_str(self.new_node_id(), span, value))
    }

    fn parse_lit_bool(&mut self) -> Expr {
        let span = self.current_span();
        let kind = self.current();
        self.assert(kind);
        let value = kind == TRUE;

        Arc::new(ExprData::create_lit_bool(self.new_node_id(), span, value))
    }

    fn parse_this(&mut self) -> Expr {
        let span = self.current_span();
        self.assert(SELF_KW);

        Arc::new(ExprData::create_this(self.new_node_id(), span))
    }

    fn parse_lambda(&mut self) -> Expr {
        let start = self.current_span().start();
        self.start_node();

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
            let len = self.token_widths[self.token_idx];
            self.offset += len;
            debug_assert!(kind <= EOF);
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

    fn is2(&self, fst: TokenKind, snd: TokenKind) -> bool {
        if !self.is(fst) {
            return false;
        }

        let mut idx = 1;

        while self.nth(idx).is_trivia() {
            idx += 1;
        }

        self.nth(idx) == snd
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

    fn cancel_node(&mut self) {
        self.nodes.pop().expect("missing scope");
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
        AS_KW => Some("as"),
        COLON_COLON => Some("::"),
        _ => None,
    }
}
