use std::sync::Arc;

use id_arena::Arena;

use crate::ast;
use crate::ast::*;
use crate::error::{ParseError, ParseErrorWithLocation};

use crate::TokenKind::*;
use crate::token::{
    ELEM_FIRST, EMPTY, ENUM_VARIANT_RS, EXPRESSION_FIRST, FIELD_FIRST, MODIFIER_FIRST,
    PARAM_LIST_RS, PATTERN_FIRST, PATTERN_RS, TYPE_FIRST, TYPE_PARAM_RS, UNNAMED_FIELD_FIRST,
    USE_PATH_ATOM_FIRST, USE_PATH_FIRST,
};
use crate::{Span, TokenKind, TokenSet, lex};

// Usage: finish!(self, marker, Variant { field1, field2 })
// Invokes self.finish_node(marker) and injects span and text_length as fields.
macro_rules! finish {
    ($self:expr, $marker:expr, $variant:ident { $($field:tt)* }) => {{
        let idx = $marker.green_elements_idx;
        let span = $self.finish_node($marker.clone());
        let green_elements: Vec<GreenElement> = $self.green_elements
            .drain(idx..)
            .collect();

        // Calculate text_length by summing lengths of all tokens and nodes
        let text_length: u32 = green_elements.iter().map(|elem| {
            match elem {
                GreenElement::Token(token) => token.text.len() as u32,
                GreenElement::Node(node_id) => $self.ast_nodes[*node_id].text_length(),
            }
        }).sum();

        let ast_id = $self.ast_nodes.alloc(Ast::$variant($variant { span, green_elements, text_length, $($field)* }));
        $self.green_elements.push(GreenElement::Node(ast_id));

        ast_id
    }};
}

#[derive(Clone)]
pub struct Marker {
    token_idx: usize,
    offset: u32,
    green_elements_idx: usize,
}

#[cfg(test)]
mod tests;

pub struct Parser {
    tokens: Vec<TokenKind>,
    token_widths: Vec<u32>,
    token_idx: usize,
    content: Arc<String>,
    ast_nodes: Arena<Ast>,
    errors: Vec<ParseErrorWithLocation>,
    offset: u32,
    #[allow(unused)]
    green_elements: Vec<GreenElement>,
    #[allow(unused)]
    starts: Vec<usize>,
}

enum StmtOrExpr {
    Stmt(AstId),
    Expr(AstId),
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
            offset: 0,
            content,
            ast_nodes: Arena::new(),
            errors: result.errors,
            green_elements: Vec::new(),
            starts: Vec::new(),
        }
    }

    pub fn parse(mut self) -> (ast::File, Vec<ParseErrorWithLocation>) {
        self.parse_file();
        self.into_file()
    }

    pub fn into_file(self) -> (ast::File, Vec<ParseErrorWithLocation>) {
        assert_eq!(self.green_elements.len(), 1);
        let root_id = self.green_elements[0].to_node().expect("node expected");

        (
            ast::File::new(self.content.clone(), self.ast_nodes, root_id),
            self.errors,
        )
    }

    fn parse_file(&mut self) -> AstId {
        let m = self.start_node();
        self.skip_trivia();
        let mut elements = vec![];

        while !self.is_eof() {
            elements.push(self.parse_element());
        }

        let root_id = finish!(self, m, Root { elements });
        assert_eq!(
            self.ast_nodes[root_id].text_length() as usize,
            self.content.len()
        );
        root_id
    }

    fn parse_element(&mut self) -> AstId {
        let modifiers = self.parse_modifiers();
        match self.current() {
            FN_KW => self.parse_function(modifiers),
            CLASS_KW => self.parse_class(modifiers),
            STRUCT_KW => self.parse_struct(modifiers),
            TRAIT_KW => self.parse_trait(modifiers),
            IMPL_KW => self.parse_impl(modifiers),
            LET_KW => self.parse_global(modifiers),
            CONST_KW => self.parse_const(modifiers),
            ENUM_KW => self.parse_enum(modifiers),
            MOD_KW => self.parse_module(modifiers),
            USE_KW => self.parse_use(modifiers),
            EXTERN_KW => self.parse_extern(modifiers),
            TYPE_KW => self.parse_alias(modifiers),
            _ => {
                assert!(!ELEM_FIRST.contains(self.current()));
                let m = self.start_node();
                self.report_error(ParseError::ExpectedElement);
                self.advance();
                finish!(self, m, Error {})
            }
        }
    }

    fn parse_extern(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();

        self.assert(EXTERN_KW);
        self.expect(PACKAGE_KW);
        let name = self.expect_identifier();
        let identifier = if self.eat(AS_KW) {
            self.expect_identifier()
        } else {
            None
        };
        self.expect(SEMICOLON);

        finish!(
            self,
            m,
            Extern {
                modifiers,
                name,
                identifier,
            }
        )
    }

    fn parse_use(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();
        self.assert(USE_KW);
        let path = self.parse_use_path();
        self.expect(SEMICOLON);

        finish!(self, m, Use { modifiers, path })
    }

    fn parse_use_path(&mut self) -> AstId {
        let m = self.start_node();
        let mut path = Vec::new();

        while self.is_set(USE_PATH_ATOM_FIRST) && self.is_next(COLON_COLON) {
            path.push(self.parse_use_atom());
            self.assert(COLON_COLON);
        }

        let target = if self.is_set(USE_PATH_ATOM_FIRST) {
            if self.is_set(USE_PATH_ATOM_FIRST) {
                if self.is_next(AS_KW) {
                    self.parse_use_as()
                } else {
                    self.parse_use_atom()
                }
            } else if self.is(L_BRACE) {
                self.parse_use_group()
            } else {
                let m = self.start_node();
                self.report_error(ParseError::ExpectedUsePath);
                finish!(self, m, Error {})
            }
        } else if self.is(L_BRACE) {
            self.parse_use_group()
        } else {
            let m = self.start_node();
            self.report_error(ParseError::ExpectedUsePath);
            finish!(self, m, Error {})
        };

        finish!(self, m, UsePath { path, target })
    }

    fn parse_use_as(&mut self) -> AstId {
        let m = self.start_node();
        let original_name = self.parse_use_atom();
        self.assert(AS_KW);

        let target_name = if self.eat(UNDERSCORE) {
            None
        } else {
            self.expect_identifier()
        };

        finish!(
            self,
            m,
            UseAs {
                original_name,
                target_name
            }
        )
    }

    fn parse_use_atom(&mut self) -> AstId {
        assert!(self.is_set(USE_PATH_ATOM_FIRST));
        let m = self.start_node();

        if self.eat(SELF_KW) || self.eat(PACKAGE_KW) || self.eat(SUPER_KW) {
            // Nothing to do.
        } else {
            let ident = self.expect_identifier();
            assert!(ident.is_some());
        };

        finish!(self, m, UseAtom {})
    }

    fn parse_use_group(&mut self) -> AstId {
        let m = self.start_node();

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

        finish!(self, m, UseGroup { targets })
    }

    fn parse_enum(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();
        self.assert(ENUM_KW);
        let name = self.expect_identifier();
        let type_params = self.parse_type_param_list();
        let where_clause = self.parse_where_clause();

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

        finish!(
            self,
            m,
            Enum {
                modifiers,
                name,
                type_params,
                variants,
                where_clause,
            }
        )
    }

    fn parse_module(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();
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

        finish!(
            self,
            m,
            Module {
                modifiers,
                name,
                elements,
            }
        )
    }

    fn parse_enum_variant(&mut self) -> AstId {
        let m = self.start_node();
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

        finish!(
            self,
            m,
            EnumVariant {
                name,
                field_name_style,
                fields
            }
        )
    }

    fn parse_const(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();
        self.assert(CONST_KW);
        let name = self.expect_identifier();
        self.expect(COLON);
        let ty = self.parse_type();
        self.expect(EQ);
        let expr = self.parse_expr();
        self.expect(SEMICOLON);

        finish!(
            self,
            m,
            Const {
                modifiers,
                name,
                data_type: ty,
                expr,
            }
        )
    }

    fn parse_impl(&mut self, modifiers: Option<AstId>) -> AstId {
        let start = self.current_span().start();
        let m = self.start_node();
        self.assert(IMPL_KW);
        let type_params = self.parse_type_param_list();

        let type_name = self.parse_type();

        let (extended_type, trait_type) = if self.eat(FOR_KW) {
            let extended_type = self.parse_type();

            (extended_type, Some(type_name))
        } else {
            (type_name, None)
        };

        let where_clause = self.parse_where_clause();
        let declaration_span = self.span_from(start);

        self.expect(L_BRACE);

        let mut elements = Vec::new();

        while !self.is(R_BRACE) && !self.is_eof() {
            elements.push(self.parse_element());
        }

        self.expect(R_BRACE);

        finish!(
            self,
            m,
            Impl {
                declaration_span,
                modifiers,
                type_params,
                trait_type,
                extended_type,
                where_clause,
                elements,
            }
        )
    }

    fn parse_global(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();
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

        finish!(
            self,
            m,
            Global {
                name,
                modifiers: modifiers,
                data_type,
                mutable,
                initial_value: expr.clone(),
            }
        )
    }

    fn parse_trait(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();
        self.assert(TRAIT_KW);
        let name = self.expect_identifier();
        let type_params = self.parse_type_param_list();
        let bounds = self.parse_type_bounds();
        let where_clause = self.parse_where_clause();

        self.expect(L_BRACE);

        let mut elements = Vec::new();

        while !self.is(R_BRACE) && !self.is_eof() {
            elements.push(self.parse_element());
        }

        self.expect(R_BRACE);

        finish!(
            self,
            m,
            Trait {
                name,
                modifiers,
                type_params,
                bounds,
                where_clause,
                elements,
            }
        )
    }

    fn parse_alias(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();
        self.assert(TYPE_KW);
        let name = self.expect_identifier();
        let type_params = self.parse_type_param_list();
        let pre_where_clause = self.parse_where_clause();
        let bounds = self.parse_type_bounds();
        let (ty, post_where_clause) = if self.eat(EQ) {
            let ty = self.parse_type();
            let post_where_clause = self.parse_where_clause();
            (Some(ty), post_where_clause)
        } else {
            (None, None)
        };
        self.expect(SEMICOLON);

        finish!(
            self,
            m,
            Alias {
                modifiers,
                name,
                type_params,
                pre_where_clause,
                bounds,
                ty,
                post_where_clause,
            }
        )
    }

    fn parse_struct(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();
        self.assert(STRUCT_KW);
        let ident = self.expect_identifier();
        let type_params = self.parse_type_param_list();
        let where_clause = self.parse_where_clause();
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

        finish!(
            self,
            m,
            Struct {
                name: ident,
                modifiers,
                fields,
                type_params,
                where_clause,
                field_style,
            }
        )
    }

    fn parse_named_field(&mut self) -> AstId {
        let m = self.start_node();

        let modifiers = self.parse_modifiers();

        let ident = self.expect_identifier();

        self.expect(COLON);
        let ty = self.parse_type();

        finish!(
            self,
            m,
            Field {
                modifiers,
                name: ident,
                data_type: ty,
            }
        )
    }

    fn parse_unnamed_field(&mut self) -> AstId {
        let m = self.start_node();

        let modifiers = self.parse_modifiers();
        let ty = self.parse_type();

        finish!(
            self,
            m,
            Field {
                modifiers,
                name: None,
                data_type: ty,
            }
        )
    }

    fn parse_class(&mut self, modifiers: Option<AstId>) -> AstId {
        let m = self.start_node();
        self.assert(CLASS_KW);

        let name = self.expect_identifier();
        let type_params = self.parse_type_param_list();
        let where_clause = self.parse_where_clause();
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

        finish!(
            self,
            m,
            Class {
                modifiers,
                name,
                fields,
                type_params,
                where_clause,
                field_name_style,
            }
        )
    }

    fn parse_type_param_list(&mut self) -> Option<AstId> {
        if self.is(L_BRACKET) {
            let m = self.start_node();
            let params = self.parse_list(
                L_BRACKET,
                COMMA,
                R_BRACKET,
                TYPE_PARAM_RS,
                ParseError::ExpectedTypeParam,
                |p| p.parse_type_param_wrapper(),
            );

            Some(finish!(self, m, TypeParamList { params }))
        } else {
            None
        }
    }

    fn parse_type_param_wrapper(&mut self) -> Option<AstId> {
        if self.is(IDENTIFIER) {
            Some(self.parse_type_param())
        } else {
            None
        }
    }

    fn parse_type_param(&mut self) -> AstId {
        let m = self.start_node();
        let name = self.expect_identifier();
        let bounds = self.parse_type_bounds();
        finish!(self, m, TypeParam { name, bounds })
    }

    fn parse_type_bounds(&mut self) -> AstId {
        let m = self.start_node();
        let mut items = Vec::new();

        if !self.eat(COLON) {
            return finish!(self, m, TypeBounds { items: items });
        }

        loop {
            items.push(self.parse_type());

            if !self.eat(ADD) {
                break;
            }
        }

        finish!(self, m, TypeBounds { items })
    }

    fn parse_modifiers(&mut self) -> Option<AstId> {
        if self.is_set(MODIFIER_FIRST) {
            let m = self.start_node();
            let mut modifiers = Vec::new();

            while self.is_set(MODIFIER_FIRST) {
                modifiers.push(self.parse_modifier());
            }

            assert!(!modifiers.is_empty());
            Some(finish!(self, m, ModifierList { modifiers }))
        } else {
            None
        }
    }

    fn parse_modifier(&mut self) -> AstId {
        let m = self.start_node();

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

        finish!(self, m, Modifier { kind, ident })
    }

    fn parse_function(&mut self, modifiers: Option<AstId>) -> AstId {
        let start = self.current_span().start();
        let m = self.start_node();
        self.assert(FN_KW);
        let name = self.expect_identifier();
        let type_params = self.parse_type_param_list();
        let params = self.parse_function_params();
        let return_type = self.parse_function_type();
        let where_clause = self.parse_where_clause();
        let declaration_span = self.span_from(start);
        let block = self.parse_function_block();

        finish!(
            self,
            m,
            Function {
                kind: FunctionKind::Function,
                modifiers,
                name,
                declaration_span,
                params,
                return_type,
                block,
                type_params,
                where_clause,
            }
        )
    }

    fn parse_function_params(&mut self) -> Vec<AstId> {
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

    fn parse_function_param_wrapper(&mut self) -> Option<AstId> {
        Some(self.parse_function_param())
    }

    fn parse_function_param(&mut self) -> AstId {
        let m = self.start_node();
        let pattern = self.parse_pattern_alt();

        self.expect(COLON);

        let data_type = self.parse_type();

        let variadic = self.eat(DOT_DOT_DOT);

        finish!(
            self,
            m,
            Param {
                pattern,
                data_type,
                variadic,
            }
        )
    }

    fn parse_function_type(&mut self) -> Option<AstId> {
        if self.eat(COLON) {
            let ty = self.parse_type();

            Some(ty)
        } else {
            None
        }
    }

    fn parse_where_clause(&mut self) -> Option<AstId> {
        if self.is(WHERE_KW) {
            let m = self.start_node();
            self.assert(WHERE_KW);

            let mut clauses = Vec::new();

            loop {
                clauses.push(self.parse_where_clause_item());

                if !self.eat(COMMA) {
                    break;
                }
            }

            Some(finish!(self, m, WhereClause { clauses }))
        } else {
            None
        }
    }

    fn parse_where_clause_item(&mut self) -> AstId {
        let m = self.start_node();
        let ty = self.parse_type();
        self.expect(COLON);
        let mut bounds = Vec::new();

        loop {
            bounds.push(self.parse_type());

            if !self.eat(ADD) {
                break;
            }
        }

        finish!(self, m, WhereClauseItem { ty, bounds })
    }

    fn parse_function_block(&mut self) -> Option<AstId> {
        if self.eat(SEMICOLON) {
            None
        } else {
            let block = self.parse_block();
            Some(block)
        }
    }

    fn parse_type_wrapper(&mut self) -> Option<AstId> {
        if self.is_set(TYPE_FIRST) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn parse_type(&mut self) -> AstId {
        match self.current() {
            IDENTIFIER | UPCASE_SELF_KW => {
                let m = self.start_node();
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

                finish!(self, m, RegularType { path, params })
            }

            REF_KW => {
                let m = self.start_node();
                self.assert(REF_KW);
                let ty = self.parse_type();
                finish!(self, m, RefType { ty })
            }

            L_BRACKET => {
                let m = self.start_node();
                self.assert(L_BRACKET);
                let ty = self.parse_type();
                self.expect(AS_KW);
                let trait_ty = self.parse_type();
                self.expect(R_BRACKET);
                self.expect(COLON_COLON);
                let name = self.expect_identifier();

                finish!(self, m, QualifiedPathType { ty, trait_ty, name })
            }

            L_PAREN => {
                let m = self.start_node();
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

                    finish!(
                        self,
                        m,
                        LambdaType {
                            params: subtypes,
                            ret: Some(ret),
                        }
                    )
                } else {
                    finish!(self, m, TupleType { subtypes })
                }
            }

            _ => {
                let m = self.start_node();
                self.report_error(ParseError::ExpectedType);
                finish!(self, m, Error {})
            }
        }
    }

    fn parse_type_argument(&mut self) -> Option<AstId> {
        let m = self.start_node();

        if self.is2(IDENTIFIER, EQ) {
            let name = self.expect_identifier().expect("identifier expected");
            self.assert(EQ);
            let ty = self.parse_type();

            Some(finish!(
                self,
                m,
                TypeArgument {
                    name: Some(name),
                    ty,
                }
            ))
        } else if let Some(ty) = self.parse_type_wrapper() {
            Some(finish!(self, m, TypeArgument { name: None, ty }))
        } else {
            self.cancel_node();
            None
        }
    }

    fn parse_path(&mut self) -> AstId {
        let m = self.start_node();
        let mut segments = Vec::new();
        let segment = self.parse_path_segment();
        segments.push(segment);

        while self.eat(COLON_COLON) {
            let segment = self.parse_path_segment();
            segments.push(segment);
        }

        finish!(self, m, PathData { segments })
    }

    fn parse_path_segment(&mut self) -> AstId {
        if self.is(IDENTIFIER) {
            self.expect_identifier().expect("ident expected")
        } else if self.is(UPCASE_SELF_KW) {
            let m = self.start_node();
            self.assert(UPCASE_SELF_KW);
            finish!(self, m, UpcaseThis {})
        } else {
            let m = self.start_node();
            finish!(self, m, Error {})
        }
    }

    fn parse_let(&mut self) -> AstId {
        let m = self.start_node();

        self.assert(LET_KW);
        let pattern = self.parse_pattern();
        let data_type = self.parse_var_type();
        let expr = self.parse_var_assignment();

        self.expect(SEMICOLON);

        finish!(
            self,
            m,
            Let {
                pattern,
                data_type,
                expr,
            }
        )
    }

    fn parse_var_type(&mut self) -> Option<AstId> {
        if self.eat(COLON) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn parse_var_assignment(&mut self) -> Option<AstId> {
        if self.eat(EQ) {
            let expr = self.parse_expr();

            Some(expr)
        } else {
            None
        }
    }

    fn parse_block(&mut self) -> AstId {
        let m = self.start_node();
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

        finish!(self, m, Block { stmts, expr })
    }

    fn parse_block_stmt(&mut self) -> StmtOrExpr {
        match self.current() {
            LET_KW => StmtOrExpr::Stmt(self.parse_let()),
            _ => {
                let m = self.start_node();

                if self.is_set(EXPRESSION_FIRST) {
                    let expr = self.parse_expr_stmt();

                    if self.is(R_BRACE) {
                        self.cancel_node();
                        StmtOrExpr::Expr(expr)
                    } else {
                        if self.ast_nodes[expr].is_blocklike() {
                            self.eat(SEMICOLON);
                        } else {
                            self.expect(SEMICOLON);
                        }

                        StmtOrExpr::Stmt(finish!(self, m, ExprStmt { expr }))
                    }
                } else {
                    self.report_error(ParseError::ExpectedStatement);

                    if !self.is(R_BRACE) {
                        self.advance();
                    }

                    StmtOrExpr::Stmt(finish!(self, m, Error {}))
                }
            }
        }
    }

    fn parse_if(&mut self) -> AstId {
        let m = self.start_node();
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

        finish!(
            self,
            m,
            If {
                cond,
                then_block,
                else_block,
            }
        )
    }

    fn parse_match(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(MATCH_KW);

        let expr = self.parse_expr();
        let mut cases = Vec::new();

        self.expect(L_BRACE);

        while !self.is(R_BRACE) && !self.is_eof() {
            let arm_id = self.parse_match_arm();
            let arm_value_id = self.ast_nodes[arm_id]
                .to_match_arm()
                .expect("arm expected")
                .value;
            let is_block = self.ast_nodes[arm_value_id].is_blocklike();
            cases.push(arm_id);

            if !self.is(R_BRACE) && !self.is_eof() {
                if is_block {
                    self.eat(COMMA);
                } else {
                    self.expect(COMMA);
                }
            }
        }

        self.expect(R_BRACE);

        finish!(self, m, Match { expr, arms: cases })
    }

    fn parse_match_arm(&mut self) -> AstId {
        let m = self.start_node();
        let pattern = self.parse_pattern();

        let cond = if self.eat(IF_KW) {
            let expr = self.parse_expr();
            Some(expr)
        } else {
            None
        };

        self.expect(DOUBLE_ARROW);

        let value = self.parse_expr_stmt();

        finish!(
            self,
            m,
            MatchArm {
                pattern,
                cond,
                value,
            }
        )
    }

    fn parse_pattern(&mut self) -> AstId {
        let m = self.start_node();

        let pattern_id = self.parse_pattern_alt();

        if self.is(OR) {
            let mut alts = vec![pattern_id];

            while self.eat(OR) {
                alts.push(self.parse_pattern_alt());
            }

            finish!(self, m, Alt { alts })
        } else {
            self.cancel_node();
            pattern_id
        }
    }

    fn parse_pattern_alt(&mut self) -> AstId {
        let m = self.start_node();

        if self.eat(UNDERSCORE) {
            finish!(self, m, UnderscorePattern {})
        } else if self.eat(DOT_DOT) {
            finish!(self, m, Rest {})
        } else if self.is(TRUE) || self.is(FALSE) {
            let expr = self.parse_lit_bool();

            finish!(
                self,
                m,
                LitPattern {
                    expr,
                    kind: PatternLitKind::Bool,
                }
            )
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

            finish!(self, m, TuplePattern { params })
        } else if self.is(CHAR_LITERAL) {
            let expr = self.parse_lit_char();

            finish!(
                self,
                m,
                LitPattern {
                    expr,
                    kind: PatternLitKind::Char,
                }
            )
        } else if self.is(STRING_LITERAL) {
            let expr = self.parse_string();

            finish!(
                self,
                m,
                LitPattern {
                    expr,
                    kind: PatternLitKind::String,
                }
            )
        } else if self.is(INT_LITERAL) || self.is2(SUB, INT_LITERAL) {
            let expr = self.parse_lit_int_minus();

            finish!(
                self,
                m,
                LitPattern {
                    expr,
                    kind: PatternLitKind::Int,
                }
            )
        } else if self.is(FLOAT_LITERAL) || self.is2(SUB, FLOAT_LITERAL) {
            let expr = self.parse_lit_float_minus();

            finish!(
                self,
                m,
                LitPattern {
                    expr,
                    kind: PatternLitKind::Float,
                }
            )
        } else if self.is2(MUT_KW, IDENTIFIER) {
            self.assert(MUT_KW);
            let name = self.expect_identifier().expect("identifier expected");

            finish!(
                self,
                m,
                IdentPattern {
                    mutable: true,
                    name,
                }
            )
        } else if self.is(IDENTIFIER) {
            if !self.nth_is(1, COLON_COLON) && !self.nth_is(1, L_PAREN) {
                let name = self.expect_identifier().expect("identifier expected");

                return finish!(
                    self,
                    m,
                    IdentPattern {
                        mutable: false,
                        name,
                    }
                );
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
                            let m2 = p.start_node();
                            let ident = p.expect_identifier().expect("identifier expected");
                            p.assert(EQ);
                            let pattern = p.parse_pattern();
                            Some(finish!(
                                p,
                                m2,
                                CtorField {
                                    ident: Some(ident),
                                    pattern
                                }
                            ))
                        } else if p.is_set(PATTERN_FIRST) {
                            let m2 = p.start_node();
                            let pattern = p.parse_pattern();
                            Some(finish!(
                                p,
                                m2,
                                CtorField {
                                    ident: None,
                                    pattern
                                }
                            ))
                        } else {
                            None
                        }
                    },
                );

                Some(params)
            } else {
                None
            };

            finish!(self, m, CtorPattern { path, params })
        } else {
            self.report_error(ParseError::ExpectedPattern);
            self.advance();

            finish!(self, m, Error {})
        }
    }

    fn parse_for(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(FOR_KW);
        let pattern = self.parse_pattern();
        self.expect(IN_KW);
        let expr = self.parse_expr();
        let block = self.parse_block();

        finish!(
            self,
            m,
            For {
                pattern,
                expr,
                block,
            }
        )
    }

    fn parse_while(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(WHILE_KW);
        let expr = self.parse_expr();
        let block = self.parse_block();

        finish!(self, m, While { cond: expr, block })
    }

    fn parse_break(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(BREAK_KW);

        finish!(self, m, Break {})
    }

    fn parse_continue(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(CONTINUE_KW);

        finish!(self, m, Continue {})
    }

    fn parse_return(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(RETURN_KW);
        let expr = if self.is_set(EXPRESSION_FIRST) {
            let expr = self.parse_expr();
            Some(expr)
        } else {
            None
        };

        finish!(self, m, Return { expr })
    }

    fn parse_expr(&mut self) -> AstId {
        self.parse_expr_bp(0, false)
    }

    fn parse_expr_stmt(&mut self) -> AstId {
        self.parse_expr_bp(0, true)
    }

    fn parse_expr_bp(&mut self, min_bp: u32, prefer_stmt: bool) -> AstId {
        if !self.is_set(EXPRESSION_FIRST) {
            self.report_error(ParseError::ExpectedExpression);

            let m = self.start_node();
            return finish!(self, m, Error {});
        }

        let m = self.start_node();
        let mut left = self.parse_unary_expr(prefer_stmt);

        loop {
            let op = self.current();

            left = match op {
                AS_KW => {
                    self.assert(AS_KW);
                    let right = self.parse_type();

                    finish!(
                        self,
                        m.clone(),
                        Conv {
                            object: left,
                            data_type: right,
                        }
                    )
                }

                IS_KW => {
                    self.assert(IS_KW);
                    let right = self.parse_pattern();

                    finish!(
                        self,
                        m.clone(),
                        Is {
                            value: left,
                            pattern: right,
                        }
                    )
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
            left = self.create_binary(m.clone(), op, left, right);
        }
    }

    fn parse_unary_expr(&mut self, prefer_stmt: bool) -> AstId {
        match self.current() {
            SUB | NOT => {
                let m = self.start_node();
                let kind = self.current();
                self.advance();
                let op = match kind {
                    SUB => UnOp::Neg,
                    NOT => UnOp::Not,
                    _ => unreachable!(),
                };

                let expr = self.parse_postfix_expr(prefer_stmt);

                finish!(self, m, Un { op, opnd: expr })
            }

            _ => self.parse_postfix_expr(prefer_stmt),
        }
    }

    fn parse_postfix_expr(&mut self, prefer_stmt: bool) -> AstId {
        let m = self.start_node();
        let mut left = self.parse_factor();

        loop {
            left = match self.current() {
                DOT => {
                    let op_span = self.current_span();
                    self.assert(DOT);

                    if false && self.is(IDENTIFIER) {
                        let name = self.expect_identifier().unwrap();

                        if self.is(L_BRACKET) || self.is(L_PAREN) {
                            let type_params = if self.is(L_BRACKET) {
                                Some(self.parse_list(
                                    L_BRACKET,
                                    COMMA,
                                    R_BRACKET,
                                    TYPE_PARAM_RS,
                                    ParseError::ExpectedType,
                                    |p| p.parse_type_wrapper(),
                                ))
                            } else {
                                None
                            };

                            let args = if self.is(L_PAREN) {
                                self.parse_call_arguments()
                            } else {
                                Vec::new()
                            };

                            finish!(
                                self,
                                m.clone(),
                                MethodCallExpr {
                                    object: left,
                                    name,
                                    type_params,
                                    args,
                                }
                            )
                        } else {
                            finish!(
                                self,
                                m.clone(),
                                DotExpr {
                                    op_span,
                                    lhs: left,
                                    rhs: name,
                                }
                            )
                        }
                    } else {
                        let rhs = self.parse_factor();

                        finish!(
                            self,
                            m.clone(),
                            DotExpr {
                                op_span,
                                lhs: left,
                                rhs,
                            }
                        )
                    }
                }

                L_PAREN if !(self.ast_nodes[left].is_blocklike() && prefer_stmt) => {
                    self.parse_call(m.clone(), left)
                }

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
                    finish!(
                        self,
                        m.clone(),
                        TypedExpr {
                            op_span,
                            callee: left,
                            args: types,
                        }
                    )
                }

                COLON_COLON => {
                    let op_span = self.current_span();
                    self.assert(COLON_COLON);
                    let rhs = self.parse_factor();

                    finish!(
                        self,
                        m.clone(),
                        Path {
                            op_span,
                            lhs: left,
                            rhs,
                        }
                    )
                }

                _ => {
                    return left;
                }
            }
        }
    }

    fn parse_call(&mut self, marker: Marker, left: AstId) -> AstId {
        let args = self.parse_call_arguments();
        finish!(self, marker, Call { callee: left, args })
    }

    fn parse_call_arguments(&mut self) -> Vec<AstId> {
        self.parse_list(
            L_PAREN,
            COMMA,
            R_PAREN,
            EMPTY,
            ParseError::ExpectedExpression,
            |p| {
                if p.is2(IDENTIFIER, EQ) {
                    let m = p.start_node();
                    let name = p.expect_identifier();
                    p.assert(EQ);
                    let expr = p.parse_expr();
                    Some(finish!(p, m, Argument { name, expr }))
                } else if p.is_set(EXPRESSION_FIRST) {
                    let m = p.start_node();
                    let expr = p.parse_expr();
                    Some(finish!(p, m, Argument { name: None, expr }))
                } else {
                    None
                }
            },
        )
    }

    fn create_binary(
        &mut self,
        marker: Marker,
        kind: TokenKind,
        left: AstId,
        right: AstId,
    ) -> AstId {
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

        finish!(
            self,
            marker,
            Bin {
                op: op,
                lhs: left,
                rhs: right,
            }
        )
    }

    fn parse_factor(&mut self) -> AstId {
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
                let m = self.start_node();
                self.report_error(ParseError::ExpectedFactor);
                finish!(self, m, Error {})
            }
        }
    }

    fn parse_identifier(&mut self) -> AstId {
        self.expect_identifier().expect("identifier expected")
    }

    fn parse_parentheses(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(L_PAREN);

        if self.eat(R_PAREN) {
            return finish!(self, m, Tuple { values: Vec::new() });
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

            finish!(self, m, Tuple { values: values })
        } else {
            self.expect(R_PAREN);

            finish!(self, m, Paren { expr: expr })
        }
    }

    fn parse_lit_char(&mut self) -> AstId {
        let m = self.start_node();
        let value = self.assert_value(CHAR_LITERAL);

        finish!(self, m, LitChar { value: value })
    }

    fn parse_lit_int(&mut self) -> AstId {
        let m = self.start_node();
        let value = self.assert_value(INT_LITERAL);
        finish!(self, m, LitInt { value: value })
    }

    fn parse_lit_int_minus(&mut self) -> AstId {
        self.parse_lit_with_minus(|p| p.parse_lit_int())
    }

    fn parse_lit_float_minus(&mut self) -> AstId {
        self.parse_lit_with_minus(|p| p.parse_lit_float())
    }

    fn parse_lit_with_minus<F: FnOnce(&mut Parser) -> AstId>(&mut self, fct: F) -> AstId {
        if self.is(SUB) {
            let m = self.start_node();
            self.assert(SUB);

            let expr = fct(self);

            finish!(
                self,
                m,
                Un {
                    op: UnOp::Neg,
                    opnd: expr,
                }
            )
        } else {
            fct(self)
        }
    }

    fn parse_lit_float(&mut self) -> AstId {
        let m = self.start_node();
        let value = self.assert_value(FLOAT_LITERAL);
        finish!(self, m, LitFloat { value: value })
    }

    fn parse_template(&mut self) -> AstId {
        let m = self.start_node(); // TEMPLATE node
        let m2 = self.start_node(); // Start literal node

        let mut parts: Vec<AstId> = Vec::new();

        let value = self.current_value();
        self.assert(TEMPLATE_LITERAL);

        parts.push(finish!(self, m2, LitStr { value: value }));

        let mut done = false;

        while !done {
            parts.push(self.parse_expr());

            if !self.is(TEMPLATE_LITERAL) {
                done = true;
            }

            if !self.is(TEMPLATE_LITERAL) && !self.is(TEMPLATE_END_LITERAL) {
                self.report_error(ParseError::UnclosedStringTemplate);
                break;
            }

            let m3 = self.start_node();
            let value = self.current_value();
            self.advance();
            parts.push(finish!(self, m3, LitStr { value: value }));
        }

        finish!(self, m, Template { parts: parts })
    }

    fn parse_string(&mut self) -> AstId {
        let m = self.start_node();
        let value = self.assert_value(STRING_LITERAL);
        finish!(self, m, LitStr { value: value })
    }

    fn parse_lit_bool(&mut self) -> AstId {
        let m = self.start_node();
        let kind = self.current();
        self.assert(kind);
        let value = kind == TRUE;

        finish!(self, m, LitBool { value: value })
    }

    fn parse_this(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(SELF_KW);

        finish!(self, m, This {})
    }

    fn parse_lambda(&mut self) -> AstId {
        let start = self.current_span().start();
        let m = self.start_node();
        let m2 = self.start_node();

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

        let function_id = finish!(
            self,
            m2,
            Function {
                kind: FunctionKind::Lambda,
                modifiers: None,
                name: None,
                declaration_span,
                params,
                return_type,
                block: Some(block),
                type_params: None,
                where_clause: None,
            }
        );

        finish!(
            self,
            m,
            Lambda {
                fct_id: function_id,
            }
        )
    }

    fn assert(&mut self, kind: TokenKind) {
        assert!(self.eat(kind));
    }

    fn assert_value(&mut self, kind: TokenKind) -> String {
        if self.is(kind) {
            let value = self.current_value();
            self.advance();
            value
        } else {
            panic!("unexpected token")
        }
    }

    fn expect_identifier(&mut self) -> Option<AstId> {
        let m = self.start_node();

        if self.is(IDENTIFIER) {
            let name = self.assert_value(IDENTIFIER);
            Some(finish!(self, m, Ident { name }))
        } else {
            self.cancel_node();
            self.report_error_at(ParseError::ExpectedIdentifier, self.current_span());
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
            let text = self.current_value();
            let len = self.token_widths[self.token_idx];
            self.offset += len;
            debug_assert!(kind <= EOF);
            self.green_elements
                .push(GreenElement::Token(GreenToken { kind, text }));
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

    fn current_value(&self) -> String {
        let start = self.offset as usize;
        let end = start + self.token_widths[self.token_idx] as usize;
        let slice = &self.content[start..end];
        String::from(slice)
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

    fn is_next(&self, kind: TokenKind) -> bool {
        let mut idx = 1;

        while self.nth(idx).is_trivia() {
            idx += 1;
        }

        self.nth(idx) == kind
    }

    fn is2(&self, fst: TokenKind, snd: TokenKind) -> bool {
        self.is(fst) && self.is_next(snd)
    }

    fn is_set(&self, set: TokenSet) -> bool {
        set.contains(self.current())
    }

    #[allow(unused)]
    fn nth_is(&self, idx: usize, kind: TokenKind) -> bool {
        self.nth(idx) == kind
    }

    #[allow(unused)]
    fn nth_is_set(&self, idx: usize, set: TokenSet) -> bool {
        set.contains(self.nth(idx))
    }

    fn is_eof(&self) -> bool {
        self.current() == EOF
    }

    fn start_node(&mut self) -> Marker {
        Marker {
            token_idx: self.token_idx,
            offset: self.offset,
            green_elements_idx: self.green_elements.len(),
        }
    }

    fn cancel_node(&mut self) {
        // No longer needed - markers are now explicit
    }

    fn finish_node(&mut self, marker: Marker) -> Span {
        let start_token = marker.token_idx;
        let start_offset = marker.offset;

        assert!(start_token <= self.token_idx);
        if start_token == self.token_idx {
            return Span::new(start_offset, start_offset);
        }

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
