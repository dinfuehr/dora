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
// Invokes self.prepare_finish_node(marker) and injects full_span and text_length as fields.
macro_rules! finish {
    ($self:expr, $marker:expr, $variant:ident { $($field:tt)* }) => {{
        let (full_span, green_elements, text_length) = $self.prepare_finish_node($marker);
        let variant = $variant { full_span, green_elements, text_length, $($field)* };
        let ast = Ast::$variant(variant);
        let ast_id = $self.ast_nodes.alloc(ast);
        let ast_id = AstId::new(ast_id);
        $self.green_elements.push(GreenElement::Node(ast_id));
        ast_id
    }};
    ($self:expr, $marker:expr, $token_kind:expr) => {{
        let (full_span, green_elements, text_length) = $self.prepare_finish_node($marker);
        let ast = Ast::from_plain_kind($token_kind, full_span, green_elements, text_length);
        let ast_id = $self.ast_nodes.alloc(ast);
        let ast_id = AstId::new(ast_id);
        $self.green_elements.push(GreenElement::Node(ast_id));
        ast_id
    }};
}

#[derive(Clone)]
pub struct Marker {
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
    green_elements: Vec<GreenElement>,
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

        while !self.is_eof() {
            self.parse_element();
        }

        let root_id = finish!(self, m, ELEMENT_LIST);
        assert_eq!(
            self.ast_nodes[root_id.value()].text_length() as usize,
            self.content.len()
        );
        root_id
    }

    fn parse_element(&mut self) -> AstId {
        let m = self.start_node();
        self.parse_modifier_list();
        match self.current() {
            FN_KW => self.parse_function(m),
            CLASS_KW => self.parse_class(m),
            STRUCT_KW => self.parse_struct(m),
            TRAIT_KW => self.parse_trait(m),
            IMPL_KW => self.parse_impl(m),
            LET_KW => self.parse_global(m),
            CONST_KW => self.parse_const(m),
            ENUM_KW => self.parse_enum(m),
            MOD_KW => self.parse_module(m),
            USE_KW => self.parse_use(m),
            EXTERN_KW => self.parse_extern(m),
            TYPE_KW => self.parse_alias(m),
            _ => {
                assert!(!ELEM_FIRST.contains(self.current()));
                self.report_error(ParseError::ExpectedElement);
                self.advance();
                finish!(self, m, TokenKind::ERROR_ELEM)
            }
        }
    }

    fn parse_extern(&mut self, m: Marker) -> AstId {
        self.assert(EXTERN_KW);
        self.expect(PACKAGE_KW);
        self.expect_name();
        if self.eat(AS_KW) {
            self.expect_name();
        }
        self.expect(SEMICOLON);

        finish!(self, m, Extern {})
    }

    fn parse_use(&mut self, m: Marker) -> AstId {
        self.assert(USE_KW);
        let path = self.parse_use_path();
        self.expect(SEMICOLON);

        finish!(self, m, Use { path })
    }

    fn parse_use_path(&mut self) -> AstId {
        let m = self.start_node();
        let mut path_segments = Vec::new();

        while self.is_set(USE_PATH_ATOM_FIRST) && self.is_next(COLON_COLON) {
            path_segments.push(self.parse_use_atom());
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
                finish!(self, m, TokenKind::ERROR_USE_TARGET)
            }
        } else if self.is(L_BRACE) {
            self.parse_use_group()
        } else {
            let m = self.start_node();
            self.report_error(ParseError::ExpectedUsePath);
            finish!(self, m, TokenKind::ERROR)
        };

        finish!(
            self,
            m,
            UsePath {
                path: path_segments,
                target
            }
        )
    }

    fn parse_use_as(&mut self) -> AstId {
        let m = self.start_node();
        let original_name = self.parse_use_atom();
        self.assert(AS_KW);

        if !self.eat(UNDERSCORE) {
            self.expect_name();
        }

        finish!(self, m, UseAs { original_name })
    }

    fn parse_use_atom(&mut self) -> AstId {
        assert!(self.is_set(USE_PATH_ATOM_FIRST));
        let m = self.start_node();

        if self.eat(SELF_KW) || self.eat(PACKAGE_KW) || self.eat(SUPER_KW) {
            // Nothing to do.
        } else {
            let ident = self.expect_name();
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

    fn parse_enum(&mut self, m: Marker) -> AstId {
        self.assert(ENUM_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_where_clause();

        if self.is(L_BRACE) {
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
            );
        } else {
            self.report_error(ParseError::ExpectedEnumVariants);
        }

        finish!(self, m, ENUM)
    }

    fn parse_module(&mut self, m: Marker) -> AstId {
        self.assert(MOD_KW);
        self.expect_name();

        if self.is(L_BRACE) {
            self.parse_element_list();
        } else {
            self.expect(SEMICOLON);
        }

        finish!(self, m, TokenKind::MODULE)
    }

    fn parse_element_list(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(TokenKind::L_BRACE);

        while !self.is(R_BRACE) && !self.is_eof() {
            self.parse_element();
        }

        self.expect(R_BRACE);
        finish!(self, m, ELEMENT_LIST)
    }

    fn parse_enum_variant(&mut self) -> AstId {
        let m = self.start_node();
        self.expect_name();
        let field_name_style;

        if self.is(L_PAREN) {
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
            );
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
            );
        } else {
            field_name_style = FieldNameStyle::Positional;
        };

        finish!(self, m, EnumVariant { field_name_style })
    }

    fn parse_const(&mut self, m: Marker) -> AstId {
        self.assert(CONST_KW);
        self.expect_name();
        self.expect(COLON);
        self.parse_type();
        self.expect(EQ);
        self.parse_expr();
        self.expect(SEMICOLON);
        finish!(self, m, CONST)
    }

    fn parse_impl(&mut self, m: Marker) -> AstId {
        let start = self.current_span().start();
        self.assert(IMPL_KW);
        self.parse_type_param_list();

        self.parse_type();

        if self.eat(FOR_KW) {
            self.parse_type();
        }

        self.parse_where_clause();
        let declaration_span = self.span_from(start);

        if self.is(L_BRACE) {
            self.parse_element_list();
        }

        finish!(self, m, Impl { declaration_span })
    }

    fn parse_global(&mut self, m: Marker) -> AstId {
        self.assert(LET_KW);
        self.eat(MUT_KW);
        self.expect_name();
        self.expect(COLON);
        self.parse_type();

        if self.eat(EQ) {
            self.parse_expr();
        }

        self.expect(SEMICOLON);
        finish!(self, m, GLOBAL)
    }

    fn parse_trait(&mut self, m: Marker) -> AstId {
        self.assert(TRAIT_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_type_bounds();
        self.parse_where_clause();

        if self.is(L_BRACE) {
            self.parse_element_list();
        }

        finish!(self, m, Trait {})
    }

    fn parse_alias(&mut self, m: Marker) -> AstId {
        self.assert(TYPE_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_type_bounds();
        if self.eat(EQ) {
            self.parse_type();
        }
        self.parse_where_clause();
        self.expect(SEMICOLON);

        finish!(self, m, ALIAS)
    }

    fn parse_struct(&mut self, m: Marker) -> AstId {
        self.assert(STRUCT_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_where_clause();
        let field_style;

        if self.is(L_PAREN) {
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
            );
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
            );
        } else {
            field_style = FieldNameStyle::Positional;
        };

        finish!(self, m, Struct { field_style })
    }

    fn parse_named_field(&mut self) -> AstId {
        let m = self.start_node();
        self.parse_modifier_list();
        self.expect_name();
        self.expect(COLON);
        self.parse_type();
        finish!(self, m, Field {})
    }

    fn parse_unnamed_field(&mut self) -> AstId {
        let m = self.start_node();
        self.parse_modifier_list();
        self.parse_type();
        finish!(self, m, Field {})
    }

    fn parse_class(&mut self, m: Marker) -> AstId {
        self.assert(CLASS_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_where_clause();
        let field_name_style;

        if self.is(L_PAREN) {
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

        finish!(self, m, Class { field_name_style })
    }

    fn parse_type_param_list(&mut self) -> Option<AstId> {
        if self.is(L_BRACKET) {
            let m = self.start_node();
            self.parse_list(
                L_BRACKET,
                COMMA,
                R_BRACKET,
                TYPE_PARAM_RS,
                ParseError::ExpectedTypeParam,
                |p| p.parse_type_param_wrapper(),
            );

            Some(finish!(self, m, TYPE_PARAM_LIST))
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
        self.expect_name();
        self.parse_type_bounds();
        finish!(self, m, TYPE_PARAM)
    }

    fn parse_type_bounds(&mut self) -> Option<AstId> {
        if self.eat(COLON) {
            let m = self.start_node();

            loop {
                self.parse_type();

                if !self.eat(ADD) {
                    break;
                }
            }

            Some(finish!(self, m, TYPE_BOUNDS))
        } else {
            None
        }
    }

    fn parse_modifier_list(&mut self) -> Option<AstId> {
        if self.is_set(MODIFIER_FIRST) {
            let m = self.start_node();
            let mut modifiers = Vec::new();

            while self.is_set(MODIFIER_FIRST) {
                modifiers.push(self.parse_modifier());
            }

            assert!(!modifiers.is_empty());
            Some(finish!(self, m, ModifierList {}))
        } else {
            None
        }
    }

    fn parse_modifier(&mut self) -> AstId {
        let m = self.start_node();

        let kind = self.current();
        let mut ident_present = false;

        if self.eat(PUB_KW) {
            // done
        } else if self.eat(STATIC_KW) {
            // done
        } else {
            self.assert(AT);
            ident_present = self.expect_name().is_some();
        }

        if ident_present {
            // name already parsed as child node
        }

        finish!(self, m, Modifier { kind })
    }

    fn parse_function(&mut self, m: Marker) -> AstId {
        let start = self.current_span().start();
        self.assert(FN_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_function_params();
        self.parse_function_type();
        self.parse_where_clause();
        let declaration_span = self.span_from(start);
        self.parse_function_block();

        finish!(
            self,
            m,
            Function {
                kind: FunctionKind::Function,
                declaration_span
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
        self.parse_pattern_no_top_alt();
        self.expect(COLON);
        self.parse_type();
        self.eat(DOT_DOT_DOT);
        finish!(self, m, PARAM)
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

            loop {
                self.parse_where_clause_item();

                if !self.eat(COMMA) {
                    break;
                }
            }

            Some(finish!(self, m, WHERE_CLAUSE))
        } else {
            None
        }
    }

    fn parse_where_clause_item(&mut self) -> AstId {
        let m = self.start_node();
        self.parse_type();
        self.expect(COLON);
        loop {
            self.parse_type();

            if !self.eat(ADD) {
                break;
            }
        }

        finish!(self, m, WHERE_CLAUSE_ITEM)
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
                let _path = self.parse_path();

                if self.is(L_BRACKET) {
                    self.parse_list(
                        L_BRACKET,
                        COMMA,
                        R_BRACKET,
                        TYPE_PARAM_RS,
                        ParseError::ExpectedType,
                        |p| p.parse_type_argument(),
                    );
                }

                finish!(self, m, REGULAR_TYPE)
            }

            REF_KW => {
                let m = self.start_node();
                self.assert(REF_KW);
                self.parse_type();
                finish!(self, m, RefType {})
            }

            L_BRACKET => {
                let m = self.start_node();
                self.assert(L_BRACKET);
                self.parse_type();
                self.expect(AS_KW);
                self.parse_type();
                self.expect(R_BRACKET);
                self.expect(COLON_COLON);
                self.expect_name();

                finish!(self, m, QualifiedPathType {})
            }

            L_PAREN => {
                let m = self.start_node();
                self.parse_list(
                    L_PAREN,
                    COMMA,
                    R_PAREN,
                    TYPE_PARAM_RS,
                    ParseError::ExpectedType,
                    |p| p.parse_type_wrapper(),
                );

                if self.eat(COLON) {
                    self.parse_type();

                    finish!(self, m, LambdaType {})
                } else {
                    finish!(self, m, TupleType {})
                }
            }

            _ => {
                let m = self.start_node();
                self.report_error(ParseError::ExpectedType);
                finish!(self, m, TokenKind::ERROR_TYPE)
            }
        }
    }

    fn parse_type_argument_list(&mut self) -> AstId {
        let m = self.start_node();
        self.parse_list(
            L_BRACKET,
            COMMA,
            R_BRACKET,
            TYPE_PARAM_RS,
            ParseError::ExpectedType,
            |p| p.parse_type_argument(),
        );

        finish!(self, m, TYPE_ARGUMENT_LIST)
    }

    fn parse_type_argument(&mut self) -> Option<AstId> {
        let m = self.start_node();

        if self.is2(IDENTIFIER, EQ) {
            self.expect_name();
            self.assert(EQ);
            self.parse_type();

            Some(finish!(self, m, TypeArgument {}))
        } else if self.parse_type_wrapper().is_some() {
            Some(finish!(self, m, TypeArgument {}))
        } else {
            self.cancel_node();
            None
        }
    }

    fn parse_path(&mut self) -> AstId {
        let m = self.start_node();
        self.parse_path_segment();

        while self.eat(COLON_COLON) {
            self.parse_path_segment();
        }

        finish!(self, m, PathData {})
    }

    fn parse_path_segment(&mut self) -> AstId {
        if self.is(IDENTIFIER) {
            self.expect_name().expect("ident expected")
        } else if self.is(UPCASE_SELF_KW) {
            let m = self.start_node();
            self.assert(UPCASE_SELF_KW);
            finish!(self, m, UpcaseThis {})
        } else {
            let m = self.start_node();
            finish!(self, m, TokenKind::ERROR_PATH_SEGMENT)
        }
    }

    fn parse_let(&mut self) -> AstId {
        let m = self.start_node();

        self.assert(LET_KW);
        self.parse_pattern();
        self.parse_var_type();
        self.parse_var_assignment();

        self.expect(SEMICOLON);

        finish!(self, m, Let {})
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

        if self.expect(L_BRACE) {
            while !self.is(R_BRACE) && !self.is_eof() {
                self.parse_block_stmt();
            }

            self.expect(R_BRACE);
        }

        finish!(self, m, BLOCK)
    }

    fn parse_block_stmt(&mut self) -> AstId {
        match self.current() {
            LET_KW => self.parse_let(),
            _ => {
                let m = self.start_node();

                if self.is_set(EXPRESSION_FIRST) {
                    let expr = self.parse_expr_stmt();

                    if self.is(R_BRACE) {
                        self.cancel_node();
                        expr
                    } else {
                        if self.is_blocklike(expr) {
                            self.eat(SEMICOLON);
                        } else {
                            self.expect(SEMICOLON);
                        }

                        finish!(self, m, EXPR_STMT)
                    }
                } else {
                    self.report_error(ParseError::ExpectedStatement);

                    if !self.is(R_BRACE) {
                        self.advance();
                    }

                    finish!(self, m, TokenKind::ERROR_STMT)
                }
            }
        }
    }

    fn parse_if(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(IF_KW);

        self.parse_expr();

        self.parse_block();

        if self.eat(ELSE_KW) {
            if self.is(IF_KW) {
                self.parse_if();
            } else {
                self.parse_block();
            }
        }

        finish!(self, m, IF)
    }

    fn parse_match(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(MATCH_KW);

        self.parse_expr();

        self.expect(L_BRACE);

        while !self.is(R_BRACE) && !self.is_eof() {
            let (_arm_id, is_block) = self.parse_match_arm();

            if !self.is(R_BRACE) && !self.is_eof() {
                if is_block {
                    self.eat(COMMA);
                } else {
                    self.expect(COMMA);
                }
            }
        }

        self.expect(R_BRACE);

        finish!(self, m, TokenKind::MATCH)
    }

    fn parse_match_arm(&mut self) -> (AstId, bool) {
        let m = self.start_node();
        self.parse_pattern();

        if self.eat(IF_KW) {
            self.parse_expr();
        }

        self.expect(DOUBLE_ARROW);

        let value = self.parse_expr_stmt();

        let arm_id = finish!(self, m, MATCH_ARM);
        let is_block = self.is_blocklike(value);

        (arm_id, is_block)
    }

    fn parse_pattern(&mut self) -> AstId {
        let m = self.start_node();
        let pattern_id = self.parse_pattern_no_top_alt();

        if self.is(OR) {
            let mut alts = vec![pattern_id];

            while self.eat(OR) {
                alts.push(self.parse_pattern_no_top_alt());
            }

            finish!(self, m, ALT)
        } else {
            self.cancel_node();
            pattern_id
        }
    }

    fn parse_pattern_no_top_alt(&mut self) -> AstId {
        let m = self.start_node();

        if self.eat(UNDERSCORE) {
            finish!(self, m, UnderscorePattern {})
        } else if self.eat(DOT_DOT) {
            finish!(self, m, Rest {})
        } else if self.is(TRUE) || self.is(FALSE) {
            self.parse_lit_bool();

            finish!(
                self,
                m,
                LitPattern {
                    kind: PatternLitKind::Bool,
                }
            )
        } else if self.is(L_PAREN) {
            self.parse_list(
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

            finish!(self, m, TuplePattern {})
        } else if self.is(CHAR_LITERAL) {
            self.parse_lit_char();

            finish!(
                self,
                m,
                LitPattern {
                    kind: PatternLitKind::Char,
                }
            )
        } else if self.is(STRING_LITERAL) {
            self.parse_string();

            finish!(
                self,
                m,
                LitPattern {
                    kind: PatternLitKind::String,
                }
            )
        } else if self.is(INT_LITERAL) || self.is2(SUB, INT_LITERAL) {
            self.parse_lit_int_minus();

            finish!(
                self,
                m,
                LitPattern {
                    kind: PatternLitKind::Int,
                }
            )
        } else if self.is(FLOAT_LITERAL) || self.is2(SUB, FLOAT_LITERAL) {
            self.parse_lit_float_minus();

            finish!(
                self,
                m,
                LitPattern {
                    kind: PatternLitKind::Float,
                }
            )
        } else if self.is2(MUT_KW, IDENTIFIER) {
            self.assert(MUT_KW);
            self.expect_name().expect("identifier expected");

            finish!(self, m, IDENT_PATTERN)
        } else if self.is(IDENTIFIER) {
            if !self.nth_is(1, COLON_COLON) && !self.nth_is(1, L_PAREN) {
                self.expect_name().expect("identifier expected");

                return finish!(self, m, IDENT_PATTERN);
            }

            self.parse_path();

            if self.is(L_PAREN) {
                let m = self.start_node();
                self.parse_list(
                    L_PAREN,
                    COMMA,
                    R_PAREN,
                    PATTERN_RS,
                    ParseError::ExpectedPattern,
                    |p| {
                        if p.is2(IDENTIFIER, EQ) {
                            let m2 = p.start_node();
                            p.expect_name().expect("identifier expected");
                            p.assert(EQ);
                            p.parse_pattern();
                            Some(finish!(p, m2, CTOR_FIELD))
                        } else if p.is_set(PATTERN_FIRST) {
                            let m2 = p.start_node();
                            p.parse_pattern();
                            Some(finish!(p, m2, CTOR_FIELD))
                        } else {
                            None
                        }
                    },
                );

                Some(finish!(self, m, CTOR_FIELD_LIST))
            } else {
                None
            };

            finish!(self, m, CTOR_PATTERN)
        } else {
            self.report_error(ParseError::ExpectedPattern);
            self.advance();

            finish!(self, m, TokenKind::ERROR_PATTERN)
        }
    }

    fn parse_for(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(FOR_KW);
        self.parse_pattern();
        self.expect(IN_KW);
        self.parse_expr();
        self.parse_block();

        finish!(self, m, FOR)
    }

    fn parse_while(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(WHILE_KW);
        self.parse_expr();
        self.parse_block();

        finish!(self, m, While {})
    }

    fn parse_break(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(BREAK_KW);
        finish!(self, m, BREAK)
    }

    fn parse_continue(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(CONTINUE_KW);
        finish!(self, m, CONTINUE)
    }

    fn parse_return(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(RETURN_KW);
        if self.is_set(EXPRESSION_FIRST) {
            self.parse_expr();
        }

        finish!(self, m, Return {})
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
            return finish!(self, m, TokenKind::ERROR_EXPR);
        }

        let m = self.start_node();
        let mut left = self.parse_unary_expr(prefer_stmt);

        loop {
            let op = self.current();

            left = match op {
                AS_KW => {
                    self.assert(AS_KW);
                    self.parse_type();
                    finish!(self, m.clone(), CONV)
                }

                IS_KW => {
                    self.assert(IS_KW);
                    self.parse_pattern();

                    finish!(self, m.clone(), IS)
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

            self.parse_expr_bp(r_bp, prefer_stmt);
            left = self.create_binary(m.clone(), op);
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

                self.parse_postfix_expr(prefer_stmt);

                finish!(self, m, Un { op })
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
                    self.current_span();
                    self.assert(DOT);

                    if false && self.is(IDENTIFIER) {
                        self.parse_identifier();

                        if self.is(L_BRACKET) || self.is(L_PAREN) {
                            if self.is(L_BRACKET) {
                                self.parse_type_argument_list();
                            }

                            if self.is(L_PAREN) {
                                self.parse_argument_list();
                            }

                            finish!(self, m.clone(), METHOD_CALL_EXPR)
                        } else {
                            finish!(self, m.clone(), DOT_EXPR)
                        }
                    } else {
                        self.parse_factor();
                        finish!(self, m.clone(), DOT_EXPR)
                    }
                }

                L_PAREN if !(self.is_blocklike(left) && prefer_stmt) => self.parse_call(m.clone()),

                L_BRACKET => {
                    let op_span = self.current_span();
                    let _types = self.parse_list(
                        L_BRACKET,
                        COMMA,
                        R_BRACKET,
                        TYPE_PARAM_RS,
                        ParseError::ExpectedType,
                        |p| p.parse_type_wrapper(),
                    );
                    finish!(self, m.clone(), TypedExpr { op_span })
                }

                COLON_COLON => {
                    let op_span = self.current_span();
                    self.assert(COLON_COLON);
                    self.parse_factor();

                    finish!(self, m.clone(), Path { op_span })
                }

                _ => {
                    return left;
                }
            }
        }
    }

    fn parse_call(&mut self, marker: Marker) -> AstId {
        self.parse_argument_list();
        finish!(self, marker, CALL)
    }

    fn parse_argument_list(&mut self) -> AstId {
        let m = self.start_node();

        self.parse_list(
            L_PAREN,
            COMMA,
            R_PAREN,
            EMPTY,
            ParseError::ExpectedExpression,
            |p| {
                if p.is2(IDENTIFIER, EQ) {
                    let m = p.start_node();
                    p.expect_name();
                    p.assert(EQ);
                    p.parse_expr();
                    Some(finish!(p, m, TokenKind::ARGUMENT))
                } else if p.is_set(EXPRESSION_FIRST) {
                    let m = p.start_node();
                    p.parse_expr();
                    Some(finish!(p, m, TokenKind::ARGUMENT))
                } else {
                    None
                }
            },
        );

        finish!(self, m, ARGUMENT_LIST)
    }

    fn create_binary(&mut self, marker: Marker, kind: TokenKind) -> AstId {
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

        finish!(self, marker, Bin { op: op })
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
                finish!(self, m, TokenKind::ERROR_EXPR)
            }
        }
    }

    fn parse_identifier(&mut self) -> AstId {
        let m = self.start_node();
        let name = self.assert_value(IDENTIFIER);
        finish!(self, m, NameExpr { name })
    }

    fn parse_parentheses(&mut self) -> AstId {
        let m = self.start_node();
        self.assert(L_PAREN);

        if self.eat(R_PAREN) {
            return finish!(self, m, Tuple {});
        }

        self.parse_expr();

        if self.current() == COMMA {
            loop {
                self.expect(COMMA);

                if self.eat(R_PAREN) {
                    break;
                }

                if !self.is_set(EXPRESSION_FIRST) {
                    break;
                }

                self.parse_expr();

                if self.eat(R_PAREN) {
                    break;
                }
            }

            finish!(self, m, Tuple {})
        } else {
            self.expect(R_PAREN);

            finish!(self, m, PAREN)
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

            fct(self);

            finish!(self, m, Un { op: UnOp::Neg })
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

        let value = self.current_value();
        self.assert(TEMPLATE_LITERAL);

        finish!(self, m2, LitStr { value: value });

        let mut done = false;

        while !done {
            self.parse_expr();

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
            finish!(self, m3, LitStr { value: value });
        }

        finish!(self, m, Template {})
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

        if self.eat(OR_OR) {
            // nothing to do
        } else {
            assert!(self.is(OR));
            self.parse_list(
                OR,
                COMMA,
                OR,
                PARAM_LIST_RS,
                ParseError::ExpectedParam,
                |p| p.parse_function_param_wrapper(),
            );
        };

        if self.eat(COLON) {
            self.parse_type();
        }

        let declaration_span = self.span_from(start);

        self.parse_block();

        finish!(
            self,
            m2,
            Function {
                kind: FunctionKind::Lambda,
                declaration_span
            }
        );

        finish!(self, m, Lambda {})
    }

    fn is_blocklike(&self, id: AstId) -> bool {
        let kind = self.ast_nodes[id.value()].syntax_kind();
        match kind {
            TokenKind::BLOCK => true,
            TokenKind::IF => true,
            TokenKind::MATCH => true,
            TokenKind::FOR => true,
            TokenKind::WHILE => true,
            _ => false,
        }
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

    fn expect_name(&mut self) -> Option<AstId> {
        let m = self.start_node();

        if self.is(IDENTIFIER) {
            let name = self.assert_value(IDENTIFIER);
            Some(finish!(self, m, Name { name }))
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
            let token = GreenElement::Token(GreenToken { kind, text });
            self.green_elements.push(token);
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
            offset: self.offset,
            green_elements_idx: self.green_elements.len(),
        }
    }

    fn prepare_finish_node(&mut self, marker: Marker) -> (Span, Vec<GreenElement>, u32) {
        let idx = marker.green_elements_idx;
        let offset = marker.offset;
        let green_elements: Vec<GreenElement> = self.green_elements.drain(idx..).collect();

        let text_length = text_length_for_slice(self, &green_elements[..]);
        let full_span = Span::new(offset, text_length);

        (full_span, green_elements, text_length)
    }

    fn cancel_node(&mut self) {
        // No longer needed - markers are now explicit
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.offset - start)
    }
}

fn text_length_for_slice(p: &Parser, green_elements: &[GreenElement]) -> u32 {
    green_elements
        .iter()
        .map(|elem| match elem {
            GreenElement::Token(token) => token.text.len() as u32,
            GreenElement::Node(node_id) => p.ast_nodes[node_id.value()].text_length(),
        })
        .sum()
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
