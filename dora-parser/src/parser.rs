use std::sync::Arc;

use id_arena::{Arena, Id};

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

#[derive(Clone)]
pub struct Marker {
    start: usize,
}

#[derive(Clone, Copy)]
enum Blocklike {
    Yes,
    No,
}

impl Blocklike {
    fn is_yes(&self) -> bool {
        match self {
            Blocklike::Yes => true,
            Blocklike::No => false,
        }
    }
}

enum Event {
    Open { kinds: Vec<TokenKind> },
    Advance,
    Close,
}

#[cfg(test)]
mod tests;

pub struct Parser {
    tokens: Vec<TokenKind>,
    token_widths: Vec<u32>,
    token_idx: usize,
    leading: usize,
    content: Arc<String>,
    errors: Vec<ParseErrorWithLocation>,
    offset: u32,
    events: Vec<Event>,
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
            leading: 0,
            offset: 0,
            content,
            errors: result.errors,
            events: Vec::new(),
        }
    }

    pub fn parse(mut self) -> (ast::File, Vec<ParseErrorWithLocation>) {
        self.parse_file();
        self.into_file()
    }

    pub fn into_file(self) -> (ast::File, Vec<ParseErrorWithLocation>) {
        let (nodes, root_id) = build_tree(
            self.content.as_str(),
            &self.tokens,
            &self.token_widths,
            self.events,
        );

        (
            ast::File::new(self.content.clone(), nodes, root_id),
            self.errors,
        )
    }

    fn parse_file(&mut self) {
        let m = self.open();
        self.skip_trivia();

        while !self.is_eof() {
            self.parse_element();
        }

        self.advance_by_all_trivia();
        self.close(m, ELEMENT_LIST);
    }

    fn advance_by_all_trivia(&mut self) {
        if self.leading > 0 {
            for _ in 0..self.leading {
                self.events.push(Event::Advance);
            }
            self.leading = 0;
        }
    }

    fn parse_element(&mut self) {
        self.advance_by_non_leading_trivia();
        let m = self.open();
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
                self.close(m, TokenKind::ERROR_ELEM);
            }
        }
    }

    fn parse_extern(&mut self, m: Marker) {
        self.assert(EXTERN_KW);
        self.expect(PACKAGE_KW);
        self.expect_name();
        if self.eat(AS_KW) {
            self.expect_name();
        }
        self.expect(SEMICOLON);

        self.close(m, EXTERN);
    }

    fn parse_use(&mut self, m: Marker) {
        self.assert(USE_KW);

        if self.is_set(USE_PATH_ATOM_FIRST) && self.is_next(COLON_COLON) {
            self.parse_use_atom();
            self.expect(COLON_COLON);
        } else {
            self.report_error(ParseError::ExpectedUsePath);
        }

        self.parse_use_path();
        self.expect(SEMICOLON);
        self.close(m, USE);
    }

    fn parse_use_path(&mut self) {
        let m = self.open();

        while self.is(IDENTIFIER) && self.is_next(COLON_COLON) {
            self.expect_name().expect("expected identifier");
            self.assert(COLON_COLON);
        }

        if self.is(IDENTIFIER) {
            if self.is_next(AS_KW) {
                self.parse_use_as();
            } else {
                self.parse_use_name();
            }
        } else if self.is(L_BRACE) {
            self.parse_use_group();
        } else {
            self.report_error(ParseError::ExpectedUsePath);
        }

        self.close(m, USE_PATH);
    }

    fn parse_use_name(&mut self) {
        let m = self.open();
        self.expect_name().expect("identifier expected");
        self.close(m, USE_NAME);
    }

    fn parse_use_as(&mut self) {
        let m = self.open();
        self.expect_name().expect("identifier expected");
        self.assert(AS_KW);

        if !self.eat(UNDERSCORE) {
            self.expect_name();
        }

        self.close(m, USE_AS);
    }

    fn parse_use_atom(&mut self) {
        assert!(self.is_set(USE_PATH_ATOM_FIRST));
        let m = self.open();

        if self.eat(SELF_KW) || self.eat(PACKAGE_KW) || self.eat(SUPER_KW) {
            // Nothing to do.
        } else {
            let ident = self.expect_name();
            assert!(ident.is_some());
        };

        self.close(m, USE_ATOM);
    }

    fn parse_use_group(&mut self) {
        let m = self.open();

        self.parse_list(
            L_BRACE,
            COMMA,
            R_BRACE,
            ELEM_FIRST,
            ParseError::ExpectedUsePath,
            |p| {
                if p.is_set(USE_PATH_FIRST) {
                    p.parse_use_path();
                    true
                } else {
                    false
                }
            },
        );

        self.close(m, USE_GROUP);
    }

    fn parse_enum(&mut self, m: Marker) {
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
                        p.parse_enum_variant();
                        true
                    } else {
                        false
                    }
                },
            );
        } else {
            self.report_error(ParseError::ExpectedEnumVariants);
        }

        self.close(m, ENUM);
    }

    fn parse_module(&mut self, m: Marker) {
        self.assert(MOD_KW);
        self.expect_name();

        if self.is(L_BRACE) {
            self.parse_element_list();
        } else {
            self.expect(SEMICOLON);
        }

        self.close(m, TokenKind::MODULE);
    }

    fn parse_element_list(&mut self) {
        let m = self.open();
        self.assert(TokenKind::L_BRACE);

        while !self.is(R_BRACE) && !self.is_eof() {
            self.parse_element();
        }

        self.expect(R_BRACE);
        self.close(m, ELEMENT_LIST);
    }

    fn parse_enum_variant(&mut self) {
        let m = self.open();
        self.expect_name();

        if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(UNNAMED_FIELD_FIRST) {
                        p.parse_unnamed_field();
                        true
                    } else {
                        false
                    }
                },
            );
        } else if self.is(L_BRACE) {
            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        p.parse_named_field();
                        true
                    } else {
                        false
                    }
                },
            );
        };

        self.close(m, ENUM_VARIANT);
    }

    fn parse_const(&mut self, m: Marker) {
        self.assert(CONST_KW);
        self.expect_name();
        self.expect(COLON);
        self.parse_type();
        self.expect(EQ);
        self.parse_expr();
        self.expect(SEMICOLON);
        self.close(m, CONST);
    }

    fn parse_impl(&mut self, m: Marker) {
        self.assert(IMPL_KW);
        self.parse_type_param_list();

        self.parse_type();

        if self.eat(FOR_KW) {
            self.parse_type();
        }

        self.parse_where_clause();

        if self.is(L_BRACE) {
            self.parse_element_list();
        }

        self.close(m, IMPL);
    }

    fn parse_global(&mut self, m: Marker) {
        self.assert(LET_KW);
        self.eat(MUT_KW);
        self.expect_name();
        self.expect(COLON);
        self.parse_type();

        if self.eat(EQ) {
            self.parse_expr();
        }

        self.expect(SEMICOLON);
        self.close(m, GLOBAL);
    }

    fn parse_trait(&mut self, m: Marker) {
        self.assert(TRAIT_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_type_bounds();
        self.parse_where_clause();

        if self.is(L_BRACE) {
            self.parse_element_list();
        }

        self.close(m, TRAIT);
    }

    fn parse_alias(&mut self, m: Marker) {
        self.assert(TYPE_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_type_bounds();
        if self.eat(EQ) {
            self.parse_type();
        }
        self.parse_where_clause();
        self.expect(SEMICOLON);

        self.close(m, ALIAS);
    }

    fn parse_struct(&mut self, m: Marker) {
        self.assert(STRUCT_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_where_clause();

        if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(UNNAMED_FIELD_FIRST) {
                        p.parse_unnamed_field();
                        true
                    } else {
                        false
                    }
                },
            );
        } else if self.is(L_BRACE) {
            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        p.parse_named_field();
                        true
                    } else {
                        false
                    }
                },
            );
        }

        self.close(m, STRUCT);
    }

    fn parse_named_field(&mut self) {
        self.advance_by_non_leading_trivia();
        let m = self.open();
        self.advance_by_all_trivia();
        self.parse_modifier_list();
        self.expect_name();
        self.expect(COLON);
        self.parse_type();
        self.close(m, FIELD);
    }

    fn parse_unnamed_field(&mut self) {
        self.advance_by_non_leading_trivia();
        let m = self.open();
        self.advance_by_all_trivia();
        self.parse_modifier_list();
        self.parse_type();
        self.close(m, FIELD);
    }

    fn parse_class(&mut self, m: Marker) {
        self.assert(CLASS_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_where_clause();

        if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ELEM_FIRST,
                ParseError::ExpectedField,
                |p| {
                    if p.is_set(UNNAMED_FIELD_FIRST) {
                        p.parse_unnamed_field();
                        true
                    } else {
                        false
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
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        p.parse_named_field();
                        true
                    } else {
                        false
                    }
                },
            )
        }

        self.close(m, CLASS);
    }

    fn parse_type_param_list(&mut self) {
        if self.is(L_BRACKET) {
            let m = self.open();
            self.parse_list(
                L_BRACKET,
                COMMA,
                R_BRACKET,
                TYPE_PARAM_RS,
                ParseError::ExpectedTypeParam,
                |p| {
                    if p.is(IDENTIFIER) {
                        p.parse_type_param();
                        true
                    } else {
                        false
                    }
                },
            );

            self.close(m, TYPE_PARAM_LIST);
        }
    }

    fn parse_type_param(&mut self) {
        let m = self.open();
        self.expect_name();
        self.parse_type_bounds();
        self.close(m, TYPE_PARAM);
    }

    fn parse_type_bounds(&mut self) {
        if self.eat(COLON) {
            let m = self.open();

            loop {
                self.parse_type();

                if !self.eat(ADD) {
                    break;
                }
            }

            self.close(m, TYPE_BOUNDS);
        }
    }

    fn parse_modifier_list(&mut self) {
        if self.is_set(MODIFIER_FIRST) {
            let m = self.open();

            while self.is_set(MODIFIER_FIRST) {
                self.parse_modifier();
            }

            self.close(m, MODIFIER_LIST);
        }
    }

    fn parse_modifier(&mut self) {
        let m = self.open();

        if self.eat(PUB_KW) {
            // done
        } else if self.eat(STATIC_KW) {
            // done
        } else {
            self.assert(AT);
            self.expect_name();
        }

        self.close(m, MODIFIER);
    }

    fn parse_function(&mut self, m: Marker) {
        self.assert(FN_KW);
        self.expect_name();
        self.parse_type_param_list();
        self.parse_function_params();
        if self.eat(COLON) {
            self.parse_type();
        }
        self.parse_where_clause();
        if !self.eat(SEMICOLON) {
            self.parse_block();
        }
        self.close(m, FUNCTION);
    }

    fn parse_function_params(&mut self) {
        if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                PARAM_LIST_RS,
                ParseError::ExpectedParam,
                |p| {
                    p.parse_function_param();
                    true
                },
            )
        } else {
            self.report_error(ParseError::ExpectedParams);
        }
    }

    fn parse_list<F>(
        &mut self,
        start: TokenKind,
        sep: TokenKind,
        stop: TokenKind,
        recovery_set: TokenSet,
        msg: ParseError,
        mut parse: F,
    ) where
        F: FnMut(&mut Parser) -> bool,
    {
        self.assert(start);

        while !self.is(stop.clone()) && !self.is_eof() {
            let pos_before_element = self.token_idx;

            if parse(self) {
                // Callback needs to at least advance by one token, otherwise
                // we might loop forever here.
                assert!(self.token_idx > pos_before_element);
            } else {
                if self.is_set(recovery_set) {
                    break;
                }

                self.report_error(msg.clone());
                self.advance();
            }

            if !self.is(stop.clone()) {
                self.expect(sep);
            }
        }

        self.expect(stop);
    }

    fn parse_function_param(&mut self) {
        let m = self.open();
        self.parse_pattern_no_top_alt();
        self.expect(COLON);
        self.parse_type();
        self.eat(DOT_DOT_DOT);
        self.close(m, PARAM);
    }

    fn parse_where_clause(&mut self) {
        if self.is(WHERE_KW) {
            let m = self.open();
            self.assert(WHERE_KW);

            loop {
                self.parse_where_clause_item();

                if !self.eat(COMMA) {
                    break;
                }
            }

            self.close(m, WHERE_CLAUSE);
        }
    }

    fn parse_where_clause_item(&mut self) {
        let m = self.open();
        self.parse_type();
        self.expect(COLON);
        loop {
            self.parse_type();

            if !self.eat(ADD) {
                break;
            }
        }

        self.close(m, WHERE_CLAUSE_ITEM);
    }

    fn parse_type(&mut self) {
        match self.current() {
            IDENTIFIER | UPCASE_SELF_KW => {
                let m = self.open();
                self.parse_path();

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

                self.close(m, PATH_TYPE);
            }

            REF_KW => {
                let m = self.open();
                self.assert(REF_KW);
                self.parse_type();
                self.close(m, REF_TYPE);
            }

            L_BRACKET => {
                let m = self.open();
                self.assert(L_BRACKET);
                self.parse_type();
                self.expect(AS_KW);
                self.parse_type();
                self.expect(R_BRACKET);
                self.expect(COLON_COLON);
                self.expect_name();

                self.close(m, QUALIFIED_PATH_TYPE);
            }

            L_PAREN => {
                let m = self.open();
                self.parse_list(
                    L_PAREN,
                    COMMA,
                    R_PAREN,
                    TYPE_PARAM_RS,
                    ParseError::ExpectedType,
                    |p| {
                        if p.is_set(TYPE_FIRST) {
                            p.parse_type();
                            true
                        } else {
                            false
                        }
                    },
                );

                if self.eat(COLON) {
                    self.parse_type();

                    self.close(m, LAMBDA_TYPE);
                } else {
                    self.close(m, TUPLE_TYPE);
                }
            }

            _ => {
                let m = self.open();
                self.report_error(ParseError::ExpectedType);
                self.close(m, TokenKind::ERROR_TYPE);
            }
        }
    }

    fn parse_type_argument_list(&mut self) {
        let m = self.open();
        self.parse_list(
            L_BRACKET,
            COMMA,
            R_BRACKET,
            TYPE_PARAM_RS,
            ParseError::ExpectedType,
            |p| p.parse_type_argument(),
        );

        self.close(m, TYPE_ARGUMENT_LIST);
    }

    fn parse_type_argument(&mut self) -> bool {
        let m = self.open();

        if self.is2(IDENTIFIER, EQ) {
            self.expect_name();
            self.assert(EQ);
            self.parse_type();

            self.close(m, TYPE_ARGUMENT);
            true
        } else if self.is_set(TYPE_FIRST) {
            self.parse_type();
            self.close(m, TYPE_ARGUMENT);
            true
        } else {
            self.cancel_node(m);
            false
        }
    }

    fn parse_path(&mut self) {
        let m = self.open();
        self.parse_path_segment();

        while self.eat(COLON_COLON) {
            self.parse_path_segment();
        }

        self.close(m, PATH_DATA);
    }

    fn parse_path_segment(&mut self) {
        if self.is(IDENTIFIER) {
            self.expect_name().expect("ident expected");
        } else if self.is(UPCASE_SELF_KW) {
            self.assert(UPCASE_SELF_KW);
        } else {
            let m = self.open();
            self.close(m, TokenKind::ERROR_PATH_SEGMENT);
        }
    }

    #[cfg(test)]
    fn parse_let_for_testing(&mut self) {
        let m = self.open();
        self.parse_let(m);
    }

    fn parse_let(&mut self, m: Marker) {
        self.assert(LET_KW);
        self.parse_pattern();
        if self.eat(COLON) {
            self.parse_type();
        }
        if self.eat(EQ) {
            self.parse_expr();
        }

        self.expect(SEMICOLON);

        self.close(m, LET);
    }

    fn parse_block(&mut self) {
        let m = self.open();

        if self.expect(L_BRACE) {
            while !self.is(R_BRACE) && !self.is_eof() {
                self.parse_stmt();
            }

            self.expect(R_BRACE);
        }

        self.close(m, BLOCK);
    }

    fn parse_stmt(&mut self) {
        self.advance_by_non_leading_trivia();
        let m = self.open();
        self.advance_by_all_trivia();

        match self.current() {
            LET_KW => {
                self.parse_let(m);
            }
            _ => {
                if self.is_set(EXPRESSION_FIRST) {
                    let blocklike = self.parse_expr_stmt();

                    if !self.is(R_BRACE) {
                        if blocklike.is_yes() {
                            self.eat(SEMICOLON);
                        } else {
                            self.expect(SEMICOLON);
                        }
                    }

                    self.close(m, EXPR_STMT);
                } else {
                    self.report_error(ParseError::ExpectedStatement);

                    if !self.is(R_BRACE) {
                        self.advance();
                    }

                    self.close(m, TokenKind::ERROR_STMT);
                }
            }
        }
    }

    fn parse_if(&mut self) {
        let m = self.open();
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

        self.close(m, IF);
    }

    fn parse_match(&mut self) {
        let m = self.open();
        self.assert(MATCH_KW);

        self.parse_expr();

        self.expect(L_BRACE);

        while !self.is(R_BRACE) && !self.is_eof() {
            let blocklike = self.parse_match_arm();

            if !self.is(R_BRACE) && !self.is_eof() {
                if blocklike.is_yes() {
                    self.eat(COMMA);
                } else {
                    self.expect(COMMA);
                }
            }
        }

        self.expect(R_BRACE);

        self.close(m, TokenKind::MATCH);
    }

    fn parse_match_arm(&mut self) -> Blocklike {
        let m = self.open();
        self.parse_pattern();

        if self.eat(IF_KW) {
            self.parse_expr();
        }

        self.expect(DOUBLE_ARROW);
        let blocklike = self.parse_expr_stmt();
        self.close(m, MATCH_ARM);
        blocklike
    }

    fn parse_pattern(&mut self) {
        let m = self.open();
        self.parse_pattern_no_top_alt();

        if self.is(OR) {
            while self.eat(OR) {
                self.parse_pattern_no_top_alt();
            }

            self.close(m, ALT);
        } else {
            self.cancel_node(m);
        }
    }

    fn parse_pattern_no_top_alt(&mut self) {
        let m = self.open();

        if self.eat(UNDERSCORE) {
            self.close(m, UNDERSCORE_PATTERN);
        } else if self.eat(DOT_DOT) {
            self.close(m, REST);
        } else if self.is(TRUE) || self.is(FALSE) {
            self.parse_lit_bool();

            self.close(m, LIT_PATTERN_BOOL);
        } else if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                PATTERN_RS,
                ParseError::ExpectedPattern,
                |p| {
                    if p.is_set(PATTERN_FIRST) {
                        p.parse_pattern();
                        true
                    } else {
                        false
                    }
                },
            );

            self.close(m, TUPLE_PATTERN);
        } else if self.is(CHAR_LITERAL) {
            self.parse_lit_char();
            self.close(m, LIT_PATTERN_CHAR);
        } else if self.is(STRING_LITERAL) {
            self.parse_string();
            self.close(m, LIT_PATTERN_STR);
        } else if self.is(INT_LITERAL) || self.is2(SUB, INT_LITERAL) {
            self.parse_lit_int_minus();
            self.close(m, LIT_PATTERN_INT);
        } else if self.is(FLOAT_LITERAL) || self.is2(SUB, FLOAT_LITERAL) {
            self.parse_lit_float_minus();
            self.close(m, LIT_PATTERN_FLOAT);
        } else if self.is2(MUT_KW, IDENTIFIER) {
            self.assert(MUT_KW);
            self.expect_name().expect("identifier expected");
            self.close(m, IDENT_PATTERN);
        } else if self.is(IDENTIFIER) {
            if !self.nth_is(1, COLON_COLON) && !self.nth_is(1, L_PAREN) {
                self.expect_name().expect("identifier expected");
                self.close(m, IDENT_PATTERN);
                return;
            }

            self.parse_path();

            if self.is(L_PAREN) {
                let m = self.open();
                self.parse_list(
                    L_PAREN,
                    COMMA,
                    R_PAREN,
                    PATTERN_RS,
                    ParseError::ExpectedPattern,
                    |p| {
                        if p.is2(IDENTIFIER, EQ) {
                            let m2 = p.open();
                            p.expect_name().expect("identifier expected");
                            p.assert(EQ);
                            p.parse_pattern();
                            p.close(m2, CTOR_FIELD);
                            true
                        } else if p.is_set(PATTERN_FIRST) {
                            let m2 = p.open();
                            p.parse_pattern();
                            p.close(m2, CTOR_FIELD);
                            true
                        } else {
                            false
                        }
                    },
                );

                self.close(m, CTOR_FIELD_LIST);
            }

            self.close(m, CTOR_PATTERN);
        } else {
            self.report_error(ParseError::ExpectedPattern);
            self.advance();

            self.close(m, TokenKind::ERROR_PATTERN);
        }
    }

    fn parse_for(&mut self) {
        let m = self.open();
        self.assert(FOR_KW);
        self.parse_pattern();
        self.expect(IN_KW);
        self.parse_expr();
        self.parse_block();

        self.close(m, FOR);
    }

    fn parse_while(&mut self) {
        let m = self.open();
        self.assert(WHILE_KW);
        self.parse_expr();
        self.parse_block();

        self.close(m, WHILE);
    }

    fn parse_break(&mut self) {
        let m = self.open();
        self.assert(BREAK_KW);
        self.close(m, BREAK);
    }

    fn parse_continue(&mut self) {
        let m = self.open();
        self.assert(CONTINUE_KW);
        self.close(m, CONTINUE);
    }

    fn parse_return(&mut self) {
        let m = self.open();
        self.assert(RETURN_KW);
        if self.is_set(EXPRESSION_FIRST) {
            self.parse_expr();
        }

        self.close(m, RETURN);
    }

    fn parse_expr(&mut self) -> Blocklike {
        self.parse_expr_bp(0, false)
    }

    fn parse_expr_stmt(&mut self) -> Blocklike {
        self.parse_expr_bp(0, true)
    }

    fn parse_expr_bp(&mut self, min_bp: u32, prefer_stmt: bool) -> Blocklike {
        if !self.is_set(EXPRESSION_FIRST) {
            self.report_error(ParseError::ExpectedExpression);

            let m = self.open();
            self.close(m, TokenKind::ERROR_EXPR);
            return Blocklike::No;
        }

        let m = self.open();
        let mut blocklike = self.parse_unary_expr(prefer_stmt);

        loop {
            let op = self.current();

            blocklike = match op {
                AS_KW => {
                    self.assert(AS_KW);
                    self.parse_type();
                    self.close(m.clone(), CONV);
                    Blocklike::No
                }

                IS_KW => {
                    self.assert(IS_KW);
                    self.parse_pattern();
                    self.close(m.clone(), IS);
                    Blocklike::No
                }

                _ => blocklike,
            };

            let op = self.current();
            let (l_bp, r_bp) = match op {
                EQ | ADD_EQ | SUB_EQ | MUL_EQ | DIV_EQ | MOD_EQ | OR_EQ | AND_EQ | CARET_EQ
                | LT_LT_EQ | GT_GT_EQ | GT_GT_GT_EQ => (1, 2),
                OR_OR => (2, 3),
                AND_AND => (3, 4),
                EQ_EQ | NOT_EQ | LT | LE | GT | GE | EQ_EQ_EQ | NOT_EQ_EQ => (4, 5),
                ADD | SUB | OR | CARET => (5, 6),
                MUL | DIV | MODULO | AND | LT_LT | GT_GT | GT_GT_GT => (6, 7),
                _ => {
                    return blocklike;
                }
            };

            if l_bp < min_bp {
                return blocklike;
            }

            self.advance();

            self.parse_expr_bp(r_bp, prefer_stmt);
            self.close(m.clone(), BIN);
            blocklike = Blocklike::No;
        }
    }

    fn parse_unary_expr(&mut self, prefer_stmt: bool) -> Blocklike {
        match self.current() {
            SUB | NOT => {
                let m = self.open();
                self.advance();

                self.parse_postfix_expr(prefer_stmt);
                self.close(m, UN);
                Blocklike::No
            }

            _ => self.parse_postfix_expr(prefer_stmt),
        }
    }

    fn parse_postfix_expr(&mut self, prefer_stmt: bool) -> Blocklike {
        let m = self.open();
        let mut blocklike = self.parse_factor();

        loop {
            match self.current() {
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

                            self.close(m.clone(), METHOD_CALL_EXPR);
                        } else {
                            self.close(m.clone(), DOT_EXPR);
                        }
                    } else {
                        self.parse_factor();
                        self.close(m.clone(), DOT_EXPR);
                    }
                }

                L_PAREN if !(blocklike.is_yes() && prefer_stmt) => {
                    self.parse_call(m.clone());
                }

                L_BRACKET => {
                    self.parse_list(
                        L_BRACKET,
                        COMMA,
                        R_BRACKET,
                        TYPE_PARAM_RS,
                        ParseError::ExpectedType,
                        |p| {
                            if p.is_set(TYPE_FIRST) {
                                p.parse_type();
                                true
                            } else {
                                false
                            }
                        },
                    );
                    self.close(m.clone(), TYPED_EXPR);
                }

                COLON_COLON => {
                    self.assert(COLON_COLON);
                    self.parse_factor();
                    self.close(m.clone(), PATH);
                }

                _ => {
                    return blocklike;
                }
            }

            blocklike = Blocklike::No;
        }
    }

    fn parse_call(&mut self, marker: Marker) {
        self.parse_argument_list();
        self.close(marker, CALL);
    }

    fn parse_argument_list(&mut self) {
        let m = self.open();

        self.parse_list(
            L_PAREN,
            COMMA,
            R_PAREN,
            EMPTY,
            ParseError::ExpectedExpression,
            |p| {
                if p.is2(IDENTIFIER, EQ) {
                    let m = p.open();
                    p.expect_name();
                    p.assert(EQ);
                    p.parse_expr();
                    p.close(m, TokenKind::ARGUMENT);
                    true
                } else if p.is_set(EXPRESSION_FIRST) {
                    let m = p.open();
                    p.parse_expr();
                    p.close(m, TokenKind::ARGUMENT);
                    true
                } else {
                    false
                }
            },
        );

        self.close(m, ARGUMENT_LIST);
    }

    fn parse_factor(&mut self) -> Blocklike {
        match self.current() {
            L_PAREN => {
                self.parse_parentheses();
                Blocklike::No
            }
            L_BRACE => {
                self.parse_block();
                Blocklike::Yes
            }
            IF_KW => {
                self.parse_if();
                Blocklike::Yes
            }
            CHAR_LITERAL => {
                self.parse_lit_char();
                Blocklike::No
            }
            INT_LITERAL => {
                self.parse_lit_int();
                Blocklike::No
            }
            FLOAT_LITERAL => {
                self.parse_lit_float();
                Blocklike::No
            }
            STRING_LITERAL => {
                self.parse_string();
                Blocklike::No
            }
            TEMPLATE_LITERAL => {
                self.parse_template();
                Blocklike::No
            }
            IDENTIFIER => {
                self.parse_identifier();
                Blocklike::No
            }
            TRUE => {
                self.parse_lit_bool();
                Blocklike::No
            }
            FALSE => {
                self.parse_lit_bool();
                Blocklike::No
            }
            SELF_KW => {
                self.parse_this();
                Blocklike::No
            }
            OR | OR_OR => {
                self.parse_lambda();
                Blocklike::No
            }
            FOR_KW => {
                self.parse_for();
                Blocklike::Yes
            }
            WHILE_KW => {
                self.parse_while();
                Blocklike::Yes
            }
            BREAK_KW => {
                self.parse_break();
                Blocklike::No
            }
            CONTINUE_KW => {
                self.parse_continue();
                Blocklike::No
            }
            RETURN_KW => {
                self.parse_return();
                Blocklike::No
            }
            MATCH_KW => {
                self.parse_match();
                Blocklike::Yes
            }
            _ => {
                let m = self.open();
                self.report_error(ParseError::ExpectedFactor);
                self.close(m, TokenKind::ERROR_EXPR);
                Blocklike::No
            }
        }
    }

    fn parse_identifier(&mut self) {
        let m = self.open();
        self.assert_value(IDENTIFIER);
        self.close(m, NAME_EXPR);
    }

    fn parse_parentheses(&mut self) {
        let m = self.open();
        self.assert(L_PAREN);

        if self.eat(R_PAREN) {
            self.close(m, TUPLE);
            return;
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

            self.close(m, TUPLE);
        } else {
            self.expect(R_PAREN);
            self.close(m, PAREN);
        }
    }

    fn parse_lit_char(&mut self) {
        let m = self.open();
        self.assert_value(CHAR_LITERAL);
        self.close(m, LIT_CHAR);
    }

    fn parse_lit_int(&mut self) {
        let m = self.open();
        self.assert_value(INT_LITERAL);
        self.close(m, LIT_INT);
    }

    fn parse_lit_int_minus(&mut self) {
        self.parse_lit_with_minus(|p| p.parse_lit_int());
    }

    fn parse_lit_float_minus(&mut self) {
        self.parse_lit_with_minus(|p| p.parse_lit_float());
    }

    fn parse_lit_with_minus<F: FnOnce(&mut Parser)>(&mut self, fct: F) {
        if self.is(SUB) {
            let m = self.open();
            self.assert(SUB);

            fct(self);

            self.close(m, UN);
        } else {
            fct(self);
        }
    }

    fn parse_lit_float(&mut self) {
        let m = self.open();
        self.assert_value(FLOAT_LITERAL);
        self.close(m, LIT_FLOAT);
    }

    fn parse_template(&mut self) {
        let m = self.open(); // TEMPLATE node
        let m2 = self.open(); // Start literal node
        self.assert(TEMPLATE_LITERAL);

        self.close(m2, LIT_STR);

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

            let m3 = self.open();
            self.advance();
            self.close(m3, LIT_STR);
        }

        self.close(m, TEMPLATE);
    }

    fn parse_string(&mut self) {
        let m = self.open();
        self.assert_value(STRING_LITERAL);
        self.close(m, LIT_STR);
    }

    fn parse_lit_bool(&mut self) {
        let m = self.open();
        let kind = self.current();
        self.assert(kind);
        self.close(m, LIT_BOOL);
    }

    fn parse_this(&mut self) {
        let m = self.open();
        self.assert(SELF_KW);

        self.close(m, THIS);
    }

    fn parse_lambda(&mut self) {
        let m = self.open();

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
                |p| {
                    p.parse_function_param();
                    true
                },
            );
        };

        if self.eat(COLON) {
            self.parse_type();
        }

        self.parse_block();
        self.close(m, LAMBDA);
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

    fn expect_name(&mut self) -> Option<()> {
        if self.is(IDENTIFIER) {
            self.assert_value(IDENTIFIER);
            Some(())
        } else {
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
        self.raw_advance(false);
        self.skip_trivia();
    }

    fn skip_trivia(&mut self) {
        while self.current().is_trivia() {
            self.raw_advance(true);
        }
    }

    fn raw_advance(&mut self, is_leading_trivia: bool) {
        if self.token_idx < self.tokens.len() {
            let kind = self.current();
            let len = self.token_widths[self.token_idx];
            self.offset += len;
            debug_assert!(kind <= EOF);
            self.token_idx += 1;

            if is_leading_trivia {
                self.leading += 1;
            } else {
                for _ in 0..self.leading + 1 {
                    self.events.push(Event::Advance);
                }
                self.leading = 0;
            }
        } else {
            for _ in 0..self.leading {
                self.events.push(Event::Advance);
            }
            self.leading = 0;
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

    fn open(&mut self) -> Marker {
        let start = self.events.len();
        self.events.push(Event::Open { kinds: Vec::new() });
        Marker { start }
    }

    fn advance_by_trailing_trivia(&mut self) {
        if self.leading == 0 {
            return;
        }

        let leading = self.leading;
        let idx_start = self.token_idx - leading;
        let leading_len: usize = self.token_widths[idx_start..self.token_idx]
            .iter()
            .map(|len| *len as usize)
            .sum();
        let mut pos = self.offset as usize - leading_len;
        let mut emit_count = 0usize;
        let mut multiline_count = 0usize;

        for idx in idx_start..self.token_idx {
            let len = self.token_widths[idx] as usize;
            let text = &self.content[pos..pos + len];
            let kind = self.tokens[idx];

            let current_count = (idx - idx_start) + 1;

            match kind {
                WHITESPACE => {}
                NEWLINE => {
                    emit_count = multiline_count;
                    break;
                }
                LINE_COMMENT => {
                    emit_count = current_count;
                    break;
                }
                MULTILINE_COMMENT => {
                    let has_newline = text.contains('\n') || text.contains('\r');
                    if has_newline {
                        emit_count = current_count;
                        break;
                    }

                    multiline_count = current_count;
                }
                _ => unreachable!(),
            }

            pos += len;
        }

        for _ in 0..emit_count {
            self.events.push(Event::Advance);
        }
        self.leading -= emit_count;
    }

    fn advance_by_non_leading_trivia(&mut self) {
        if self.leading == 0 {
            return;
        }

        let mut newlines = 0;
        let mut empty = true;
        let mut leading_count = 0;
        let mut last_leading_count = 0;

        while leading_count < self.leading {
            let kind = self.tokens[self.token_idx - leading_count - 1];

            match kind {
                NEWLINE => {
                    if newlines > 0 && empty {
                        leading_count = last_leading_count;
                        break;
                    }
                    newlines += 1;
                    empty = true;
                    last_leading_count = leading_count;
                }
                WHITESPACE => {}
                LINE_COMMENT | MULTILINE_COMMENT => {
                    empty = false;
                }
                _ => unreachable!(),
            }

            leading_count += 1;
        }

        let non_leading_count = self.leading - leading_count;
        for _ in 0..non_leading_count {
            self.events.push(Event::Advance);
        }
        self.leading -= non_leading_count;
    }

    fn close(&mut self, m: Marker, kind: TokenKind) {
        let event = &mut self.events[m.start];

        match event {
            Event::Open { kinds } => {
                kinds.push(kind);
            }
            _ => unreachable!(),
        }

        self.advance_by_trailing_trivia();
        // self.advance_by_non_leading_comments();
        self.events.push(Event::Close);
    }

    fn cancel_node(&mut self, _m: Marker) {
        // No longer needed - markers are now explicit
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

fn build_tree(
    content: &str,
    tokens: &[TokenKind],
    token_widths: &[u32],
    mut events: Vec<Event>,
) -> (Arena<GreenNode>, GreenId) {
    let mut arena = Arena::<GreenNode>::new();

    let mut stack: Vec<Id<GreenNode>> = Vec::new();
    let mut token_start: usize = 0;
    let mut token_idx = 0;

    let last = events.pop().unwrap();
    assert!(matches!(last, Event::Close));

    for event in events {
        match event {
            Event::Open { kinds } => {
                for kind in kinds.into_iter().rev() {
                    let id = arena.alloc(GreenNode {
                        syntax_kind: kind,
                        children: Vec::new(),
                        text_length: 0,
                    });
                    stack.push(id);
                }
            }

            Event::Advance => {
                let kind = tokens[token_idx];
                let width_u32 = token_widths[token_idx];
                let width = width_u32 as usize;
                let text = String::from(&content[token_start..token_start + width]);
                let last_id = stack.last().cloned().unwrap();
                let node = &mut arena[last_id];
                node.children
                    .push(GreenElement::Token(GreenToken { kind, text }));
                node.text_length += width_u32;
                token_idx += 1;
                token_start += width;
            }

            Event::Close => {
                let id = stack.pop().unwrap();
                let width = arena[id].text_length;
                let last_id = stack.last().cloned().unwrap();
                let node = &mut arena[last_id];
                node.children.push(GreenElement::Node(GreenId::new(id)));
                node.text_length += width;
            }
        }
    }

    assert_eq!(stack.len(), 1);
    let root_id = stack.pop().unwrap();
    assert_eq!(arena[root_id].text_length as usize, content.len());
    let root_id = GreenId::new(root_id);

    (arena, root_id)
}
