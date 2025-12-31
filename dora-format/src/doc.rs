use id_arena::{Arena, Id};

use dora_parser::TokenKind;
use dora_parser::TokenKind::*;
use dora_parser::ast::*;

use crate::doc::utils::{Options, has_between, needs_space, print_rest, print_token, print_while};

pub(crate) mod expr;
pub(crate) mod lit;
pub(crate) mod pattern;
pub mod print;
pub(crate) mod stmt;
pub(crate) mod ty;
pub(crate) mod utils;

pub type DocId = Id<Doc>;

#[allow(unused)]
pub enum Doc {
    Concat { children: Vec<DocId> },
    Nest { indent: u32, doc: DocId },
    Group { doc: DocId },
    Text { text: String },
    SoftLine,
    HardLine,
}

pub struct DocBuilder {
    arena: Arena<Doc>,
    out: Vec<DocId>,
}

impl DocBuilder {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            out: Vec::new(),
        }
    }

    pub fn text(&mut self, text: impl Into<String>) -> &mut Self {
        let id = self.arena.alloc(Doc::Text { text: text.into() });
        self.out.push(id);
        self
    }

    pub fn soft_line(&mut self) -> &mut Self {
        let id = self.arena.alloc(Doc::SoftLine);
        self.out.push(id);
        self
    }

    pub fn hard_line(&mut self) -> &mut Self {
        let id = self.arena.alloc(Doc::HardLine);
        self.out.push(id);
        self
    }

    pub fn group<F>(&mut self, f: F) -> &mut Self
    where
        F: FnOnce(&mut DocBuilder),
    {
        let saved = self.out.len();
        f(self);
        let children = self.out.split_off(saved);
        let children = self.concat_doc(children);
        let id = self.arena.alloc(Doc::Group { doc: children });
        self.out.push(id);
        self
    }

    pub fn nest<F>(&mut self, indent: u32, f: F) -> &mut Self
    where
        F: FnOnce(&mut DocBuilder),
    {
        let saved = self.out.len();
        f(self);
        let children = self.out.split_off(saved);
        let children = self.concat_doc(children);
        let id = self.arena.alloc(Doc::Nest {
            indent,
            doc: children,
        });
        self.out.push(id);
        self
    }

    pub fn concat(&mut self, children: Vec<DocId>) -> &mut Self {
        let id = self.concat_doc(children);
        self.out.push(id);
        self
    }

    fn concat_doc(&mut self, children: Vec<DocId>) -> DocId {
        if children.is_empty() {
            self.arena.alloc(Doc::Text { text: "".into() })
        } else if children.len() == 1 {
            children[0]
        } else {
            self.arena.alloc(Doc::Concat { children })
        }
    }

    pub fn finish(mut self) -> (Arena<Doc>, DocId) {
        let children = std::mem::take(&mut self.out);
        let root_id = self.concat_doc(children);
        (self.arena, root_id)
    }
}

pub fn format(root: SyntaxNode) -> (Arena<Doc>, DocId) {
    let mut f = Formatter::new();
    format_node(root, &mut f);
    f.finish()
}

pub(crate) struct Formatter {
    arena: Arena<Doc>,
    out: Vec<DocId>,
    last_kind: TokenKind,
    in_block: bool,
}

impl Formatter {
    pub(crate) fn new() -> Self {
        Self {
            arena: Arena::new(),
            out: Vec::new(),
            last_kind: TokenKind::ERROR,
            in_block: false,
        }
    }

    fn token(&mut self, token: SyntaxToken) -> DocId {
        if needs_space(self.last_kind, token.syntax_kind()) {
            self.text(" ");
        }

        let id = self.arena.alloc(Doc::Text {
            text: token.text().to_string(),
        });
        self.out.push(id);
        self.last_kind = token.syntax_kind();
        id
    }

    fn text(&mut self, text: &str) -> DocId {
        let id = self.arena.alloc(Doc::Text {
            text: text.to_string(),
        });
        self.out.push(id);
        self.last_kind = TokenKind::ERROR;
        id
    }

    pub(crate) fn reset_spacing(&mut self) {
        self.last_kind = TokenKind::ERROR;
    }

    pub(crate) fn hard_line(&mut self) -> DocId {
        let id = self.arena.alloc(Doc::HardLine);
        self.out.push(id);
        self.last_kind = TokenKind::ERROR;
        id
    }

    pub(crate) fn nest<F>(&mut self, increase: u32, fct: F) -> DocId
    where
        F: FnOnce(&mut Formatter),
    {
        let saved = self.out.len();

        fct(self);

        let children = self.out.split_off(saved);
        let children = self.concat(children);
        let nest_doc = self.arena.alloc(Doc::Nest {
            indent: increase,
            doc: children,
        });
        self.out.push(nest_doc);
        self.last_kind = TokenKind::ERROR;
        nest_doc
    }

    fn concat(&mut self, children: Vec<DocId>) -> DocId {
        if children.len() == 1 {
            children[0]
        } else {
            self.arena.alloc(Doc::Concat { children })
        }
    }

    pub(crate) fn finish(mut self) -> (Arena<Doc>, DocId) {
        let children = std::mem::replace(&mut self.out, Vec::new());
        let root_id = self.arena.alloc(Doc::Concat { children });
        (self.arena, root_id)
    }
}

pub(crate) fn format_node(node: SyntaxNode, f: &mut Formatter) {
    match node.syntax_kind() {
        TokenKind::ELEMENT_LIST => format_element_list(node.as_element_list(), f),
        TokenKind::ALT => pattern::format_alt(node.as_alt(), f),
        TokenKind::BLOCK => format_block(node.as_block(), f),
        TokenKind::CTOR_FIELD => pattern::format_ctor_field(node.as_ctor_field(), f),
        TokenKind::CTOR_FIELD_LIST => pattern::format_ctor_field_list(node.as_ctor_field_list(), f),
        TokenKind::CTOR_PATTERN => pattern::format_ctor_pattern(node.as_ctor_pattern(), f),
        TokenKind::LET => stmt::format_let(node.as_let(), f),
        TokenKind::IDENT_PATTERN => pattern::format_ident_pattern(node.as_ident_pattern(), f),
        TokenKind::BIN => expr::format_bin(node.as_bin(), f),
        TokenKind::BREAK => expr::format_break(node.as_break(), f),
        TokenKind::CALL => expr::format_call(node.as_call(), f),
        TokenKind::CONV => expr::format_conv(node.as_conv(), f),
        TokenKind::CONTINUE => expr::format_continue(node.as_continue(), f),
        TokenKind::DOT_EXPR => expr::format_dot_expr(node.as_dot_expr(), f),
        TokenKind::EXPR_STMT => stmt::format_expr_stmt(node.as_expr_stmt(), f),
        TokenKind::FOR => expr::format_for(node.as_for(), f),
        TokenKind::IS => expr::format_is(node.as_is(), f),
        TokenKind::LAMBDA => expr::format_lambda(node.as_lambda(), f),
        TokenKind::LIT_PATTERN_BOOL => {
            pattern::format_lit_pattern_bool(node.as_lit_pattern_bool(), f)
        }
        TokenKind::LIT_PATTERN_CHAR => {
            pattern::format_lit_pattern_char(node.as_lit_pattern_char(), f)
        }
        TokenKind::LIT_PATTERN_FLOAT => {
            pattern::format_lit_pattern_float(node.as_lit_pattern_float(), f)
        }
        TokenKind::LIT_PATTERN_INT => pattern::format_lit_pattern_int(node.as_lit_pattern_int(), f),
        TokenKind::LIT_PATTERN_STR => pattern::format_lit_pattern_str(node.as_lit_pattern_str(), f),
        TokenKind::MATCH => expr::format_match(node.as_match(), f),
        TokenKind::MATCH_ARM => expr::format_match_arm(node.as_match_arm(), f),
        TokenKind::LAMBDA_TYPE => ty::format_lambda_type(node.as_lambda_type(), f),
        TokenKind::PATH_DATA => pattern::format_path_data(node.as_path_data(), f),
        TokenKind::PATH => expr::format_path(node.as_path(), f),
        TokenKind::PATH_TYPE => ty::format_path_type(node.as_path_type(), f),
        TokenKind::NAME_EXPR => expr::format_name_expr(node.as_name_expr(), f),
        TokenKind::PAREN => expr::format_paren(node.as_paren(), f),
        TokenKind::QUALIFIED_PATH_TYPE => {
            ty::format_qualified_path_type(node.as_qualified_path_type(), f)
        }
        TokenKind::REF_TYPE => ty::format_ref_type(node.as_ref_type(), f),
        TokenKind::RETURN => expr::format_return(node.as_return(), f),
        TokenKind::REST => pattern::format_rest_pattern(node.as_rest(), f),
        TokenKind::THIS => expr::format_this(node.as_this(), f),
        TokenKind::TUPLE_PATTERN => pattern::format_tuple_pattern(node.as_tuple_pattern(), f),
        TokenKind::TUPLE_TYPE => ty::format_tuple_type(node.as_tuple_type(), f),
        TokenKind::TYPED_EXPR => expr::format_typed_expr(node.as_typed_expr(), f),
        TokenKind::TUPLE => expr::format_tuple(node.as_tuple(), f),
        TokenKind::UNDERSCORE_PATTERN => {
            pattern::format_underscore_pattern(node.as_underscore_pattern(), f)
        }
        TokenKind::UN => expr::format_un(node.as_un(), f),
        TokenKind::WHILE => expr::format_while(node.as_while(), f),
        TokenKind::LIT_BOOL => lit::format_lit_bool(node.as_lit_bool(), f),
        TokenKind::LIT_CHAR => lit::format_lit_char(node.as_lit_char(), f),
        TokenKind::LIT_FLOAT => lit::format_lit_float(node.as_lit_float(), f),
        TokenKind::LIT_INT => lit::format_lit_int(node.as_lit_int(), f),
        TokenKind::LIT_STR => lit::format_lit_str(node.as_lit_str(), f),
        _ => format_generic_node(node, f),
    }
}

fn format_element_list(node: AstElementList, f: &mut Formatter) {
    let mut iter = node.children_with_tokens().peekable();
    let opt = Options::build().emit_line_before().emit_line_after().new();
    print_while::<AstElement, _>(f, &mut iter, &opt);
}

fn format_block(node: AstBlock, f: &mut Formatter) {
    let mut iter = node.children_with_tokens().peekable();
    if !has_between(node.syntax_node(), L_BRACE, R_BRACE) {
        let opt = Options::new();
        print_token(f, &mut iter, L_BRACE, &opt);
        print_token(f, &mut iter, R_BRACE, &opt);
        print_rest(f, iter, &opt);
    } else {
        let opt = Options::build().emit_line_after().new();
        print_token(f, &mut iter, L_BRACE, &opt);
        f.hard_line();
        f.nest(BLOCK_INDENT, |f| {
            print_while::<AstStmt, _>(f, &mut iter, &opt);
            print_while::<AstExpr, _>(f, &mut iter, &opt);
        });
        print_token(f, &mut iter, R_BRACE, &opt);
        print_rest(f, iter, &opt);
    }
}

pub(crate) const BLOCK_INDENT: u32 = 4;

fn format_generic_node(node: SyntaxNode, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
                    format_whitespace(token.text(), f);
                }

                _ => {
                    f.token(token);
                }
            },

            SyntaxElement::Node(node) => {
                format_node(node, f);
            }
        }
    }
}

fn format_whitespace(text: &str, f: &mut Formatter) {
    let bytes = text.as_bytes();
    let mut i = 0;
    let mut breaks = 0;

    while i < bytes.len() {
        match bytes[i] {
            b'\r' => {
                breaks += 1;
                if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                    i += 2;
                } else {
                    i += 1;
                }
            }
            b'\n' => {
                breaks += 1;
                i += 1;
            }
            _ => {
                i += 1;
            }
        }
    }

    let emit = if f.in_block {
        if breaks >= 2 { 1 } else { 0 }
    } else {
        breaks.min(2)
    };

    for _ in 0..emit {
        f.hard_line();
    }
}
