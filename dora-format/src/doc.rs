use id_arena::{Arena, Id};

use dora_parser::TokenKind;
use dora_parser::TokenKind::*;
use dora_parser::ast::*;

use crate::doc::utils::{Options, has_between, if_token, print_rest, print_token, print_while};

pub(crate) mod element;
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
}

impl Formatter {
    pub(crate) fn new() -> Self {
        Self {
            arena: Arena::new(),
            out: Vec::new(),
        }
    }

    fn token(&mut self, token: SyntaxToken) -> DocId {
        let id = self.arena.alloc(Doc::Text {
            text: token.text().to_string(),
        });
        self.out.push(id);
        id
    }

    fn text(&mut self, text: &str) -> DocId {
        let id = self.arena.alloc(Doc::Text {
            text: text.to_string(),
        });
        self.out.push(id);
        id
    }

    pub(crate) fn hard_line(&mut self) -> DocId {
        let id = self.arena.alloc(Doc::HardLine);
        self.out.push(id);
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
        TokenKind::ALIAS => element::format_alias(node.as_alias(), f),
        TokenKind::ARGUMENT => element::format_argument(node.as_argument(), f),
        TokenKind::ARGUMENT_LIST => element::format_argument_list(node.as_argument_list(), f),
        TokenKind::BLOCK => format_block(node.as_block(), f),
        TokenKind::CTOR_FIELD => pattern::format_ctor_field(node.as_ctor_field(), f),
        TokenKind::CTOR_FIELD_LIST => pattern::format_ctor_field_list(node.as_ctor_field_list(), f),
        TokenKind::CTOR_PATTERN => pattern::format_ctor_pattern(node.as_ctor_pattern(), f),
        TokenKind::LET => stmt::format_let(node.as_let(), f),
        TokenKind::IDENT_PATTERN => pattern::format_ident_pattern(node.as_ident_pattern(), f),
        TokenKind::BIN => expr::format_bin(node.as_bin(), f),
        TokenKind::BREAK => expr::format_break(node.as_break(), f),
        TokenKind::CALL => expr::format_call(node.as_call(), f),
        TokenKind::CLASS => element::format_class(node.as_class(), f),
        TokenKind::CONST => element::format_const(node.as_const(), f),
        TokenKind::CONV => expr::format_conv(node.as_conv(), f),
        TokenKind::CONTINUE => expr::format_continue(node.as_continue(), f),
        TokenKind::DOT_EXPR => expr::format_dot_expr(node.as_dot_expr(), f),
        TokenKind::ENUM => element::format_enum(node.as_enum(), f),
        TokenKind::ENUM_VARIANT => element::format_enum_variant(node.as_enum_variant(), f),
        TokenKind::EXTERN => element::format_extern(node.as_extern(), f),
        TokenKind::EXPR_STMT => stmt::format_expr_stmt(node.as_expr_stmt(), f),
        TokenKind::FOR => expr::format_for(node.as_for(), f),
        TokenKind::FIELD => element::format_field(node.as_field(), f),
        TokenKind::FUNCTION => element::format_function(node.as_function(), f),
        TokenKind::IF => expr::format_if(node.as_if(), f),
        TokenKind::IMPL => element::format_impl(node.as_impl(), f),
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
        TokenKind::MODULE => element::format_module(node.as_module(), f),
        TokenKind::MODIFIER => element::format_modifier(node.as_modifier(), f),
        TokenKind::MODIFIER_LIST => element::format_modifier_list(node.as_modifier_list(), f),
        TokenKind::NAME => element::format_name(node.as_name(), f),
        TokenKind::PARAM => element::format_param(node.as_param(), f),
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
        TokenKind::STRUCT => element::format_struct(node.as_struct(), f),
        TokenKind::TEMPLATE => expr::format_template(node.as_template(), f),
        TokenKind::TRAIT => element::format_trait(node.as_trait(), f),
        TokenKind::TUPLE_PATTERN => pattern::format_tuple_pattern(node.as_tuple_pattern(), f),
        TokenKind::TUPLE_TYPE => ty::format_tuple_type(node.as_tuple_type(), f),
        TokenKind::TYPED_EXPR => expr::format_typed_expr(node.as_typed_expr(), f),
        TokenKind::TYPE_ARGUMENT => element::format_type_argument(node.as_type_argument(), f),
        TokenKind::TYPE_ARGUMENT_LIST => {
            element::format_type_argument_list(node.as_type_argument_list(), f)
        }
        TokenKind::TYPE_BOUNDS => element::format_type_bounds(node.as_type_bounds(), f),
        TokenKind::TYPE_PARAM => element::format_type_param(node.as_type_param(), f),
        TokenKind::TYPE_PARAM_LIST => element::format_type_param_list(node.as_type_param_list(), f),
        TokenKind::TUPLE => expr::format_tuple(node.as_tuple(), f),
        TokenKind::UNDERSCORE_PATTERN => {
            pattern::format_underscore_pattern(node.as_underscore_pattern(), f)
        }
        TokenKind::UN => expr::format_un(node.as_un(), f),
        TokenKind::UPCASE_THIS => element::format_upcase_this(node.as_upcase_this(), f),
        TokenKind::WHILE => expr::format_while(node.as_while(), f),
        TokenKind::LIT_BOOL => lit::format_lit_bool(node.as_lit_bool(), f),
        TokenKind::LIT_CHAR => lit::format_lit_char(node.as_lit_char(), f),
        TokenKind::LIT_FLOAT => lit::format_lit_float(node.as_lit_float(), f),
        TokenKind::LIT_INT => lit::format_lit_int(node.as_lit_int(), f),
        TokenKind::LIT_STR => lit::format_lit_str(node.as_lit_str(), f),
        TokenKind::GLOBAL => element::format_global(node.as_global(), f),
        TokenKind::USE => element::format_use(node.as_use(), f),
        TokenKind::USE_AS => element::format_use_as(node.as_use_as(), f),
        TokenKind::USE_GROUP => element::format_use_group(node.as_use_group(), f),
        TokenKind::USE_NAME => element::format_use_name(node.as_use_name(), f),
        TokenKind::USE_PATH => element::format_use_path(node.as_use_path(), f),
        TokenKind::USE_ATOM => element::format_use_atom(node.as_use_atom(), f),
        TokenKind::WHERE_CLAUSE => element::format_where_clause(node.as_where_clause(), f),
        TokenKind::WHERE_CLAUSE_ITEM => {
            element::format_where_clause_item(node.as_where_clause_item(), f)
        }
        _ => {
            panic!("unsupported node {}", node.syntax_kind());
        }
    }
}

fn format_element_list(node: AstElementList, f: &mut Formatter) {
    let mut iter = node.children_with_tokens().peekable();
    let opt = Options::build().emit_line_before().emit_line_after().new();
    if if_token(f, &mut iter, L_BRACE) {
        if !has_between(node.syntax_node(), L_BRACE, R_BRACE) {
            print_token(f, &mut iter, L_BRACE, &opt);
            print_token(f, &mut iter, R_BRACE, &opt);
            print_rest(f, iter, &opt);
        } else {
            print_token(f, &mut iter, L_BRACE, &opt);
            f.hard_line();
            f.nest(BLOCK_INDENT, |f| {
                print_while::<AstElement, _>(f, &mut iter, &opt);
            });
            print_token(f, &mut iter, R_BRACE, &opt);
            print_rest(f, iter, &opt);
        }
    } else {
        print_while::<AstElement, _>(f, &mut iter, &opt);
        print_rest(f, iter, &opt);
    }
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
