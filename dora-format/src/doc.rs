use smol_str::SmolStr;

use dora_parser::TokenKind;
use dora_parser::ast::*;

pub(crate) mod bin;
pub(crate) mod element;
pub(crate) mod element_list;
pub(crate) mod expr;
pub(crate) mod lit;
pub(crate) mod pattern;
pub mod print;
pub(crate) mod stmt;
pub(crate) mod ty;
pub(crate) mod use_;
pub(crate) mod utils;

#[allow(unused)]
pub enum Doc {
    Concat { children: Vec<Doc> },
    Nest { indent: u32, doc: Box<Doc> },
    Group { doc: Box<Doc> },
    Text { text: SmolStr },
    SoftLine,
    SoftBreak,
    IfBreak { doc: Box<Doc> },
    HardLine,
}

pub struct DocBuilder {
    out: Vec<Doc>,
}

impl DocBuilder {
    pub fn new() -> Self {
        Self { out: Vec::new() }
    }

    pub fn text(&mut self, text: impl Into<SmolStr>) -> &mut Self {
        self.out.push(Doc::Text { text: text.into() });
        self
    }

    pub fn soft_line(&mut self) -> &mut Self {
        self.out.push(Doc::SoftLine);
        self
    }

    pub fn soft_break(&mut self) -> &mut Self {
        self.out.push(Doc::SoftBreak);
        self
    }

    pub fn if_break<F>(&mut self, f: F) -> &mut Self
    where
        F: FnOnce(&mut DocBuilder),
    {
        let saved = self.out.len();
        f(self);
        let children = self.out.split_off(saved);
        let doc = self.concat_doc(children);
        self.out.push(Doc::IfBreak { doc: Box::new(doc) });
        self
    }

    pub fn hard_line(&mut self) -> &mut Self {
        self.out.push(Doc::HardLine);
        self
    }

    pub fn group<F>(&mut self, f: F) -> &mut Self
    where
        F: FnOnce(&mut DocBuilder),
    {
        let saved = self.out.len();
        f(self);
        let children = self.out.split_off(saved);
        let doc = self.concat_doc(children);
        self.out.push(Doc::Group { doc: Box::new(doc) });
        self
    }

    pub fn nest<F>(&mut self, indent: u32, f: F) -> &mut Self
    where
        F: FnOnce(&mut DocBuilder),
    {
        let saved = self.out.len();
        f(self);
        let children = self.out.split_off(saved);
        let doc = self.concat_doc(children);
        self.out.push(Doc::Nest {
            indent,
            doc: Box::new(doc),
        });
        self
    }

    pub fn concat<F>(&mut self, f: F) -> &mut Self
    where
        F: FnOnce(&mut DocBuilder),
    {
        let saved = self.out.len();
        f(self);
        let children = self.out.split_off(saved);
        let doc = self.concat_doc(children);
        self.out.push(doc);
        self
    }

    fn concat_doc(&mut self, children: Vec<Doc>) -> Doc {
        if children.is_empty() {
            Doc::Text {
                text: SmolStr::new(""),
            }
        } else if children.len() == 1 {
            children.into_iter().next().expect("doc")
        } else {
            Doc::Concat { children }
        }
    }

    pub fn finish(mut self) -> Doc {
        let children = std::mem::take(&mut self.out);
        self.concat_doc(children)
    }
}

pub fn format(root: SyntaxNode) -> Doc {
    let mut f = Formatter::new();
    format_node(root, &mut f);
    f.finish()
}

pub(crate) struct Formatter {
    out: Vec<Doc>,
}

impl Formatter {
    pub(crate) fn new() -> Self {
        Self { out: Vec::new() }
    }

    pub(crate) fn append(&mut self, doc: Doc) {
        self.out.push(doc);
    }

    fn token(&mut self, token: SyntaxToken) {
        self.out.push(Doc::Text {
            text: SmolStr::new(token.text()),
        });
    }

    fn text(&mut self, text: &str) {
        self.out.push(Doc::Text {
            text: SmolStr::new(text),
        });
    }

    pub(crate) fn soft_break(&mut self) {
        self.out.push(Doc::SoftBreak);
    }

    pub(crate) fn soft_line(&mut self) {
        self.out.push(Doc::SoftLine);
    }

    pub(crate) fn hard_line(&mut self) {
        self.out.push(Doc::HardLine);
    }

    pub(crate) fn if_break<F>(&mut self, fct: F)
    where
        F: FnOnce(&mut Formatter),
    {
        let saved = self.out.len();

        fct(self);

        let children = self.out.split_off(saved);
        let doc = self.concat_docs(children);
        self.out.push(Doc::IfBreak { doc: Box::new(doc) });
    }

    pub(crate) fn nest<F>(&mut self, increase: u32, fct: F)
    where
        F: FnOnce(&mut Formatter),
    {
        let saved = self.out.len();

        fct(self);

        let children = self.out.split_off(saved);
        let doc = self.concat_docs(children);
        self.out.push(Doc::Nest {
            indent: increase,
            doc: Box::new(doc),
        });
    }

    #[allow(unused)]
    pub(crate) fn group<F>(&mut self, fct: F)
    where
        F: FnOnce(&mut Formatter),
    {
        let saved = self.out.len();

        fct(self);

        let children = self.out.split_off(saved);
        let doc = self.concat_docs(children);
        self.out.push(Doc::Group { doc: Box::new(doc) });
    }

    pub(crate) fn concat<F>(&mut self, fct: F) -> Doc
    where
        F: FnOnce(&mut Formatter),
    {
        let saved = self.out.len();

        fct(self);

        let children = self.out.split_off(saved);
        self.concat_docs(children)
    }

    fn concat_docs(&mut self, children: Vec<Doc>) -> Doc {
        if children.is_empty() {
            Doc::Text {
                text: SmolStr::new(""),
            }
        } else if children.len() == 1 {
            children.into_iter().next().expect("doc")
        } else {
            Doc::Concat { children }
        }
    }

    pub(crate) fn finish(mut self) -> Doc {
        let children = std::mem::take(&mut self.out);
        self.concat_docs(children)
    }
}

pub(crate) fn format_node(node: SyntaxNode, f: &mut Formatter) {
    match node.syntax_kind() {
        TokenKind::ELEMENT_LIST => element_list::format_element_list(node.as_element_list(), f),
        TokenKind::ALT => pattern::format_alt(node.as_alt(), f),
        TokenKind::ALIAS => element::format_alias(node.as_alias(), f),
        TokenKind::ARGUMENT => expr::format_argument(node.as_argument(), f),
        TokenKind::ARGUMENT_LIST => expr::format_argument_list(node.as_argument_list(), f),
        TokenKind::BLOCK_EXPR => expr::format_block(node.as_block_expr(), f),
        TokenKind::CTOR_FIELD => pattern::format_ctor_field(node.as_ctor_field(), f),
        TokenKind::CTOR_FIELD_LIST => pattern::format_ctor_field_list(node.as_ctor_field_list(), f),
        TokenKind::CTOR_PATTERN => pattern::format_ctor_pattern(node.as_ctor_pattern(), f),
        TokenKind::LET => stmt::format_let(node.as_let(), f),
        TokenKind::IDENT_PATTERN => pattern::format_ident_pattern(node.as_ident_pattern(), f),
        TokenKind::ASSIGN_EXPR => bin::format_assign(node.as_assign_expr(), f),
        TokenKind::BIN_EXPR => bin::format_bin(node.as_bin_expr(), f),
        TokenKind::BREAK_EXPR => expr::format_break(node.as_break_expr(), f),
        TokenKind::CALL_EXPR => expr::format_call(node.as_call_expr(), f),
        TokenKind::CLASS => element::format_class(node.as_class(), f),
        TokenKind::CONST => element::format_const(node.as_const(), f),
        TokenKind::AS_EXPR => expr::format_as_expr(node.as_as_expr(), f),
        TokenKind::CONTINUE_EXPR => expr::format_continue(node.as_continue_expr(), f),
        TokenKind::FIELD_EXPR => expr::format_field_expr(node.as_field_expr(), f),
        TokenKind::ENUM => element::format_enum(node.as_enum(), f),
        TokenKind::ENUM_VARIANT => element::format_enum_variant(node.as_enum_variant(), f),
        TokenKind::EXTERN => element::format_extern(node.as_extern(), f),
        TokenKind::EXPR_STMT => stmt::format_expr_stmt(node.as_expr_stmt(), f),
        TokenKind::FOR_EXPR => expr::format_for(node.as_for_expr(), f),
        TokenKind::FIELD_DECL => element::format_field(node.as_field_decl(), f),
        TokenKind::FUNCTION => element::format_function(node.as_function(), f),
        TokenKind::IF_EXPR => expr::format_if(node.as_if_expr(), f),
        TokenKind::IMPL => element::format_impl(node.as_impl(), f),
        TokenKind::IS_EXPR => expr::format_is(node.as_is_expr(), f),
        TokenKind::LAMBDA_EXPR => expr::format_lambda(node.as_lambda_expr(), f),
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
        TokenKind::MATCH_EXPR => expr::format_match(node.as_match_expr(), f),
        TokenKind::MATCH_ARM => expr::format_match_arm(node.as_match_arm(), f),
        TokenKind::METHOD_CALL_EXPR => expr::format_method_call_expr(node.as_method_call_expr(), f),
        TokenKind::MODULE => element::format_module(node.as_module(), f),
        TokenKind::MODIFIER => element::format_modifier(node.as_modifier(), f),
        TokenKind::MODIFIER_LIST => element::format_modifier_list(node.as_modifier_list(), f),
        TokenKind::PARAM => element::format_param(node.as_param(), f),
        TokenKind::LAMBDA_TYPE => ty::format_lambda_type(node.as_lambda_type(), f),
        TokenKind::PATH_DATA => pattern::format_path_data(node.as_path_data(), f),
        TokenKind::PATH_TYPE => ty::format_path_type(node.as_path_type(), f),
        TokenKind::PATH_EXPR => expr::format_path_expr(node.as_path_expr(), f),
        TokenKind::PATH_SEGMENT => expr::format_path_segment(node.as_path_segment(), f),
        TokenKind::PAREN_EXPR => expr::format_paren(node.as_paren_expr(), f),
        TokenKind::QUALIFIED_PATH_TYPE => {
            ty::format_qualified_path_type(node.as_qualified_path_type(), f)
        }
        TokenKind::REF_TYPE => ty::format_ref_type(node.as_ref_type(), f),
        TokenKind::RETURN_EXPR => expr::format_return(node.as_return_expr(), f),
        TokenKind::REST => pattern::format_rest_pattern(node.as_rest(), f),
        TokenKind::THIS_EXPR => expr::format_this(node.as_this_expr(), f),
        TokenKind::STRUCT => element::format_struct(node.as_struct(), f),
        TokenKind::TEMPLATE_EXPR => expr::format_template(node.as_template_expr(), f),
        TokenKind::TRAIT => element::format_trait(node.as_trait(), f),
        TokenKind::TUPLE_PATTERN => pattern::format_tuple_pattern(node.as_tuple_pattern(), f),
        TokenKind::TUPLE_TYPE => ty::format_tuple_type(node.as_tuple_type(), f),
        TokenKind::TYPE_ARGUMENT => element::format_type_argument(node.as_type_argument(), f),
        TokenKind::TYPE_ARGUMENT_LIST => {
            element::format_type_argument_list(node.as_type_argument_list(), f)
        }
        TokenKind::TYPE_BOUNDS => element::format_type_bounds(node.as_type_bounds(), f),
        TokenKind::TYPE_PARAM => element::format_type_param(node.as_type_param(), f),
        TokenKind::TYPE_PARAM_LIST => element::format_type_param_list(node.as_type_param_list(), f),
        TokenKind::TUPLE_EXPR => expr::format_tuple(node.as_tuple_expr(), f),
        TokenKind::UNDERSCORE_PATTERN => {
            pattern::format_underscore_pattern(node.as_underscore_pattern(), f)
        }
        TokenKind::UN_EXPR => expr::format_un(node.as_un_expr(), f),
        TokenKind::WHILE_EXPR => expr::format_while(node.as_while_expr(), f),
        TokenKind::LIT_BOOL_EXPR => lit::format_lit_bool(node.as_lit_bool_expr(), f),
        TokenKind::LIT_CHAR_EXPR => lit::format_lit_char(node.as_lit_char_expr(), f),
        TokenKind::LIT_FLOAT_EXPR => lit::format_lit_float(node.as_lit_float_expr(), f),
        TokenKind::LIT_INT_EXPR => lit::format_lit_int(node.as_lit_int_expr(), f),
        TokenKind::LIT_STR_EXPR => lit::format_lit_str(node.as_lit_str_expr(), f),
        TokenKind::GLOBAL => element::format_global(node.as_global(), f),
        TokenKind::USE => use_::format_use(node.as_use(), f),
        TokenKind::USE_AS => use_::format_use_as(node.as_use_as(), f),
        TokenKind::USE_GROUP => use_::format_use_group(node.as_use_group(), f),
        TokenKind::USE_NAME => use_::format_use_name(node.as_use_name(), f),
        TokenKind::USE_PATH => use_::format_use_path(node.as_use_path(), f),
        TokenKind::USE_ATOM => use_::format_use_atom(node.as_use_atom(), f),
        TokenKind::WHERE_CLAUSE => element::format_where_clause(node.as_where_clause(), f),
        TokenKind::WHERE_CLAUSE_ITEM => {
            element::format_where_clause_item(node.as_where_clause_item(), f)
        }
        _ => {
            panic!("unsupported node {}", node.syntax_kind());
        }
    }
}

pub(crate) const BLOCK_INDENT: u32 = 4;
