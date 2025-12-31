use dora_parser::TokenKind::*;
use dora_parser::ast::*;

use crate::doc::Formatter;
use crate::doc::utils::{Options, print_rest, print_token};

pub(crate) fn format_lit_bool(node: AstLitBool, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    let literal_kind = node.token().syntax_kind();
    debug_assert!(matches!(literal_kind, TRUE | FALSE));
    print_token(f, &mut iter, literal_kind, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_lit_char(node: AstLitChar, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, CHAR_LITERAL, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_lit_float(node: AstLitFloat, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, FLOAT_LITERAL, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_lit_int(node: AstLitInt, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, INT_LITERAL, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_lit_str(node: AstLitStr, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, STRING_LITERAL, &opt);
    print_rest(f, iter, &opt);
}
