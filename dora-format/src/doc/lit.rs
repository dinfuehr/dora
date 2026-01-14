use dora_parser::TokenKind::*;
use dora_parser::ast::*;

use crate::doc::Formatter;
use crate::doc::utils::print_token;
use crate::with_iter;

pub(crate) fn format_lit_bool(node: AstLitBoolExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        let literal_kind = node.token().syntax_kind();
        debug_assert!(matches!(literal_kind, TRUE | FALSE));
        print_token(f, &mut iter, literal_kind, &opt);
    });
}

pub(crate) fn format_lit_char(node: AstLitCharExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, CHAR_LITERAL, &opt);
    });
}

pub(crate) fn format_lit_float(node: AstLitFloatExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, FLOAT_LITERAL, &opt);
    });
}

pub(crate) fn format_lit_int(node: AstLitIntExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, INT_LITERAL, &opt);
    });
}

pub(crate) fn format_lit_str(node: AstLitStrExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        let literal_kind = node.token().syntax_kind();
        print_token(f, &mut iter, literal_kind, &opt);
    });
}
