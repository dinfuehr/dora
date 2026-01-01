use dora_parser::TokenKind::*;
use dora_parser::ast::*;

use crate::doc::Formatter;
use crate::doc::utils::{is_token, print_node, print_token};
use crate::with_iter;

pub(crate) fn format_let(node: AstLet, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, LET_KW, &opt);
        f.text(" ");
        print_node::<AstPattern>(f, &mut iter);
        if is_token(f, &mut iter, COLON) {
            print_token(f, &mut iter, COLON, &opt);
            f.text(" ");
            print_node::<AstType>(f, &mut iter);
        }
        if is_token(f, &mut iter, EQ) {
            f.text(" ");
            print_token(f, &mut iter, EQ, &opt);
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter);
        }
        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_expr_stmt(node: AstExprStmt, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter);
        if is_token(f, &mut iter, SEMICOLON) {
            print_token(f, &mut iter, SEMICOLON, &opt);
        }
    });
}
