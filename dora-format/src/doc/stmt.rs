use dora_parser::TokenKind::*;
use dora_parser::ast::*;

use crate::doc::Formatter;
use crate::doc::utils::{print_node, print_token};
use crate::with_iter;

pub(crate) fn format_let(node: AstLet, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, LET_KW, &opt);
        f.text(" ");
        print_node::<AstPattern, _>(f, &mut iter);
        f.text(" ");
        print_token(f, &mut iter, EQ, &opt);
        f.text(" ");
        print_node::<AstExpr, _>(f, &mut iter);
        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_expr_stmt(node: AstExprStmt, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr, _>(f, &mut iter);
        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}
