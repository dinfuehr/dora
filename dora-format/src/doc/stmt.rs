use dora_parser::TokenKind::*;
use dora_parser::ast::*;

use crate::doc::Formatter;
use crate::doc::utils::{Options, print_rest, print_token, print_while};

pub(crate) fn format_let(node: AstLet, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, LET_KW, &opt);
    f.text(" ");
    print_while::<AstPattern, _>(f, &mut iter, &opt);
    f.text(" ");
    print_token(f, &mut iter, EQ, &opt);
    f.text(" ");
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_token(f, &mut iter, SEMICOLON, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_expr_stmt(node: AstExprStmt, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_token(f, &mut iter, SEMICOLON, &opt);
    print_rest(f, iter, &opt);
}
