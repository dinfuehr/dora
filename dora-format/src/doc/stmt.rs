use dora_parser::TokenKind::*;
use dora_parser::ast::*;

use crate::doc::Formatter;
use crate::doc::utils::{Options, is_token, print_node, print_token, print_trivia};
use crate::with_iter;

pub(crate) fn format_let(node: AstLet, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_trivia(f, &mut iter, &Options::keep_empty_lines());
        print_token(f, &mut iter, LET_KW, &opt);
        f.text(" ");
        print_node::<AstPattern>(f, &mut iter, &opt);
        if is_token(f, &mut iter, COLON, &opt) {
            print_token(f, &mut iter, COLON, &opt);
            f.text(" ");
            print_node::<AstType>(f, &mut iter, &opt);
        }
        if is_token(f, &mut iter, EQ, &opt) {
            f.text(" ");
            print_token(f, &mut iter, EQ, &opt);
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter, &opt);
        }
        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_expr_stmt(node: AstExprStmt, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_trivia(f, &mut iter, &Options::keep_empty_lines());
        print_node::<AstExpr>(f, &mut iter, &opt);
        if is_token(f, &mut iter, SEMICOLON, &opt) {
            print_token(f, &mut iter, SEMICOLON, &opt);
        }
    });
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_source;

    #[test]
    fn keep_empty_line_for_expr_stmt() {
        let input = "fn main() { f(); g();\n\nh();}";
        let expected = "fn main() {\n    f();\n    g();\n\n    h();\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn keep_empty_line_for_let() {
        let input = "fn main() { f(); g();\n\nlet x=1;}";
        let expected = "fn main() {\n    f();\n    g();\n\n    let x = 1;\n}\n";
        assert_source(input, expected);
    }
}
