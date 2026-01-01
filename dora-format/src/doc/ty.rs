use std::iter::Peekable;

use dora_parser::TokenKind;
use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstLambdaType, AstPathData, AstPathType, AstQualifiedPathType, AstRefType, AstTupleType,
    AstType, AstTypeArgument, SyntaxElement, SyntaxNodeBase,
};

use crate::doc::Formatter;
use crate::doc::utils::{Options, if_token, print_node, print_token, print_token_opt};
use crate::with_iter;

pub(crate) fn format_path_type(node: AstPathType, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstPathData, _>(f, &mut iter);

        if if_token(f, &mut iter, L_BRACKET) {
            print_token(f, &mut iter, L_BRACKET, &opt);
            let mut first = true;

            while !if_token(f, &mut iter, R_BRACKET) {
                if !first {
                    f.text(" ");
                }
                print_node::<AstTypeArgument, _>(f, &mut iter);
                print_token_opt(f, &mut iter, COMMA, &opt);
                first = false;
            }

            print_token(f, &mut iter, R_BRACKET, &opt);
        }
    });
}

pub(crate) fn format_qualified_path_type(node: AstQualifiedPathType, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_BRACKET, &opt);
        print_node::<AstType, _>(f, &mut iter);
        f.text(" ");
        print_token(f, &mut iter, AS_KW, &opt);
        f.text(" ");
        print_node::<AstType, _>(f, &mut iter);
        print_token(f, &mut iter, R_BRACKET, &opt);
        print_token(f, &mut iter, COLON_COLON, &opt);
        print_token(f, &mut iter, IDENTIFIER, &opt);
    });
}

pub(crate) fn format_lambda_type(node: AstLambdaType, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        format_type_list(f, &mut iter, &opt, L_PAREN, R_PAREN);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType, _>(f, &mut iter);
    });
}

pub(crate) fn format_tuple_type(node: AstTupleType, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        format_type_list(f, &mut iter, &opt, L_PAREN, R_PAREN);
    });
}

pub(crate) fn format_ref_type(node: AstRefType, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, REF_KW, &opt);
        f.text(" ");
        print_node::<AstType, _>(f, &mut iter);
    });
}

fn format_type_list<I>(
    f: &mut Formatter,
    mut iter: &mut Peekable<I>,
    opt: &Options,
    open: TokenKind,
    closing: TokenKind,
) where
    I: Iterator<Item = SyntaxElement>,
{
    print_token(f, &mut iter, open, &opt);

    let mut first = true;

    while !if_token(f, &mut iter, closing) {
        if !first {
            f.text(" ");
        }
        print_node::<AstType, _>(f, &mut iter);
        print_token_opt(f, &mut iter, COMMA, &opt);
        first = false;
    }

    print_token(f, &mut iter, closing, &opt);
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_source;

    #[test]
    fn formats_path_type() {
        let input = "fn  main ( x : Foo [ Bar , Baz ] ) { }";
        let expected = "fn main(x: Foo[Bar, Baz]) {}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_path_type_with_upcase_self() {
        let input = "fn  main ( x : Self :: Item ) { }";
        let expected = "fn main(x: Self::Item) {}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_qualified_path_type() {
        let input = "fn  main ( x : [ Int as Trait ] :: Item ) { }";
        let expected = "fn main(x: [Int as Trait]::Item) {}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_lambda_type() {
        let input = "fn  main ( x : ( Int , String ) : Bool ) { }";
        let expected = "fn main(x: (Int, String): Bool) {}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_tuple_type() {
        let input = "fn  main ( x : ( Int , String ) ) { }";
        let expected = "fn main(x: (Int, String)) {}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_ref_type() {
        let input = "fn  main ( x : ref  Int ) { }";
        let expected = "fn main(x: ref Int) {}\n";
        assert_source(input, expected);
    }
}
