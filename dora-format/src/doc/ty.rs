use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstLambdaParamList, AstLambdaType, AstPathData, AstPathType, AstQualifiedPathType, AstRefType,
    AstTupleType, AstType, AstTypeArgumentList, SyntaxNodeBase,
};

use crate::doc::Formatter;
use crate::doc::utils::{Options, is_node, print_comma_list_grouped, print_node, print_token};
use crate::with_iter;

pub(crate) fn format_path_type(node: AstPathType, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstPathData>(f, &mut iter, &opt);

        if is_node::<AstTypeArgumentList>(&iter) {
            print_node::<AstTypeArgumentList>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_qualified_path_type(node: AstQualifiedPathType, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_BRACKET, &opt);
        print_node::<AstType>(f, &mut iter, &opt);
        f.text(" ");
        print_token(f, &mut iter, AS_KW, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);
        print_token(f, &mut iter, R_BRACKET, &opt);
        print_token(f, &mut iter, COLON_COLON, &opt);
        print_token(f, &mut iter, IDENTIFIER, &opt);
    });
}

pub(crate) fn format_lambda_type(node: AstLambdaType, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstLambdaParamList>(f, &mut iter, &opt);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_lambda_param_list(node: AstLambdaParamList, f: &mut Formatter) {
    let opt = Options::new();
    print_comma_list_grouped(f, &node, &opt);
}

pub(crate) fn format_tuple_type(node: AstTupleType, f: &mut Formatter) {
    let opt = Options::new();
    print_comma_list_grouped(f, &node, &opt);
}

pub(crate) fn format_ref_type(node: AstRefType, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, REF_KW, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);
    });
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
