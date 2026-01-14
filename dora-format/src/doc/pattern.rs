use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstAlt, AstCtorField, AstCtorFieldList, AstCtorPattern, AstIdentPattern, AstLitBoolExpr,
    AstLitCharExpr, AstLitFloatExpr, AstLitIntExpr, AstLitPatternBool, AstLitPatternChar,
    AstLitPatternFloat, AstLitPatternInt, AstLitPatternStr, AstLitStrExpr, AstPathData, AstPattern,
    AstRest, AstTuplePattern, AstUnExpr, AstUnderscorePattern, SyntaxNode, SyntaxNodeBase,
};

use crate::doc::Formatter;
use crate::doc::utils::{is_node, is_token, print_comma_list, print_node, print_token};
use crate::with_iter;

pub(crate) fn format_alt(node: AstAlt, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        f.group(|f| {
            print_node::<AstPattern>(f, &mut iter, &opt);

            while is_token(&mut iter, OR) {
                f.soft_line();
                print_token(f, &mut iter, OR, &opt);
                f.text(" ");
                print_node::<AstPattern>(f, &mut iter, &opt);
            }
        });
    });
}

pub(crate) fn format_ctor_pattern(node: AstCtorPattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstPathData>(f, &mut iter, &opt);

        if is_node::<AstCtorFieldList>(&mut iter) {
            print_node::<AstCtorFieldList>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_ctor_field_list(node: AstCtorFieldList, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_comma_list::<AstCtorField>(f, &mut iter, L_PAREN, R_PAREN, &opt);
    });
}

pub(crate) fn format_ctor_field(node: AstCtorField, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_token(&mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            f.text(" ");
            print_token(f, &mut iter, EQ, &opt);
            f.text(" ");
        }
        print_node::<AstPattern>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_ident_pattern(node: AstIdentPattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_token(&mut iter, MUT_KW) {
            print_token(f, &mut iter, MUT_KW, &opt);
            f.text(" ");
        }
        print_token(f, &mut iter, IDENTIFIER, &opt);
    });
}

pub(crate) fn format_lit_pattern_bool(node: AstLitPatternBool, f: &mut Formatter) {
    format_literal_pattern::<AstLitBoolExpr>(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_char(node: AstLitPatternChar, f: &mut Formatter) {
    format_literal_pattern::<AstLitCharExpr>(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_float(node: AstLitPatternFloat, f: &mut Formatter) {
    format_literal_pattern::<AstLitFloatExpr>(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_int(node: AstLitPatternInt, f: &mut Formatter) {
    format_literal_pattern::<AstLitIntExpr>(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_str(node: AstLitPatternStr, f: &mut Formatter) {
    format_literal_pattern::<AstLitStrExpr>(node.unwrap(), f);
}

pub(crate) fn format_rest_pattern(node: AstRest, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, DOT_DOT, &opt);
    });
}

pub(crate) fn format_tuple_pattern(node: AstTuplePattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_comma_list::<AstPattern>(f, &mut iter, L_PAREN, R_PAREN, &opt);
    });
}

pub(crate) fn format_underscore_pattern(node: AstUnderscorePattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, UNDERSCORE, &opt);
    });
}

pub(crate) fn format_path_data(node: AstPathData, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_token(&mut iter, UPCASE_SELF_KW) {
            print_token(f, &mut iter, UPCASE_SELF_KW, &opt);
        } else {
            print_token(f, &mut iter, IDENTIFIER, &opt);
        }

        while is_token(&mut iter, COLON_COLON) {
            print_token(f, &mut iter, COLON_COLON, &opt);

            if is_token(&mut iter, UPCASE_SELF_KW) {
                print_token(f, &mut iter, UPCASE_SELF_KW, &opt);
            } else {
                print_token(f, &mut iter, IDENTIFIER, &opt);
            }
        }
    });
}

fn format_literal_pattern<T: SyntaxNodeBase>(node: SyntaxNode, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstUnExpr>(&mut iter) {
            print_node::<AstUnExpr>(f, &mut iter, &opt);
        } else {
            print_node::<T>(f, &mut iter, &opt);
        }
    });
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_source;

    #[test]
    fn formats_ident_pattern() {
        let input = "fn  main (  ) {  let  mut  x  =  1 ; }";
        let expected = "fn main() {\n    let mut x = 1;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_tuple_pattern() {
        let input = "fn  main (  ) {  let  ( a , .. , b )  =  t ; }";
        let expected = "fn main() {\n    let (a, .., b) = t;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_ctor_pattern() {
        let input = "fn  main (  ) {  match  x  {  Foo( x , y )  =>  1 } }";
        let expected = "fn main() {\n    match x {\n        Foo(x, y) => 1,\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_alt_pattern() {
        let input = "fn  main (  ) {  match  x  {  1 | 2  =>  3 } }";
        let expected = "fn main() {\n    match x {\n        1 | 2 => 3,\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_literal_patterns() {
        let input = "fn  main (  ) {  match  x  {  true => 1 , 'a' => 2 , \"s\" => 3 , -1 => 4 , 1.5 => 5 , _ => 6 } }";
        let expected = "fn main() {\n    match x {\n        true => 1,\n        'a' => 2,\n        \"s\" => 3,\n        -1 => 4,\n        1.5 => 5,\n        _ => 6,\n    }\n}\n";
        assert_source(input, expected);
    }
}
