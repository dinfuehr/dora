use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstAlt, AstCtorField, AstCtorFieldList, AstCtorPattern, AstIdentPattern, AstLitBool,
    AstLitChar, AstLitFloat, AstLitInt, AstLitPatternBool, AstLitPatternChar, AstLitPatternFloat,
    AstLitPatternInt, AstLitPatternStr, AstLitStr, AstPathData, AstPattern, AstRest,
    AstTuplePattern, AstUn, AstUnderscorePattern, AstUpcaseThis, SyntaxNode, SyntaxNodeBase,
};

use crate::doc::Formatter;
use crate::doc::utils::{if_node, if_token, print_node, print_token};
use crate::with_iter;

pub(crate) fn format_alt(node: AstAlt, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstPattern, _>(f, &mut iter);

        while if_token(f, &mut iter, OR) {
            f.text(" ");
            print_token(f, &mut iter, OR, &opt);
            f.text(" ");
            print_node::<AstPattern, _>(f, &mut iter);
        }
    });
}

pub(crate) fn format_ctor_pattern(node: AstCtorPattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstPathData, _>(f, &mut iter);

        if if_node::<AstCtorFieldList, _>(f, &mut iter) {
            print_node::<AstCtorFieldList, _>(f, &mut iter);
        }
    });
}

pub(crate) fn format_ctor_field_list(node: AstCtorFieldList, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_PAREN, &opt);

        while !if_token(f, &mut iter, R_PAREN) {
            print_node::<AstCtorField, _>(f, &mut iter);
            if if_token(f, &mut iter, COMMA) {
                print_token(f, &mut iter, COMMA, &opt);
                f.text(" ");
            }
        }

        print_token(f, &mut iter, R_PAREN, &opt);
    });
}

pub(crate) fn format_ctor_field(node: AstCtorField, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_token(f, &mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            f.text(" ");
            print_token(f, &mut iter, EQ, &opt);
            f.text(" ");
        }
        print_node::<AstPattern, _>(f, &mut iter);
    });
}

pub(crate) fn format_ident_pattern(node: AstIdentPattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_token(f, &mut iter, MUT_KW) {
            print_token(f, &mut iter, MUT_KW, &opt);
            f.text(" ");
        }
        print_token(f, &mut iter, IDENTIFIER, &opt);
    });
}

pub(crate) fn format_lit_pattern_bool(node: AstLitPatternBool, f: &mut Formatter) {
    format_literal_pattern::<AstLitBool>(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_char(node: AstLitPatternChar, f: &mut Formatter) {
    format_literal_pattern::<AstLitChar>(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_float(node: AstLitPatternFloat, f: &mut Formatter) {
    format_literal_pattern::<AstLitFloat>(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_int(node: AstLitPatternInt, f: &mut Formatter) {
    format_literal_pattern::<AstLitInt>(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_str(node: AstLitPatternStr, f: &mut Formatter) {
    format_literal_pattern::<AstLitStr>(node.unwrap(), f);
}

pub(crate) fn format_rest_pattern(node: AstRest, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, DOT_DOT, &opt);
    });
}

pub(crate) fn format_tuple_pattern(node: AstTuplePattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_PAREN, &opt);

        while !if_token(f, &mut iter, R_PAREN) {
            print_node::<AstPattern, _>(f, &mut iter);
            if if_token(f, &mut iter, COMMA) {
                print_token(f, &mut iter, COMMA, &opt);
                f.text(" ");
            }
        }

        print_token(f, &mut iter, R_PAREN, &opt);
    });
}

pub(crate) fn format_underscore_pattern(node: AstUnderscorePattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, UNDERSCORE, &opt);
    });
}

pub(crate) fn format_path_data(node: AstPathData, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstUpcaseThis, _>(f, &mut iter) {
            print_node::<AstUpcaseThis, _>(f, &mut iter);
        } else {
            print_token(f, &mut iter, IDENTIFIER, &opt);
        }

        while if_token(f, &mut iter, COLON_COLON) {
            print_token(f, &mut iter, COLON_COLON, &opt);

            if if_node::<AstUpcaseThis, _>(f, &mut iter) {
                print_node::<AstUpcaseThis, _>(f, &mut iter);
            } else {
                print_token(f, &mut iter, IDENTIFIER, &opt);
            }
        }
    });
}

fn format_literal_pattern<T: SyntaxNodeBase>(node: SyntaxNode, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstUn, _>(f, &mut iter) {
            print_node::<AstUn, _>(f, &mut iter);
        } else {
            print_node::<T, _>(f, &mut iter);
        }
    });
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use dora_parser::Parser;

    use crate::doc;
    use crate::render;

    fn format_to_string(input: &str) -> String {
        let content = Arc::new(input.to_string());
        let parser = Parser::from_shared_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty(), "unexpected parse errors: {:?}", errors);

        let root = file.root();
        let (arena, root_id) = doc::format(root);
        render::render_doc(&arena, root_id)
    }

    #[test]
    fn formats_ident_pattern() {
        let input = "fn  main (  ) {  let  mut  x  =  1 ; }";
        let expected = "fn main() {\n    let mut x = 1;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_tuple_pattern() {
        let input = "fn  main (  ) {  let  ( a , .. , b )  =  t ; }";
        let expected = "fn main() {\n    let (a, .., b) = t;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_ctor_pattern() {
        let input = "fn  main (  ) {  match  x  {  Foo( x , y )  =>  1 } }";
        let expected = "fn main() {\n    match x {\n        Foo(x, y) => 1\n    }\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_alt_pattern() {
        let input = "fn  main (  ) {  match  x  {  1 | 2  =>  3 } }";
        let expected = "fn main() {\n    match x {\n        1 | 2 => 3\n    }\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_literal_patterns() {
        let input = "fn  main (  ) {  match  x  {  true => 1 , 'a' => 2 , \"s\" => 3 , -1 => 4 , 1.5 => 5 , _ => 6 } }";
        let expected = "fn main() {\n    match x {\n        true => 1,\n        'a' => 2,\n        \"s\" => 3,\n        -1 => 4,\n        1.5 => 5,\n        _ => 6\n    }\n}\n";
        assert_eq!(format_to_string(input), expected);
    }
}
