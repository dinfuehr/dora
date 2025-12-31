use dora_parser::TokenKind::*;
use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, WHITESPACE};
use dora_parser::ast::{
    AstAlt, AstCtorField, AstCtorFieldList, AstCtorPattern, AstIdentPattern, AstLitPatternBool,
    AstLitPatternChar, AstLitPatternFloat, AstLitPatternInt, AstLitPatternStr, AstName,
    AstPathData, AstPattern, AstRest, AstTuplePattern, AstUnderscorePattern, SyntaxElement,
    SyntaxNode, SyntaxNodeBase,
};

use crate::doc::Formatter;
use crate::doc::utils::{if_token, print_node, print_token};
use crate::with_iter;

pub(crate) fn format_alt(node: AstAlt, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstPattern, _>(f, &mut iter);

        loop {
            match iter.peek() {
                Some(SyntaxElement::Token(token)) if token.syntax_kind() == OR => {
                    print_token(f, &mut iter, OR, &opt);
                    print_node::<AstPattern, _>(f, &mut iter);
                }
                Some(SyntaxElement::Token(token)) if token.syntax_kind().is_trivia() => {
                    iter.next();
                }
                _ => break,
            }
        }
    });
}

pub(crate) fn format_ctor_pattern(node: AstCtorPattern, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {}
                LINE_COMMENT => {
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
                    f.token(token);
                }
                COLON_COLON => {
                    f.token(token);
                    f.reset_spacing();
                }
                _ => {
                    f.token(token);
                }
            },
            SyntaxElement::Node(node) => {
                if let Some(path_data) = AstPathData::cast(node.clone()) {
                    format_path_data(path_data, f);
                } else if let Some(field_list) = AstCtorFieldList::cast(node.clone()) {
                    format_ctor_field_list(field_list, f);
                } else {
                    crate::doc::format_node(node, f);
                }
            }
        }
    }
}

pub(crate) fn format_ctor_field_list(node: AstCtorFieldList, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_PAREN, &opt);

        loop {
            print_node::<AstCtorField, _>(f, &mut iter);

            match iter.peek() {
                Some(SyntaxElement::Token(token)) if token.syntax_kind() == COMMA => {
                    print_token(f, &mut iter, COMMA, &opt);
                }
                Some(SyntaxElement::Token(token)) if token.syntax_kind() == R_PAREN => {
                    print_token(f, &mut iter, R_PAREN, &opt);
                    break;
                }
                Some(SyntaxElement::Token(token)) if token.syntax_kind().is_trivia() => {
                    iter.next();
                }
                _ => break,
            }
        }
    });
}

pub(crate) fn format_ctor_field(node: AstCtorField, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {}
                LINE_COMMENT => {
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
                    f.token(token);
                }
                _ => {
                    f.token(token);
                }
            },
            SyntaxElement::Node(node) => {
                if let Some(name) = AstName::cast(node.clone()) {
                    format_name(name, f);
                } else {
                    crate::doc::format_node(node, f);
                }
            }
        }
    }
}

pub(crate) fn format_ident_pattern(node: AstIdentPattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_token(f, &mut iter, MUT_KW) {
            print_token(f, &mut iter, MUT_KW, &opt);
        }
        print_node::<AstName, _>(f, &mut iter);
    });
}

pub(crate) fn format_lit_pattern_bool(node: AstLitPatternBool, f: &mut Formatter) {
    format_literal_pattern(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_char(node: AstLitPatternChar, f: &mut Formatter) {
    format_literal_pattern(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_float(node: AstLitPatternFloat, f: &mut Formatter) {
    format_literal_pattern(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_int(node: AstLitPatternInt, f: &mut Formatter) {
    format_literal_pattern(node.unwrap(), f);
}

pub(crate) fn format_lit_pattern_str(node: AstLitPatternStr, f: &mut Formatter) {
    format_literal_pattern(node.unwrap(), f);
}

pub(crate) fn format_rest_pattern(node: AstRest, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, DOT_DOT, &opt);
    });
}

pub(crate) fn format_tuple_pattern(node: AstTuplePattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_PAREN, &opt);

        loop {
            print_node::<AstPattern, _>(f, &mut iter);

            match iter.peek() {
                Some(SyntaxElement::Token(token)) if token.syntax_kind() == COMMA => {
                    print_token(f, &mut iter, COMMA, &opt);
                }
                Some(SyntaxElement::Token(token)) if token.syntax_kind() == R_PAREN => {
                    print_token(f, &mut iter, R_PAREN, &opt);
                    break;
                }
                Some(SyntaxElement::Token(token)) if token.syntax_kind().is_trivia() => {
                    iter.next();
                }
                _ => break,
            }
        }
    });
}

pub(crate) fn format_underscore_pattern(node: AstUnderscorePattern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, UNDERSCORE, &opt);
    });
}

pub(crate) fn format_path_data(node: AstPathData, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {}
                LINE_COMMENT => {
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
                    f.token(token);
                }
                COLON_COLON => {
                    f.token(token);
                    f.reset_spacing();
                }
                _ => {
                    f.token(token);
                }
            },
            SyntaxElement::Node(node) => {
                crate::doc::format_node(node, f);
            }
        }
    }
}

fn format_literal_pattern(node: SyntaxNode, f: &mut Formatter) {
    with_iter!(node, f, |iter, _opt| {
        while let Some(item) = iter.next() {
            match item {
                SyntaxElement::Token(token) => match token.syntax_kind() {
                    WHITESPACE => {}
                    LINE_COMMENT => {
                        f.token(token);
                        f.hard_line();
                    }
                    MULTILINE_COMMENT => {
                        f.token(token);
                    }
                    SUB => {
                        f.token(token);
                        f.reset_spacing();
                    }
                    _ => {
                        f.token(token);
                    }
                },
                SyntaxElement::Node(node) => {
                    crate::doc::format_node(node, f);
                }
            }
        }
    });
}

fn format_name(node: AstName, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
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
