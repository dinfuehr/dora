use std::iter::Peekable;

use dora_parser::TokenKind;
use dora_parser::TokenKind::*;
use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, WHITESPACE};
use dora_parser::ast::{
    AstLambdaType, AstName, AstPathData, AstPathType, AstQualifiedPathType, AstRefType,
    AstTupleType, AstType, AstTypeArgument, SyntaxElement, SyntaxNodeBase,
};

use crate::doc::Formatter;
use crate::doc::utils::{Options, print_rest, print_token, print_while};

pub(crate) fn format_path_type(node: AstPathType, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();

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
                L_BRACKET => {
                    f.reset_spacing();
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
                } else {
                    crate::doc::format_node(node, f);
                }
            }
        }
    }

    print_rest(f, iter, &opt);
}

pub(crate) fn format_qualified_path_type(node: AstQualifiedPathType, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, L_BRACKET, &opt);
    print_while::<AstType, _>(f, &mut iter, &opt);
    print_token(f, &mut iter, AS_KW, &opt);
    print_while::<AstType, _>(f, &mut iter, &opt);
    print_token(f, &mut iter, R_BRACKET, &opt);
    print_token(f, &mut iter, COLON_COLON, &opt);
    f.reset_spacing();
    print_while::<AstName, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_lambda_type(node: AstLambdaType, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, L_PAREN, &opt);
    format_type_list(f, &mut iter, &opt, R_PAREN);
    print_token(f, &mut iter, COLON, &opt);
    print_while::<AstType, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_tuple_type(node: AstTupleType, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, L_PAREN, &opt);
    format_type_list(f, &mut iter, &opt, R_PAREN);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_ref_type(node: AstRefType, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, REF_KW, &opt);
    print_while::<AstType, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

fn format_path_data(node: AstPathData, f: &mut Formatter) {
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

fn format_type_list<I>(f: &mut Formatter, iter: &mut Peekable<I>, opt: &Options, closing: TokenKind)
where
    I: Iterator<Item = SyntaxElement>,
{
    loop {
        print_while::<AstType, _>(f, iter, opt);

        match iter.peek() {
            Some(SyntaxElement::Token(token)) if token.syntax_kind() == COMMA => {
                print_token(f, iter, COMMA, opt);
            }
            Some(SyntaxElement::Token(token)) if token.syntax_kind() == closing => {
                print_token(f, iter, closing, opt);
                break;
            }
            Some(SyntaxElement::Token(token)) if token.syntax_kind().is_trivia() => {
                iter.next();
            }
            Some(SyntaxElement::Node(node)) if AstTypeArgument::cast(node.clone()).is_some() => {
                let node = iter.next().unwrap().to_node().unwrap();
                crate::doc::format_node(node, f);
            }
            _ => {
                break;
            }
        }
    }
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
    fn formats_path_type() {
        let input = "fn  main ( x : Foo [ Bar , Baz ] ) { }";
        let expected = "fn main(x: Foo[Bar, Baz]) {}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_qualified_path_type() {
        let input = "fn  main ( x : [ Int as Trait ] :: Item ) { }";
        let expected = "fn main(x: [Int as Trait]::Item) {}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_lambda_type() {
        let input = "fn  main ( x : ( Int , String ) : Bool ) { }";
        let expected = "fn main(x: (Int, String): Bool) {}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_tuple_type() {
        let input = "fn  main ( x : ( Int , String ) ) { }";
        let expected = "fn main(x: (Int, String)) {}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_ref_type() {
        let input = "fn  main ( x : ref  Int ) { }";
        let expected = "fn main(x: ref Int) {}\n";
        assert_eq!(format_to_string(input), expected);
    }
}
