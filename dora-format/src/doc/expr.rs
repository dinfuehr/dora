use dora_parser::TokenKind::*;
use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, WHITESPACE};
use dora_parser::ast::{
    AstArgumentList, AstBin, AstBlock, AstBreak, AstCall, AstContinue, AstConv, AstDotExpr,
    AstExpr, AstFor, AstIs, AstLambda, AstMatch, AstMatchArm, AstNameExpr, AstParam, AstParen,
    AstPath, AstPattern, AstReturn, AstThis, AstTuple, AstType, AstTypedExpr, AstUn, AstWhile,
    SyntaxElement, SyntaxNodeBase,
};

use crate::doc::utils::{Options, print_next_token, print_rest, print_token, print_while};
use crate::doc::{BLOCK_INDENT, Formatter};

pub(crate) fn format_bin(node: AstBin, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_next_token(f, &mut iter);
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_break(node: AstBreak, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, BREAK_KW, &opt);
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_call(node: AstCall, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_while::<AstArgumentList, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_continue(node: AstContinue, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, CONTINUE_KW, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_conv(node: AstConv, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_token(f, &mut iter, AS_KW, &opt);
    print_while::<AstType, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_dot_expr(node: AstDotExpr, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_token(f, &mut iter, DOT, &opt);
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_for(node: AstFor, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, FOR_KW, &opt);
    f.text(" ");
    print_while::<AstPattern, _>(f, &mut iter, &opt);
    f.text(" ");
    print_token(f, &mut iter, IN_KW, &opt);
    f.text(" ");
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_while::<AstBlock, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_is(node: AstIs, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_token(f, &mut iter, IS_KW, &opt);
    print_while::<AstPattern, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_lambda(node: AstLambda, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();

    if matches!(
        iter.peek(),
        Some(SyntaxElement::Token(token)) if token.syntax_kind() == OR_OR
    ) {
        print_token(f, &mut iter, OR_OR, &opt);
        f.reset_spacing();
    } else {
        print_token(f, &mut iter, OR, &opt);
        f.reset_spacing();

        loop {
            print_while::<AstParam, _>(f, &mut iter, &opt);

            match iter.peek() {
                Some(SyntaxElement::Token(token)) if token.syntax_kind() == COMMA => {
                    print_token(f, &mut iter, COMMA, &opt);
                }
                Some(SyntaxElement::Token(token)) if token.syntax_kind() == OR => {
                    f.reset_spacing();
                    print_token(f, &mut iter, OR, &opt);
                    break;
                }
                Some(SyntaxElement::Token(token)) if token.syntax_kind().is_trivia() => {
                    iter.next();
                }
                _ => break,
            }
        }
    }

    if matches!(
        iter.peek(),
        Some(SyntaxElement::Token(token)) if token.syntax_kind() == COLON
    ) {
        print_token(f, &mut iter, COLON, &opt);
        print_while::<AstType, _>(f, &mut iter, &opt);
    }

    print_while::<AstBlock, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_match(node: AstMatch, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, MATCH_KW, &opt);
    f.text(" ");
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    f.text(" ");
    print_token(f, &mut iter, L_BRACE, &opt);

    if node.arms().next().is_none() {
        print_token(f, &mut iter, R_BRACE, &opt);
        print_rest(f, iter, &opt);
        return;
    }

    f.hard_line();
    f.nest(BLOCK_INDENT, |f| {
        format_match_arms(f, &mut iter, &opt);
    });
    print_token(f, &mut iter, R_BRACE, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_match_arm(node: AstMatchArm, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstPattern, _>(f, &mut iter, &opt);
    if matches!(
        iter.peek(),
        Some(SyntaxElement::Token(token)) if token.syntax_kind() == IF_KW
    ) {
        print_token(f, &mut iter, IF_KW, &opt);
        print_while::<AstExpr, _>(f, &mut iter, &opt);
    }
    print_token(f, &mut iter, DOUBLE_ARROW, &opt);
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_un(node: AstUn, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_next_token(f, &mut iter);
    f.reset_spacing();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_name_expr(node: AstNameExpr, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, IDENTIFIER, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_path(node: AstPath, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_token(f, &mut iter, COLON_COLON, &opt);
    f.reset_spacing();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_paren(node: AstParen, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, L_PAREN, &opt);
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_token(f, &mut iter, R_PAREN, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_return(node: AstReturn, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, RETURN_KW, &opt);
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_this(node: AstThis, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, SELF_KW, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_tuple(node: AstTuple, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, L_PAREN, &opt);

    loop {
        print_while::<AstExpr, _>(f, &mut iter, &opt);

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
            Some(SyntaxElement::Node(node)) => {
                let node = node.clone();
                if AstExpr::cast(node).is_some() {
                    let node = iter.next().unwrap().to_node().unwrap();
                    crate::doc::format_node(node, f);
                } else {
                    break;
                }
            }
            _ => {
                break;
            }
        }
    }

    print_rest(f, iter, &opt);
}

pub(crate) fn format_typed_expr(node: AstTypedExpr, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    f.reset_spacing();
    print_token(f, &mut iter, L_BRACKET, &opt);

    loop {
        print_while::<AstType, _>(f, &mut iter, &opt);

        match iter.peek() {
            Some(SyntaxElement::Token(token)) if token.syntax_kind() == COMMA => {
                print_token(f, &mut iter, COMMA, &opt);
            }
            Some(SyntaxElement::Token(token)) if token.syntax_kind() == R_BRACKET => {
                print_token(f, &mut iter, R_BRACKET, &opt);
                break;
            }
            Some(SyntaxElement::Token(token)) if token.syntax_kind().is_trivia() => {
                iter.next();
            }
            Some(SyntaxElement::Node(node)) => {
                let node = node.clone();
                if AstType::cast(node).is_some() {
                    let node = iter.next().unwrap().to_node().unwrap();
                    crate::doc::format_node(node, f);
                } else {
                    break;
                }
            }
            _ => {
                break;
            }
        }
    }

    print_rest(f, iter, &opt);
}

pub(crate) fn format_while(node: AstWhile, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, WHILE_KW, &opt);
    f.text(" ");
    print_while::<AstExpr, _>(f, &mut iter, &opt);
    print_while::<AstBlock, _>(f, &mut iter, &opt);
    print_rest(f, iter, &opt);
}

fn format_match_arms<I>(f: &mut Formatter, iter: &mut std::iter::Peekable<I>, opt: &Options)
where
    I: Iterator<Item = SyntaxElement>,
{
    loop {
        match iter.peek() {
            Some(SyntaxElement::Node(node)) if AstMatchArm::cast(node.clone()).is_some() => {
                let node = iter.next().unwrap().to_node().unwrap();
                crate::doc::format_node(node, f);
                if matches!(
                    iter.peek(),
                    Some(SyntaxElement::Token(token)) if token.syntax_kind() == R_BRACE
                ) {
                    f.hard_line();
                }
            }
            Some(SyntaxElement::Token(token)) => match token.syntax_kind() {
                WHITESPACE => {
                    iter.next();
                }
                LINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    f.token(token);
                }
                COMMA => {
                    print_token(f, iter, COMMA, opt);
                    f.hard_line();
                }
                R_BRACE => {
                    break;
                }
                _ => {
                    iter.next();
                }
            },
            Some(SyntaxElement::Node(_)) => {
                let node = iter.next().unwrap().to_node().unwrap();
                crate::doc::format_node(node, f);
            }
            None => break,
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
    fn formats_paren_expr() {
        let input = "fn  main (  ) {  let  x  =  ( 1+2 ) ; }";
        let expected = "fn main() {\n    let x = (1 + 2);\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_nested_paren_expr() {
        let input = "fn  main (  ) {  let  x  =  ( ( 1+2 ) * 3 ) ; }";
        let expected = "fn main() {\n    let x = ((1 + 2) * 3);\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_binary_exprs() {
        let input = "fn  main (  ) {  let  x  =  1+2 ; let  y  =  3*4 ; }";
        let expected = "fn main() {\n    let x = 1 + 2;\n    let y = 3 * 4;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_unary_exprs() {
        let input = "fn  main (  ) {  let  y  =  -1 ; let  z  =  ! true ; }";
        let expected = "fn main() {\n    let y = -1;\n    let z = !true;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_name_binary_expr() {
        let input = "fn  main (  ) {  let  x  =  a+1 ; }";
        let expected = "fn main() {\n    let x = a + 1;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_dot_expr() {
        let input = "fn  main (  ) {  let  x  =  a . b ; }";
        let expected = "fn main() {\n    let x = a.b;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_call_expr() {
        let input = "fn  main (  ) {  let  x  =  f ( 1 , 2 ) ; }";
        let expected = "fn main() {\n    let x = f(1, 2);\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_break_expr() {
        let input = "fn  main (  ) {  while  true  {  break ; } }";
        let expected = "fn main() {\n    while true {\n        break;\n    }\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_continue_expr() {
        let input = "fn  main (  ) {  while  true  {  continue ; } }";
        let expected = "fn main() {\n    while true {\n        continue;\n    }\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_while_expr() {
        let input = "fn  main (  ) {  while  true  {  a = 1 ; } }";
        let expected = "fn main() {\n    while true {\n        a = 1;\n    }\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_for_expr() {
        let input = "fn  main (  ) {  for  i  in  xs  {  a = 1 ; } }";
        let expected = "fn main() {\n    for i in xs {\n        a = 1;\n    }\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_path_expr() {
        let input = "fn  main (  ) {  let  x  =  a :: b ; }";
        let expected = "fn main() {\n    let x = a::b;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_typed_expr() {
        let input = "fn  main (  ) {  let  x  =  f [ Int , String ] ; }";
        let expected = "fn main() {\n    let x = f[Int, String];\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_return_expr() {
        let input = "fn  main (  ) {  return  1 ; }";
        let expected = "fn main() {\n    return 1;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_match_expr() {
        let input = "fn  main (  ) {  match  x  {  1  =>  2 , _  =>  3 } }";
        let expected = "fn main() {\n    match x {\n        1 => 2,\n        _ => 3\n    }\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_lambda_expr() {
        let input = "fn  main (  ) {  let  f  =  | x : Int , y : Int | { x + y } ; }";
        let expected = "fn main() {\n    let f = |x: Int, y: Int| {\n        x + y\n    };\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_tuple_expr() {
        let input = "fn  main (  ) {  let  x  =  ( 1 , 2 , 3 ) ; }";
        let expected = "fn main() {\n    let x = (1, 2, 3);\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_this_expr() {
        let input = "fn  main (  ) {  let  x  =  self ; }";
        let expected = "fn main() {\n    let x = self;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_char_lit_expr() {
        let input = "fn  main (  ) {  let  c  =  'a' ; }";
        let expected = "fn main() {\n    let c = 'a';\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_is_expr() {
        let input = "fn  main (  ) {  let  x  =  a  is  _ ; }";
        let expected = "fn main() {\n    let x = a is _;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_conv_expr() {
        let input = "fn  main (  ) {  let  x  =  a  as  Int ; }";
        let expected = "fn main() {\n    let x = a as Int;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }
}
