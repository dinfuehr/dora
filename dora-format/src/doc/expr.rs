use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstArgument, AstArgumentList, AstBin, AstBlock, AstBreak, AstCall, AstContinue, AstConv,
    AstDotExpr, AstExpr, AstFor, AstIf, AstIs, AstLambda, AstMatch, AstMatchArm, AstMethodCallExpr,
    AstNameExpr, AstParam, AstParen, AstPath, AstPattern, AstReturn, AstStmt, AstTemplate, AstThis,
    AstTuple, AstType, AstTypeArgumentList, AstTypedExpr, AstUn, AstWhile, SyntaxElement,
    SyntaxNodeBase,
};

use crate::doc::utils::{
    Options, has_between, is_node, is_token, print_comma_list, print_next_token, print_node,
    print_rest, print_token, print_token_opt, print_while,
};
use crate::doc::{BLOCK_INDENT, Formatter};
use crate::with_iter;

pub(crate) fn format_block(node: AstBlock, f: &mut Formatter) {
    let mut iter = node.children_with_tokens().peekable();
    if !has_between(node.syntax_node(), L_BRACE, R_BRACE) {
        let opt = Options::new();
        print_token(f, &mut iter, L_BRACE, &opt);
        print_token(f, &mut iter, R_BRACE, &opt);
        print_rest(f, iter);
    } else {
        let opt = Options::build().emit_line_after().new();
        print_token(f, &mut iter, L_BRACE, &opt);
        f.hard_line();
        f.nest(BLOCK_INDENT, |f| {
            print_while::<AstStmt>(f, &mut iter, &opt);
        });
        print_token(f, &mut iter, R_BRACE, &opt);
        print_rest(f, iter);
    }
}

pub(crate) fn format_bin(node: AstBin, f: &mut Formatter) {
    with_iter!(node, f, |iter, _opt| {
        print_node::<AstExpr>(f, &mut iter);
        f.text(" ");
        print_next_token(f, &mut iter);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter);
    });
}

pub(crate) fn format_break(node: AstBreak, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, BREAK_KW, &opt);
    });
}

pub(crate) fn format_call(node: AstCall, f: &mut Formatter) {
    with_iter!(node, f, |iter, _opt| {
        print_node::<AstExpr>(f, &mut iter);
        print_node::<AstArgumentList>(f, &mut iter);
    });
}

pub(crate) fn format_argument_list(node: AstArgumentList, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_comma_list::<AstArgument>(f, &mut iter, L_PAREN, R_PAREN, &opt);
    });
}

pub(crate) fn format_argument(node: AstArgument, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_token(f, &mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            if is_token(f, &mut iter, EQ) {
                f.text(" ");
                print_token(f, &mut iter, EQ, &opt);
                f.text(" ");
            }
        }
        print_node::<AstExpr>(f, &mut iter);
    });
}

pub(crate) fn format_continue(node: AstContinue, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, CONTINUE_KW, &opt);
    });
}

pub(crate) fn format_conv(node: AstConv, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter);
        f.text(" ");
        print_token(f, &mut iter, AS_KW, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter);
    });
}

pub(crate) fn format_dot_expr(node: AstDotExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter);
        print_token(f, &mut iter, DOT, &opt);
        print_node::<AstExpr>(f, &mut iter);
    });
}

pub(crate) fn format_method_call_expr(node: AstMethodCallExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter);
        print_token(f, &mut iter, DOT, &opt);
        print_token(f, &mut iter, IDENTIFIER, &opt);
        if is_node::<AstTypeArgumentList>(f, &mut iter) {
            print_node::<AstTypeArgumentList>(f, &mut iter);
        }
        if is_node::<AstArgumentList>(f, &mut iter) {
            print_node::<AstArgumentList>(f, &mut iter);
        }
    });
}

pub(crate) fn format_for(node: AstFor, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, FOR_KW, &opt);
        f.text(" ");
        print_node::<AstPattern>(f, &mut iter);
        f.text(" ");
        print_token(f, &mut iter, IN_KW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter);
        f.text(" ");
        print_node::<AstBlock>(f, &mut iter);
    });
}

pub(crate) fn format_if(node: AstIf, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IF_KW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter);

        if is_token(f, &mut iter, ELSE_KW) {
            f.text(" ");
            print_token(f, &mut iter, ELSE_KW, &opt);
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter);
        }
    });
}

pub(crate) fn format_is(node: AstIs, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter);
        f.text(" ");
        print_token(f, &mut iter, IS_KW, &opt);
        f.text(" ");
        print_node::<AstPattern>(f, &mut iter);
    });
}

pub(crate) fn format_lambda(node: AstLambda, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_token(f, &mut iter, OR_OR) {
            print_token(f, &mut iter, OR_OR, &opt);
        } else {
            print_token(f, &mut iter, OR, &opt);

            while !is_token(f, &mut iter, OR) {
                print_node::<AstParam>(f, &mut iter);
                if is_token(f, &mut iter, COMMA) {
                    print_token(f, &mut iter, COMMA, &opt);
                    f.text(" ");
                }
            }

            print_token(f, &mut iter, OR, &opt);
        }

        if is_token(f, &mut iter, COLON) {
            print_token(f, &mut iter, COLON, &opt);
            f.text(" ");
            print_node::<AstType>(f, &mut iter);
        }

        f.text(" ");
        print_node::<AstBlock>(f, &mut iter);
    });
}

pub(crate) fn format_match(node: AstMatch, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, MATCH_KW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter);
        f.text(" ");
        print_token(f, &mut iter, L_BRACE, &opt);

        if node.arms().next().is_none() {
            print_token(f, &mut iter, R_BRACE, &opt);
        } else {
            f.hard_line();
            f.nest(BLOCK_INDENT, |f| {
                while !is_token(f, &mut iter, R_BRACE) {
                    print_node::<AstMatchArm>(f, &mut iter);
                    print_token_opt(f, &mut iter, COMMA, &opt);
                    f.hard_line();
                }
            });
            print_token(f, &mut iter, R_BRACE, &opt);
        }
    });
}

pub(crate) fn format_match_arm(node: AstMatchArm, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstPattern>(f, &mut iter);
        if is_token(f, &mut iter, IF_KW) {
            f.text(" ");
            print_token(f, &mut iter, IF_KW, &opt);
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter);
        }
        f.text(" ");
        print_token(f, &mut iter, DOUBLE_ARROW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter);
    });
}

pub(crate) fn format_un(node: AstUn, f: &mut Formatter) {
    with_iter!(node, f, |iter, _opt| {
        print_next_token(f, &mut iter);
        print_node::<AstExpr>(f, &mut iter);
    });
}

pub(crate) fn format_name_expr(node: AstNameExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
    });
}

pub(crate) fn format_path(node: AstPath, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter);
        print_token(f, &mut iter, COLON_COLON, &opt);
        print_node::<AstExpr>(f, &mut iter);
    });
}

pub(crate) fn format_paren(node: AstParen, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_PAREN, &opt);
        print_node::<AstExpr>(f, &mut iter);
        print_token(f, &mut iter, R_PAREN, &opt);
    });
}

pub(crate) fn format_return(node: AstReturn, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, RETURN_KW, &opt);
        if is_node::<AstExpr>(f, &mut iter) {
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter);
        }
    });
}

pub(crate) fn format_this(node: AstThis, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, SELF_KW, &opt);
    });
}

pub(crate) fn format_tuple(node: AstTuple, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_comma_list::<AstExpr>(f, &mut iter, L_PAREN, R_PAREN, &opt);
    });
}

pub(crate) fn format_typed_expr(node: AstTypedExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter);
        print_comma_list::<AstType>(f, &mut iter, L_BRACKET, R_BRACKET, &opt);
    });
}

pub(crate) fn format_template(node: AstTemplate, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {}
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

pub(crate) fn format_while(node: AstWhile, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, WHILE_KW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter);
        f.text(" ");
        print_node::<AstBlock>(f, &mut iter);
    });
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_source;

    #[test]
    fn formats_paren_expr() {
        let input = "fn  main (  ) {  let  x  =  ( 1+2 ) ; }";
        let expected = "fn main() {\n    let x = (1 + 2);\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_nested_paren_expr() {
        let input = "fn  main (  ) {  let  x  =  ( ( 1+2 ) * 3 ) ; }";
        let expected = "fn main() {\n    let x = ((1 + 2) * 3);\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_let_without_initializer() {
        let input = "fn  main (  ) {  let  x  :  Int ; }";
        let expected = "fn main() {\n    let x: Int;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_let_without_type() {
        let input = "fn  main (  ) {  let  x ; }";
        let expected = "fn main() {\n    let x;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_binary_exprs() {
        let input = "fn  main (  ) {  let  x  =  1+2 ; let  y  =  3*4 ; }";
        let expected = "fn main() {\n    let x = 1 + 2;\n    let y = 3 * 4;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_unary_exprs() {
        let input = "fn  main (  ) {  let  y  =  -1 ; let  z  =  ! true ; }";
        let expected = "fn main() {\n    let y = -1;\n    let z = !true;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_name_binary_expr() {
        let input = "fn  main (  ) {  let  x  =  a+1 ; }";
        let expected = "fn main() {\n    let x = a + 1;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_dot_expr() {
        let input = "fn  main (  ) {  let  x  =  a . b ; }";
        let expected = "fn main() {\n    let x = a.b;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_call_expr() {
        let input = "fn  main (  ) {  let  x  =  f ( 1 , 2 ) ; }";
        let expected = "fn main() {\n    let x = f(1, 2);\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn drops_trailing_comma_in_call() {
        let input = "fn  main (  ) {  let  x  =  f ( 1 , 2 , ) ; }";
        let expected = "fn main() {\n    let x = f(1, 2);\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_break_expr() {
        let input = "fn  main (  ) {  while  true  {  break ; } }";
        let expected = "fn main() {\n    while true {\n        break;\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_continue_expr() {
        let input = "fn  main (  ) {  while  true  {  continue ; } }";
        let expected = "fn main() {\n    while true {\n        continue;\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_while_expr() {
        let input = "fn  main (  ) {  while  true  {  a = 1 ; } }";
        let expected = "fn main() {\n    while true {\n        a = 1;\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_for_expr() {
        let input = "fn  main (  ) {  for  i  in  xs  {  a = 1 ; } }";
        let expected = "fn main() {\n    for i in xs {\n        a = 1;\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_path_expr() {
        let input = "fn  main (  ) {  let  x  =  a :: b ; }";
        let expected = "fn main() {\n    let x = a::b;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_typed_expr() {
        let input = "fn  main (  ) {  let  x  =  f [ Int , String ] ; }";
        let expected = "fn main() {\n    let x = f[Int, String];\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_return_expr() {
        let input = "fn  main (  ) {  return  1 ; }";
        let expected = "fn main() {\n    return 1;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_match_expr() {
        let input = "fn  main (  ) {  match  x  {  1  =>  2 , _  =>  3 } }";
        let expected = "fn main() {\n    match x {\n        1 => 2,\n        _ => 3\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_lambda_expr() {
        let input = "fn  main (  ) {  let  f  =  | x : Int , y : Int | { x + y } ; }";
        let expected = "fn main() {\n    let f = |x: Int, y: Int| {\n        x + y\n    };\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_lambda_expr_with_return_type() {
        let input = "fn  main (  ) {  let  f  =  | x : Int | :  Int { x } ; }";
        let expected = "fn main() {\n    let f = |x: Int|: Int {\n        x\n    };\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_tuple_expr() {
        let input = "fn  main (  ) {  let  x  =  ( 1 , 2 , 3 ) ; }";
        let expected = "fn main() {\n    let x = (1, 2, 3);\n}\n";
        assert_source(input, expected);
    }

    #[test]
    #[ignore]
    fn formats_inline_line_comment() {
        let input = "fn  main (  ) {  let  x  =  1 ;   // comment\n }";
        let expected = "fn main() {\n    let x = 1; // comment\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_if_expr() {
        let input = "fn  main (  ) {  if  true  {  1 }  else  {  2 } }";
        let expected = "fn main() {\n    if true {\n        1\n    } else {\n        2\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_if_expr_with_comment_in_empty_block() {
        let input = "fn  main (  ) {  if  true  {  1 }  else  if  false  {  // There might be live entries after a deleted one.\n  }  else  {  2 } }";
        let expected = "fn main() {\n    if true {\n        1\n    } else if false {\n        // There might be live entries after a deleted one.\n    } else {\n        2\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_this_expr() {
        let input = "fn  main (  ) {  let  x  =  self ; }";
        let expected = "fn main() {\n    let x = self;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_char_lit_expr() {
        let input = "fn  main (  ) {  let  c  =  'a' ; }";
        let expected = "fn main() {\n    let c = 'a';\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_is_expr() {
        let input = "fn  main (  ) {  let  x  =  a  is  _ ; }";
        let expected = "fn main() {\n    let x = a is _;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_conv_expr() {
        let input = "fn  main (  ) {  let  x  =  a  as  Int ; }";
        let expected = "fn main() {\n    let x = a as Int;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_template_expr() {
        let input = "fn  main (  ) {  let  x  =  \"a${1}b${2}c\" ; }";
        let expected = "fn main() {\n    let x = \"a${1}b${2}c\";\n}\n";
        assert_source(input, expected);
    }
}
