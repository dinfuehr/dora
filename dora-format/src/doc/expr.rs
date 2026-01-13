use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstArgument, AstArgumentList, AstBlock, AstBreak, AstCall, AstContinue, AstConv, AstDotExpr,
    AstExpr, AstFor, AstIf, AstIs, AstLambda, AstMatch, AstMatchArm, AstMethodCallExpr,
    AstNameExpr, AstParam, AstParen, AstPath, AstPattern, AstReturn, AstStmt, AstTemplate, AstThis,
    AstTuple, AstType, AstTypeArgumentList, AstTypedExpr, AstUn, AstWhile, SyntaxElement,
    SyntaxNodeBase,
};

use crate::doc::utils::{
    CollectElement, Options, collect_nodes, is_node, is_token, print_comma_list,
    print_comma_list_ungrouped, print_next_token, print_node, print_rest, print_token,
};
use crate::doc::{BLOCK_INDENT, Formatter};
use crate::with_iter;

fn is_blocklike(expr: &AstExpr) -> bool {
    matches!(
        expr,
        AstExpr::Block(_)
            | AstExpr::For(_)
            | AstExpr::If(_)
            | AstExpr::Match(_)
            | AstExpr::While(_)
            | AstExpr::Lambda(_)
    )
}

pub(crate) fn format_block(node: AstBlock, f: &mut Formatter) {
    let mut iter = node.children_with_tokens();
    let opt = Options::keep_empty_lines();
    print_token(f, &mut iter, L_BRACE, &opt);

    let elements = collect_nodes::<AstStmt>(f, &mut iter, &opt, false);

    if !elements.is_empty() {
        f.hard_line();
        f.nest(BLOCK_INDENT, |f| {
            for element in elements {
                match element {
                    CollectElement::Comment(doc_id) => {
                        f.append(doc_id);
                    }
                    CollectElement::Element(_, doc_id) => f.append(doc_id),
                    CollectElement::Gap => {}
                }
                f.hard_line();
            }
        });
    }

    print_token(f, &mut iter, R_BRACE, &opt);
    print_rest(f, iter, &opt);
}

pub(crate) fn format_break(node: AstBreak, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, BREAK_KW, &opt);
    });
}

pub(crate) fn format_call(node: AstCall, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        f.group(|f| {
            print_node::<AstExpr>(f, &mut iter, &opt);

            if let Some(SyntaxElement::Node(arg_list)) = iter.next() {
                let mut arg_iter = arg_list.children_with_tokens();
                print_comma_list_ungrouped::<AstArgument>(f, &mut arg_iter, L_PAREN, R_PAREN, &opt);
            }
        });
    });
}

pub(crate) fn format_argument_list(node: AstArgumentList, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_comma_list::<AstArgument>(f, &mut iter, L_PAREN, R_PAREN, &opt);
    });
}

pub(crate) fn format_argument(node: AstArgument, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_token(&mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            if is_token(&mut iter, EQ) {
                f.text(" ");
                print_token(f, &mut iter, EQ, &opt);
                f.text(" ");
            }
        }
        print_node::<AstExpr>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_continue(node: AstContinue, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, CONTINUE_KW, &opt);
    });
}

pub(crate) fn format_conv(node: AstConv, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_token(f, &mut iter, AS_KW, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);
    });
}

fn collect_dot_chain(node: AstDotExpr) -> (AstExpr, Vec<AstDotExpr>) {
    let mut chain: Vec<AstDotExpr> = Vec::new();
    let mut current = node;

    loop {
        chain.push(current.clone());
        match current.lhs() {
            AstExpr::DotExpr(dot) => current = dot,
            _ => break,
        }
    }

    let base = chain.last().unwrap().lhs();
    chain.reverse();
    (base, chain)
}

pub(crate) fn format_dot_expr(node: AstDotExpr, f: &mut Formatter) {
    let (base, chain) = collect_dot_chain(node);

    f.group(|f| {
        crate::doc::format_node(base.syntax_node().clone(), f);

        f.nest(BLOCK_INDENT, |f| {
            for dot in chain {
                f.soft_break();
                f.text(".");
                if let Some(name_token) = dot.name() {
                    f.token(name_token);
                }
            }
        });
    });
}

pub(crate) fn format_method_call_expr(node: AstMethodCallExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter, &opt);
        print_token(f, &mut iter, DOT, &opt);
        print_token(f, &mut iter, IDENTIFIER, &opt);
        if is_node::<AstTypeArgumentList>(&iter) {
            print_node::<AstTypeArgumentList>(f, &mut iter, &opt);
        }
        if is_node::<AstArgumentList>(&iter) {
            print_node::<AstArgumentList>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_for(node: AstFor, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, FOR_KW, &opt);
        f.text(" ");
        print_node::<AstPattern>(f, &mut iter, &opt);
        f.text(" ");
        print_token(f, &mut iter, IN_KW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_node::<AstBlock>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_if(node: AstIf, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IF_KW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter, &opt);

        if is_token(&mut iter, ELSE_KW) {
            f.text(" ");
            print_token(f, &mut iter, ELSE_KW, &opt);
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_is(node: AstIs, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_token(f, &mut iter, IS_KW, &opt);
        f.text(" ");
        print_node::<AstPattern>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_lambda(node: AstLambda, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_token(&mut iter, OR_OR) {
            print_token(f, &mut iter, OR_OR, &opt);
        } else {
            print_token(f, &mut iter, OR, &opt);

            while !is_token(&mut iter, OR) {
                print_node::<AstParam>(f, &mut iter, &opt);
                if is_token(&mut iter, COMMA) {
                    print_token(f, &mut iter, COMMA, &opt);
                    f.text(" ");
                }
            }

            print_token(f, &mut iter, OR, &opt);
        }

        if is_token(&mut iter, COLON) {
            print_token(f, &mut iter, COLON, &opt);
            f.text(" ");
            print_node::<AstType>(f, &mut iter, &opt);
        }

        f.text(" ");
        print_node::<AstBlock>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_match(node: AstMatch, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, MATCH_KW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_token(f, &mut iter, L_BRACE, &opt);
        let elements = collect_nodes::<AstMatchArm>(f, &mut iter, &opt, true);

        if !elements.is_empty() {
            f.hard_line();
            f.nest(BLOCK_INDENT, |f| {
                for element in elements {
                    match element {
                        CollectElement::Comment(doc_id) => {
                            f.append(doc_id);
                            f.hard_line();
                        }
                        CollectElement::Element(arm, doc_id) => {
                            f.append(doc_id);
                            if !is_blocklike(&arm.value()) {
                                f.text(",");
                            }
                            f.hard_line();
                        }
                        CollectElement::Gap => {
                            f.hard_line();
                        }
                    }
                }
            });
        }

        print_token(f, &mut iter, R_BRACE, &opt);
    });
}

pub(crate) fn format_match_arm(node: AstMatchArm, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstPattern>(f, &mut iter, &opt);
        if is_token(&mut iter, IF_KW) {
            f.text(" ");
            print_token(f, &mut iter, IF_KW, &opt);
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter, &opt);
        }
        f.text(" ");
        print_token(f, &mut iter, DOUBLE_ARROW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_un(node: AstUn, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_next_token(f, &mut iter, &opt);
        print_node::<AstExpr>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_name_expr(node: AstNameExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
    });
}

pub(crate) fn format_path(node: AstPath, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter, &opt);
        print_token(f, &mut iter, COLON_COLON, &opt);
        print_node::<AstExpr>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_paren(node: AstParen, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_PAREN, &opt);
        print_node::<AstExpr>(f, &mut iter, &opt);
        print_token(f, &mut iter, R_PAREN, &opt);
    });
}

pub(crate) fn format_return(node: AstReturn, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, RETURN_KW, &opt);
        if is_node::<AstExpr>(&iter) {
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter, &opt);
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
        print_node::<AstExpr>(f, &mut iter, &opt);
        print_comma_list::<AstType>(f, &mut iter, L_BRACKET, R_BRACKET, &opt);
    });
}

pub(crate) fn format_template(node: AstTemplate, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                NEWLINE | WHITESPACE => {}
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
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_node::<AstBlock>(f, &mut iter, &opt);
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
    fn formats_dot_chain() {
        let input = "fn  main (  ) {  let  x  =  a . b . c . d ; }";
        let expected = "fn main() {\n    let x = a.b.c.d;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_dot_chain_multiline() {
        let input = "fn main() { let x = some_object.very_long_field_name.another_long_field_name.yet_another_field_name; }";
        let expected = "fn main() {\n    let x = some_object\n        .very_long_field_name\n        .another_long_field_name\n        .yet_another_field_name;\n}\n";
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
    fn formats_call_expr_multiline() {
        let input = "fn main() { let x = some_function(aaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccccc); }";
        let expected = "fn main() {\n    let x = some_function(\n        aaaaaaaaaaaaaaaaaaaaaa,\n        bbbbbbbbbbbbbbbbbbbbbb,\n        cccccccccccccccccccccc,\n    );\n}\n";
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
        let expected = "fn main() {\n    match x {\n        1 => 2,\n        _ => 3,\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_match_with_gap_lines() {
        let input = "fn main() { match x { 1 => 2,\n\n_ => 3 } }";
        let expected = "fn main() {\n    match x {\n        1 => 2,\n\n        _ => 3,\n    }\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_match_with_block_expr() {
        let input = "fn main() { match x { 1 => { foo(); }, _ => { bar(); } } }";
        let expected = "fn main() {\n    match x {\n        1 => {\n            foo();\n        }\n        _ => {\n            bar();\n        }\n    }\n}\n";
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
