use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstArgument, AstArgumentList, AstAsExpr, AstBlockExpr, AstBreakExpr, AstCallExpr,
    AstContinueExpr, AstExpr, AstFieldExpr, AstForExpr, AstIfExpr, AstIsExpr, AstLambdaExpr,
    AstMatchArm, AstMatchExpr, AstMethodCallExpr, AstParam, AstParenExpr, AstPathExpr,
    AstPathSegment, AstPattern, AstReturnExpr, AstStmt, AstTemplateExpr, AstThisExpr, AstTupleExpr,
    AstType, AstTypeArgument, AstTypeArgumentList, AstUnExpr, AstWhileExpr, SyntaxElement,
    SyntaxNodeBase, SyntaxToken,
};

use crate::doc::DocId;

use crate::doc::utils::{
    CollectElement, Options, collect_comment_docs, collect_nodes, format_node_as_doc, is_node,
    is_token, next_node, next_token, print_comma_list, print_comma_list_ungrouped,
    print_next_token, print_node, print_rest, print_token, print_token_opt,
};
use crate::doc::{BLOCK_INDENT, Formatter};
use crate::with_iter;

fn is_blocklike(expr: &AstExpr) -> bool {
    matches!(
        expr,
        AstExpr::BlockExpr(_)
            | AstExpr::ForExpr(_)
            | AstExpr::IfExpr(_)
            | AstExpr::MatchExpr(_)
            | AstExpr::WhileExpr(_)
            | AstExpr::LambdaExpr(_)
    )
}

pub(crate) fn format_block(node: AstBlockExpr, f: &mut Formatter) {
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

pub(crate) fn format_break(node: AstBreakExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, BREAK_KW, &opt);
    });
}

pub(crate) fn format_call(node: AstCallExpr, f: &mut Formatter) {
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

pub(crate) fn format_continue(node: AstContinueExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, CONTINUE_KW, &opt);
    });
}

pub(crate) fn format_as_expr(node: AstAsExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_token(f, &mut iter, AS_KW, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);
    });
}

enum DotChainElement {
    Comment(DocId),
    Base(DocId),
    Dot(SyntaxToken),
    Name(SyntaxToken),
}

fn collect_field_chain(node: AstFieldExpr, f: &mut Formatter) -> Vec<DotChainElement> {
    let mut elements = Vec::new();
    let mut iter = node.children_with_tokens();

    collect_comments(&mut iter, &mut elements, f);
    let lhs = next_node::<AstExpr>(&mut iter);
    match lhs {
        AstExpr::FieldExpr(inner_dot) => {
            elements.extend(collect_field_chain(inner_dot, f));
        }
        _ => {
            elements.push(DotChainElement::Base(format_node_as_doc(lhs, f)));
        }
    }

    collect_comments(&mut iter, &mut elements, f);
    let dot_token = next_token(&mut iter);
    elements.push(DotChainElement::Dot(dot_token));

    collect_comments(&mut iter, &mut elements, f);
    let name_token = next_token(&mut iter);
    elements.push(DotChainElement::Name(name_token));

    collect_comments(&mut iter, &mut elements, f);

    elements
}

fn collect_comments(
    iter: &mut dora_parser::ast::SyntaxElementIter<'_>,
    elements: &mut Vec<DotChainElement>,
    f: &mut Formatter,
) {
    for doc_id in collect_comment_docs(iter, f) {
        elements.push(DotChainElement::Comment(doc_id));
    }
}

pub(crate) fn format_field_expr(node: AstFieldExpr, f: &mut Formatter) {
    let elements = collect_field_chain(node, f);

    f.group(|f| {
        let mut in_chain = false;
        f.nest(BLOCK_INDENT, |f| {
            for element in elements {
                match element {
                    DotChainElement::Comment(doc_id) => {
                        f.append(doc_id);
                    }
                    DotChainElement::Base(doc_id) => {
                        f.append(doc_id);
                        in_chain = true;
                    }
                    DotChainElement::Dot(token) => {
                        if in_chain {
                            f.soft_break();
                        }
                        f.token(token);
                    }
                    DotChainElement::Name(token) => {
                        f.token(token);
                    }
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

pub(crate) fn format_for(node: AstForExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, FOR_KW, &opt);
        f.text(" ");
        print_node::<AstPattern>(f, &mut iter, &opt);
        f.text(" ");
        print_token(f, &mut iter, IN_KW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_node::<AstBlockExpr>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_if(node: AstIfExpr, f: &mut Formatter) {
    let cond_is_paren = matches!(node.cond(), AstExpr::ParenExpr(_));

    with_iter!(node, f, |iter, opt| {
        f.group(|f| {
            print_token(f, &mut iter, IF_KW, &opt);
            if cond_is_paren {
                f.text(" ");
                print_node::<AstExpr>(f, &mut iter, &opt);
            } else {
                // Get something like this:
                // if (
                //     expr
                // ) { ...
                f.if_break(|f| {
                    f.text(" (");
                });
                f.nest(BLOCK_INDENT, |f| {
                    f.soft_line();
                    print_node::<AstExpr>(f, &mut iter, &opt);
                });
                f.if_break(|f| {
                    f.soft_break();
                    f.text(")");
                });
            }
        });
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

pub(crate) fn format_is(node: AstIsExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_token(f, &mut iter, IS_KW, &opt);
        f.text(" ");
        print_node::<AstPattern>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_lambda(node: AstLambdaExpr, f: &mut Formatter) {
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
        print_node::<AstBlockExpr>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_match(node: AstMatchExpr, f: &mut Formatter) {
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

pub(crate) fn format_un(node: AstUnExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_next_token(f, &mut iter, &opt);
        print_node::<AstExpr>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_path_expr(node: AstPathExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstPathSegment>(f, &mut iter, &opt);
        while print_token_opt(f, &mut iter, COLON_COLON, &opt) {
            print_node::<AstPathSegment>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_path_segment(node: AstPathSegment, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
        if is_token(&iter, L_BRACKET) {
            print_comma_list::<AstTypeArgument>(f, &mut iter, L_BRACKET, R_BRACKET, &opt);
        }
    });
}

pub(crate) fn format_paren(node: AstParenExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        f.group(|f| {
            print_token(f, &mut iter, L_PAREN, &opt);
            f.nest(BLOCK_INDENT, |f| {
                f.soft_break();
                print_node::<AstExpr>(f, &mut iter, &opt);
            });
            f.soft_break();
            print_token(f, &mut iter, R_PAREN, &opt);
        });
    });
}

pub(crate) fn format_return(node: AstReturnExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, RETURN_KW, &opt);
        if is_node::<AstExpr>(&iter) {
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_this(node: AstThisExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, SELF_KW, &opt);
    });
}

pub(crate) fn format_tuple(node: AstTupleExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_comma_list::<AstExpr>(f, &mut iter, L_PAREN, R_PAREN, &opt);
    });
}

pub(crate) fn format_template(node: AstTemplateExpr, f: &mut Formatter) {
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

pub(crate) fn format_while(node: AstWhileExpr, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, WHILE_KW, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter, &opt);
        f.text(" ");
        print_node::<AstBlockExpr>(f, &mut iter, &opt);
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
    fn formats_field_expr() {
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
    fn formats_as_expr() {
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
