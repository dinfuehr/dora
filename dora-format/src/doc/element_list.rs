use dora_parser::TokenKind::*;
use dora_parser::ast::{AstElement, AstElementList, SyntaxNodeBase};

use crate::doc::BLOCK_INDENT;
use crate::doc::Formatter;
use crate::doc::utils::{
    CollectElement, Options, collect_nodes, print_rest, print_token, print_trivia,
};

pub(crate) fn format_element_list(node: AstElementList, f: &mut Formatter) {
    let mut iter = node.children_with_tokens().peekable();
    let opt = Options::new();

    let has_brace = node.parent().is_some();

    if has_brace {
        print_token(f, &mut iter, L_BRACE, &opt);
    }

    let elements = collect_nodes::<AstElement>(f, &mut iter, &opt);
    let mut additional_gaps = vec![false; elements.len()];

    if elements.is_empty() {
        if has_brace {
            print_token(f, &mut iter, R_BRACE, &opt);
            print_trivia(f, &mut iter, &opt);
        }

        return;
    }

    for idx in 1..elements.len() {
        let lhs = &elements[idx - 1];
        let rhs = &elements[idx];

        if lhs.is_gap() || rhs.is_gap() {
            continue;
        }

        if (lhs.is_comment() && rhs.is_element()) || (lhs.is_element() && rhs.is_comment()) {
            additional_gaps[idx - 1] = true;
            continue;
        }

        let CollectElement::Element(lhs, ..) = lhs else {
            unreachable!()
        };

        let CollectElement::Element(rhs, ..) = rhs else {
            unreachable!()
        };

        if (lhs.is_const() && rhs.is_const())
            || (lhs.is_alias() && rhs.is_alias())
            || (lhs.is_global() && rhs.is_global())
            || (lhs.is_use() && rhs.is_use())
        {
            continue;
        }

        additional_gaps[idx - 1] = true;
    }

    let content = f.concat(|f| {
        for (idx, element) in elements.into_iter().enumerate() {
            match element {
                CollectElement::Comment(doc_id) => {
                    f.append(doc_id);
                    f.hard_line();
                }
                CollectElement::Element(_, doc_id) => {
                    f.append(doc_id);
                    f.hard_line();
                }
                CollectElement::Gap => {
                    f.hard_line();
                }
            }

            if additional_gaps[idx] {
                f.hard_line();
            }
        }
    });

    if has_brace {
        f.hard_line();
        f.nest(BLOCK_INDENT, |f| {
            f.append(content);
        });
        print_token(f, &mut iter, R_BRACE, &opt);
        print_rest(f, iter, &opt);
    } else {
        f.append(content);
    }
}
