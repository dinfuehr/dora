use dora_parser::TokenKind::*;
use dora_parser::ast::{AstElement, AstElementList, SyntaxNodeBase};

use crate::doc::BLOCK_INDENT;
use crate::doc::Formatter;
use crate::doc::utils::{
    CollectElement, Options, collect_nodes_with_gaps, is_token, print_token, print_trivia,
};

pub(crate) fn format_element_list(node: AstElementList, f: &mut Formatter) {
    let mut iter = node.children_with_tokens().peekable();
    let opt = Options::new();

    let has_brace = is_token(f, &mut iter, L_BRACE, &opt);

    if has_brace {
        print_token(f, &mut iter, L_BRACE, &opt);
    }

    let elements = collect_nodes_with_gaps::<AstElement>(f, &mut iter, &opt);

    if elements.is_empty() {
        if has_brace {
            print_token(f, &mut iter, R_BRACE, &opt);
            print_trivia(f, &mut iter, &opt);
        }

        return;
    }

    let content = f.concat(|f| {
        for (idx, element) in elements.into_iter().enumerate() {
            if idx > 0 {
                f.hard_line();
                f.hard_line();
            }

            match element {
                CollectElement::Comment(doc_id) => f.append(doc_id),
                CollectElement::Element(_, doc_id) => f.append(doc_id),
            }
        }
    });

    if has_brace {
        f.hard_line();
        f.nest(BLOCK_INDENT, |f| {
            f.append(content);
        });
        f.hard_line();
        print_token(f, &mut iter, R_BRACE, &opt);
    } else {
        f.append(content);
        f.hard_line();
    }
}
