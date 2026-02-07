use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstListItem, AstModifierList, AstUse, AstUseAs, AstUseGroup, AstUseName, AstUsePathSegment,
    AstUseTarget, AstUseTree, SyntaxNodeBase,
};

use crate::doc::utils::{
    collect_node, eat_token, is_node, is_token, print_node, print_token, print_token_opt,
};
use crate::doc::{BLOCK_INDENT, Formatter};
use crate::with_iter;

pub(crate) fn format_use(node: AstUse, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&mut iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, USE_KW, &opt);
        f.text(" ");
        print_node::<AstUsePathSegment>(f, &mut iter, &opt);
        print_token(f, &mut iter, COLON_COLON, &opt);
        print_node::<AstUseTree>(f, &mut iter, &opt);
        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_use_path_segment(node: AstUsePathSegment, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_token(&mut iter, SELF_KW) {
            print_token(f, &mut iter, SELF_KW, &opt);
        } else if is_token(&mut iter, PACKAGE_KW) {
            print_token(f, &mut iter, PACKAGE_KW, &opt);
        } else if is_token(&mut iter, SUPER_KW) {
            print_token(f, &mut iter, SUPER_KW, &opt);
        } else {
            print_token(f, &mut iter, IDENTIFIER, &opt);
        }
    });
}

pub(crate) fn format_use_name(node: AstUseName, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
    });
}

pub(crate) fn format_use_as(node: AstUseAs, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
        f.text(" ");
        print_token(f, &mut iter, AS_KW, &opt);
        f.text(" ");
        if !print_token_opt(f, &mut iter, UNDERSCORE, &opt) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
        }
    });
}

pub(crate) fn format_use_group(node: AstUseGroup, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        eat_token(f, &mut iter, L_BRACE, &opt);
        let mut elements = Vec::new();
        while is_node::<AstListItem>(&mut iter) {
            let list_item = iter.next().unwrap().to_node().unwrap();
            let mut list_item_iter = list_item.children_with_tokens();
            let (node, doc_id) = collect_node::<AstUseTree>(f, &mut list_item_iter, &opt);
            let node = node.expect("node not found");
            let doc_id = doc_id.expect("doc not found");
            elements.push((node, doc_id));
        }
        eat_token(f, &mut iter, R_BRACE, &opt);

        elements.sort_by(|(left, _), (right, _)| compare_use_trees(left, right));
        let group_count = elements.len();

        f.group(|f| {
            f.text("{");
            f.soft_break();
            f.nest(BLOCK_INDENT, |f| {
                for (idx, (_, doc_id)) in elements.into_iter().enumerate() {
                    f.append(doc_id);
                    if idx + 1 < group_count {
                        f.text(",");
                        f.soft_line();
                    } else {
                        f.if_break(|f| {
                            f.text(",");
                            f.hard_line();
                        });
                    }
                }
            });
            f.text("}");
        });
    });
}

fn compare_use_trees(left: &AstUseTree, right: &AstUseTree) -> std::cmp::Ordering {
    let left_key = use_tree_sort_key(left);
    let right_key = use_tree_sort_key(right);
    left_key
        .0
        .cmp(&right_key.0)
        .then_with(|| natural_cmp(&left_key.1, &right_key.1))
}

fn natural_cmp(a: &str, b: &str) -> std::cmp::Ordering {
    let mut a = a.as_bytes();
    let mut b = b.as_bytes();

    loop {
        match (a.first(), b.first()) {
            (None, None) => return std::cmp::Ordering::Equal,
            (None, Some(_)) => return std::cmp::Ordering::Less,
            (Some(_), None) => return std::cmp::Ordering::Greater,
            (Some(&ac), Some(&bc)) => {
                if ac.is_ascii_digit() && bc.is_ascii_digit() {
                    // Compare numeric segments by value.
                    let (a_num, a_rest) = split_digits(a);
                    let (b_num, b_rest) = split_digits(b);
                    match a_num.len().cmp(&b_num.len()).then_with(|| a_num.cmp(b_num)) {
                        std::cmp::Ordering::Equal => {}
                        ord => return ord,
                    }
                    a = a_rest;
                    b = b_rest;
                } else {
                    match ac.cmp(&bc) {
                        std::cmp::Ordering::Equal => {
                            a = &a[1..];
                            b = &b[1..];
                        }
                        ord => return ord,
                    }
                }
            }
        }
    }
}

fn split_digits(s: &[u8]) -> (&[u8], &[u8]) {
    let end = s
        .iter()
        .position(|b| !b.is_ascii_digit())
        .unwrap_or(s.len());
    (&s[..end], &s[end..])
}

fn use_tree_sort_key(path: &AstUseTree) -> (u8, String) {
    match path.target() {
        Some(AstUseTarget::UseGroup(_)) => (0, String::new()),
        Some(AstUseTarget::UseName(name)) => (1, name.name().text().to_string()),
        Some(AstUseTarget::UseAs(use_as)) => (
            1,
            use_as
                .target_name()
                .map(|token| token.text().to_string())
                .unwrap_or_default(),
        ),
        None => (1, String::new()),
    }
}

pub(crate) fn format_use_tree(node: AstUseTree, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        while is_token(&mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            print_token_opt(f, &mut iter, COLON_COLON, &opt);
        }
        print_node::<AstUseTarget>(f, &mut iter, &opt);
    });
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_source;

    #[test]
    fn formats_use_group() {
        let input = "use  self  :: { C , A , B } ;";
        let expected = "use self::{A, B, C};\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_use_group_with_comments() {
        let input = "use  self  :: { /* C */ C , /* A */ A , /* B */ B } ;";
        let expected = "use self::{/* A */A, /* B */B, /* C */C};\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_use_nested_group() {
        let input = "use  super :: foo :: bar :: baz :: { a  as  x , b , c :: { f , e,d } } ;";
        let expected = "use super::foo::bar::baz::{c::{d, e, f}, b, a as x};\n";
        assert_source(input, expected);
    }

    #[test]
    #[ignore]
    fn formats_use_empty_group() {
        let input = "use self::{/*nothing*/}";
        let expected = "use self::{/*nothing*/};\n";
        assert_source(input, expected);
    }
}
