use smol_str::SmolStr;

use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstCommaList, AstElement, AstElementList, AstUse, AstUseTarget, AstUseTree, SyntaxNodeBase,
};

use crate::doc::BLOCK_INDENT;
use crate::doc::Formatter;
use crate::doc::utils::{
    CollectElement, Options, collect_nodes, ends_with_hard_line, has_comment, natural_cmp,
    print_rest, print_token, print_trivia,
};

pub(crate) fn format_element_list(node: AstElementList, f: &mut Formatter) {
    let mut iter = node.children_with_tokens();
    let opt = Options::new();

    let has_brace = node.parent().is_some();

    if has_brace {
        print_token(f, &mut iter, L_BRACE, &opt);
    }

    let mut elements = collect_nodes::<AstElement>(f, &mut iter, &opt, false);

    if elements.is_empty() {
        if has_brace {
            print_token(f, &mut iter, R_BRACE, &opt);
            print_trivia(f, &mut iter, &opt);
        }

        return;
    }

    sort_use_declarations(&mut elements);
    let mut additional_gaps = vec![false; elements.len()];

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

        if lhs.is_comment() && rhs.is_comment() {
            continue;
        }

        let CollectElement::Element(lhs, ..) = lhs else {
            unreachable!()
        };

        let CollectElement::Element(rhs, ..) = rhs else {
            unreachable!()
        };

        let lhs_is_bodyless_mod =
            lhs.is_module() && lhs.clone().as_module().element_list().is_none();
        let rhs_is_bodyless_mod =
            rhs.is_module() && rhs.clone().as_module().element_list().is_none();

        if (lhs.is_const() && rhs.is_const())
            || (lhs.is_alias() && rhs.is_alias())
            || (lhs.is_global() && rhs.is_global())
            || (lhs.is_use() && rhs.is_use())
            || (lhs_is_bodyless_mod && rhs_is_bodyless_mod)
        {
            continue;
        }

        additional_gaps[idx - 1] = true;
    }

    let content = f.concat(|f| {
        for (idx, element) in elements.into_iter().enumerate() {
            let needs_hard_line = match element {
                CollectElement::Comment(doc) => {
                    let needs_hard_line = !ends_with_hard_line(&doc);
                    f.append(doc);
                    needs_hard_line
                }
                CollectElement::Element(_, doc) => {
                    let needs_hard_line = !ends_with_hard_line(&doc);
                    f.append(doc);
                    needs_hard_line
                }
                CollectElement::Gap => true,
            };

            if needs_hard_line {
                f.hard_line();
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

fn sort_use_declarations(elements: &mut [CollectElement<AstElement>]) {
    let mut start = 0;

    while start < elements.len() {
        if !is_comment_free_use(&elements[start]) {
            start += 1;
            continue;
        }

        let mut end = start + 1;
        while end < elements.len() && is_comment_free_use(&elements[end]) {
            end += 1;
        }

        elements[start..end].sort_by_cached_key(use_sort_key);
        start = end;
    }
}

fn is_comment_free_use(element: &CollectElement<AstElement>) -> bool {
    matches!(
        element,
        CollectElement::Element(AstElement::Use(node), _) if !has_comment(node.syntax_node())
    )
}

#[derive(Eq, PartialEq)]
struct UseSortKey(Vec<SmolStr>);

impl Ord for UseSortKey {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        for (left, right) in self.0.iter().zip(&other.0) {
            match natural_cmp(left.as_str(), right.as_str()) {
                std::cmp::Ordering::Equal => {}
                ordering => return ordering,
            }
        }

        self.0.len().cmp(&other.0.len())
    }
}

impl PartialOrd for UseSortKey {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn use_sort_key(element: &CollectElement<AstElement>) -> UseSortKey {
    let CollectElement::Element(AstElement::Use(node), _) = element else {
        unreachable!()
    };
    let mut path = vec![use_initial_atom(node)];
    append_use_tree_path(&node.path(), &mut path);
    UseSortKey(path)
}

fn use_initial_atom(node: &AstUse) -> SmolStr {
    let atom = node.initial_atom().expect("initial use path segment");
    match atom.kind() {
        IDENTIFIER => SmolStr::new(atom.to_name().unwrap().text()),
        SELF_KW => SmolStr::new_static("self"),
        PACKAGE_KW => SmolStr::new_static("package"),
        SUPER_KW => SmolStr::new_static("super"),
        kind => unreachable!("unexpected initial use path segment {kind}"),
    }
}

fn append_use_tree_path(tree: &AstUseTree, path: &mut Vec<SmolStr>) {
    // These are the intermediate `identifier::` segments. For
    // `package::foo::bar::Baz`, this yields `foo` and `bar`; the initial
    // `package` segment is stored separately on `AstUse`.
    path.extend(tree.path().map(|token| SmolStr::new(token.text())));

    // The target is the terminal import: a name (`Baz`), an alias
    // (`Baz as B`), or a group (`{A, B}`). Group entries are use trees too.
    match tree.target() {
        Some(AstUseTarget::UseName(name)) => path.push(SmolStr::new(name.name().text())),
        Some(AstUseTarget::UseAs(use_as)) => {
            path.push(SmolStr::new(use_as.original_name().text()));
        }
        Some(AstUseTarget::UseGroup(group)) => {
            if group.items_len() == 1 {
                append_use_tree_path(&group.items_at(0), path);
            }
        }
        None => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_source;

    #[test]
    fn sorts_contiguous_use_declarations_by_path() {
        let input =
            "use package::z; use package::v10; use package::a; use package::v2; fn main() {}";
        let expected = "use package::a;\nuse package::v2;\nuse package::v10;\nuse package::z;\n\nfn main() {}\n";
        assert_source(input, expected);
    }

    #[test]
    fn sorts_public_use_declarations_by_path() {
        let input = "pub use package::z; use package::a;";
        let expected = "use package::a;\npub use package::z;\n";
        assert_source(input, expected);
    }

    #[test]
    fn sorts_shorter_use_path_first() {
        let input = "use package::foo::B; use package::foo; use package::foo::{A, C};";
        let expected = "use package::foo;\nuse package::foo::{A, C};\nuse package::foo::B;\n";
        assert_source(input, expected);
    }

    #[test]
    fn sorts_collapsed_single_use_groups_idempotently() {
        let input = "use package::foo::B; use package::foo::{A}; use package::foo::{C};";
        let expected = "use package::foo::A;\nuse package::foo::B;\nuse package::foo::C;\n";
        assert_source(input, expected);
    }

    #[test]
    fn preserves_use_blocks_separated_by_a_gap() {
        let input = "use package::z;\n\nuse package::b;\nuse package::a;";
        let expected = "use package::z;\n\nuse package::a;\nuse package::b;\n";
        assert_source(input, expected);
    }

    #[test]
    fn preserves_commented_use_declaration_position() {
        let input = "use package::{/* pinned */ Z}; use package::b; use package::a;";
        let expected = "use package::{/* pinned */Z};\nuse package::a;\nuse package::b;\n";
        assert_source(input, expected);
    }

    #[test]
    fn preserves_use_declaration_with_leading_comment() {
        let input = "use package::z;\n// pinned\nuse package::b;\nuse package::a;";
        let expected = "use package::z;\n// pinned\nuse package::b;\nuse package::a;\n";
        assert_source(input, expected);
    }
}
