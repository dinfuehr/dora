use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstModifierList, AstUse, AstUseAs, AstUseAtom, AstUseGroup, AstUseName, AstUsePath,
    AstUseTarget, SyntaxNodeBase,
};

use crate::doc::Formatter;
use crate::doc::utils::{
    collect_node, eat_token, eat_token_opt, is_node, is_token, print_node, print_token,
    print_token_opt,
};
use crate::with_iter;

pub(crate) fn format_use(node: AstUse, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&mut iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, USE_KW, &opt);
        f.text(" ");
        print_node::<AstUseAtom>(f, &mut iter, &opt);
        print_token(f, &mut iter, COLON_COLON, &opt);
        print_node::<AstUsePath>(f, &mut iter, &opt);
        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_use_atom(node: AstUseAtom, f: &mut Formatter) {
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
        while !is_token(&mut iter, R_BRACE) {
            let (node, doc_id) = collect_node::<AstUsePath>(f, &mut iter, &opt);
            let node = node.expect("node not found");
            let doc_id = doc_id.expect("doc not found");
            elements.push((node, doc_id));
            eat_token_opt(f, &mut iter, COMMA, &opt);
        }
        eat_token(f, &mut iter, R_BRACE, &opt);

        elements.sort_by(|(left, _), (right, _)| compare_use_paths(left, right));
        let group_count = elements.len();

        f.text("{");
        for (idx, (_, doc_id)) in elements.into_iter().enumerate() {
            if idx > 0 {
                f.text(" ");
            }
            f.append(doc_id);
            if idx + 1 != group_count {
                f.text(",");
            }
        }
        f.text("}");
    });
}

fn compare_use_paths(left: &AstUsePath, right: &AstUsePath) -> std::cmp::Ordering {
    let left_key = use_path_sort_key(left);
    let right_key = use_path_sort_key(right);
    left_key.cmp(&right_key)
}

fn use_path_sort_key(path: &AstUsePath) -> (u8, String) {
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

pub(crate) fn format_use_path(node: AstUsePath, f: &mut Formatter) {
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
