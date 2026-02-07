use dora_parser::ast::{AstAssignExpr, AstBinExpr, AstExpr, BinOp, SyntaxNodeBase, SyntaxToken};

use crate::doc::utils::{collect_comment_docs, format_node_as_doc, next_node, next_token};
use crate::doc::{Doc, Formatter};

enum BinChainElement {
    Comment(Doc),
    Operand(Doc),
    Operator(SyntaxToken),
}

fn bin_op_precedence(op: BinOp) -> u8 {
    match op {
        BinOp::Or => 2,
        BinOp::And => 3,
        BinOp::Cmp(_) => 4,
        BinOp::Add | BinOp::Sub | BinOp::BitOr | BinOp::BitXor => 5,
        BinOp::Mul
        | BinOp::Div
        | BinOp::Mod
        | BinOp::BitAnd
        | BinOp::ShiftL
        | BinOp::ArithShiftR
        | BinOp::LogicalShiftR => 6,
    }
}

fn collect_bin_chain(node: AstBinExpr, precedence: u8, f: &mut Formatter) -> Vec<BinChainElement> {
    let mut elements = Vec::new();
    let mut iter = node.children_with_tokens();

    collect_comments(&mut iter, &mut elements, f);
    let lhs = next_node::<AstExpr>(&mut iter);
    if matches!(&lhs, AstExpr::BinExpr(b) if bin_op_precedence(b.op()) == precedence) {
        elements.extend(collect_bin_chain(lhs.as_bin_expr(), precedence, f));
    } else {
        elements.push(BinChainElement::Operand(format_node_as_doc(lhs, f)));
    }

    collect_comments(&mut iter, &mut elements, f);
    let op_token = next_token(&mut iter);
    elements.push(BinChainElement::Operator(op_token));

    collect_comments(&mut iter, &mut elements, f);
    let rhs = next_node::<AstExpr>(&mut iter);
    elements.push(BinChainElement::Operand(format_node_as_doc(rhs, f)));
    collect_comments(&mut iter, &mut elements, f);

    elements
}

fn collect_comments(
    iter: &mut dora_parser::ast::SyntaxElementIter<'_>,
    elements: &mut Vec<BinChainElement>,
    f: &mut Formatter,
) {
    for (doc, _) in collect_comment_docs(iter, f) {
        elements.push(BinChainElement::Comment(doc));
    }
}

pub(crate) fn format_bin(node: AstBinExpr, f: &mut Formatter) {
    let precedence = bin_op_precedence(node.op());
    let elements = collect_bin_chain(node, precedence, f);

    f.group(|f| {
        let mut need_space = false;
        for element in elements {
            match element {
                BinChainElement::Comment(doc) => {
                    f.append(doc);
                }
                BinChainElement::Operand(doc) => {
                    f.append(doc);
                    need_space = true;
                }
                BinChainElement::Operator(token) => {
                    if need_space {
                        f.text(" ");
                    }
                    f.token(token);
                    f.soft_line();
                    need_space = false;
                }
            }
        }
    });
}

pub(crate) fn format_assign(node: AstAssignExpr, f: &mut Formatter) {
    let mut iter = node.children_with_tokens();

    let comment_docs = collect_comment_docs(&mut iter, f);
    for (doc_id, _) in comment_docs {
        f.append(doc_id);
    }

    let lhs = next_node::<AstExpr>(&mut iter);
    let lhs_doc = format_node_as_doc(lhs, f);

    let comment_docs = collect_comment_docs(&mut iter, f);
    let op_token = next_token(&mut iter);

    let comment_docs2 = collect_comment_docs(&mut iter, f);
    let rhs = next_node::<AstExpr>(&mut iter);
    let rhs_doc = format_node_as_doc(rhs, f);

    f.group(|f| {
        f.append(lhs_doc);
        for (doc_id, _) in comment_docs {
            f.append(doc_id);
        }
        f.text(" ");
        f.token(op_token);
        for (doc_id, _) in comment_docs2 {
            f.append(doc_id);
        }
        f.soft_line();
        f.append(rhs_doc);
    });
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_source;

    #[test]
    fn formats_binary_exprs() {
        let input = "fn  main (  ) {  let  x  =  1+2 ; let  y  =  3*4 ; }";
        let expected = "fn main() {\n    let x = 1 + 2;\n    let y = 3 * 4;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_binary_chain() {
        let input = "fn main() { let x = a + b + c + d; }";
        let expected = "fn main() {\n    let x = a + b + c + d;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_binary_chain_multiline() {
        let input = "fn main() { let x = aaaaaaaaaaaaaaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbbbbbbbbb + ccccccccccccccccccccccccccc + ddddddddddddddddddddddddddd; }";
        let expected = "fn main() {\n    let x = aaaaaaaaaaaaaaaaaaaaaaaaaaa +\n    bbbbbbbbbbbbbbbbbbbbbbbbbbb +\n    ccccccccccccccccccccccccccc +\n    ddddddddddddddddddddddddddd;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_binary_same_precedence() {
        // + and - have same precedence, should be grouped
        let input = "fn main() { let x = a + b - c + d; }";
        let expected = "fn main() {\n    let x = a + b - c + d;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_binary_different_precedence() {
        // * has higher precedence than +, should not be grouped with +
        let input = "fn main() { let x = a + b * c + d; }";
        let expected = "fn main() {\n    let x = a + b * c + d;\n}\n";
        assert_source(input, expected);
    }
}
