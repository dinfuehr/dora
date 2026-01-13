use dora_parser::ast::{AstBin, AstExpr, BinOp, SyntaxNodeBase};

use crate::doc::{Formatter, format_node};

fn bin_op_precedence(op: BinOp) -> u8 {
    match op {
        // (1, 2) in parser
        BinOp::Assign
        | BinOp::AddAssign
        | BinOp::SubAssign
        | BinOp::MulAssign
        | BinOp::DivAssign
        | BinOp::ModAssign
        | BinOp::BitOrAssign
        | BinOp::BitAndAssign
        | BinOp::BitXorAssign
        | BinOp::ShiftLAssign
        | BinOp::ArithShiftRAssign
        | BinOp::LogicalShiftRAssign => 1,
        // (2, 3) in parser
        BinOp::Or => 2,
        // (3, 4) in parser
        BinOp::And => 3,
        // (4, 5) in parser
        BinOp::Cmp(_) => 4,
        // (5, 6) in parser: ADD | SUB | OR | CARET
        BinOp::Add | BinOp::Sub | BinOp::BitOr | BinOp::BitXor => 5,
        // (6, 7) in parser: MUL | DIV | MODULO | AND | LT_LT | GT_GT | GT_GT_GT
        BinOp::Mul
        | BinOp::Div
        | BinOp::Mod
        | BinOp::BitAnd
        | BinOp::ShiftL
        | BinOp::ArithShiftR
        | BinOp::LogicalShiftR => 6,
    }
}

fn collect_bin_chain(node: AstBin) -> (AstExpr, Vec<(AstBin, AstExpr)>) {
    let precedence = bin_op_precedence(node.op());
    let mut parts: Vec<(AstBin, AstExpr)> = Vec::new();
    let mut current_bin = node;
    let leftmost;

    loop {
        let lhs = current_bin.lhs();
        parts.push((current_bin.clone(), current_bin.rhs()));

        match lhs {
            AstExpr::Bin(inner_bin) if bin_op_precedence(inner_bin.op()) == precedence => {
                current_bin = inner_bin;
            }
            _ => {
                leftmost = lhs;
                break;
            }
        }
    }

    parts.reverse();
    (leftmost, parts)
}

pub(crate) fn format_bin(node: AstBin, f: &mut Formatter) {
    let (lhs, parts) = collect_bin_chain(node);

    f.group(|f| {
        format_node(lhs.unwrap(), f);

        for (bin, rhs) in parts {
            f.text(" ");
            f.text(bin.op().as_str());
            f.soft_line();
            format_node(rhs.unwrap(), f);
        }
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
