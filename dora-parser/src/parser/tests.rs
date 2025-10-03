use std::sync::Arc;

use crate::ast::*;
use crate::error::ParseError;
use crate::parser::Parser;
use crate::{compute_line_column, compute_line_starts};

fn parse_expr(code: &'static str) -> (AstId, Arc<File>) {
    let mut parser = Parser::from_string(code);

    let expr_id = parser.parse_expr();
    assert!(parser.current().is_eof());
    let (file, errors) = parser.into_file(expr_id);
    assert!(errors.is_empty());

    (expr_id, file)
}

fn err_expr(code: &'static str, msg: ParseError, line: u32, col: u32) {
    let mut parser = Parser::from_string(code);

    let _expr = parser.parse_expr();

    let errors = parser.errors;
    assert_eq!(errors.len(), 1);
    let err = &errors[0];

    assert_eq!(msg, err.error);

    let line_starts = compute_line_starts(code);
    let (computed_line, computed_column) = compute_line_column(&line_starts, err.span.start());
    assert_eq!(line, computed_line);
    assert_eq!(col, computed_column);
}

fn parse_let(code: &'static str) -> (AstId, Arc<File>) {
    let mut parser = Parser::from_string(code);
    let node_id = parser.parse_let();
    assert!(parser.current().is_eof());
    let (file, errors) = parser.into_file(node_id);
    if !errors.is_empty() {
        for err in &errors {
            eprintln!(
                "error at {}: {:?} / {}",
                err.span,
                err.error,
                err.error.message()
            );
        }
    }
    assert!(errors.is_empty());
    (node_id, file)
}

fn parse_type(code: &'static str) -> (AstId, Arc<File>) {
    let mut parser = Parser::from_string(code);
    let node_id = parser.parse_type();
    assert!(parser.current().is_eof());
    let (file, errors) = parser.into_file(node_id);
    assert!(errors.is_empty());
    (node_id, file)
}

fn parse(code: &'static str) -> Arc<File> {
    let (file, errors) = Parser::from_string(code).parse();
    if !errors.is_empty() {
        for error in &errors {
            println!("{} at {}", error.error.message(), error.span);
        }
    }
    assert!(errors.is_empty());
    file
}

fn parse_with_some_errors(code: &'static str) -> Arc<File> {
    let (file, errors) = Parser::from_string(code).parse();
    assert!(!errors.is_empty());
    file
}

fn parse_with_error(code: &'static str, expected: Vec<(u32, u32, u32, ParseError)>) -> Arc<File> {
    let (file, errors) = Parser::from_string(code).parse();
    let line_starts = compute_line_starts(code);

    for error in &errors {
        let (line, col) = compute_line_column(&line_starts, error.span.start());

        println!(
            "{} at {}:{} of length {}",
            error.error.message(),
            line,
            col,
            error.span.len()
        );
    }

    assert_eq!(expected.len(), errors.len());

    for ((exp_line, exp_col, exp_len, exp_error), actual_error) in expected.into_iter().zip(errors)
    {
        let (line, col) = compute_line_column(&line_starts, actual_error.span.start());

        assert_eq!(line, exp_line);
        assert_eq!(col, exp_col);
        assert_eq!(actual_error.span.len(), exp_len);

        assert_eq!(exp_error, actual_error.error);
    }

    file
}

#[test]
fn parse_ident() {
    let (expr, f) = parse_expr("a");
    let ident = f.node(expr).to_ident().unwrap();
    assert_eq!("a", ident.name);
}

#[test]
fn parse_number() {
    let (expr, f) = parse_expr("10");

    let lit = f.node(expr).to_lit_int().unwrap();
    assert_eq!(String::from("10"), lit.value);
}

#[test]
fn parse_number_with_underscore() {
    let (expr, f) = parse_expr("1____0");

    let lit = f.node(expr).to_lit_int().unwrap();
    assert_eq!(String::from("1____0"), lit.value);
}

#[test]
fn parse_string() {
    let (expr, f) = parse_expr("\"abc\"");

    let lit = f.node(expr).to_lit_str().unwrap();
    assert_eq!("\"abc\"", &lit.value);
}

#[test]
fn parse_true() {
    let (expr, f) = parse_expr("true");

    let lit = f.node(expr).to_lit_bool().unwrap();
    assert_eq!(true, lit.value);
}

#[test]
fn parse_false() {
    let (expr, f) = parse_expr("true");

    let lit = f.node(expr).to_lit_bool().unwrap();
    assert_eq!(true, lit.value);
}

#[test]
fn parse_field_access() {
    let (expr, f) = parse_expr("obj.field");
    let dot = f.node(expr).to_dot().unwrap();

    let ident = f.node(dot.lhs).to_ident().unwrap();
    assert_eq!("obj", ident.name);

    let ident = f.node(dot.rhs).to_ident().unwrap();
    assert_eq!("field", ident.name);
}

#[test]
fn parse_field_negated() {
    let (expr, f) = parse_expr("-obj.field");
    let un = f.node(expr).to_un().unwrap();
    assert!(f.node(un.opnd).is_dot());
}

#[test]
fn parse_field_non_ident() {
    let (expr, f) = parse_expr("bar.12");
    let dot = f.node(expr).to_dot().unwrap();

    let ident = f.node(dot.lhs).to_ident().unwrap();
    assert_eq!("bar", ident.name);

    assert_eq!(
        String::from("12"),
        f.node(dot.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_self() {
    let (expr, f) = parse_expr("self");
    assert!(f.node(expr).is_this());
}

#[test]
fn parse_neg() {
    let (expr, f) = parse_expr("-1");

    let un = f.node(expr).to_un().unwrap();
    assert_eq!(UnOp::Neg, un.op);

    assert!(f.node(un.opnd).is_lit_int());
}

#[test]
fn parse_neg_twice() {
    let (expr, f) = parse_expr("-(-3)");

    let neg1 = f.node(expr).to_un().unwrap();
    assert_eq!(UnOp::Neg, neg1.op);

    let paren = f.node(neg1.opnd).to_paren().unwrap();
    let neg2 = f.node(paren.expr).to_un().unwrap();
    assert_eq!(UnOp::Neg, neg2.op);

    assert!(f.node(neg2.opnd).is_lit_int());
}

#[test]
fn parse_neg_twice_without_parentheses() {
    err_expr("- -2", ParseError::ExpectedFactor, 1, 3);
}

#[test]
fn parse_mul() {
    let (expr, f) = parse_expr("6*3");

    let mul = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Mul, mul.op);
    assert_eq!(
        String::from("6"),
        f.node(mul.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("3"),
        f.node(mul.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_multiple_muls() {
    let (expr, f) = parse_expr("6*3*4");

    let mul1 = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Mul, mul1.op);

    let mul2 = f.node(mul1.lhs).to_bin().unwrap();
    assert_eq!(BinOp::Mul, mul2.op);
    assert_eq!(
        String::from("6"),
        f.node(mul2.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("3"),
        f.node(mul2.rhs).to_lit_int().unwrap().value
    );

    assert_eq!(
        String::from("4"),
        f.node(mul1.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_div() {
    let (expr, f) = parse_expr("4/5");

    let div = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Div, div.op);
    assert_eq!(
        String::from("4"),
        f.node(div.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("5"),
        f.node(div.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_mod() {
    let (expr, f) = parse_expr("2%15");

    let div = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Mod, div.op);
    assert_eq!(
        String::from("2"),
        f.node(div.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("15"),
        f.node(div.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_add() {
    let (expr, f) = parse_expr("2+3");

    let add = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Add, add.op);
    assert_eq!(
        String::from("2"),
        f.node(add.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("3"),
        f.node(add.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_add_left_associativity() {
    let (expr, f) = parse_expr("1+2+3");

    let add = f.node(expr).to_bin().unwrap();
    assert_eq!(
        String::from("3"),
        f.node(add.rhs).to_lit_int().unwrap().value
    );

    let lhs = f.node(add.lhs).to_bin().unwrap();
    assert_eq!(
        String::from("1"),
        f.node(lhs.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(lhs.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_add_right_associativity_via_parens() {
    let (expr, f) = parse_expr("1+(2+3)");

    let add = f.node(expr).to_bin().unwrap();
    assert_eq!(
        String::from("1"),
        f.node(add.lhs).to_lit_int().unwrap().value
    );

    let paren = f.node(add.rhs).to_paren().unwrap();
    let rhs = f.node(paren.expr).to_bin().unwrap();
    assert_eq!(
        String::from("2"),
        f.node(rhs.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("3"),
        f.node(rhs.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_sub() {
    let (expr, f) = parse_expr("1-2");

    let add = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Sub, add.op);
    assert_eq!(
        String::from("1"),
        f.node(add.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(add.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_or() {
    let (expr, f) = parse_expr("1||2");

    let add = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Or, add.op);
    assert_eq!(
        String::from("1"),
        f.node(add.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(add.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_and() {
    let (expr, f) = parse_expr("1&&2");

    let add = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::And, add.op);
    assert_eq!(
        String::from("1"),
        f.node(add.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(add.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_bit_or() {
    let (expr, f) = parse_expr("1|2");

    let or = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::BitOr, or.op);
    assert_eq!(
        String::from("1"),
        f.node(or.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(or.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_bit_and() {
    let (expr, f) = parse_expr("1&2");

    let and = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::BitAnd, and.op);
    assert_eq!(
        String::from("1"),
        f.node(and.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(and.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_bit_xor() {
    let (expr, f) = parse_expr("1^2");

    let xor = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::BitXor, xor.op);
    assert_eq!(
        String::from("1"),
        f.node(xor.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(xor.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_lt() {
    let (expr, f) = parse_expr("1<2");

    let cmp = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Lt), cmp.op);
    assert_eq!(
        String::from("1"),
        f.node(cmp.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(cmp.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_le() {
    let (expr, f) = parse_expr("1<=2");

    let cmp = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Le), cmp.op);
    assert_eq!(
        String::from("1"),
        f.node(cmp.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(cmp.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_gt() {
    let (expr, f) = parse_expr("1>2");

    let cmp = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Gt), cmp.op);
    assert_eq!(
        String::from("1"),
        f.node(cmp.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(cmp.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_ge() {
    let (expr, f) = parse_expr("1>=2");

    let cmp = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Ge), cmp.op);
    assert_eq!(
        String::from("1"),
        f.node(cmp.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(cmp.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_eq() {
    let (expr, f) = parse_expr("1==2");

    let cmp = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Eq), cmp.op);
    assert_eq!(
        String::from("1"),
        f.node(cmp.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(cmp.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_ne() {
    let (expr, f) = parse_expr("1!=2");

    let cmp = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Ne), cmp.op);
    assert_eq!(
        String::from("1"),
        f.node(cmp.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(cmp.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_identity_not() {
    let (expr, f) = parse_expr("1!==2");

    let cmp = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::IsNot), cmp.op);
    assert_eq!(
        String::from("1"),
        f.node(cmp.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(cmp.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_identity() {
    let (expr, f) = parse_expr("1===2");

    let cmp = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Is), cmp.op);
    assert_eq!(
        String::from("1"),
        f.node(cmp.lhs).to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(cmp.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_assign() {
    let (expr, f) = parse_expr("a=4");

    let assign = f.node(expr).to_bin().unwrap();
    assert!(f.node(assign.lhs).is_ident());
    assert_eq!(BinOp::Assign, assign.op);
    assert_eq!(
        String::from("4"),
        f.node(assign.rhs).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_shift_right() {
    let (expr, f) = parse_expr("a>>4");

    let bin = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::ArithShiftR, bin.op);
}

#[test]
fn parse_unsigned_shift_right() {
    let (expr, f) = parse_expr("a>>>4");

    let bin = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::LogicalShiftR, bin.op);
}

#[test]
fn parse_left() {
    let (expr, f) = parse_expr("a<<4");

    let bin = f.node(expr).to_bin().unwrap();
    assert_eq!(BinOp::ShiftL, bin.op);
}

#[test]
fn parse_call_without_params() {
    let (expr, f) = parse_expr("fname()");

    let call = f.node(expr).to_call().unwrap();
    assert_eq!("fname", f.node(call.callee).to_ident().unwrap().name);
    assert_eq!(0, call.args.len());
}

#[test]
fn parse_call_with_params() {
    let (expr, f) = parse_expr("fname2(1,2,3)");

    let call = f.node(expr).to_call().unwrap();
    assert_eq!("fname2", f.node(call.callee).to_ident().unwrap().name);
    assert_eq!(3, call.args.len());
}

#[test]
fn parse_function() {
    let prog = parse("fn b() { }");
    let fct = prog.fct0();

    assert_eq!("b", id_name(&prog, fct.name));
    assert_eq!(0, fct.params.len());
    assert!(fct.return_type.is_none());
}

#[test]
fn parse_function_with_single_param() {
    let file1 = parse("fn f(a:int) { }");
    let f1 = file1.fct0();

    let file2 = parse("fn f(a:int,) { }");
    let f2 = file2.fct0();

    assert_eq!(f1.params.len(), 1);
    assert_eq!(f2.params.len(), 1);

    let p1 = file1.node(f1.params[0]).to_param().unwrap();
    let p2 = file2.node(f2.params[0]).to_param().unwrap();

    assert_eq!("a", pat_name(&file1, p1.pattern));
    assert_eq!("a", pat_name(&file2, p2.pattern));

    assert_eq!("int", tr_name(&file1, p1.data_type));
    assert_eq!("int", tr_name(&file2, p2.data_type));
}

#[test]
fn parse_function_with_multiple_params() {
    let file1 = parse("fn f(a:int, b:str) { }");
    let f1 = file1.fct0();

    let file2 = parse("fn f(a:int, b:str,) { }");
    let f2 = file2.fct0();

    let p1a = file1.node(f1.params[0]).to_param().unwrap();
    let p1b = file1.node(f1.params[1]).to_param().unwrap();
    let p2a = file2.node(f2.params[0]).to_param().unwrap();
    let p2b = file2.node(f2.params[1]).to_param().unwrap();

    assert_eq!("a", pat_name(&file1, p1a.pattern));
    assert_eq!("a", pat_name(&file2, p2a.pattern));

    assert_eq!("b", pat_name(&file1, p1b.pattern));
    assert_eq!("b", pat_name(&file2, p2b.pattern));

    assert_eq!("int", tr_name(&file1, p1a.data_type));
    assert_eq!("int", tr_name(&file2, p2a.data_type));

    assert_eq!("str", tr_name(&file1, p1b.data_type));
    assert_eq!("str", tr_name(&file2, p2b.data_type));
}

#[test]
fn parse_let_without_type() {
    let (stmt, f) = parse_let("let a = 1;");
    let var = f.node(stmt).to_let().unwrap();

    assert!(var.data_type.is_none());
    assert!(f.node(var.expr.unwrap()).is_lit_int());
}

#[test]
fn parse_let_rest() {
    let (stmt, f) = parse_let("let .. = 1;");
    let var = f.node(stmt).to_let().unwrap();
    assert!(f.node(var.pattern).is_rest());

    assert!(var.data_type.is_none());
    assert!(f.node(var.expr.unwrap()).is_lit_int());
}

#[test]
fn parse_let_with_type() {
    let (stmt, f) = parse_let("let x : int = 1;");
    let var = f.node(stmt).to_let().unwrap();

    assert!(var.data_type.is_some());
    assert!(f.node(var.expr.unwrap()).is_lit_int());
}

#[test]
fn parse_let_underscore() {
    let (stmt, f) = parse_let("let _ = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();
    assert!(f.node(let_decl.pattern).is_underscore());
}

#[test]
fn parse_let_tuple() {
    let (stmt, f) = parse_let("let (mut a, b, (c, d)) = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    let tuple = f.node(let_decl.pattern).to_tuple_pattern().unwrap();
    let first = f.node(tuple.params[0]);
    assert!(first.is_ident_pattern());
    assert!(first.to_ident_pattern().unwrap().mutable);
    let last = f.node(tuple.params[2]);
    assert!(last.is_tuple_pattern());
}

#[test]
fn parse_let_lit_bool() {
    let (stmt, f) = parse_let("let (a, true) = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    let tuple = f.node(let_decl.pattern).to_tuple_pattern().unwrap();
    assert!(f.node(tuple.params[0]).is_ident_pattern());
    assert!(f.node(tuple.params[1]).is_lit_pattern());
}

#[test]
fn parse_let_lit_char() {
    let (stmt, f) = parse_let("let (a, 'x') = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    let tuple = f.node(let_decl.pattern).to_tuple_pattern().unwrap();
    assert!(f.node(tuple.params[0]).is_ident_pattern());
    assert!(f.node(tuple.params[1]).is_lit_pattern());
}

#[test]
fn parse_let_lit_string() {
    let (stmt, f) = parse_let("let (a, \"x\") = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    let tuple = f.node(let_decl.pattern).to_tuple_pattern().unwrap();
    assert!(f.node(tuple.params[0]).is_ident_pattern());
    assert!(f.node(tuple.params[1]).is_lit_pattern());
}

#[test]
fn parse_let_lit_int() {
    let (stmt, f) = parse_let("let (a, 17) = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    let tuple = f.node(let_decl.pattern).to_tuple_pattern().unwrap();
    assert!(f.node(tuple.params[0]).is_ident_pattern());
    assert!(f.node(tuple.params[1]).is_lit_pattern());
}

#[test]
fn parse_let_lit_int_neg() {
    let (stmt, f) = parse_let("let (a, -17) = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    let tuple = f.node(let_decl.pattern).to_tuple_pattern().unwrap();
    assert!(f.node(tuple.params[0]).is_ident_pattern());
    assert!(f.node(tuple.params[1]).is_lit_pattern());
}

#[test]
fn parse_let_lit_float() {
    let (stmt, f) = parse_let("let (a, 17.5) = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    let tuple = f.node(let_decl.pattern).to_tuple_pattern().unwrap();
    assert!(f.node(tuple.params[0]).is_ident_pattern());
    assert!(f.node(tuple.params[1]).is_lit_pattern());
}

#[test]
fn parse_let_lit_float_neg() {
    let (stmt, f) = parse_let("let (a, -17.5) = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    let tuple = f.node(let_decl.pattern).to_tuple_pattern().unwrap();
    assert!(f.node(tuple.params[0]).is_ident_pattern());
    assert!(f.node(tuple.params[1]).is_lit_pattern());
}

#[test]
fn parse_let_ident() {
    let (stmt, f) = parse_let("let x = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    assert!(f.node(let_decl.pattern).is_ident_pattern());
}

#[test]
fn parse_let_ident_mut() {
    let (stmt, f) = parse_let("let mut x = 1;");
    let let_decl = f.node(stmt).to_let().unwrap();

    assert!(f.node(let_decl.pattern).to_ident_pattern().unwrap().mutable);
}

#[test]
fn parse_let_with_type_but_without_assignment() {
    let (stmt, f) = parse_let("let x : int;");
    let var = f.node(stmt).to_let().unwrap();

    assert!(var.data_type.is_some());
    assert!(var.expr.is_none());
}

#[test]
fn parse_let_without_type_and_assignment() {
    let (stmt, f) = parse_let("let x;");
    let var = f.node(stmt).to_let().unwrap();

    assert!(var.data_type.is_none());
    assert!(var.expr.is_none());
}

#[test]
fn parse_multiple_functions() {
    let prog = parse("fn f() { } fn g() { }");

    let f = prog.fct0();
    assert_eq!("f", id_name(&prog, f.name));

    let g = prog.fct(1);
    assert_eq!("g", id_name(&prog, g.name));
}

#[test]
fn parse_if() {
    let (expr, f) = parse_expr("if true { 2; } else { 3; }");
    let ifexpr = f.node(expr).to_if().unwrap();

    assert!(f.node(ifexpr.cond).is_lit_bool());
    assert!(ifexpr.else_block.is_some());
}

#[test]
fn parse_if_without_else() {
    let (expr, f) = parse_expr("if true { 2; }");
    let ifexpr = f.node(expr).to_if().unwrap();

    assert!(f.node(ifexpr.cond).is_lit_bool());
    assert!(ifexpr.else_block.is_none());
}

#[test]
fn parse_while() {
    let (expr, f) = parse_expr("while true { 2; }");
    let whilestmt = f.node(expr).to_while().unwrap();

    assert!(f.node(whilestmt.cond).is_lit_bool());
    assert!(f.node(whilestmt.block).is_blocklike());
}

#[test]
fn parse_empty_block() {
    let (expr, f) = parse_expr("{}");
    let block = f.node(expr).to_block().unwrap();

    assert_eq!(0, block.stmts.len());
}

#[test]
fn parse_block_with_one_stmt() {
    let (expr, f) = parse_expr("{ 1; 2 }");
    let block = f.node(expr).to_block().unwrap();

    assert_eq!(1, block.stmts.len());

    let expr = f.node(block.stmts[0]).to_expr_stmt().unwrap().expr;
    assert_eq!(String::from("1"), f.node(expr).to_lit_int().unwrap().value);

    assert_eq!(
        String::from("2"),
        f.node(block.expr.unwrap()).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_block_with_multiple_stmts() {
    let (expr, f) = parse_expr("{ 1; 2; }");
    let block = f.node(expr).to_block().unwrap();

    assert_eq!(2, block.stmts.len());

    let expr = f.node(block.stmts[0]).to_expr_stmt().unwrap().expr;
    assert_eq!(String::from("1"), f.node(expr).to_lit_int().unwrap().value);

    let expr = f.node(block.stmts[1]).to_expr_stmt().unwrap().expr;
    assert_eq!(String::from("2"), f.node(expr).to_lit_int().unwrap().value);

    assert!(block.expr.is_none());
}

#[test]
fn parse_break() {
    let (expr, f) = parse_expr("break");
    assert!(f.node(expr).is_break());
}

#[test]
fn parse_continue() {
    let (expr, f) = parse_expr("continue");
    assert!(f.node(expr).is_continue());
}

#[test]
fn parse_return_value() {
    let (expr, f) = parse_expr("return 1");
    let ret = f.node(expr).to_return().unwrap();

    assert_eq!(
        String::from("1"),
        f.node(ret.expr.unwrap()).to_lit_int().unwrap().value
    );
}

#[test]
fn parse_return() {
    let (expr, f) = parse_expr("return");
    let ret = f.node(expr).to_return().unwrap();

    assert!(ret.expr.is_none());
}

#[test]
fn parse_type_regular() {
    let (ty_id, f) = parse_type("bla");
    let ty = f.node(ty_id).to_regular_type().unwrap();

    assert_eq!(0, ty.params.len());
    assert_eq!("bla", tr_name(&f, ty_id));
}

#[test]
fn parse_type_regular_mod() {
    let (ty, f) = parse_type("foo::bla");
    let regular = f.node(ty).to_regular_type().unwrap();

    assert_eq!(0, regular.params.len());
    let path = f.node(regular.path).to_path_data().expect("path expected");
    assert_eq!(2, path.segments.len());
    assert_eq!("foo", ident_name(&f, path.segments[0]));
    assert_eq!("bla", ident_name(&f, path.segments[1]));
}

#[test]
fn parse_type_regular_with_params() {
    let (ty_id, f) = parse_type("Foo[A, B]");
    let regular = f.node(ty_id).to_regular_type().unwrap();

    assert_eq!(2, regular.params.len());
    assert_eq!("Foo", tr_name(&f, ty_id));
    assert_eq!("A", ta_name(&f, regular.params[0]));
    assert_eq!("B", ta_name(&f, regular.params[1]));
}

#[test]
fn parse_type_regular_with_bindings() {
    let (ty_id, f) = parse_type("Foo[A, X = B]");
    let ty = f.node(ty_id).to_regular_type().unwrap();

    assert_eq!(2, ty.params.len());
    assert_eq!("Foo", tr_name(&f, ty_id));
    let arg0_id = ty.params[0];
    let arg0 = f
        .node(arg0_id)
        .to_type_argument()
        .expect("type argument expected");
    assert!(arg0.name.is_none());
    assert_eq!("A", tr_name(&f, arg0.ty));

    let arg1_id = ty.params[1];
    let arg1 = f
        .node(arg1_id)
        .to_type_argument()
        .expect("type argument expected");
    assert_eq!("X", id_name(&f, arg1.name));
    assert_eq!("B", tr_name(&f, arg1.ty));
}

#[test]
fn parse_type_lambda_no_params() {
    let (ty, f) = parse_type("(): ()");
    let fct = f.node(ty).to_lambda_type().unwrap();

    assert_eq!(0, fct.params.len());
    assert!(f.node(fct.ret.unwrap()).is_unit());
}

#[test]
fn parse_type_lambda_one_param() {
    let (ty_id, f) = parse_type("(A): B");
    let fct = f.node(ty_id).to_lambda_type().unwrap();

    assert_eq!(1, fct.params.len());
    assert_eq!("A", tr_name(&f, fct.params[0]));
    assert_eq!("B", tr_name(&f, fct.ret.unwrap()));
}

#[test]
fn parse_type_lambda_two_params() {
    let (ty_id, f) = parse_type("(A, B): C");
    let fct = f.node(ty_id).to_lambda_type().unwrap();

    assert_eq!(2, fct.params.len());
    assert_eq!("A", tr_name(&f, fct.params[0]));
    assert_eq!("B", tr_name(&f, fct.params[1]));
    assert_eq!("C", tr_name(&f, fct.ret.unwrap()));
}

#[test]
fn parse_type_unit() {
    let (ty, f) = parse_type("()");
    let ty = f.node(ty).to_tuple_type().unwrap();

    assert!(ty.subtypes.is_empty());
}

#[test]
fn parse_type_tuple_with_one_type() {
    let (ty, f) = parse_type("(c)");

    let subtypes = &f.node(ty).to_tuple_type().unwrap().subtypes;
    assert_eq!(1, subtypes.len());

    assert_eq!("c", tr_name(&f, subtypes[0]));
}

#[test]
fn parse_type_tuple_with_two_types() {
    let (ty, f) = parse_type("(a, b)");

    let subtypes = &f.node(ty).to_tuple_type().unwrap().subtypes;
    assert_eq!(2, subtypes.len());
    assert_eq!("a", tr_name(&f, subtypes[0]));
    assert_eq!("b", tr_name(&f, subtypes[1]));
}

#[test]
fn parse_class_with_param() {
    let prog = parse("class Foo{a: int}");
    let class = prog.cls0();
    assert_eq!(1, class.fields.len());
}

#[test]
fn parse_class_with_params() {
    let prog = parse("class Foo{a: int, b: int}");
    let class = prog.cls0();

    assert_eq!(2, class.fields.len());
}

#[test]
fn parse_class() {
    let prog = parse("class Foo { a: Int64, b: Bool }");
    let class = prog.cls0();
    assert_eq!(class.fields.len(), 2);

    let prog = parse("class Foo { a: Int64, b: Bool }");
    let class = prog.cls0();
    assert_eq!(class.fields.len(), 2);

    let prog = parse("class Foo");
    let class = prog.cls0();
    assert!(class.fields.is_empty());
}

#[test]
fn parse_method_invocation() {
    let (expr, f) = parse_expr("a.foo()");
    let call = f.node(expr).to_call().unwrap();
    assert!(f.node(call.callee).is_dot());
    assert_eq!(0, call.args.len());

    let (expr, f) = parse_expr("a.foo(1)");
    let call = f.node(expr).to_call().unwrap();
    assert!(f.node(call.callee).is_dot());
    assert_eq!(1, call.args.len());

    let (expr, f) = parse_expr("a.foo(1,2)");
    let call = f.node(expr).to_call().unwrap();
    assert!(f.node(call.callee).is_dot());
    assert_eq!(2, call.args.len());
}

#[test]
fn parse_array_index() {
    let (expr, f) = parse_expr("a(b)");
    let call = f.node(expr).to_call().unwrap();
    assert_eq!("a", f.node(call.callee).to_ident().unwrap().name);
    assert_eq!(1, call.args.len());
    let index_arg = f.node(call.args[0]).to_argument().unwrap();
    assert_eq!("b", f.node(index_arg.expr).to_ident().unwrap().name);
}

#[test]
fn parse_call_with_named_arguments() {
    let (expr, f) = parse_expr("a(1, 2, x = 3, y = 4)");
    let call = f.node(expr).to_call().unwrap();
    assert!(f.node(call.callee).is_ident());
    assert_eq!(4, call.args.len());
    assert!(f.node(call.args[0]).to_argument().unwrap().name.is_none());
    assert!(f.node(call.args[1]).to_argument().unwrap().name.is_none());
    assert!(f.node(call.args[2]).to_argument().unwrap().name.is_some());
    assert!(f.node(call.args[3]).to_argument().unwrap().name.is_some());
}

#[test]
fn parse_field() {
    let prog = parse("class A { f1: int, f2: int }");
    let cls = prog.cls0();

    let f1 = cls.fields[0];
    assert_eq!("f1", id_name(&prog, prog.node(f1).to_field().unwrap().name));

    let f2 = cls.fields[1];
    assert_eq!("f2", id_name(&prog, prog.node(f2).to_field().unwrap().name));
}

#[test]
fn parse_as_expr() {
    let (expr, f) = parse_expr("a as String");
    let expr = f.node(expr).to_conv().unwrap();
    assert_eq!(true, f.node(expr.object).is_ident());
}

#[test]
fn parse_internal() {
    parse("@internal fn foo();");
}

#[test]
fn parse_function_without_body() {
    let prog = parse("fn foo();");
    let fct = prog.fct0();
    assert!(fct.block.is_none());
}

#[test]
fn parse_struct_empty() {
    let prog = parse("struct Foo {}");
    let struc = prog.struct0();
    assert_eq!(0, struc.fields.len());
    assert_eq!("Foo", id_name(&prog, struc.name));
}

#[test]
fn parse_struct_unnamed() {
    let prog = parse("struct Foo (A, B)");
    let struc = prog.struct0();
    assert_eq!(2, struc.fields.len());
    assert_eq!("Foo", id_name(&prog, struc.name));
}

#[test]
fn parse_class_unnamed() {
    let prog = parse("class Foo(A, B)");
    let cls = prog.cls0();
    assert_eq!(2, cls.fields.len());
    assert_eq!("Foo", id_name(&prog, cls.name));
}

#[test]
fn parse_struct_one_field() {
    let prog = parse(
        "struct Bar {
        f1: Foo1,
    }",
    );
    let struc = prog.struct0();
    assert_eq!(1, struc.fields.len());
    assert_eq!("Bar", id_name(&prog, struc.name));

    let f1 = struc.fields[0];
    assert_eq!("f1", id_name(&prog, prog.node(f1).to_field().unwrap().name));
}

#[test]
fn parse_struct_multiple_fields() {
    let prog = parse(
        "struct FooBar {
        fa: Foo1,
        fb: Foo2,
    }",
    );
    let struc = prog.struct0();
    assert_eq!(2, struc.fields.len());
    assert_eq!("FooBar", id_name(&prog, struc.name));

    let f1 = struc.fields[0];
    assert_eq!("fa", id_name(&prog, prog.node(f1).to_field().unwrap().name));

    let f2 = struc.fields[1];
    assert_eq!("fb", id_name(&prog, prog.node(f2).to_field().unwrap().name));
}

#[test]
fn parse_struct_with_type_params() {
    let prog = parse(
        "struct Bar[T1, T2] {
        f1: T1, f2: T2,
    }",
    );
    let struct_ = prog.struct0();
    assert_eq!(2, struct_.fields.len());
    assert_eq!("Bar", id_name(&prog, struct_.name));

    let type_params = prog
        .node(struct_.type_params.unwrap())
        .to_type_param_list()
        .unwrap();
    assert_eq!(2, type_params.params.len());
}

#[test]
fn parse_struct_lit_while() {
    let (expr, f) = parse_expr("while i < n { }");
    let while_expr = f.node(expr).to_while().unwrap();
    let bin = f.node(while_expr.cond).to_bin().unwrap();

    assert!(f.node(bin.lhs).is_ident());
    assert!(f.node(bin.rhs).is_ident());
}

#[test]
fn parse_struct_lit_if() {
    let (expr, f) = parse_expr("if i < n { }");
    let ifexpr = f.node(expr).to_if().unwrap();
    let bin = f.node(ifexpr.cond).to_bin().unwrap();

    assert!(f.node(bin.lhs).is_ident());
    assert!(f.node(bin.rhs).is_ident());
}

#[test]
fn parse_lit_float() {
    let (expr, f) = parse_expr("1.2");
    let lit = f.node(expr).to_lit_float().unwrap();
    assert_eq!("1.2", lit.value);
}

#[test]
fn parse_template() {
    let (expr, f) = parse_expr("\"a${1}b${2}c\"");
    let tmpl = f.node(expr).to_template().unwrap();
    assert_eq!(tmpl.parts.len(), 5);

    assert_eq!(
        "\"a${".to_string(),
        f.node(tmpl.parts[0]).to_lit_str().unwrap().value
    );
    assert_eq!(
        String::from("1"),
        f.node(tmpl.parts[1]).to_lit_int().unwrap().value
    );
    assert_eq!(
        "}b${".to_string(),
        f.node(tmpl.parts[2]).to_lit_str().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        f.node(tmpl.parts[3]).to_lit_int().unwrap().value
    );
    assert_eq!(
        "}c\"".to_string(),
        f.node(tmpl.parts[4]).to_lit_str().unwrap().value
    );

    let (expr, f) = parse_expr("\"a\\${1}b\"");
    assert!(f.node(expr).is_lit_str());
}

#[test]
fn parse_class_type_params() {
    let prog = parse("class Foo[T]");
    let cls = prog.cls0();

    let type_params = prog
        .node(cls.type_params.unwrap())
        .to_type_param_list()
        .unwrap();
    assert_eq!(1, type_params.params.len());
    let type_param = prog.node(type_params.params[0]).to_type_param().unwrap();
    assert_eq!("T", id_name(&prog, type_param.name));

    let prog = parse("class Foo[X]");
    let cls = prog.cls0();

    let type_params = prog
        .node(cls.type_params.unwrap())
        .to_type_param_list()
        .unwrap();
    assert_eq!(1, type_params.params.len());
    let type_param = prog.node(type_params.params[0]).to_type_param().unwrap();
    assert_eq!("X", id_name(&prog, type_param.name));
}

#[test]
fn parse_type_path() {
    let (ty, f) = parse_type("Foo::Bar::Baz");
    let ty = f.node(ty).to_regular_type().unwrap();
    let path = f.node(ty.path).to_path_data().expect("path expected");
    assert_eq!(path.segments.len(), 3);
    assert_eq!(ident_name(&f, path.segments[0]), "Foo");
    assert_eq!(ident_name(&f, path.segments[1]), "Bar");
    assert_eq!(ident_name(&f, path.segments[2]), "Baz");
}

#[test]
fn parse_multiple_class_type_params() {
    let prog = parse("class Foo[A, B]");
    let cls = prog.cls0();

    let type_params = prog
        .node(cls.type_params.unwrap())
        .to_type_param_list()
        .unwrap();
    assert_eq!(2, type_params.params.len());
    let type_param = prog.node(type_params.params[0]).to_type_param().unwrap();
    assert_eq!("A", id_name(&prog, type_param.name));
    let type_param = prog.node(type_params.params[1]).to_type_param().unwrap();
    assert_eq!("B", id_name(&prog, type_param.name));
}

#[test]
fn parse_empty_trait() {
    let prog = parse("trait Foo { }");
    let trait_ = prog.trait0();

    assert_eq!("Foo", id_name(&prog, trait_.name));
    assert_eq!(0, trait_.methods.len());
}

#[test]
fn parse_trait_with_function() {
    let prog = parse("trait Foo { fn empty(); }");
    let trait_ = prog.trait0();

    assert_eq!("Foo", id_name(&prog, trait_.name));
    assert_eq!(1, trait_.methods.len());
}

#[test]
fn parse_trait_with_bounds() {
    let prog = parse("trait Foo: A + B {}");
    let trait_ = prog.trait0();

    assert_eq!("Foo", id_name(&prog, trait_.name));
    assert_eq!(2, trait_.bounds.len());
    assert_eq!("A", tr_name(&prog, trait_.bounds[0]));
    assert_eq!("B", tr_name(&prog, trait_.bounds[1]));
}

#[test]
fn parse_trait_with_static_function() {
    let prog = parse("trait Foo { static fn empty(); }");
    let trait_ = prog.trait0();

    assert_eq!("Foo", id_name(&prog, trait_.name));
    assert_eq!(1, trait_.methods.len());
}

#[test]
fn parse_empty_impl() {
    let prog = parse("impl Foo for A {}");
    let impl_ = prog.impl0();

    assert_eq!("Foo", tr_name(&prog, impl_.trait_type.unwrap()));
    assert_eq!("A", tr_name(&prog, impl_.extended_type));
    assert_eq!(0, impl_.methods.len());
}

#[test]
fn parse_impl_with_function() {
    let prog = parse("impl Bar for B { fn foo(); }");
    let impl_ = prog.impl0();

    assert_eq!("Bar", tr_name(&prog, impl_.trait_type.unwrap()));
    assert_eq!("B", tr_name(&prog, impl_.extended_type));
    assert_eq!(1, impl_.methods.len());
}

#[test]
fn parse_impl_with_static_function() {
    let prog = parse("impl Bar for B { static fn foo(); }");
    let impl_ = prog.impl0();

    assert_eq!("Bar", tr_name(&prog, impl_.trait_type.unwrap()));
    assert_eq!("B", tr_name(&prog, impl_.extended_type));
    assert_eq!(1, impl_.methods.len());
}

#[test]
fn parse_global_let() {
    let prog = parse("let b: int = 0;");
    let global = prog.global0();

    assert_eq!("b", id_name(&prog, global.name));
    assert_eq!(false, global.mutable);
}

#[test]
fn parse_lit_char() {
    let (expr, f) = parse_expr("'a'");
    let lit = f.node(expr).to_lit_char().unwrap();

    assert_eq!("'a'", lit.value);
}

#[test]
fn parse_fct_call_with_type_param() {
    let (expr, f) = parse_expr("Array[Int]()");
    let call = f.node(expr).to_call().unwrap();
    let type_params = f.node(call.callee).to_typed_expr().unwrap();

    assert_eq!(1, type_params.args.len());

    let (expr, f) = parse_expr("Foo[Int, Long]()");
    let call = f.node(expr).to_call().unwrap();
    let type_params = f.node(call.callee).to_typed_expr().unwrap();

    assert_eq!(2, type_params.args.len());

    let (expr, f) = parse_expr("Bar[]()");
    let call = f.node(expr).to_call().unwrap();
    let type_params = f.node(call.callee).to_typed_expr().unwrap();

    assert_eq!(0, type_params.args.len());

    let (expr, f) = parse_expr("Vec()");
    let call = f.node(expr).to_call().unwrap();

    assert!(f.node(call.callee).is_ident());
}

#[test]
fn parse_call_with_path() {
    let (expr, f) = parse_expr("Foo::get()");
    let call = f.node(expr).to_call().unwrap();

    assert!(f.node(call.callee).is_path());
    assert_eq!(0, call.args.len());
}

#[test]
fn parse_fct_with_type_params() {
    let prog = parse("fn f[T]() {}");
    let fct = prog.fct0();

    let type_params = prog
        .node(fct.type_params.unwrap())
        .to_type_param_list()
        .unwrap();
    assert_eq!(1, type_params.params.len());
}

#[test]
fn parse_const() {
    let prog = parse("const x: int = 0;");
    let const_ = prog.const0();

    assert_eq!("x", id_name(&prog, const_.name));
}

#[test]
fn parse_generic_with_bound() {
    let prog = parse("class A[T: Foo]");
    let cls = prog.cls0();

    let type_params = prog
        .node(cls.type_params.unwrap())
        .to_type_param_list()
        .unwrap();
    let type_param = prog.node(type_params.params[0]).to_type_param().unwrap();
    assert_eq!(1, type_param.bounds.len());
}

#[test]
fn parse_generic_with_multiple_bounds() {
    let prog = parse("class A[T: Foo + Bar]");
    let cls = prog.cls0();

    let type_params = prog
        .node(cls.type_params.unwrap())
        .to_type_param_list()
        .unwrap();
    let type_param = prog.node(type_params.params[0]).to_type_param().unwrap();
    assert_eq!(2, type_param.bounds.len());
}

#[test]
fn parse_lambda_no_params_no_return_value() {
    let (expr, f) = parse_expr("|| {}");
    let lambda = f.node(expr).to_lambda().unwrap();

    let node = f.node(lambda.fct_id).to_function().expect("fct expected");
    assert!(node.return_type.is_none());
}

#[test]
fn parse_lambda_no_params_unit_as_return_value() {
    let (expr, f) = parse_expr("|| : () {}");
    let lambda = f.node(expr).to_lambda().unwrap();
    let node = f.node(lambda.fct_id).to_function().expect("fct expected");
    let ret = node.return_type.unwrap();

    assert!(f.node(ret).is_unit());
}

#[test]
fn parse_lambda_no_params_with_return_value() {
    let (expr, f) = parse_expr("||: A {}");
    let lambda = f.node(expr).to_lambda().unwrap();
    let node = f.node(lambda.fct_id).to_function().expect("fct expected");
    let ret = node.return_type.unwrap();

    assert_eq!("A", tr_name(&f, ret));
}

#[test]
fn parse_lambda_with_one_param() {
    let (expr, f) = parse_expr("|a: A|: B {}");
    let lambda = f.node(expr).to_lambda().unwrap();
    let node = f.node(lambda.fct_id).to_function().expect("fct expected");

    assert_eq!(1, node.params.len());

    let param = f.node(node.params[0]).to_param().unwrap();
    assert_eq!("a", pat_name(&f, param.pattern));
    assert_eq!("A", tr_name(&f, param.data_type));
    assert_eq!("B", tr_name(&f, node.return_type.unwrap()));
}

#[test]
fn parse_lambda_with_two_params() {
    let (expr, f) = parse_expr("|a: A, b: B|: C {}");
    let lambda = f.node(expr).to_lambda().unwrap();
    let node = f.node(lambda.fct_id).to_function().expect("fct expected");

    assert_eq!(2, node.params.len());

    let param = f.node(node.params[0]).to_param().unwrap();
    assert_eq!("a", pat_name(&f, param.pattern));
    assert_eq!("A", tr_name(&f, param.data_type));

    let param = f.node(node.params[1]).to_param().unwrap();
    assert_eq!("b", pat_name(&f, param.pattern));
    assert_eq!("B", tr_name(&f, param.data_type));

    assert_eq!("C", tr_name(&f, node.return_type.unwrap()));
}

#[test]
fn parse_for() {
    let (expr, f) = parse_expr("for i in a+b {}");
    assert!(f.node(expr).is_for());
}

#[test]
fn parse_new_call_ident() {
    let (expr, f) = parse_expr("i");
    assert!(f.node(expr).is_ident());
}

#[test]
fn parse_new_call_path() {
    let (expr, f) = parse_expr("Foo::bar");
    let path = f.node(expr).to_path().unwrap();
    assert!(f.node(path.lhs).is_ident());
    assert!(f.node(path.rhs).is_ident());
}

#[test]
fn parse_new_call_call() {
    let (expr, f) = parse_expr("foo(1,2)");
    let call = f.node(expr).to_call().unwrap();
    assert!(f.node(call.callee).is_ident());
    assert_eq!(call.args.len(), 2);
}

#[test]
fn parse_block() {
    let (expr, f) = parse_expr("{1}");
    assert!(
        f.node(f.node(expr).to_block().unwrap().expr.unwrap())
            .is_lit_int()
    );

    let (expr, f) = parse_expr("({}) + 1");
    assert!(f.node(expr).is_bin());

    let (expr, f) = parse_expr("1 + {}");
    assert!(f.node(expr).is_bin());
}

#[test]
fn parse_tuple() {
    let (expr, f) = parse_expr("(1,)");
    assert_eq!(f.node(expr).to_tuple().unwrap().values.len(), 1);

    let (expr, f) = parse_expr("(1)");
    assert!(f.node(expr).is_paren());

    let (expr, f) = parse_expr("(1,2,3)");
    assert_eq!(f.node(expr).to_tuple().unwrap().values.len(), 3);

    let (expr, f) = parse_expr("(1,2,3,4,)");
    assert_eq!(f.node(expr).to_tuple().unwrap().values.len(), 4);
}

#[test]
fn parse_enum() {
    let prog = parse("enum Foo { A, B, C }");
    let enum_ = prog.enum0();
    assert_eq!(enum_.variants.len(), 3);
}

#[test]
fn parse_enum_with_type_params() {
    let prog = parse("enum MyOption[T] { None, Some(T), }");
    let enum_ = prog.enum0();
    assert_eq!(enum_.variants.len(), 2);
    assert!(enum_.variants[0].fields.is_empty());
    assert_eq!(enum_.variants[1].fields.len(), 1);
}

#[test]
fn parse_module() {
    let prog = parse("mod foo { fn bar() {} fn baz() {} }");
    let module = prog.module0();
    let elements = module.elements.as_ref().unwrap();
    assert_eq!(elements.len(), 2);
    assert!(prog.node(elements[0]).to_function().is_some());
    assert!(prog.node(elements[1]).to_function().is_some());
}

#[test]
fn parse_mod_without_body() {
    let prog = parse("mod foo;");
    let module = prog.module0();
    assert!(module.elements.is_none());
}

#[test]
fn parse_match() {
    parse_expr("match x { }");
    parse_expr("match x { A(x, b) => 1, B => 2 }");
    parse_expr("match x { A(x, b) if foo => 1, B => 2 }");
    parse_expr("match x { A(x, b) => 1, B | C => 2 }");
    parse_expr("match x { A(x, b) => { 1 } B | C => { 2 } }");

    parse_with_error(
        "
            fn main() {
                match x { A(x, b) => 1, as }
            }
        ",
        vec![
            (3, 41, 2, ParseError::ExpectedPattern),
            (3, 44, 1, ParseError::ExpectedToken("=>".into())),
            (3, 44, 1, ParseError::ExpectedExpression),
        ],
    );
}

#[test]
fn parse_alias_in_trait() {
    parse(
        "trait Foo {
        type MY_TYPE;
    }",
    );
}

#[test]
fn parse_where_clauses() {
    parse(
        "
        fn f() where A: B {}
        struct F where A: B, C: D {}
        class F where A: B + C, D: E {}
        impl F for X where A: B + C + D, E: F {}
        trait F where A: B {}
        enum F where A: B + C { A, B }
    ",
    );
}

#[test]
fn parse_is() {
    parse_expr("x is Foo");
    parse_expr("x is Foo::Bar");
    parse_expr("x is Foo::Bar(a, b, c)");
    parse_expr("x is Foo && y > 0");
}

#[test]
fn parse_extern_decl() {
    parse("extern package foo;");
}

#[test]
fn parse_compound_assignments() {
    parse_expr("x += 12");
    parse_expr("x -= 12");
    parse_expr("x *= 12");
    parse_expr("x /= 12");
    parse_expr("x %= 12");
    parse_expr("x |= 12");
    parse_expr("x &= 12");
    parse_expr("x ^= 12");
    parse_expr("x <<= 12");
    parse_expr("x >>= 12");
    parse_expr("x >>>= 12");
}

#[test]
fn parse_invalid_expr_in_block() {
    parse_with_error(
        "fn f() { 0..10 }",
        vec![
            (1, 11, 2, ParseError::ExpectedToken(";".into())),
            (1, 11, 2, ParseError::ExpectedStatement),
        ],
    );
}

#[test]
fn parse_invalid_type_with_unclosed_bracket() {
    parse_with_error(
        "fn next(): Option[Self::X;",
        vec![
            (1, 26, 1, ParseError::ExpectedToken(",".into())),
            (1, 26, 1, ParseError::ExpectedType),
            (1, 27, 0, ParseError::ExpectedToken(",".into())),
            (1, 27, 0, ParseError::ExpectedToken("]".into())),
            (1, 27, 0, ParseError::ExpectedToken("{".into())),
        ],
    );
}

#[test]
fn parse_function_with_pattern_in_param() {
    parse("fn f((x, _): Foo) {}");
    parse_with_some_errors("fn next(x|y: Foo);");
}

#[test]
fn parse_qualified_type() {
    parse("fn f(x: [T as Foo]::Bar) {}");
}

fn id_name(f: &File, id: Option<AstId>) -> &str {
    &f.node(id.expect("id expected"))
        .to_ident()
        .expect("ident expected")
        .name
}

fn ident_name<'a>(f: &'a File, node_id: AstId) -> &'a str {
    let node = f.node(node_id).to_ident().expect("ident expected");
    &node.name
}

fn tr_name<'a>(f: &'a File, id: AstId) -> &'a str {
    let node = f.node(id).to_regular_type().expect("regular type expected");
    let path = f.node(node.path).to_path_data().expect("path expected");
    assert_eq!(path.segments.len(), 1);
    let segment_id = path.segments.first().cloned().expect("missing segment");
    &f.node(segment_id).to_ident().expect("ident expected").name
}

fn ta_name<'a>(f: &'a File, id: AstId) -> &'a str {
    let node = f
        .node(id)
        .to_type_argument()
        .expect("regular type expected");
    tr_name(f, node.ty)
}

fn pat_name<'a>(f: &'a File, node_id: AstId) -> &'a str {
    let ident_id = f
        .node(node_id)
        .to_ident_pattern()
        .expect("ident expected")
        .name;
    &f.node(ident_id).to_ident().expect("ident expected").name
}
