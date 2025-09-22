use id_arena::Arena;

use std::sync::Arc;

use crate::ast::*;
use crate::error::ParseError;
use crate::parser::Parser;
use crate::{compute_line_column, compute_line_starts};

fn parse_expr(code: &'static str) -> (AstId, Arena<Ast>) {
    let mut parser = Parser::from_string(code);

    let result = parser.parse_expr();
    assert!(parser.errors.is_empty());
    assert!(parser.current().is_eof());

    (result, parser.ast_nodes)
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

fn parse_let(code: &'static str) -> (AstId, Arena<Ast>) {
    let mut parser = Parser::from_string(code);
    let result = parser.parse_let();
    if !parser.errors.is_empty() {
        for err in &parser.errors {
            eprintln!(
                "error at {}: {:?} / {}",
                err.span,
                err.error,
                err.error.message()
            );
        }
    }
    assert!(parser.errors.is_empty());
    (result, parser.ast_nodes)
}

fn parse_type(code: &'static str) -> (AstId, Arena<Ast>) {
    let mut parser = Parser::from_string(code);
    (parser.parse_type(), parser.ast_nodes)
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
    let (expr, arena) = parse_expr("a");
    let ident = arena[expr].to_ident().unwrap();
    assert_eq!("a", ident.name);
}

#[test]
fn parse_number() {
    let (expr, arena) = parse_expr("10");

    let lit = arena[expr].to_lit_int().unwrap();
    assert_eq!(String::from("10"), lit.value);
}

#[test]
fn parse_number_with_underscore() {
    let (expr, arena) = parse_expr("1____0");

    let lit = arena[expr].to_lit_int().unwrap();
    assert_eq!(String::from("1____0"), lit.value);
}

#[test]
fn parse_string() {
    let (expr, arena) = parse_expr("\"abc\"");

    let lit = arena[expr].to_lit_str().unwrap();
    assert_eq!("\"abc\"", &lit.value);
}

#[test]
fn parse_true() {
    let (expr, arena) = parse_expr("true");

    let lit = arena[expr].to_lit_bool().unwrap();
    assert_eq!(true, lit.value);
}

#[test]
fn parse_false() {
    let (expr, arena) = parse_expr("true");

    let lit = arena[expr].to_lit_bool().unwrap();
    assert_eq!(true, lit.value);
}

#[test]
fn parse_field_access() {
    let (expr, arena) = parse_expr("obj.field");
    let dot = arena[expr].to_dot().unwrap();

    let ident = arena[dot.lhs].to_ident().unwrap();
    assert_eq!("obj", ident.name);

    let ident = arena[dot.rhs].to_ident().unwrap();
    assert_eq!("field", ident.name);
}

#[test]
fn parse_field_negated() {
    let (expr, arena) = parse_expr("-obj.field");
    let un = arena[expr].to_un().unwrap();
    assert!(arena[un.opnd].is_dot());
}

#[test]
fn parse_field_non_ident() {
    let (expr, arena) = parse_expr("bar.12");
    let dot = arena[expr].to_dot().unwrap();

    let ident = arena[dot.lhs].to_ident().unwrap();
    assert_eq!("bar", ident.name);

    assert_eq!(
        String::from("12"),
        arena[dot.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_self() {
    let (expr, arena) = parse_expr("self");
    assert!(arena[expr].is_this());
}

#[test]
fn parse_neg() {
    let (expr, arena) = parse_expr("-1");

    let un = arena[expr].to_un().unwrap();
    assert_eq!(UnOp::Neg, un.op);

    assert!(arena[un.opnd].is_lit_int());
}

#[test]
fn parse_neg_twice() {
    let (expr, arena) = parse_expr("-(-3)");

    let neg1 = arena[expr].to_un().unwrap();
    assert_eq!(UnOp::Neg, neg1.op);

    let paren = arena[neg1.opnd].to_paren().unwrap();
    let neg2 = arena[paren.expr].to_un().unwrap();
    assert_eq!(UnOp::Neg, neg2.op);

    assert!(arena[neg2.opnd].is_lit_int());
}

#[test]
fn parse_neg_twice_without_parentheses() {
    err_expr("- -2", ParseError::ExpectedFactor, 1, 3);
}

#[test]
fn parse_mul() {
    let (expr, arena) = parse_expr("6*3");

    let mul = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Mul, mul.op);
    assert_eq!(
        String::from("6"),
        arena[mul.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("3"),
        arena[mul.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_multiple_muls() {
    let (expr, arena) = parse_expr("6*3*4");

    let mul1 = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Mul, mul1.op);

    let mul2 = arena[mul1.lhs].to_bin().unwrap();
    assert_eq!(BinOp::Mul, mul2.op);
    assert_eq!(
        String::from("6"),
        arena[mul2.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("3"),
        arena[mul2.rhs].to_lit_int().unwrap().value
    );

    assert_eq!(
        String::from("4"),
        arena[mul1.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_div() {
    let (expr, arena) = parse_expr("4/5");

    let div = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Div, div.op);
    assert_eq!(
        String::from("4"),
        arena[div.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("5"),
        arena[div.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_mod() {
    let (expr, arena) = parse_expr("2%15");

    let div = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Mod, div.op);
    assert_eq!(
        String::from("2"),
        arena[div.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("15"),
        arena[div.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_add() {
    let (expr, arena) = parse_expr("2+3");

    let add = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Add, add.op);
    assert_eq!(
        String::from("2"),
        arena[add.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("3"),
        arena[add.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_add_left_associativity() {
    let (expr, arena) = parse_expr("1+2+3");

    let add = arena[expr].to_bin().unwrap();
    assert_eq!(
        String::from("3"),
        arena[add.rhs].to_lit_int().unwrap().value
    );

    let lhs = arena[add.lhs].to_bin().unwrap();
    assert_eq!(
        String::from("1"),
        arena[lhs.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[lhs.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_add_right_associativity_via_parens() {
    let (expr, arena) = parse_expr("1+(2+3)");

    let add = arena[expr].to_bin().unwrap();
    assert_eq!(
        String::from("1"),
        arena[add.lhs].to_lit_int().unwrap().value
    );

    let paren = arena[add.rhs].to_paren().unwrap();
    let rhs = arena[paren.expr].to_bin().unwrap();
    assert_eq!(
        String::from("2"),
        arena[rhs.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("3"),
        arena[rhs.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_sub() {
    let (expr, arena) = parse_expr("1-2");

    let add = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Sub, add.op);
    assert_eq!(
        String::from("1"),
        arena[add.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[add.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_or() {
    let (expr, arena) = parse_expr("1||2");

    let add = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Or, add.op);
    assert_eq!(
        String::from("1"),
        arena[add.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[add.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_and() {
    let (expr, arena) = parse_expr("1&&2");

    let add = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::And, add.op);
    assert_eq!(
        String::from("1"),
        arena[add.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[add.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_bit_or() {
    let (expr, arena) = parse_expr("1|2");

    let or = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::BitOr, or.op);
    assert_eq!(String::from("1"), arena[or.lhs].to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), arena[or.rhs].to_lit_int().unwrap().value);
}

#[test]
fn parse_bit_and() {
    let (expr, arena) = parse_expr("1&2");

    let and = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::BitAnd, and.op);
    assert_eq!(
        String::from("1"),
        arena[and.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[and.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_bit_xor() {
    let (expr, arena) = parse_expr("1^2");

    let xor = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::BitXor, xor.op);
    assert_eq!(
        String::from("1"),
        arena[xor.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[xor.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_lt() {
    let (expr, arena) = parse_expr("1<2");

    let cmp = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Lt), cmp.op);
    assert_eq!(
        String::from("1"),
        arena[cmp.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[cmp.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_le() {
    let (expr, arena) = parse_expr("1<=2");

    let cmp = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Le), cmp.op);
    assert_eq!(
        String::from("1"),
        arena[cmp.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[cmp.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_gt() {
    let (expr, arena) = parse_expr("1>2");

    let cmp = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Gt), cmp.op);
    assert_eq!(
        String::from("1"),
        arena[cmp.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[cmp.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_ge() {
    let (expr, arena) = parse_expr("1>=2");

    let cmp = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Ge), cmp.op);
    assert_eq!(
        String::from("1"),
        arena[cmp.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[cmp.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_eq() {
    let (expr, arena) = parse_expr("1==2");

    let cmp = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Eq), cmp.op);
    assert_eq!(
        String::from("1"),
        arena[cmp.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[cmp.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_ne() {
    let (expr, arena) = parse_expr("1!=2");

    let cmp = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Ne), cmp.op);
    assert_eq!(
        String::from("1"),
        arena[cmp.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[cmp.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_identity_not() {
    let (expr, arena) = parse_expr("1!==2");

    let cmp = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::IsNot), cmp.op);
    assert_eq!(
        String::from("1"),
        arena[cmp.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[cmp.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_identity() {
    let (expr, arena) = parse_expr("1===2");

    let cmp = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Is), cmp.op);
    assert_eq!(
        String::from("1"),
        arena[cmp.lhs].to_lit_int().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[cmp.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_assign() {
    let (expr, arena) = parse_expr("a=4");

    let assign = arena[expr].to_bin().unwrap();
    assert!(arena[assign.lhs].is_ident());
    assert_eq!(BinOp::Assign, assign.op);
    assert_eq!(
        String::from("4"),
        arena[assign.rhs].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_shift_right() {
    let (expr, arena) = parse_expr("a>>4");

    let bin = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::ArithShiftR, bin.op);
}

#[test]
fn parse_unsigned_shift_right() {
    let (expr, arena) = parse_expr("a>>>4");

    let bin = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::LogicalShiftR, bin.op);
}

#[test]
fn parse_left() {
    let (expr, arena) = parse_expr("a<<4");

    let bin = arena[expr].to_bin().unwrap();
    assert_eq!(BinOp::ShiftL, bin.op);
}

#[test]
fn parse_call_without_params() {
    let (expr, arena) = parse_expr("fname()");

    let call = arena[expr].to_call().unwrap();
    assert_eq!("fname", arena[call.callee].to_ident().unwrap().name);
    assert_eq!(0, call.args.len());
}

#[test]
fn parse_call_with_params() {
    let (expr, arena) = parse_expr("fname2(1,2,3)");

    let call = arena[expr].to_call().unwrap();
    assert_eq!("fname2", arena[call.callee].to_ident().unwrap().name);
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

    assert_eq!("a", pat_name(&file1, &p1.pattern));
    assert_eq!("a", pat_name(&file2, &p2.pattern));

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

    assert_eq!("a", pat_name(&file1, &p1a.pattern));
    assert_eq!("a", pat_name(&file2, &p2a.pattern));

    assert_eq!("b", pat_name(&file1, &p1b.pattern));
    assert_eq!("b", pat_name(&file2, &p2b.pattern));

    assert_eq!("int", tr_name(&file1, p1a.data_type));
    assert_eq!("int", tr_name(&file2, p2a.data_type));

    assert_eq!("str", tr_name(&file1, p1b.data_type));
    assert_eq!("str", tr_name(&file2, p2b.data_type));
}

#[test]
fn parse_let_without_type() {
    let (stmt, arena) = parse_let("let a = 1;");
    let var = arena[stmt].to_let().unwrap();

    assert!(var.data_type.is_none());
    assert!(arena[var.expr.unwrap()].is_lit_int());
}

#[test]
fn parse_let_rest() {
    let (stmt, arena) = parse_let("let .. = 1;");
    let var = arena[stmt].to_let().unwrap();
    assert!(var.pattern.is_rest());

    assert!(var.data_type.is_none());
    assert!(arena[var.expr.unwrap()].is_lit_int());
}

#[test]
fn parse_let_with_type() {
    let (stmt, arena) = parse_let("let x : int = 1;");
    let var = arena[stmt].to_let().unwrap();

    assert!(var.data_type.is_some());
    assert!(arena[var.expr.unwrap()].is_lit_int());
}

#[test]
fn parse_let_underscore() {
    let (stmt, arena) = parse_let("let _ = 1;");
    let let_decl = arena[stmt].to_let().unwrap();
    assert!(let_decl.pattern.is_underscore());
}

#[test]
fn parse_let_tuple() {
    let (stmt, arena) = parse_let("let (mut a, b, (c, d)) = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    let tuple = let_decl.pattern.to_tuple().unwrap();
    let first: &Arc<Pattern> = &tuple.params.first().unwrap();
    assert!(first.is_ident());
    assert!(first.to_ident().unwrap().mutable);
    let last = &tuple.params.last().unwrap();
    assert!(last.is_tuple());
}

#[test]
fn parse_let_lit_bool() {
    let (stmt, arena) = parse_let("let (a, true) = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    let tuple = let_decl.pattern.to_tuple().unwrap();
    assert!(tuple.params[0].is_ident());
    assert!(tuple.params[1].is_lit_bool());
}

#[test]
fn parse_let_lit_char() {
    let (stmt, arena) = parse_let("let (a, 'x') = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    let tuple = let_decl.pattern.to_tuple().unwrap();
    assert!(tuple.params[0].is_ident());
    assert!(tuple.params[1].is_lit_char());
}

#[test]
fn parse_let_lit_string() {
    let (stmt, arena) = parse_let("let (a, \"x\") = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    let tuple = let_decl.pattern.to_tuple().unwrap();
    assert!(tuple.params[0].is_ident());
    assert!(tuple.params[1].is_lit_string());
}

#[test]
fn parse_let_lit_int() {
    let (stmt, arena) = parse_let("let (a, 17) = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    let tuple = let_decl.pattern.to_tuple().unwrap();
    assert!(tuple.params[0].is_ident());
    assert!(tuple.params[1].is_lit_int());
}

#[test]
fn parse_let_lit_int_neg() {
    let (stmt, arena) = parse_let("let (a, -17) = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    let tuple = let_decl.pattern.to_tuple().unwrap();
    assert!(tuple.params[0].is_ident());
    assert!(tuple.params[1].is_lit_int());
}

#[test]
fn parse_let_lit_float() {
    let (stmt, arena) = parse_let("let (a, 17.5) = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    let tuple = let_decl.pattern.to_tuple().unwrap();
    assert!(tuple.params[0].is_ident());
    assert!(tuple.params[1].is_lit_float());
}

#[test]
fn parse_let_lit_float_neg() {
    let (stmt, arena) = parse_let("let (a, -17.5) = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    let tuple = let_decl.pattern.to_tuple().unwrap();
    assert!(tuple.params[0].is_ident());
    assert!(tuple.params[1].is_lit_float());
}

#[test]
fn parse_let_ident() {
    let (stmt, arena) = parse_let("let x = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    assert!(let_decl.pattern.is_ident());
}

#[test]
fn parse_let_ident_mut() {
    let (stmt, arena) = parse_let("let mut x = 1;");
    let let_decl = arena[stmt].to_let().unwrap();

    assert!(let_decl.pattern.to_ident().unwrap().mutable);
}

#[test]
fn parse_let_with_type_but_without_assignment() {
    let (stmt, arena) = parse_let("let x : int;");
    let var = arena[stmt].to_let().unwrap();

    assert!(var.data_type.is_some());
    assert!(var.expr.is_none());
}

#[test]
fn parse_let_without_type_and_assignment() {
    let (stmt, arena) = parse_let("let x;");
    let var = arena[stmt].to_let().unwrap();

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
    let (expr, arena) = parse_expr("if true { 2; } else { 3; }");
    let ifexpr = arena[expr].to_if().unwrap();

    assert!(arena[ifexpr.cond].is_lit_bool());
    assert!(ifexpr.else_block.is_some());
}

#[test]
fn parse_if_without_else() {
    let (expr, arena) = parse_expr("if true { 2; }");
    let ifexpr = arena[expr].to_if().unwrap();

    assert!(arena[ifexpr.cond].is_lit_bool());
    assert!(ifexpr.else_block.is_none());
}

#[test]
fn parse_while() {
    let (expr, arena) = parse_expr("while true { 2; }");
    let whilestmt = arena[expr].to_while().unwrap();

    assert!(arena[whilestmt.cond].is_lit_bool());
    assert!(arena[whilestmt.block].is_block());
}

#[test]
fn parse_empty_block() {
    let (expr, arena) = parse_expr("{}");
    let block = arena[expr].to_block().unwrap();

    assert_eq!(0, block.stmts.len());
}

#[test]
fn parse_block_with_one_stmt() {
    let (expr, arena) = parse_expr("{ 1; 2 }");
    let block = arena[expr].to_block().unwrap();

    assert_eq!(1, block.stmts.len());

    let expr = arena[block.stmts[0]].to_expr().unwrap().expr;
    assert_eq!(String::from("1"), arena[expr].to_lit_int().unwrap().value);

    assert_eq!(
        String::from("2"),
        arena[block.expr.unwrap()].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_block_with_multiple_stmts() {
    let (expr, arena) = parse_expr("{ 1; 2; }");
    let block = arena[expr].to_block().unwrap();

    assert_eq!(2, block.stmts.len());

    let expr = arena[block.stmts[0]].to_expr().unwrap().expr;
    assert_eq!(String::from("1"), arena[expr].to_lit_int().unwrap().value);

    let expr = arena[block.stmts[1]].to_expr().unwrap().expr;
    assert_eq!(String::from("2"), arena[expr].to_lit_int().unwrap().value);

    assert!(block.expr.is_none());
}

#[test]
fn parse_break() {
    let (expr, arena) = parse_expr("break");
    assert!(arena[expr].is_break());
}

#[test]
fn parse_continue() {
    let (expr, arena) = parse_expr("continue");
    assert!(arena[expr].is_continue());
}

#[test]
fn parse_return_value() {
    let (expr, arena) = parse_expr("return 1");
    let ret = arena[expr].to_return().unwrap();

    assert_eq!(
        String::from("1"),
        arena[ret.expr.unwrap()].to_lit_int().unwrap().value
    );
}

#[test]
fn parse_return() {
    let (expr, arena) = parse_expr("return");
    let ret = arena[expr].to_return().unwrap();

    assert!(ret.expr.is_none());
}

#[test]
fn parse_type_regular() {
    let (ty_id, arena) = parse_type("bla");
    let ty = arena[ty_id].to_regular().unwrap();

    assert_eq!(0, ty.params.len());
    assert_eq!("bla", tra_name(&arena, ty_id));
}

#[test]
fn parse_type_regular_mod() {
    let (ty, arena) = parse_type("foo::bla");
    let regular = arena[ty].to_regular().unwrap();

    assert_eq!(0, regular.params.len());
    assert_eq!(2, regular.path.segments.len());
    assert_eq!("foo", psa_name(&arena, &regular.path.segments[0]));
    assert_eq!("bla", psa_name(&arena, &regular.path.segments[1]));
}

#[test]
fn parse_type_regular_with_params() {
    let (ty_id, arena) = parse_type("Foo[A, B]");
    let regular = arena[ty_id].to_regular().unwrap();

    assert_eq!(2, regular.params.len());
    assert_eq!("Foo", tra_name(&arena, ty_id));
    assert_eq!("A", tra_name(&arena, regular.params[0].ty));
    assert_eq!("B", tra_name(&arena, regular.params[1].ty));
}

#[test]
fn parse_type_regular_with_bindings() {
    let (ty_id, arena) = parse_type("Foo[A, X = B]");
    let ty = arena[ty_id].to_regular().unwrap();

    assert_eq!(2, ty.params.len());
    assert_eq!("Foo", tra_name(&arena, ty_id));
    let arg0 = &ty.params[0];
    assert!(arg0.name.is_none());
    assert_eq!("A", tra_name(&arena, arg0.ty));

    let arg1 = &ty.params[1];
    assert_eq!("X", ida_name(&arena, arg1.name));
    assert_eq!("B", tra_name(&arena, arg1.ty));
}

#[test]
fn parse_type_lambda_no_params() {
    let (ty, arena) = parse_type("(): ()");
    let fct = arena[ty].to_fct().unwrap();

    assert_eq!(0, fct.params.len());
    assert!(arena[fct.ret.unwrap()].is_unit());
}

#[test]
fn parse_type_lambda_one_param() {
    let (ty_id, arena) = parse_type("(A): B");
    let fct = arena[ty_id].to_fct().unwrap();

    assert_eq!(1, fct.params.len());
    assert_eq!("A", tra_name(&arena, fct.params[0]));
    assert_eq!("B", tra_name(&arena, fct.ret.unwrap()));
}

#[test]
fn parse_type_lambda_two_params() {
    let (ty_id, arena) = parse_type("(A, B): C");
    let fct = arena[ty_id].to_fct().unwrap();

    assert_eq!(2, fct.params.len());
    assert_eq!("A", tra_name(&arena, fct.params[0]));
    assert_eq!("B", tra_name(&arena, fct.params[1]));
    assert_eq!("C", tra_name(&arena, fct.ret.unwrap()));
}

#[test]
fn parse_type_unit() {
    let (ty, arena) = parse_type("()");
    let ty = arena[ty].to_tuple_type().unwrap();

    assert!(ty.subtypes.is_empty());
}

#[test]
fn parse_type_tuple_with_one_type() {
    let (ty, arena) = parse_type("(c)");

    let subtypes = &arena[ty].to_tuple_type().unwrap().subtypes;
    assert_eq!(1, subtypes.len());

    assert_eq!("c", tra_name(&arena, subtypes[0]));
}

#[test]
fn parse_type_tuple_with_two_types() {
    let (ty, arena) = parse_type("(a, b)");

    let subtypes = &arena[ty].to_tuple_type().unwrap().subtypes;
    assert_eq!(2, subtypes.len());
    assert_eq!("a", tra_name(&arena, subtypes[0]));
    assert_eq!("b", tra_name(&arena, subtypes[1]));
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
    let (expr, arena) = parse_expr("a.foo()");
    let call = arena[expr].to_call().unwrap();
    assert!(arena[call.callee].is_dot());
    assert_eq!(0, call.args.len());

    let (expr, arena) = parse_expr("a.foo(1)");
    let call = arena[expr].to_call().unwrap();
    assert!(arena[call.callee].is_dot());
    assert_eq!(1, call.args.len());

    let (expr, arena) = parse_expr("a.foo(1,2)");
    let call = arena[expr].to_call().unwrap();
    assert!(arena[call.callee].is_dot());
    assert_eq!(2, call.args.len());
}

#[test]
fn parse_array_index() {
    let (expr, arena) = parse_expr("a(b)");
    let call = arena[expr].to_call().unwrap();
    assert_eq!("a", arena[call.callee].to_ident().unwrap().name);
    assert_eq!(1, call.args.len());
    let index_arg = arena[call.args[0]].to_argument().unwrap();
    assert_eq!("b", arena[index_arg.expr].to_ident().unwrap().name);
}

#[test]
fn parse_call_with_named_arguments() {
    let (expr, arena) = parse_expr("a(1, 2, x = 3, y = 4)");
    let call = arena[expr].to_call().unwrap();
    assert!(arena[call.callee].is_ident());
    assert_eq!(4, call.args.len());
    assert!(arena[call.args[0]].to_argument().unwrap().name.is_none());
    assert!(arena[call.args[1]].to_argument().unwrap().name.is_none());
    assert!(arena[call.args[2]].to_argument().unwrap().name.is_some());
    assert!(arena[call.args[3]].to_argument().unwrap().name.is_some());
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
    let (expr, arena) = parse_expr("a as String");
    let expr = arena[expr].to_conv().unwrap();
    assert_eq!(true, arena[expr.object].is_ident());
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

    assert_eq!(2, struct_.type_params.as_ref().unwrap().params.len());
}

#[test]
fn parse_struct_lit_while() {
    let (expr, arena) = parse_expr("while i < n { }");
    let while_expr = arena[expr].to_while().unwrap();
    let bin = arena[while_expr.cond].to_bin().unwrap();

    assert!(arena[bin.lhs].is_ident());
    assert!(arena[bin.rhs].is_ident());
}

#[test]
fn parse_struct_lit_if() {
    let (expr, arena) = parse_expr("if i < n { }");
    let ifexpr = arena[expr].to_if().unwrap();
    let bin = arena[ifexpr.cond].to_bin().unwrap();

    assert!(arena[bin.lhs].is_ident());
    assert!(arena[bin.rhs].is_ident());
}

#[test]
fn parse_lit_float() {
    let (expr, arena) = parse_expr("1.2");
    let lit = arena[expr].to_lit_float().unwrap();
    assert_eq!("1.2", lit.value);
}

#[test]
fn parse_template() {
    let (expr, arena) = parse_expr("\"a${1}b${2}c\"");
    let tmpl = arena[expr].to_template().unwrap();
    assert_eq!(tmpl.parts.len(), 5);

    assert_eq!(
        "\"a${".to_string(),
        arena[tmpl.parts[0]].to_lit_str().unwrap().value
    );
    assert_eq!(
        String::from("1"),
        arena[tmpl.parts[1]].to_lit_int().unwrap().value
    );
    assert_eq!(
        "}b${".to_string(),
        arena[tmpl.parts[2]].to_lit_str().unwrap().value
    );
    assert_eq!(
        String::from("2"),
        arena[tmpl.parts[3]].to_lit_int().unwrap().value
    );
    assert_eq!(
        "}c\"".to_string(),
        arena[tmpl.parts[4]].to_lit_str().unwrap().value
    );

    let (expr, arena) = parse_expr("\"a\\${1}b\"");
    assert!(arena[expr].is_lit_str());
}

#[test]
fn parse_class_type_params() {
    let prog = parse("class Foo[T]");
    let cls = prog.cls0();

    let type_params = cls.type_params.as_ref().unwrap();
    assert_eq!(1, type_params.params.len());
    assert_eq!("T", id_name(&prog, type_params.params[0].name));

    let prog = parse("class Foo[X]");
    let cls = prog.cls0();

    let type_params = cls.type_params.as_ref().unwrap();
    assert_eq!(1, type_params.params.len());
    assert_eq!("X", id_name(&prog, type_params.params[0].name));
}

#[test]
fn parse_type_path() {
    let (ty, arena) = parse_type("Foo::Bar::Baz");
    let ty = arena[ty].to_regular().unwrap();
    assert_eq!(ty.path.segments.len(), 3);
    assert_eq!(psa_name(&arena, &ty.path.segments[0]), "Foo");
    assert_eq!(psa_name(&arena, &ty.path.segments[1]), "Bar");
    assert_eq!(psa_name(&arena, &ty.path.segments[2]), "Baz");
}

#[test]
fn parse_multiple_class_type_params() {
    let prog = parse("class Foo[A, B]");
    let cls = prog.cls0();

    let type_params = cls.type_params.as_ref().unwrap();
    assert_eq!(2, type_params.params.len());
    assert_eq!("A", id_name(&prog, type_params.params[0].name));
    assert_eq!("B", id_name(&prog, type_params.params[1].name));
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
    let (expr, arena) = parse_expr("'a'");
    let lit = arena[expr].to_lit_char().unwrap();

    assert_eq!("'a'", lit.value);
}

#[test]
fn parse_fct_call_with_type_param() {
    let (expr, arena) = parse_expr("Array[Int]()");
    let call = arena[expr].to_call().unwrap();
    let type_params = arena[call.callee].to_type_param().unwrap();

    assert_eq!(1, type_params.args.len());

    let (expr, arena) = parse_expr("Foo[Int, Long]()");
    let call = arena[expr].to_call().unwrap();
    let type_params = arena[call.callee].to_type_param().unwrap();

    assert_eq!(2, type_params.args.len());

    let (expr, arena) = parse_expr("Bar[]()");
    let call = arena[expr].to_call().unwrap();
    let type_params = arena[call.callee].to_type_param().unwrap();

    assert_eq!(0, type_params.args.len());

    let (expr, arena) = parse_expr("Vec()");
    let call = arena[expr].to_call().unwrap();

    assert!(arena[call.callee].is_ident());
}

#[test]
fn parse_call_with_path() {
    let (expr, arena) = parse_expr("Foo::get()");
    let call = arena[expr].to_call().unwrap();

    assert!(arena[call.callee].is_path());
    assert_eq!(0, call.args.len());
}

#[test]
fn parse_fct_with_type_params() {
    let prog = parse("fn f[T]() {}");
    let fct = prog.fct0();

    assert_eq!(1, fct.type_params.as_ref().unwrap().params.len());
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

    let type_param = &cls.type_params.as_ref().unwrap().params[0];
    assert_eq!(1, type_param.bounds.len());
}

#[test]
fn parse_generic_with_multiple_bounds() {
    let prog = parse("class A[T: Foo + Bar]");
    let cls = prog.cls0();

    let type_param = &cls.type_params.as_ref().unwrap().params[0];
    assert_eq!(2, type_param.bounds.len());
}

#[test]
fn parse_lambda_no_params_no_return_value() {
    let (expr, arena) = parse_expr("|| {}");
    let lambda = arena[expr].to_lambda().unwrap();

    let node = arena[lambda.fct_id].to_function().expect("fct expected");
    assert!(node.return_type.is_none());
}

#[test]
fn parse_lambda_no_params_unit_as_return_value() {
    let (expr, arena) = parse_expr("|| : () {}");
    let lambda = arena[expr].to_lambda().unwrap();
    let node = arena[lambda.fct_id].to_function().expect("fct expected");
    let ret = node.return_type.unwrap();

    assert!(arena[ret].is_unit());
}

#[test]
fn parse_lambda_no_params_with_return_value() {
    let (expr, arena) = parse_expr("||: A {}");
    let lambda = arena[expr].to_lambda().unwrap();
    let node = arena[lambda.fct_id].to_function().expect("fct expected");
    let ret = node.return_type.unwrap();

    assert_eq!("A", tra_name(&arena, ret));
}

#[test]
fn parse_lambda_with_one_param() {
    let (expr, arena) = parse_expr("|a: A|: B {}");
    let lambda = arena[expr].to_lambda().unwrap();
    let node = arena[lambda.fct_id].to_function().expect("fct expected");

    assert_eq!(1, node.params.len());

    let param = arena[node.params[0]].to_param().unwrap();
    assert_eq!("a", pata_name(&arena, &param.pattern));
    assert_eq!("A", tra_name(&arena, param.data_type));
    assert_eq!("B", tra_name(&arena, node.return_type.unwrap()));
}

#[test]
fn parse_lambda_with_two_params() {
    let (expr, arena) = parse_expr("|a: A, b: B|: C {}");
    let lambda = arena[expr].to_lambda().unwrap();
    let node = arena[lambda.fct_id].to_function().expect("fct expected");

    assert_eq!(2, node.params.len());

    let param = arena[node.params[0]].to_param().unwrap();
    assert_eq!("a", pata_name(&arena, &param.pattern));
    assert_eq!("A", tra_name(&arena, param.data_type));

    let param = arena[node.params[1]].to_param().unwrap();
    assert_eq!("b", pata_name(&arena, &param.pattern));
    assert_eq!("B", tra_name(&arena, param.data_type));

    assert_eq!("C", tra_name(&arena, node.return_type.unwrap()));
}

#[test]
fn parse_for() {
    let (expr, arena) = parse_expr("for i in a+b {}");
    assert!(arena[expr].is_for());
}

#[test]
fn parse_new_call_ident() {
    let (expr, arena) = parse_expr("i");
    assert!(arena[expr].is_ident());
}

#[test]
fn parse_new_call_path() {
    let (expr, arena) = parse_expr("Foo::bar");
    let path = arena[expr].to_path().unwrap();
    assert!(arena[path.lhs].is_ident());
    assert!(arena[path.rhs].is_ident());
}

#[test]
fn parse_new_call_call() {
    let (expr, arena) = parse_expr("foo(1,2)");
    let call = arena[expr].to_call().unwrap();
    assert!(arena[call.callee].is_ident());
    assert_eq!(call.args.len(), 2);
}

#[test]
fn parse_block() {
    let (expr, arena) = parse_expr("{1}");
    assert!(arena[arena[expr].to_block().unwrap().expr.unwrap()].is_lit_int());

    let (expr, arena) = parse_expr("({}) + 1");
    assert!(arena[expr].is_bin());

    let (expr, arena) = parse_expr("1 + {}");
    assert!(arena[expr].is_bin());
}

#[test]
fn parse_tuple() {
    let (expr, arena) = parse_expr("(1,)");
    assert_eq!(arena[expr].to_tuple().unwrap().values.len(), 1);

    let (expr, arena) = parse_expr("(1)");
    assert!(arena[expr].is_paren());

    let (expr, arena) = parse_expr("(1,2,3)");
    assert_eq!(arena[expr].to_tuple().unwrap().values.len(), 3);

    let (expr, arena) = parse_expr("(1,2,3,4,)");
    assert_eq!(arena[expr].to_tuple().unwrap().values.len(), 4);
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

fn ida_name(arena: &Arena<Ast>, id: Option<AstId>) -> &str {
    &arena[id.expect("id expected")]
        .to_ident()
        .expect("ident expected")
        .name
}

fn tr_name<'a>(f: &'a File, id: AstId) -> &'a str {
    let node = f.node(id).to_regular().expect("regular type expected");
    assert_eq!(node.path.segments.len(), 1);
    let ident_id = node
        .path
        .segments
        .last()
        .expect("missing segment")
        .to_ident()
        .expect("ident expected")
        .name;
    &f.node(ident_id).to_ident().expect("ident expected").name
}

fn tra_name<'a>(arena: &'a Arena<Ast>, id: AstId) -> &'a str {
    let node = arena[id].to_regular().expect("regular type expected");
    assert_eq!(node.path.segments.len(), 1);
    let ident_id = node
        .path
        .segments
        .last()
        .expect("missing segment")
        .to_ident()
        .expect("ident expected")
        .name;
    &arena[ident_id].to_ident().expect("ident expected").name
}

fn psa_name<'a>(arena: &'a Arena<Ast>, node: &PathSegmentData) -> &'a str {
    let node = node.to_ident().expect("ident expected");
    &arena[node.name].to_ident().expect("ident expected").name
}

fn pat_name<'a>(f: &'a File, node: &Pattern) -> &'a str {
    let ident_id = node.to_ident().expect("ident expected").name;
    &f.node(ident_id).to_ident().expect("ident expected").name
}

fn pata_name<'a>(arena: &'a Arena<Ast>, node: &Pattern) -> &'a str {
    let ident_id = node.to_ident().expect("ident expected").name;
    &arena[ident_id].to_ident().expect("ident expected").name
}
