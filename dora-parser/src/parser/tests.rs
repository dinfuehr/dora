use crate::ast::*;
use std::sync::Arc;

use crate::error::ParseError;
use crate::parser::Parser;
use crate::{compute_line_column, compute_line_starts};

fn parse_expr(code: &'static str) -> Expr {
    let mut parser = Parser::from_string(code);

    let result = parser.parse_expr();
    assert!(parser.errors.is_empty());

    result
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

fn parse_let(code: &'static str) -> Stmt {
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
    result
}

fn parse_type(code: &'static str) -> Type {
    let mut parser = Parser::from_string(code);
    parser.parse_type()
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

#[test]
fn parse_ident() {
    let expr = parse_expr("a");
    let ident = expr.to_ident().unwrap();
    assert_eq!("a", ident.name);
}

#[test]
fn parse_number() {
    let expr = parse_expr("10");

    let lit = expr.to_lit_int().unwrap();
    assert_eq!(String::from("10"), lit.value);
}

#[test]
fn parse_number_with_underscore() {
    let expr = parse_expr("1____0");

    let lit = expr.to_lit_int().unwrap();
    assert_eq!(String::from("1____0"), lit.value);
}

#[test]
fn parse_string() {
    let expr = parse_expr("\"abc\"");

    let lit = expr.to_lit_str().unwrap();
    assert_eq!("\"abc\"", &lit.value);
}

#[test]
fn parse_true() {
    let expr = parse_expr("true");

    let lit = expr.to_lit_bool().unwrap();
    assert_eq!(true, lit.value);
}

#[test]
fn parse_false() {
    let expr = parse_expr("true");

    let lit = expr.to_lit_bool().unwrap();
    assert_eq!(true, lit.value);
}

#[test]
fn parse_field_access() {
    let expr = parse_expr("obj.field");
    let dot = expr.to_dot().unwrap();

    let ident = dot.lhs.to_ident().unwrap();
    assert_eq!("obj", ident.name);

    let ident = dot.rhs.to_ident().unwrap();
    assert_eq!("field", ident.name);
}

#[test]
fn parse_field_negated() {
    let expr = parse_expr("-obj.field");
    assert!(expr.to_un().unwrap().opnd.is_dot());
}

#[test]
fn parse_field_non_ident() {
    let expr = parse_expr("bar.12");
    let dot = expr.to_dot().unwrap();

    let ident = dot.lhs.to_ident().unwrap();
    assert_eq!("bar", ident.name);

    assert_eq!(String::from("12"), dot.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_self() {
    let expr = parse_expr("self");

    assert!(expr.is_this());
}

#[test]
fn parse_neg() {
    let expr = parse_expr("-1");

    let un = expr.to_un().unwrap();
    assert_eq!(UnOp::Neg, un.op);

    assert!(un.opnd.is_lit_int());
}

#[test]
fn parse_neg_twice() {
    let expr = parse_expr("-(-3)");

    let neg1 = expr.to_un().unwrap();
    assert_eq!(UnOp::Neg, neg1.op);

    let neg2 = neg1.opnd.to_paren().unwrap().expr.to_un().unwrap();
    assert_eq!(UnOp::Neg, neg2.op);

    assert!(neg2.opnd.is_lit_int());
}

#[test]
fn parse_neg_twice_without_parentheses() {
    err_expr("- -2", ParseError::ExpectedFactor, 1, 3);
}

#[test]
fn parse_mul() {
    let expr = parse_expr("6*3");

    let mul = expr.to_bin().unwrap();
    assert_eq!(BinOp::Mul, mul.op);
    assert_eq!(String::from("6"), mul.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("3"), mul.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_multiple_muls() {
    let expr = parse_expr("6*3*4");

    let mul1 = expr.to_bin().unwrap();
    assert_eq!(BinOp::Mul, mul1.op);

    let mul2 = mul1.lhs.to_bin().unwrap();
    assert_eq!(BinOp::Mul, mul2.op);
    assert_eq!(String::from("6"), mul2.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("3"), mul2.rhs.to_lit_int().unwrap().value);

    assert_eq!(String::from("4"), mul1.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_div() {
    let expr = parse_expr("4/5");

    let div = expr.to_bin().unwrap();
    assert_eq!(BinOp::Div, div.op);
    assert_eq!(String::from("4"), div.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("5"), div.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_mod() {
    let expr = parse_expr("2%15");

    let div = expr.to_bin().unwrap();
    assert_eq!(BinOp::Mod, div.op);
    assert_eq!(String::from("2"), div.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("15"), div.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_add() {
    let expr = parse_expr("2+3");

    let add = expr.to_bin().unwrap();
    assert_eq!(BinOp::Add, add.op);
    assert_eq!(String::from("2"), add.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("3"), add.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_add_left_associativity() {
    let expr = parse_expr("1+2+3");

    let add = expr.to_bin().unwrap();
    assert_eq!(String::from("3"), add.rhs.to_lit_int().unwrap().value);

    let lhs = add.lhs.to_bin().unwrap();
    assert_eq!(String::from("1"), lhs.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), lhs.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_add_right_associativity_via_parens() {
    let expr = parse_expr("1+(2+3)");

    let add = expr.to_bin().unwrap();
    assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);

    let rhs = add.rhs.to_paren().unwrap().expr.to_bin().unwrap();
    assert_eq!(String::from("2"), rhs.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("3"), rhs.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_sub() {
    let expr = parse_expr("1-2");

    let add = expr.to_bin().unwrap();
    assert_eq!(BinOp::Sub, add.op);
    assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_or() {
    let expr = parse_expr("1||2");

    let add = expr.to_bin().unwrap();
    assert_eq!(BinOp::Or, add.op);
    assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_and() {
    let expr = parse_expr("1&&2");

    let add = expr.to_bin().unwrap();
    assert_eq!(BinOp::And, add.op);
    assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_bit_or() {
    let expr = parse_expr("1|2");

    let or = expr.to_bin().unwrap();
    assert_eq!(BinOp::BitOr, or.op);
    assert_eq!(String::from("1"), or.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), or.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_bit_and() {
    let expr = parse_expr("1&2");

    let and = expr.to_bin().unwrap();
    assert_eq!(BinOp::BitAnd, and.op);
    assert_eq!(String::from("1"), and.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), and.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_bit_xor() {
    let expr = parse_expr("1^2");

    let xor = expr.to_bin().unwrap();
    assert_eq!(BinOp::BitXor, xor.op);
    assert_eq!(String::from("1"), xor.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), xor.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_lt() {
    let expr = parse_expr("1<2");

    let cmp = expr.to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Lt), cmp.op);
    assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_le() {
    let expr = parse_expr("1<=2");

    let cmp = expr.to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Le), cmp.op);
    assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_gt() {
    let expr = parse_expr("1>2");

    let cmp = expr.to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Gt), cmp.op);
    assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_ge() {
    let expr = parse_expr("1>=2");

    let cmp = expr.to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Ge), cmp.op);
    assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_eq() {
    let expr = parse_expr("1==2");

    let cmp = expr.to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Eq), cmp.op);
    assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_ne() {
    let expr = parse_expr("1!=2");

    let cmp = expr.to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Ne), cmp.op);
    assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_identity_not() {
    let expr = parse_expr("1!==2");

    let cmp = expr.to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::IsNot), cmp.op);
    assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_identity() {
    let expr = parse_expr("1===2");

    let cmp = expr.to_bin().unwrap();
    assert_eq!(BinOp::Cmp(CmpOp::Is), cmp.op);
    assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
    assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_assign() {
    let expr = parse_expr("a=4");

    let assign = expr.to_bin().unwrap();
    assert!(assign.lhs.is_ident());
    assert_eq!(BinOp::Assign, assign.op);
    assert_eq!(String::from("4"), assign.rhs.to_lit_int().unwrap().value);
}

#[test]
fn parse_shift_right() {
    let expr = parse_expr("a>>4");

    let bin = expr.to_bin().unwrap();
    assert_eq!(BinOp::ArithShiftR, bin.op);
}

#[test]
fn parse_unsigned_shift_right() {
    let expr = parse_expr("a>>>4");

    let bin = expr.to_bin().unwrap();
    assert_eq!(BinOp::LogicalShiftR, bin.op);
}

#[test]
fn parse_left() {
    let expr = parse_expr("a<<4");

    let bin = expr.to_bin().unwrap();
    assert_eq!(BinOp::ShiftL, bin.op);
}

#[test]
fn parse_call_without_params() {
    let expr = parse_expr("fname()");

    let call = expr.to_call().unwrap();
    assert_eq!("fname", call.callee.to_ident().unwrap().name);
    assert_eq!(0, call.args.len());
}

#[test]
fn parse_call_with_params() {
    let expr = parse_expr("fname2(1,2,3)");

    let call = expr.to_call().unwrap();
    assert_eq!("fname2", call.callee.to_ident().unwrap().name);
    assert_eq!(3, call.args.len());
}

#[test]
fn parse_function() {
    let prog = parse("fn b() { }");
    let fct = prog.fct0();

    assert_eq!("b", fct.name.as_ref().unwrap().name_as_string);
    assert_eq!(0, fct.params.len());
    assert!(fct.return_type.is_none());
}

#[test]
fn parse_function_with_single_param() {
    let p1 = parse("fn f(a:int) { }");
    let f1 = p1.fct0();

    let p2 = parse("fn f(a:int,) { }");
    let f2 = p2.fct0();

    assert_eq!(f1.params.len(), 1);
    assert_eq!(f2.params.len(), 1);

    let p1 = &f1.params[0];
    let p2 = &f2.params[0];

    assert_eq!("a", p1.name.as_ref().unwrap().name_as_string);
    assert_eq!("a", p2.name.as_ref().unwrap().name_as_string);

    assert_eq!("int", p1.data_type.to_regular().unwrap().name());
    assert_eq!("int", p2.data_type.to_regular().unwrap().name());
}

#[test]
fn parse_function_with_multiple_params() {
    let p1 = parse("fn f(a:int, b:str) { }");
    let f1 = p1.fct0();

    let p2 = parse("fn f(a:int, b:str,) { }");
    let f2 = p2.fct0();

    let p1a = &f1.params[0];
    let p1b = &f1.params[1];
    let p2a = &f2.params[0];
    let p2b = &f2.params[1];

    assert_eq!("a", p1a.name.as_ref().unwrap().name_as_string);
    assert_eq!("a", p2a.name.as_ref().unwrap().name_as_string);

    assert_eq!("b", p1b.name.as_ref().unwrap().name_as_string);
    assert_eq!("b", p2b.name.as_ref().unwrap().name_as_string);

    assert_eq!("int", p1a.data_type.to_regular().unwrap().name());
    assert_eq!("int", p2a.data_type.to_regular().unwrap().name());

    assert_eq!("str", p1b.data_type.to_regular().unwrap().name());
    assert_eq!("str", p2b.data_type.to_regular().unwrap().name());
}

#[test]
fn parse_let_without_type() {
    let stmt = parse_let("let a = 1;");
    let var = stmt.to_let().unwrap();

    assert!(var.data_type.is_none());
    assert!(var.expr.as_ref().unwrap().is_lit_int());
}

#[test]
fn parse_let_rest() {
    let stmt = parse_let("let .. = 1;");
    let var = stmt.to_let().unwrap();
    assert!(var.pattern.first_alt().unwrap().is_rest());

    assert!(var.data_type.is_none());
    assert!(var.expr.as_ref().unwrap().is_lit_int());
}

#[test]
fn parse_let_with_type() {
    let stmt = parse_let("let x : int = 1;");
    let var = stmt.to_let().unwrap();

    assert!(var.data_type.is_some());
    assert!(var.expr.as_ref().unwrap().is_lit_int());
}

#[test]
fn parse_let_underscore() {
    let stmt = parse_let("let _ = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    assert!(pattern.is_underscore());
}

#[test]
fn parse_let_tuple() {
    let stmt = parse_let("let (mut a, b, (c, d)) = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    let tuple = pattern.to_tuple().unwrap();
    let first: &Arc<Pattern> = &tuple.params.first().unwrap().pattern;
    let first = first.first_alt().unwrap();
    assert!(first.is_ident());
    assert!(first.to_ident().unwrap().mutable);
    let last = &tuple.params.last().unwrap().pattern;
    let last = last.first_alt().unwrap();
    assert!(last.is_tuple());
}

#[test]
fn parse_let_lit_bool() {
    let stmt = parse_let("let (a, true) = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    let tuple = pattern.to_tuple().unwrap();
    assert!(tuple.params[0].pattern.first_alt().unwrap().is_ident());
    assert!(tuple.params[1].pattern.first_alt().unwrap().is_lit_bool());
}

#[test]
fn parse_let_lit_char() {
    let stmt = parse_let("let (a, 'x') = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    let tuple = pattern.to_tuple().unwrap();
    assert!(tuple.params[0].pattern.is_ident());
    assert!(tuple.params[1].pattern.is_lit_char());
}

#[test]
fn parse_let_lit_string() {
    let stmt = parse_let("let (a, \"x\") = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    let tuple = pattern.to_tuple().unwrap();
    assert!(tuple.params[0].pattern.is_ident());
    assert!(tuple.params[1].pattern.is_lit_string());
}

#[test]
fn parse_let_lit_int() {
    let stmt = parse_let("let (a, 17) = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    let tuple = pattern.to_tuple().unwrap();
    assert!(tuple.params[0].pattern.is_ident());
    assert!(tuple.params[1].pattern.is_lit_int());
}

#[test]
fn parse_let_lit_int_neg() {
    let stmt = parse_let("let (a, -17) = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    let tuple = pattern.to_tuple().unwrap();
    assert!(tuple.params[0].pattern.is_ident());
    assert!(tuple.params[1].pattern.is_lit_int());
}

#[test]
fn parse_let_lit_float() {
    let stmt = parse_let("let (a, 17.5) = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    let tuple = pattern.to_tuple().unwrap();
    assert!(tuple.params[0].pattern.is_ident());
    assert!(tuple.params[1].pattern.is_lit_float());
}

#[test]
fn parse_let_lit_float_neg() {
    let stmt = parse_let("let (a, -17.5) = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    let tuple = pattern.to_tuple().unwrap();
    assert!(tuple.params[0].pattern.is_ident());
    assert!(tuple.params[1].pattern.is_lit_float());
}

#[test]
fn parse_let_ident() {
    let stmt = parse_let("let x = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    assert!(pattern.is_ident());
}

#[test]
fn parse_let_ident_mut() {
    let stmt = parse_let("let mut x = 1;");
    let let_decl = stmt.to_let().unwrap();

    let pattern = let_decl.pattern.first_alt().unwrap();
    assert!(pattern.to_ident().unwrap().mutable);
}

#[test]
fn parse_let_with_type_but_without_assignment() {
    let stmt = parse_let("let x : int;");
    let var = stmt.to_let().unwrap();

    assert!(var.data_type.is_some());
    assert!(var.expr.is_none());
}

#[test]
fn parse_let_without_type_and_assignment() {
    let stmt = parse_let("let x;");
    let var = stmt.to_let().unwrap();

    assert!(var.data_type.is_none());
    assert!(var.expr.is_none());
}

#[test]
fn parse_multiple_functions() {
    let prog = parse("fn f() { } fn g() { }");

    let f = prog.fct0();
    assert_eq!("f", f.name.as_ref().unwrap().name_as_string);

    let g = prog.fct(1);
    assert_eq!("g", g.name.as_ref().unwrap().name_as_string);
}

#[test]
fn parse_if() {
    let expr = parse_expr("if true { 2; } else { 3; }");
    let ifexpr = expr.to_if().unwrap();

    assert!(ifexpr.cond.is_lit_bool());
    assert!(ifexpr.else_block.is_some());
}

#[test]
fn parse_if_without_else() {
    let expr = parse_expr("if true { 2; }");
    let ifexpr = expr.to_if().unwrap();

    assert!(ifexpr.cond.is_lit_bool());
    assert!(ifexpr.else_block.is_none());
}

#[test]
fn parse_while() {
    let expr = parse_expr("while true { 2; }");
    let whilestmt = expr.to_while().unwrap();

    assert!(whilestmt.cond.is_lit_bool());
    assert!(whilestmt.block.is_block());
}

#[test]
fn parse_empty_block() {
    let expr = parse_expr("{}");
    let block = expr.to_block().unwrap();

    assert_eq!(0, block.stmts.len());
}

#[test]
fn parse_block_with_one_stmt() {
    let expr = parse_expr("{ 1; 2 }");
    let block = expr.to_block().unwrap();

    assert_eq!(1, block.stmts.len());

    let expr = &block.stmts[0].to_expr().unwrap().expr;
    assert_eq!(String::from("1"), expr.to_lit_int().unwrap().value);

    assert_eq!(
        String::from("2"),
        block.expr.as_ref().unwrap().to_lit_int().unwrap().value
    );
}

#[test]
fn parse_block_with_multiple_stmts() {
    let expr = parse_expr("{ 1; 2; }");
    let block = expr.to_block().unwrap();

    assert_eq!(2, block.stmts.len());

    let expr = &block.stmts[0].to_expr().unwrap().expr;
    assert_eq!(String::from("1"), expr.to_lit_int().unwrap().value);

    let expr = &block.stmts[1].to_expr().unwrap().expr;
    assert_eq!(String::from("2"), expr.to_lit_int().unwrap().value);

    assert!(block.expr.is_none());
}

#[test]
fn parse_break() {
    let expr = parse_expr("break;");
    assert!(expr.is_break());
}

#[test]
fn parse_continue() {
    let expr = parse_expr("continue;");
    assert!(expr.is_continue());
}

#[test]
fn parse_return_value() {
    let expr = parse_expr("return 1;");
    let ret = expr.to_return().unwrap();

    assert_eq!(
        String::from("1"),
        ret.expr.as_ref().unwrap().to_lit_int().unwrap().value
    );
}

#[test]
fn parse_return() {
    let expr = parse_expr("return;");
    let ret = expr.to_return().unwrap();

    assert!(ret.expr.is_none());
}

#[test]
fn parse_type_regular() {
    let ty = parse_type("bla");
    let ty = ty.to_regular().unwrap();

    assert_eq!(0, ty.params.len());
    assert_eq!("bla", ty.name());
}

#[test]
fn parse_type_regular_mod() {
    let ty = parse_type("foo::bla");
    let regular = ty.to_regular().unwrap();

    assert_eq!(0, regular.params.len());
    assert_eq!(2, regular.path.segments.len());
    assert_eq!("foo", regular.path.segments[0].as_name_str());
    assert_eq!("bla", regular.path.segments[1].as_name_str());
}

#[test]
fn parse_type_regular_with_params() {
    let ty = parse_type("Foo[A, B]");
    let regular = ty.to_regular().unwrap();

    assert_eq!(2, regular.params.len());
    assert_eq!("Foo", regular.name());
    assert_eq!("A", regular.params[0].ty.to_regular().unwrap().name());
    assert_eq!("B", regular.params[1].ty.to_regular().unwrap().name());
}

#[test]
fn parse_type_regular_with_bindings() {
    let ty = parse_type("Foo[A, X = B]");
    let ty = ty.to_regular().unwrap();

    assert_eq!(2, ty.params.len());
    assert_eq!("Foo", ty.name());
    let arg0 = &ty.params[0];
    assert!(arg0.name.is_none());
    assert_eq!("A", arg0.ty.to_regular().unwrap().name());

    let arg1 = &ty.params[1];
    assert_eq!("X", arg1.name.as_ref().unwrap().name_as_string);
    assert_eq!("B", arg1.ty.to_regular().unwrap().name());
}

#[test]
fn parse_type_lambda_no_params() {
    let ty = parse_type("(): ()");
    let fct = ty.to_fct().unwrap();

    assert_eq!(0, fct.params.len());
    assert!(fct.ret.as_ref().unwrap().is_unit());
}

#[test]
fn parse_type_lambda_one_param() {
    let ty = parse_type("(A): B");
    let fct = ty.to_fct().unwrap();

    assert_eq!(1, fct.params.len());
    assert_eq!("A", fct.params[0].to_regular().unwrap().name());
    assert_eq!("B", fct.ret.as_ref().unwrap().to_regular().unwrap().name());
}

#[test]
fn parse_type_lambda_two_params() {
    let ty = parse_type("(A, B): C");
    let fct = ty.to_fct().unwrap();

    assert_eq!(2, fct.params.len());
    assert_eq!("A", fct.params[0].to_regular().unwrap().name());
    assert_eq!("B", fct.params[1].to_regular().unwrap().name());
    assert_eq!("C", fct.ret.as_ref().unwrap().to_regular().unwrap().name());
}

#[test]
fn parse_type_unit() {
    let ty = parse_type("()");
    let ty = ty.to_tuple().unwrap();

    assert!(ty.subtypes.is_empty());
}

#[test]
fn parse_type_tuple_with_one_type() {
    let ty = parse_type("(c)");

    let subtypes = &ty.to_tuple().unwrap().subtypes;
    assert_eq!(1, subtypes.len());

    let ty = subtypes[0].to_regular().unwrap();
    assert_eq!("c", ty.name());
}

#[test]
fn parse_type_tuple_with_two_types() {
    let ty = parse_type("(a, b)");

    let subtypes = &ty.to_tuple().unwrap().subtypes;
    assert_eq!(2, subtypes.len());

    let ty1 = subtypes[0].to_regular().unwrap();
    assert_eq!("a", ty1.name());

    let ty2 = subtypes[1].to_regular().unwrap();
    assert_eq!("b", ty2.name());
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
    let expr = parse_expr("a.foo()");
    let call = expr.to_call().unwrap();
    assert!(call.callee.is_dot());
    assert_eq!(0, call.args.len());

    let expr = parse_expr("a.foo(1)");
    let call = expr.to_call().unwrap();
    assert!(call.callee.is_dot());
    assert_eq!(1, call.args.len());

    let expr = parse_expr("a.foo(1,2)");
    let call = expr.to_call().unwrap();
    assert!(call.callee.is_dot());
    assert_eq!(2, call.args.len());
}

#[test]
fn parse_array_index() {
    let expr = parse_expr("a(b)");
    let call = expr.to_call().unwrap();
    assert_eq!("a", call.callee.to_ident().unwrap().name);
    assert_eq!(1, call.args.len());
    assert_eq!("b", call.args[0].expr.to_ident().unwrap().name);
}

#[test]
fn parse_call_with_named_arguments() {
    let expr = parse_expr("a(1, 2, x = 3, y = 4)");
    let call = expr.to_call().unwrap();
    assert!(call.callee.is_ident());
    assert_eq!(4, call.args.len());
    assert!(call.args[0].name.is_none());
    assert!(call.args[1].name.is_none());
    assert!(call.args[2].name.is_some());
    assert!(call.args[3].name.is_some());
}

#[test]
fn parse_field() {
    let prog = parse("class A { f1: int, f2: int }");
    let cls = prog.cls0();

    let f1 = &cls.fields[0];
    assert_eq!("f1", f1.name.as_ref().unwrap().name_as_string);

    let f2 = &cls.fields[1];
    assert_eq!("f2", f2.name.as_ref().unwrap().name_as_string);
}

#[test]
fn parse_as_expr() {
    let expr = parse_expr("a as String");
    let expr = expr.to_conv().unwrap();
    assert_eq!(true, expr.object.is_ident());
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
    assert_eq!("Foo", struc.name.as_ref().unwrap().name_as_string);
}

#[test]
fn parse_struct_unnamed() {
    let prog = parse("struct Foo (A, B)");
    let struc = prog.struct0();
    assert_eq!(2, struc.fields.len());
    assert_eq!("Foo", struc.name.as_ref().unwrap().name_as_string);
}

#[test]
fn parse_class_unnamed() {
    let prog = parse("class Foo(A, B)");
    let cls = prog.cls0();
    assert_eq!(2, cls.fields.len());
    assert_eq!("Foo", cls.name.as_ref().unwrap().name_as_string);
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
    assert_eq!("Bar", struc.name.as_ref().unwrap().name_as_string);

    let f1 = &struc.fields[0];
    assert_eq!("f1", f1.name.as_ref().unwrap().name_as_string);
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
    assert_eq!("FooBar", struc.name.as_ref().unwrap().name_as_string);

    let f1 = &struc.fields[0];
    assert_eq!("fa", f1.name.as_ref().unwrap().name_as_string);

    let f2 = &struc.fields[1];
    assert_eq!("fb", f2.name.as_ref().unwrap().name_as_string);
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
    assert_eq!("Bar", struct_.name.as_ref().unwrap().name_as_string);

    assert_eq!(2, struct_.type_params.as_ref().unwrap().params.len());
}

#[test]
fn parse_struct_lit_while() {
    let expr = parse_expr("while i < n { }");
    let while_expr = expr.to_while().unwrap();
    let bin = while_expr.cond.to_bin().unwrap();

    assert!(bin.lhs.is_ident());
    assert!(bin.rhs.is_ident());
}

#[test]
fn parse_struct_lit_if() {
    let expr = parse_expr("if i < n { }");
    let ifexpr = expr.to_if().unwrap();
    let bin = ifexpr.cond.to_bin().unwrap();

    assert!(bin.lhs.is_ident());
    assert!(bin.rhs.is_ident());
}

#[test]
fn parse_lit_float() {
    let expr = parse_expr("1.2");
    let lit = expr.to_lit_float().unwrap();
    assert_eq!("1.2", lit.value);
}

#[test]
fn parse_template() {
    let expr = parse_expr("\"a${1}b${2}c\"");
    let tmpl = expr.to_template().unwrap();
    assert_eq!(tmpl.parts.len(), 5);

    assert_eq!(
        "\"a${".to_string(),
        tmpl.parts[0].to_lit_str().unwrap().value
    );
    assert_eq!(String::from("1"), tmpl.parts[1].to_lit_int().unwrap().value);
    assert_eq!(
        "}b${".to_string(),
        tmpl.parts[2].to_lit_str().unwrap().value
    );
    assert_eq!(String::from("2"), tmpl.parts[3].to_lit_int().unwrap().value);
    assert_eq!(
        "}c\"".to_string(),
        tmpl.parts[4].to_lit_str().unwrap().value
    );

    let expr = parse_expr("\"a\\${1}b\"");
    assert!(expr.is_lit_str());
}

#[test]
fn parse_class_type_params() {
    let prog = parse("class Foo[T]");
    let cls = prog.cls0();

    let type_params = cls.type_params.as_ref().unwrap();
    assert_eq!(1, type_params.params.len());
    assert_eq!(
        "T",
        type_params.params[0].name.as_ref().unwrap().name_as_string
    );

    let prog = parse("class Foo[X]");
    let cls = prog.cls0();

    let type_params = cls.type_params.as_ref().unwrap();
    assert_eq!(1, type_params.params.len());
    assert_eq!(
        "X",
        type_params.params[0].name.as_ref().unwrap().name_as_string
    );
}

#[test]
fn parse_type_path() {
    let ty = parse_type("Foo::Bar::Baz");
    let ty = ty.to_regular().unwrap();
    assert_eq!(ty.path.segments.len(), 3);
    assert_eq!(ty.path.segments[0].as_name_str(), "Foo");
    assert_eq!(ty.path.segments[1].as_name_str(), "Bar");
    assert_eq!(ty.path.segments[2].as_name_str(), "Baz");
}

#[test]
fn parse_multiple_class_type_params() {
    let prog = parse("class Foo[A, B]");
    let cls = prog.cls0();

    let type_params = cls.type_params.as_ref().unwrap();
    assert_eq!(2, type_params.params.len());
    assert_eq!(
        "A",
        type_params.params[0].name.as_ref().unwrap().name_as_string
    );
    assert_eq!(
        "B",
        type_params.params[1].name.as_ref().unwrap().name_as_string
    );
}

#[test]
fn parse_empty_trait() {
    let prog = parse("trait Foo { }");
    let trait_ = prog.trait0();

    assert_eq!("Foo", trait_.name.as_ref().unwrap().name_as_string);
    assert_eq!(0, trait_.methods.len());
}

#[test]
fn parse_trait_with_function() {
    let prog = parse("trait Foo { fn empty(); }");
    let trait_ = prog.trait0();

    assert_eq!("Foo", trait_.name.as_ref().unwrap().name_as_string);
    assert_eq!(1, trait_.methods.len());
}

#[test]
fn parse_trait_with_bounds() {
    let prog = parse("trait Foo: A + B {}");
    let trait_ = prog.trait0();

    assert_eq!("Foo", trait_.name.as_ref().unwrap().name_as_string);
    assert_eq!(2, trait_.bounds.len());
    assert_eq!("A", trait_.bounds[0].name());
    assert_eq!("B", trait_.bounds[1].name());
}

#[test]
fn parse_trait_with_static_function() {
    let prog = parse("trait Foo { static fn empty(); }");
    let trait_ = prog.trait0();

    assert_eq!("Foo", trait_.name.as_ref().unwrap().name_as_string);
    assert_eq!(1, trait_.methods.len());
}

#[test]
fn parse_empty_impl() {
    let prog = parse("impl Foo for A {}");
    let impl_ = prog.impl0();

    assert_eq!("Foo", impl_.trait_type.as_ref().unwrap().to_string());
    assert_eq!("A", impl_.extended_type.to_string());
    assert_eq!(0, impl_.methods.len());
}

#[test]
fn parse_impl_with_function() {
    let prog = parse("impl Bar for B { fn foo(); }");
    let impl_ = prog.impl0();

    assert_eq!("Bar", impl_.trait_type.as_ref().unwrap().to_string());
    assert_eq!("B", impl_.extended_type.to_string());
    assert_eq!(1, impl_.methods.len());
}

#[test]
fn parse_impl_with_static_function() {
    let prog = parse("impl Bar for B { static fn foo(); }");
    let impl_ = prog.impl0();

    assert_eq!("Bar", impl_.trait_type.as_ref().unwrap().to_string());
    assert_eq!("B", impl_.extended_type.to_string());
    assert_eq!(1, impl_.methods.len());
}

#[test]
fn parse_global_let() {
    let prog = parse("let b: int = 0;");
    let global = prog.global0();

    assert_eq!("b", global.name.as_ref().unwrap().name_as_string);
    assert_eq!(false, global.mutable);
}

#[test]
fn parse_lit_char() {
    let expr = parse_expr("'a'");
    let lit = expr.to_lit_char().unwrap();

    assert_eq!("'a'", lit.value);
}

#[test]
fn parse_fct_call_with_type_param() {
    let expr = parse_expr("Array[Int]()");
    let call = expr.to_call().unwrap();
    let type_params = call.callee.to_type_param().unwrap();

    assert_eq!(1, type_params.args.len());

    let expr = parse_expr("Foo[Int, Long]()");
    let call = expr.to_call().unwrap();
    let type_params = call.callee.to_type_param().unwrap();

    assert_eq!(2, type_params.args.len());

    let expr = parse_expr("Bar[]()");
    let call = expr.to_call().unwrap();
    let type_params = call.callee.to_type_param().unwrap();

    assert_eq!(0, type_params.args.len());

    let expr = parse_expr("Vec()");
    let call = expr.to_call().unwrap();

    assert!(call.callee.is_ident());
}

#[test]
fn parse_call_with_path() {
    let expr = parse_expr("Foo::get()");
    let call = expr.to_call().unwrap();

    assert!(call.callee.is_path());
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

    assert_eq!("x", const_.name.as_ref().unwrap().name_as_string);
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
    let expr = parse_expr("|| {}");
    let lambda = expr.to_lambda().unwrap();

    assert!(lambda.return_type.is_none());
}

#[test]
fn parse_lambda_no_params_unit_as_return_value() {
    let expr = parse_expr("|| : () {}");
    let lambda = expr.to_lambda().unwrap();
    let ret = lambda.return_type.as_ref().unwrap();

    assert!(ret.is_unit());
}

#[test]
fn parse_lambda_no_params_with_return_value() {
    let expr = parse_expr("||: A {}");
    let lambda = expr.to_lambda().unwrap();
    let ret = lambda.return_type.as_ref().unwrap();
    let regular = ret.to_regular().unwrap();

    assert_eq!("A", regular.name());
}

#[test]
fn parse_lambda_with_one_param() {
    let expr = parse_expr("|a: A|: B {}");
    let lambda = expr.to_lambda().unwrap();

    assert_eq!(1, lambda.params.len());

    let param = &lambda.params[0];
    assert_eq!("a", param.name.as_ref().unwrap().name_as_string);
    let ty = param.data_type.to_regular().unwrap();
    assert_eq!("A", ty.name());

    let ret = lambda.return_type.as_ref().unwrap();
    let ty = ret.to_regular().unwrap();

    assert_eq!("B", ty.name());
}

#[test]
fn parse_lambda_with_two_params() {
    let expr = parse_expr("|a: A, b: B|: C {}");
    let lambda = expr.to_lambda().unwrap();

    assert_eq!(2, lambda.params.len());

    let param = &lambda.params[0];
    assert_eq!("a", param.name.as_ref().unwrap().name_as_string);
    let ty = param.data_type.to_regular().unwrap();
    assert_eq!("A", ty.name());

    let param = &lambda.params[1];
    assert_eq!("b", param.name.as_ref().unwrap().name_as_string);
    let ty = param.data_type.to_regular().unwrap();
    assert_eq!("B", ty.name());

    let ret = lambda.return_type.as_ref().unwrap();
    let ty = ret.to_regular().unwrap();

    assert_eq!("C", ty.name());
}

#[test]
fn parse_for() {
    let expr = parse_expr("for i in a+b {}");
    assert!(expr.is_for());
}

#[test]
fn parse_new_call_ident() {
    let expr = parse_expr("i");
    assert!(expr.is_ident());
}

#[test]
fn parse_new_call_path() {
    let expr = parse_expr("Foo::bar");
    let path = expr.to_path().unwrap();
    assert!(path.lhs.is_ident());
    assert!(path.rhs.is_ident());
}

#[test]
fn parse_new_call_call() {
    let expr = parse_expr("foo(1,2)");
    let call = expr.to_call().unwrap();
    assert!(call.callee.is_ident());
    assert_eq!(call.args.len(), 2);
}

#[test]
fn parse_block() {
    let expr = parse_expr("{1}");
    assert!(expr.to_block().unwrap().expr.as_ref().unwrap().is_lit_int());

    let expr = parse_expr("({}) + 1");
    assert!(expr.is_bin());

    let expr = parse_expr("1 + {}");
    assert!(expr.is_bin());
}

#[test]
fn parse_tuple() {
    let expr = parse_expr("(1,)");
    assert_eq!(expr.to_tuple().unwrap().values.len(), 1);

    let expr = parse_expr("(1)");
    assert!(expr.is_paren());

    let expr = parse_expr("(1,2,3)");
    assert_eq!(expr.to_tuple().unwrap().values.len(), 3);

    let expr = parse_expr("(1,2,3,4,)");
    assert_eq!(expr.to_tuple().unwrap().values.len(), 4);
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
    assert!(elements[0].to_function().is_some());
    assert!(elements[1].to_function().is_some());
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
