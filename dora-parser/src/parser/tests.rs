use crate::ast::*;
use crate::error::ParseError;
use crate::parser::Parser;
use crate::{compute_line_column, compute_line_starts};

fn parse_expr(code: &'static str) -> SyntaxNode {
    let mut parser = Parser::from_string(code);

    parser.parse_expr();
    assert!(parser.current().is_eof());
    let (file, errors) = parser.into_file();
    assert!(errors.is_empty());

    file.root()
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

fn parse_let(code: &'static str) -> AstLet {
    let mut parser = Parser::from_string(code);
    parser.parse_let();
    assert!(parser.current().is_eof());
    let (file, errors) = parser.into_file();
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
    file.root().as_let()
}

fn parse_type(code: &'static str) -> AstType {
    let mut parser = Parser::from_string(code);
    parser.parse_type();
    assert!(parser.current().is_eof());
    let (file, errors) = parser.into_file();
    assert!(errors.is_empty());
    AstType::cast(file.root()).unwrap()
}

fn parse(code: &'static str) -> File {
    let (file, errors) = Parser::from_string(code).parse();
    if !errors.is_empty() {
        for error in &errors {
            println!("{} at {}", error.error.message(), error.span);
        }
    }
    assert!(errors.is_empty());
    file
}

fn parse_with_some_errors(code: &'static str) -> File {
    let (file, errors) = Parser::from_string(code).parse();
    assert!(!errors.is_empty());
    file
}

fn parse_with_error(code: &'static str, expected: Vec<(u32, u32, u32, ParseError)>) -> File {
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
    let expr = parse_expr("a");
    assert_eq!("a", expr.as_name_expr().name());
}

#[test]
fn parse_number() {
    let expr = parse_expr("10");

    let lit = expr.as_lit_int();
    assert_eq!("10", lit.value());
}

#[test]
fn parse_number_with_underscore() {
    let expr = parse_expr("1____0");
    assert_eq!("1____0", expr.as_lit_int().value());
}

#[test]
fn parse_string() {
    let expr = parse_expr("\"abc\"");
    assert_eq!("\"abc\"", expr.as_lit_str().value());
}

#[test]
fn parse_true() {
    let expr = parse_expr("true");
    assert_eq!(true, expr.as_lit_bool().value());
}

#[test]
fn parse_false() {
    let expr = parse_expr("true");
    assert_eq!(true, expr.as_lit_bool().value());
}

#[test]
fn parse_field_access() {
    let expr = parse_expr("obj.field").as_dot_expr();
    assert_eq!("obj", expr.lhs().as_name_expr().name());
    assert_eq!("field", expr.rhs().as_name_expr().name());
}

#[test]
fn parse_field_negated() {
    let expr = parse_expr("-obj.field").as_un();
    assert!(expr.opnd().is_dot_expr());
}

#[test]
fn parse_field_non_ident() {
    let expr = parse_expr("bar.12").as_dot_expr();
    assert_eq!("bar", expr.lhs().as_name_expr().name());
    assert_eq!("12", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_self() {
    let expr = parse_expr("self");
    assert!(expr.is_this());
}

#[test]
fn parse_neg() {
    let expr = parse_expr("-1").as_un();
    assert_eq!(UnOp::Neg, expr.op());
    assert!(expr.opnd().is_lit_int());
}

#[test]
fn parse_neg_twice() {
    let expr = parse_expr("-(-3)").as_un();
    assert_eq!(UnOp::Neg, expr.op());

    let paren = expr.opnd().as_paren();
    let neg2 = paren.expr().unwrap().as_un();
    assert_eq!(UnOp::Neg, neg2.op());

    assert!(neg2.opnd().is_lit_int());
}

#[test]
fn parse_neg_twice_without_parentheses() {
    err_expr("- -2", ParseError::ExpectedFactor, 1, 3);
}

#[test]
fn parse_mul() {
    let expr = parse_expr("6*3").as_bin();
    assert_eq!(BinOp::Mul, expr.op());
    assert_eq!("6", expr.lhs().as_lit_int().value());
    assert_eq!("3", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_multiple_muls() {
    let expr = parse_expr("6*3*4").as_bin();
    assert_eq!(BinOp::Mul, expr.op());

    let mul2 = expr.lhs().as_bin();
    assert_eq!(BinOp::Mul, mul2.op());
    assert_eq!("6", mul2.lhs().as_lit_int().value());
    assert_eq!("3", mul2.rhs().as_lit_int().value());

    assert_eq!("4", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_div() {
    let expr = parse_expr("4/5").as_bin();
    assert_eq!(BinOp::Div, expr.op());
    assert_eq!("4", expr.lhs().as_lit_int().value());
    assert_eq!("5", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_mod() {
    let expr = parse_expr("2%15").as_bin();
    assert_eq!(BinOp::Mod, expr.op());
    assert_eq!("2", expr.lhs().as_lit_int().value());
    assert_eq!("15", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_add() {
    let expr = parse_expr("2+3").as_bin();
    assert_eq!(BinOp::Add, expr.op());
    assert_eq!("2", expr.lhs().as_lit_int().value());
    assert_eq!("3", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_add_left_associativity() {
    let expr = parse_expr("1+2+3").as_bin();
    assert_eq!("3", expr.rhs().as_lit_int().value());

    let lhs = expr.lhs().as_bin();
    assert_eq!("1", lhs.lhs().as_lit_int().value());
    assert_eq!("2", lhs.rhs().as_lit_int().value());
}

#[test]
fn parse_add_right_associativity_via_parens() {
    let expr = parse_expr("1+(2+3)").as_bin();
    assert_eq!("1", expr.lhs().as_lit_int().value());

    let paren = expr.rhs().as_paren();
    let rhs = paren.expr().unwrap().as_bin();
    assert_eq!("2", rhs.lhs().as_lit_int().value());
    assert_eq!("3", rhs.rhs().as_lit_int().value());
}

#[test]
fn parse_sub() {
    let expr = parse_expr("1-2").as_bin();
    assert_eq!(BinOp::Sub, expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_or() {
    let expr = parse_expr("1||2").as_bin();
    assert_eq!(BinOp::Or, expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_and() {
    let expr = parse_expr("1&&2").as_bin();
    assert_eq!(BinOp::And, expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_bit_or() {
    let expr = parse_expr("1|2").as_bin();
    assert_eq!(BinOp::BitOr, expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_bit_and() {
    let expr = parse_expr("1&2").as_bin();
    assert_eq!(BinOp::BitAnd, expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_bit_xor() {
    let expr = parse_expr("1^2").as_bin();
    assert_eq!(BinOp::BitXor, expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_lt() {
    let expr = parse_expr("1<2").as_bin();
    assert_eq!(BinOp::Cmp(CmpOp::Lt), expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_le() {
    let expr = parse_expr("1<=2").as_bin();
    assert_eq!(BinOp::Cmp(CmpOp::Le), expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_gt() {
    let expr = parse_expr("1>2").as_bin();
    assert_eq!(BinOp::Cmp(CmpOp::Gt), expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_ge() {
    let expr = parse_expr("1>=2").as_bin();
    assert_eq!(BinOp::Cmp(CmpOp::Ge), expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_eq() {
    let expr = parse_expr("1==2").as_bin();
    assert_eq!(BinOp::Cmp(CmpOp::Eq), expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_ne() {
    let expr = parse_expr("1!=2").as_bin();
    assert_eq!(BinOp::Cmp(CmpOp::Ne), expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_identity_not() {
    let expr = parse_expr("1!==2").as_bin();
    assert_eq!(BinOp::Cmp(CmpOp::IsNot), expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_identity() {
    let expr = parse_expr("1===2").as_bin();
    assert_eq!(BinOp::Cmp(CmpOp::Is), expr.op());
    assert_eq!("1", expr.lhs().as_lit_int().value());
    assert_eq!("2", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_assign() {
    let expr = parse_expr("a=4").as_bin();
    assert!(expr.lhs().is_name_expr());
    assert_eq!(BinOp::Assign, expr.op());
    assert_eq!("4", expr.rhs().as_lit_int().value());
}

#[test]
fn parse_shift_right() {
    let expr = parse_expr("a>>4").as_bin();
    assert_eq!(BinOp::ArithShiftR, expr.op());
}

#[test]
fn parse_unsigned_shift_right() {
    let expr = parse_expr("a>>>4").as_bin();
    assert_eq!(BinOp::LogicalShiftR, expr.op());
}

#[test]
fn parse_left() {
    let expr = parse_expr("a<<4").as_bin();
    assert_eq!(BinOp::ShiftL, expr.op());
}

#[test]
fn parse_call_without_params() {
    let expr = parse_expr("fname()").as_call();
    assert_eq!("fname", expr.callee().as_name_expr().name());
    assert_eq!(0, expr.arg_list().items().count());
}

#[test]
fn parse_call_with_params() {
    let expr = parse_expr("fname2(1,2,3)").as_call();
    assert_eq!("fname2", expr.callee().as_name_expr().name());
    assert_eq!(3, expr.arg_list().items().count());
}

#[test]
fn parse_function() {
    let file = parse("fn b() { }");
    let fct = file
        .root()
        .children()
        .find_map(|n| AstFunction::cast(n))
        .unwrap();

    assert_eq!("b", fct.name().unwrap().name());
    assert_eq!(0, fct.params().len());
    assert!(fct.return_type().is_none());
}

#[test]
fn parse_function_with_single_param() {
    let file1 = parse("fn f(a:int) { }");
    let f1 = file1
        .root()
        .children()
        .find_map(|n| AstFunction::cast(n))
        .unwrap();

    let file2 = parse("fn f(a:int,) { }");
    let f2 = file2
        .root()
        .children()
        .find_map(|n| AstFunction::cast(n))
        .unwrap();

    assert_eq!(f1.params().len(), 1);
    assert_eq!(f2.params().len(), 1);

    let p1 = f1.params_at(0);
    let p2 = f2.params_at(0);

    assert_eq!("a", pat_name(p1.pattern().unwrap()));
    assert_eq!("a", pat_name(p2.pattern().unwrap()));

    assert_eq!("int", tr_name(p1.data_type().unwrap()));
    assert_eq!("int", tr_name(p2.data_type().unwrap()));
}

#[test]
fn parse_function_with_multiple_params() {
    let file1 = parse("fn f(a:int, b:str) { }");
    let f1 = file1
        .root()
        .children()
        .find_map(|n| AstFunction::cast(n))
        .unwrap();

    let file2 = parse("fn f(a:int, b:str,) { }");
    let f2 = file2
        .root()
        .children()
        .find_map(|n| AstFunction::cast(n))
        .unwrap();

    let p1a = f1.params_at(0);
    let p1b = f1.params_at(1);
    let p2a = f2.params_at(0);
    let p2b = f2.params_at(1);

    assert_eq!("a", pat_name(p1a.pattern().unwrap()));
    assert_eq!("a", pat_name(p2a.pattern().unwrap()));

    assert_eq!("b", pat_name(p1b.pattern().unwrap()));
    assert_eq!("b", pat_name(p2b.pattern().unwrap()));

    assert_eq!("int", tr_name(p1a.data_type().unwrap()));
    assert_eq!("int", tr_name(p2a.data_type().unwrap()));

    assert_eq!("str", tr_name(p1b.data_type().unwrap()));
    assert_eq!("str", tr_name(p2b.data_type().unwrap()));
}

#[test]
fn parse_let_without_type() {
    let var = parse_let("let a = 1;");

    assert!(var.data_type().is_none());
    assert!(var.expr().unwrap().is_lit_int());
}

#[test]
fn parse_let_rest() {
    let var = parse_let("let .. = 1;");
    assert!(var.pattern().is_rest());

    assert!(var.data_type().is_none());
    assert!(var.expr().unwrap().is_lit_int());
}

#[test]
fn parse_let_with_type() {
    let var = parse_let("let x : int = 1;");

    assert!(var.data_type().is_some());
    assert!(var.expr().unwrap().is_lit_int());
}

#[test]
fn parse_let_underscore() {
    let let_decl = parse_let("let _ = 1;");
    assert!(let_decl.pattern().is_underscore_pattern());
}

#[test]
fn parse_let_tuple() {
    let let_decl = parse_let("let (mut a, b, (c, d)) = 1;");

    let tuple = let_decl.pattern().as_tuple_pattern();
    let first = tuple.params_at(0);
    assert!(first.is_ident_pattern());
    assert!(first.as_ident_pattern().mutable());
    let last = tuple.params_at(2);
    assert!(last.is_tuple_pattern());
}

#[test]
fn parse_let_lit_bool() {
    let let_decl = parse_let("let (a, true) = 1;");

    let tuple = let_decl.pattern().as_tuple_pattern();
    assert!(tuple.params_at(0).is_ident_pattern());
    assert!(tuple.params_at(1).is_lit_pattern());

    let mut params = tuple.params();
    assert_eq!(params.next().unwrap().id(), tuple.params_at(0).id());
    assert_eq!(params.next().unwrap().id(), tuple.params_at(1).id());
    assert!(params.next().is_none());
}

#[test]
fn parse_let_lit_char() {
    let let_decl = parse_let("let (a, 'x') = 1;");

    let tuple = let_decl.pattern().as_tuple_pattern();
    assert!(tuple.params_at(0).is_ident_pattern());
    assert!(tuple.params_at(1).is_lit_pattern());
}

#[test]
fn parse_let_lit_string() {
    let let_decl = parse_let("let (a, \"x\") = 1;");

    let tuple = let_decl.pattern().as_tuple_pattern();
    assert!(tuple.params_at(0).is_ident_pattern());
    assert!(tuple.params_at(1).is_lit_pattern());
}

#[test]
fn parse_let_lit_int() {
    let let_decl = parse_let("let (a, 17) = 1;");

    let tuple = let_decl.pattern().as_tuple_pattern();
    assert!(tuple.params_at(0).is_ident_pattern());
    assert!(tuple.params_at(1).is_lit_pattern());
}

#[test]
fn parse_let_lit_int_neg() {
    let let_decl = parse_let("let (a, -17) = 1;");

    let tuple = let_decl.pattern().as_tuple_pattern();
    assert!(tuple.params_at(0).is_ident_pattern());
    assert!(tuple.params_at(1).is_lit_pattern());
}

#[test]
fn parse_let_lit_float() {
    let let_decl = parse_let("let (a, 17.5) = 1;");

    let tuple = let_decl.pattern().as_tuple_pattern();
    assert!(tuple.params_at(0).is_ident_pattern());
    assert!(tuple.params_at(1).is_lit_pattern());
}

#[test]
fn parse_let_lit_float_neg() {
    let let_decl = parse_let("let (a, -17.5) = 1;");

    let tuple = let_decl.pattern().as_tuple_pattern();
    assert!(tuple.params_at(0).is_ident_pattern());
    assert!(tuple.params_at(1).is_lit_pattern());
}

#[test]
fn parse_let_ident() {
    let let_decl = parse_let("let x = 1;");

    assert!(let_decl.pattern().is_ident_pattern());
}

#[test]
fn parse_let_ident_mut() {
    let let_decl = parse_let("let mut x = 1;");

    assert!(let_decl.pattern().as_ident_pattern().mutable());
}

#[test]
fn parse_let_with_type_but_without_assignment() {
    let var = parse_let("let x : int;");

    assert!(var.data_type().is_some());
    assert!(var.expr().is_none());
}

#[test]
fn parse_let_without_type_and_assignment() {
    let var = parse_let("let x;");

    assert!(var.data_type().is_none());
    assert!(var.expr().is_none());
}

#[test]
fn parse_multiple_functions() {
    let file = parse("fn f() { } fn g() { }");

    let root = file.root();
    let mut funcs = root.children().filter_map(|n| AstFunction::cast(n));
    let f = funcs.next().unwrap();
    assert_eq!("f", f.name().unwrap().name());

    let g = funcs.next().unwrap();
    assert_eq!("g", g.name().unwrap().name());
}

#[test]
fn parse_if() {
    let expr = parse_expr("if true { 2; } else { 3; }").as_if();
    assert!(expr.cond().is_lit_bool());
    assert!(expr.else_block().is_some());
}

#[test]
fn parse_if_without_else() {
    let expr = parse_expr("if true { 2; }").as_if();
    assert!(expr.cond().is_lit_bool());
    assert!(expr.else_block().is_none());
}

#[test]
fn parse_while() {
    let expr = parse_expr("while true { 2; }").as_while();
    assert!(expr.cond().is_lit_bool());
}

#[test]
fn parse_empty_block() {
    let expr = parse_expr("{}").as_block();

    assert_eq!(0, expr.stmts().count());
}

#[test]
fn parse_block_with_one_stmt() {
    let expr = parse_expr("{ 1; 2 }").as_block();

    assert_eq!(1, expr.stmts().count());

    let stmt = expr.stmts().next().unwrap().as_expr_stmt();
    assert_eq!("1", stmt.expr().as_lit_int().value());

    assert_eq!("2", expr.expr().unwrap().as_lit_int().value());
}

#[test]
fn parse_block_with_multiple_stmts() {
    let expr = parse_expr("{ 1; 2; }").as_block();

    assert_eq!(2, expr.stmts().count());

    let mut stmts = expr.stmts();

    let stmt0 = stmts.next().unwrap().as_expr_stmt();
    assert_eq!("1", stmt0.expr().as_lit_int().value());

    let stmt1 = stmts.next().unwrap().as_expr_stmt();
    assert_eq!("2", stmt1.expr().as_lit_int().value());

    assert!(expr.expr().is_none());
}

#[test]
fn parse_break() {
    let expr = parse_expr("break");
    assert!(expr.is_break());
}

#[test]
fn parse_continue() {
    let expr = parse_expr("continue");
    assert!(expr.is_continue());
}

#[test]
fn parse_return_value() {
    let expr = parse_expr("return 1").as_return();
    assert_eq!("1", expr.expr().unwrap().as_lit_int().value());
}

#[test]
fn parse_return() {
    let expr = parse_expr("return").as_return();
    assert!(expr.expr().is_none());
}

#[test]
fn parse_type_regular() {
    let ty = parse_type("bla").as_regular_type();

    assert_eq!(0, ty.params_len());
    assert_eq!("bla", ty.path().segments_at(0).as_name().name());
}

#[test]
fn parse_type_regular_mod() {
    let regular = parse_type("foo::bla").as_regular_type();

    assert_eq!(0, regular.params_len());
    let path = regular.path();
    assert_eq!(2, path.segments_len());
    assert_eq!("foo", path.segments_at(0).as_name().name());
    assert_eq!("bla", path.segments_at(1).as_name().name());
}

#[test]
fn parse_type_regular_with_params() {
    let regular = parse_type("Foo[A, B]").as_regular_type();

    assert_eq!(2, regular.params_len());
    assert_eq!("Foo", regular.path().segments_at(0).as_name().name());
    let arg0 = regular.params_at(0);
    assert_eq!(
        "A",
        arg0.ty()
            .unwrap()
            .as_regular_type()
            .path()
            .segments_at(0)
            .as_name()
            .name()
    );
    let arg1 = regular.params_at(1);
    assert_eq!(
        "B",
        arg1.ty()
            .unwrap()
            .as_regular_type()
            .path()
            .segments_at(0)
            .as_name()
            .name()
    );
}

#[test]
fn parse_type_regular_with_bindings() {
    let ty = parse_type("Foo[A, X = B]").as_regular_type();

    assert_eq!(2, ty.params_len());
    assert_eq!("Foo", ty.path().segments_at(0).as_name().name());
    let arg0 = ty.params_at(0);
    assert!(arg0.name().is_none());
    assert_eq!("A", tr_name(arg0.ty().unwrap()));

    let arg1 = ty.params_at(1);
    assert_eq!("X", arg1.name().unwrap().name());
    assert_eq!("B", tr_name(arg1.ty().unwrap()));
}

#[test]
fn parse_type_lambda_no_params() {
    let fct = parse_type("(): ()").as_lambda_type();

    assert_eq!(0, fct.params_len());
    assert!(fct.ret().unwrap().is_unit_type());
}

#[test]
fn parse_type_lambda_one_param() {
    let fct = parse_type("(A): B").as_lambda_type();

    assert_eq!(1, fct.params_len());
    assert_eq!("A", tr_name(fct.params_at(0)));
    assert_eq!("B", tr_name(fct.ret().unwrap()));
}

#[test]
fn parse_type_lambda_two_params() {
    let fct = parse_type("(A, B): C").as_lambda_type();

    assert_eq!(2, fct.params_len());
    assert_eq!("A", tr_name(fct.params_at(0)));
    assert_eq!("B", tr_name(fct.params_at(1)));
    assert_eq!("C", tr_name(fct.ret().unwrap()));
}

#[test]
fn parse_type_unit() {
    let ty = parse_type("()").as_tuple_type();

    assert!(ty.subtypes_len() == 0);
}

#[test]
fn parse_type_tuple_with_one_type() {
    let ty = parse_type("(c)").as_tuple_type();

    assert_eq!(1, ty.subtypes_len());
    assert_eq!("c", tr_name(ty.subtypes_at(0)));
}

#[test]
fn parse_type_tuple_with_two_types() {
    let ty = parse_type("(a, b)").as_tuple_type();

    assert_eq!(2, ty.subtypes_len());
    assert_eq!("a", tr_name(ty.subtypes_at(0)));
    assert_eq!("b", tr_name(ty.subtypes_at(1)));
}

#[test]
fn parse_class_with_param() {
    let file = parse("class Foo{a: int}");
    let class = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();
    assert_eq!(1, class.fields().count());
}

#[test]
fn parse_class_with_params() {
    let file = parse("class Foo{a: int, b: int}");
    let class = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();

    assert_eq!(2, class.fields().count());
}

#[test]
fn parse_class() {
    let file = parse("class Foo { a: Int64, b: Bool }");
    let class = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();
    assert_eq!(class.fields().count(), 2);

    let file = parse("class Foo { a: Int64, b: Bool }");
    let class = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();
    assert_eq!(class.fields().count(), 2);

    let file = parse("class Foo");
    let class = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();
    assert_eq!(class.fields().count(), 0);
}

#[test]
fn parse_method_invocation() {
    let expr = parse_expr("a.foo()").as_call();
    assert!(expr.callee().is_dot_expr());
    assert_eq!(0, expr.arg_list().items().count());

    let expr = parse_expr("a.foo(1)").as_call();
    assert!(expr.callee().is_dot_expr());
    assert_eq!(1, expr.arg_list().items().count());

    let expr = parse_expr("a.foo(1,2)").as_call();
    assert!(expr.callee().is_dot_expr());
    assert_eq!(2, expr.arg_list().items().count());
}

#[test]
fn parse_array_index() {
    let expr = parse_expr("a(b)").as_call();
    assert_eq!("a", expr.callee().as_name_expr().name());
    assert_eq!(1, expr.arg_list().items().count());
    let index_arg = expr.arg_list().items().next().unwrap();
    assert_eq!("b", index_arg.expr().unwrap().as_name_expr().name());
}

#[test]
fn parse_call_with_named_arguments() {
    let expr = parse_expr("a(1, 2, x = 3, y = 4)").as_call();
    assert!(expr.callee().is_name_expr());
    assert_eq!(4, expr.arg_list().items().count());
    let arg_list = expr.arg_list();
    let mut args = arg_list.items();
    assert!(args.next().unwrap().name().is_none());
    assert!(args.next().unwrap().name().is_none());
    assert!(args.next().unwrap().name().is_some());
    assert!(args.next().unwrap().name().is_some());
}

#[test]
fn parse_field() {
    let file = parse("class A { f1: int, f2: int }");
    let cls = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();

    let mut fields_iter = cls.fields();

    let f1 = fields_iter.next().unwrap();
    assert_eq!("f1", f1.name().unwrap().name());

    let f2 = fields_iter.next().unwrap();
    assert_eq!("f2", f2.name().unwrap().name());
}

#[test]
fn parse_as_expr() {
    let expr = parse_expr("a as String").as_conv();
    assert_eq!(true, expr.object().unwrap().is_name_expr());
}

#[test]
fn parse_internal() {
    parse("@internal fn foo();");
}

#[test]
fn parse_function_without_body() {
    let file = parse("fn foo();");
    let fct = file
        .root()
        .children()
        .find_map(|n| AstFunction::cast(n))
        .unwrap();
    assert!(fct.block().is_none());
}

#[test]
fn parse_struct_empty() {
    let file = parse("struct Foo {}");
    let struc = file
        .root()
        .children()
        .find_map(|n| AstStruct::cast(n))
        .unwrap();
    assert_eq!(0, struc.fields_len());
    assert_eq!("Foo", struc.name().unwrap().name());
}

#[test]
fn parse_struct_unnamed() {
    let file = parse("struct Foo (A, B)");
    let struc = file
        .root()
        .children()
        .find_map(|n| AstStruct::cast(n))
        .unwrap();
    assert_eq!(2, struc.fields_len());
    assert_eq!("Foo", struc.name().unwrap().name());
}

#[test]
fn parse_class_unnamed() {
    let file = parse("class Foo(A, B)");
    let cls = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();
    assert_eq!(2, cls.fields().count());
    assert_eq!("Foo", cls.name().unwrap().name());
}

#[test]
fn parse_struct_one_field() {
    let file = parse(
        "struct Bar {
        f1: Foo1,
    }",
    );
    let struc = file
        .root()
        .children()
        .find_map(|n| AstStruct::cast(n))
        .unwrap();
    assert_eq!(1, struc.fields_len());
    assert_eq!("Bar", struc.name().unwrap().name());

    let f1 = struc.fields_at(0);
    assert_eq!("f1", f1.name().unwrap().name());
}

#[test]
fn parse_struct_multiple_fields() {
    let file = parse(
        "struct FooBar {
        fa: Foo1,
        fb: Foo2,
    }",
    );
    let struc = file
        .root()
        .children()
        .find_map(|n| AstStruct::cast(n))
        .unwrap();
    assert_eq!(2, struc.fields_len());
    assert_eq!("FooBar", struc.name().unwrap().name());

    let f1 = struc.fields_at(0);
    assert_eq!("fa", f1.name().unwrap().name());

    let f2 = struc.fields_at(1);
    assert_eq!("fb", f2.name().unwrap().name());
}

#[test]
fn parse_struct_with_type_params() {
    let file = parse(
        "struct Bar[T1, T2] {
        f1: T1, f2: T2,
    }",
    );
    let struct_ = file
        .root()
        .children()
        .find_map(|n| AstStruct::cast(n))
        .unwrap();
    assert_eq!(2, struct_.fields_len());
    assert_eq!("Bar", struct_.name().unwrap().name());

    let type_params = struct_.type_param_list().unwrap();
    assert_eq!(2, type_params.items_len());
}

#[test]
fn parse_struct_lit_while() {
    let expr = parse_expr("while i < n { }").as_while();
    let bin = expr.cond().as_bin();
    assert!(bin.lhs().is_name_expr());
    assert!(bin.rhs().is_name_expr());
}

#[test]
fn parse_struct_lit_if() {
    let expr = parse_expr("if i < n { }").as_if();
    let bin = expr.cond().as_bin();
    assert!(bin.lhs().is_name_expr());
    assert!(bin.rhs().is_name_expr());
}

#[test]
fn parse_lit_float() {
    let expr = parse_expr("1.2").as_lit_float();
    assert_eq!("1.2", expr.value());
}

#[test]
fn parse_template() {
    let expr = parse_expr("\"a${1}b${2}c\"").as_template();
    assert_eq!(expr.parts().len(), 5);

    assert_eq!("\"a${", expr.parts_at(0).as_lit_str().value());
    assert_eq!("1", expr.parts_at(1).as_lit_int().value());
    assert_eq!("}b${", expr.parts_at(2).as_lit_str().value());
    assert_eq!("2", expr.parts_at(3).as_lit_int().value());
    assert_eq!("}c\"", expr.parts_at(4).as_lit_str().value());

    let expr = parse_expr("\"a\\${1}b\"");
    assert!(expr.is_lit_str());
}

#[test]
fn parse_class_type_params() {
    let file = parse("class Foo[T]");
    let cls = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();

    let type_params = cls.type_param_list().unwrap();
    assert_eq!(1, type_params.items_len());
    let type_param = type_params.items().next().unwrap();
    assert_eq!("T", type_param.name().unwrap().name());

    let file = parse("class Foo[X]");
    let cls = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();

    let type_params = cls.type_param_list().unwrap();
    assert_eq!(1, type_params.items_len());
    let type_param = type_params.items().next().unwrap();
    assert_eq!("X", type_param.name().unwrap().name());
}

#[test]
fn parse_type_path() {
    let ty = parse_type("Foo::Bar::Baz").as_regular_type();
    let path = ty.path();
    assert_eq!(path.segments_len(), 3);
    assert_eq!(path.segments_at(0).as_name().name(), "Foo");
    assert_eq!(path.segments_at(1).as_name().name(), "Bar");
    assert_eq!(path.segments_at(2).as_name().name(), "Baz");
}

#[test]
fn parse_multiple_class_type_params() {
    let file = parse("class Foo[A, B]");
    let cls = file
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();

    let type_params = cls.type_param_list().unwrap();
    assert_eq!(2, type_params.items_len());
    let mut params = type_params.items();
    let type_param = params.next().unwrap();
    assert_eq!("A", type_param.name().unwrap().name());
    let type_param = params.next().unwrap();
    assert_eq!("B", type_param.name().unwrap().name());
}

#[test]
fn parse_empty_trait() {
    let file = parse("trait Foo { }");
    let trait_ = file
        .root()
        .children()
        .find_map(|x| AstTrait::cast(x))
        .unwrap();

    assert_eq!("Foo", trait_.name().unwrap().name());
    assert_eq!(0, trait_.element_list().unwrap().items_len());
}

#[test]
fn parse_trait_with_function() {
    let file = parse("trait Foo { fn empty(); }");
    let trait_ = file
        .root()
        .children()
        .find_map(|x| AstTrait::cast(x))
        .unwrap();

    assert_eq!("Foo", trait_.name().unwrap().name());
    assert_eq!(1, trait_.element_list().unwrap().items_len());
}

#[test]
fn parse_trait_with_bounds() {
    let prog = parse("trait Foo: A + B {}");
    let trait_ = prog
        .root()
        .children()
        .find_map(|x| AstTrait::cast(x))
        .unwrap();

    assert_eq!("Foo", trait_.name().unwrap().name());
    let bounds = trait_.bounds().unwrap();
    assert_eq!(2, bounds.items_len());
    assert_eq!("A", tr_name(bounds.items_at(0)));
    assert_eq!("B", tr_name(bounds.items_at(1)));
}

#[test]
fn parse_trait_with_static_function() {
    let file = parse("trait Foo { static fn empty(); }");
    let trait_ = file
        .root()
        .children()
        .find_map(|x| AstTrait::cast(x))
        .unwrap();

    assert_eq!("Foo", trait_.name().unwrap().name());
    assert_eq!(1, trait_.element_list().unwrap().items_len());
}

#[test]
fn parse_empty_impl() {
    let file = parse("impl Foo for A {}");
    let impl_ = file
        .root()
        .children()
        .find_map(|n| AstImpl::cast(n))
        .unwrap();

    assert_eq!("Foo", tr_name(impl_.trait_type().unwrap()));
    assert_eq!("A", tr_name(impl_.extended_type().unwrap()));
    assert_eq!(0, impl_.element_list().unwrap().items_len());
}

#[test]
fn parse_impl_with_function() {
    let file = parse("impl Bar for B { fn foo(); }");
    let impl_ = file
        .root()
        .children()
        .find_map(|n| AstImpl::cast(n))
        .unwrap();

    assert_eq!("Bar", tr_name(impl_.trait_type().unwrap()));
    assert_eq!("B", tr_name(impl_.extended_type().unwrap()));
    assert_eq!(1, impl_.element_list().unwrap().items_len());
}

#[test]
fn parse_impl_with_static_function() {
    let file = parse("impl Bar for B { static fn foo(); }");
    let impl_ = file
        .root()
        .children()
        .find_map(|n| AstImpl::cast(n))
        .unwrap();

    assert_eq!("Bar", tr_name(impl_.trait_type().unwrap()));
    assert_eq!("B", tr_name(impl_.extended_type().unwrap()));
    assert_eq!(1, impl_.element_list().unwrap().items_len());
}

#[test]
fn parse_global_let() {
    let file = parse("let b: int = 0;");
    let global = file
        .root()
        .children()
        .find_map(|n| AstGlobal::cast(n))
        .unwrap();

    assert_eq!("b", global.name().unwrap().name());
    assert_eq!(false, global.mutable());
}

#[test]
fn parse_lit_char() {
    let expr = parse_expr("'a'").as_lit_char();
    assert_eq!("'a'", expr.value());
}

#[test]
fn parse_fct_call_with_type_param() {
    let expr = parse_expr("Array[Int]()").as_call();
    let type_params = expr.callee().as_typed_expr();

    assert_eq!(1, type_params.args().len());

    let expr = parse_expr("Foo[Int, Long]()").as_call();
    let type_params = expr.callee().as_typed_expr();

    assert_eq!(2, type_params.args().len());

    let expr = parse_expr("Bar[]()").as_call();
    let type_params = expr.callee().as_typed_expr();

    assert_eq!(0, type_params.args().len());

    let expr = parse_expr("Vec()").as_call();

    assert!(expr.callee().is_name_expr());
}

#[test]
fn parse_call_with_path() {
    let expr = parse_expr("Foo::get()").as_call();
    assert!(expr.callee().is_path());
    assert_eq!(0, expr.arg_list().items().count());
}

#[test]
#[ignore]
fn parse_method_call() {
    let expr = parse_expr("a.foo(1, 2)").as_method_call_expr();
    assert_eq!("a", expr.object().as_name_expr().name());
    assert_eq!("foo", expr.name().name());
    assert_eq!(2, expr.arg_list().unwrap().items().count());

    assert_eq!("foo", expr.name().name());
}

#[test]
#[ignore]
fn parse_method_call_with_type_params() {
    let expr = parse_expr("a.foo[A](1, 2)").as_method_call_expr();
    assert_eq!("a", expr.object().as_name_expr().name());
    assert_eq!("foo", expr.name().name());
    assert_eq!(2, expr.arg_list().unwrap().items().count());
}

#[test]
fn parse_fct_with_type_params() {
    let file = parse("fn f[T]() {}");
    let fct = file
        .root()
        .children()
        .find_map(|n| AstFunction::cast(n))
        .unwrap();

    let type_params = fct.type_param_list().unwrap();
    assert_eq!(1, type_params.items_len());
}

#[test]
fn parse_const() {
    let file = parse("const x: int = 0;");
    let const_ = file
        .root()
        .children()
        .find_map(|n| AstConst::cast(n))
        .unwrap();

    assert_eq!("x", const_.name().unwrap().name());
}

#[test]
fn parse_generic_with_bound() {
    let prog = parse("class A[T: Foo]");
    let cls = prog
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();

    let type_params = cls.type_param_list().unwrap();
    let type_param = type_params.items().next().unwrap();
    assert_eq!(1, type_param.bounds().unwrap().items_len());
}

#[test]
fn parse_generic_with_multiple_bounds() {
    let prog = parse("class A[T: Foo + Bar]");
    let cls = prog
        .root()
        .children()
        .find_map(|n| AstClass::cast(n))
        .unwrap();

    let type_params = cls.type_param_list().unwrap();
    let type_param = type_params.items().next().unwrap();
    assert_eq!(2, type_param.bounds().unwrap().items_len());
}

#[test]
fn parse_lambda_no_params_no_return_value() {
    let expr = parse_expr("|| {}").as_lambda();
    let node = expr.fct();
    assert!(node.return_type().is_none());
}

#[test]
fn parse_lambda_no_params_unit_as_return_value() {
    let expr = parse_expr("|| : () {}").as_lambda();
    let node = expr.fct();
    assert!(node.return_type().unwrap().is_unit_type());
}

#[test]
fn parse_lambda_no_params_with_return_value() {
    let expr = parse_expr("||: A {}").as_lambda();
    let node = expr.fct();
    let ret = node.return_type().unwrap();

    assert_eq!("A", tr_name(ret));
}

#[test]
fn parse_lambda_with_one_param() {
    let expr = parse_expr("|a: A|: B {}").as_lambda();
    let node = expr.fct();

    assert_eq!(1, node.params().len());

    let param = node.params_at(0);
    assert_eq!("a", pat_name(param.pattern().unwrap()));
    assert_eq!("A", tr_name(param.data_type().unwrap()));
    assert_eq!("B", tr_name(node.return_type().unwrap()));
}

#[test]
fn parse_lambda_with_two_params() {
    let expr = parse_expr("|a: A, b: B|: C {}").as_lambda();
    let node = expr.fct();

    assert_eq!(2, node.params().len());

    let param0 = node.params_at(0);
    assert_eq!("a", pat_name(param0.pattern().unwrap()));
    assert_eq!("A", tr_name(param0.data_type().unwrap()));

    let param1 = node.params_at(1);
    assert_eq!("b", pat_name(param1.pattern().unwrap()));
    assert_eq!("B", tr_name(param1.data_type().unwrap()));

    assert_eq!("C", tr_name(node.return_type().unwrap()));
}

#[test]
fn parse_for() {
    let expr = parse_expr("for i in a+b {}");
    assert!(expr.is_for());
}

#[test]
fn parse_new_call_ident() {
    let expr = parse_expr("i");
    assert!(expr.is_name_expr());
}

#[test]
fn parse_new_call_path() {
    let expr = parse_expr("Foo::bar").as_path();
    assert!(expr.lhs().is_name_expr());
    assert!(expr.rhs().is_name_expr());
}

#[test]
fn parse_new_call_call() {
    let expr = parse_expr("foo(1,2)").as_call();
    assert!(expr.callee().is_name_expr());
    assert_eq!(expr.arg_list().items().count(), 2);
}

#[test]
fn parse_block() {
    let expr = parse_expr("{1}").as_block();
    assert!(expr.expr().unwrap().is_lit_int());

    let expr = parse_expr("({}) + 1");
    assert!(expr.is_bin());

    let expr = parse_expr("1 + {}");
    assert!(expr.is_bin());
}

#[test]
fn parse_tuple() {
    let expr = parse_expr("(1,)");
    assert_eq!(expr.as_tuple().values().len(), 1);

    let expr = parse_expr("(1)");
    assert!(expr.is_paren());

    let expr = parse_expr("(1,2,3)");
    assert_eq!(expr.as_tuple().values().len(), 3);

    let expr = parse_expr("(1,2,3,4,)");
    assert_eq!(expr.as_tuple().values().len(), 4);
}

#[test]
fn parse_enum() {
    let file = parse("enum Foo { A, B, C }");
    let enum_ = file
        .root()
        .children()
        .find_map(|n| AstEnum::cast(n))
        .unwrap();
    assert_eq!(enum_.variants_len(), 3);
}

#[test]
fn parse_enum_with_type_params() {
    let file = parse("enum MyOption[T] { None, Some(T), }");
    let enum_ = file
        .root()
        .children()
        .find_map(|n| AstEnum::cast(n))
        .unwrap();
    assert_eq!(enum_.variants_len(), 2);
    let variant0 = enum_.variants_at(0);
    assert_eq!(variant0.fields().count(), 0);
    let variant1 = enum_.variants_at(1);
    assert_eq!(variant1.fields().count(), 1);
}

#[test]
fn parse_module() {
    let file = parse("mod foo { fn bar() {} fn baz() {} }");
    let module = file
        .root()
        .children()
        .find_map(|n| AstModule::cast(n))
        .unwrap();
    let element_list = module.element_list().unwrap();
    assert_eq!(element_list.items_len(), 2);
    assert!(element_list.items_at(0).to_function().is_some());
    assert!(element_list.items_at(1).to_function().is_some());
}

#[test]
fn parse_mod_without_body() {
    let file = parse("mod foo;");
    let module = file
        .root()
        .children()
        .find_map(|n| AstModule::cast(n))
        .unwrap();
    assert!(module.element_list().is_none());
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

#[test]
fn parse_ref_type() {
    parse_type("ref Foo");
    parse_type("ref Foo[Bar]");
}

fn tr_name(node: AstType) -> String {
    let node = node.as_regular_type();
    let path = node.path();
    assert_eq!(path.segments_len(), 1);
    let segment = path.segments().next().unwrap();
    segment.as_name().name().clone()
}

fn pat_name(node: AstPattern) -> String {
    node.as_ident_pattern().name().name().to_string()
}

#[test]
fn test_text_length() {
    let code = "fn foo() { baz(); }fn bar() { }";
    let parser = Parser::from_string(code);
    let (file, errors) = parser.parse();
    assert!(errors.is_empty());

    assert_eq!(code.len(), 31);
    let root = file.root();
    assert_eq!(root.text_length(), 31);

    let function = root.children().find_map(|n| AstFunction::cast(n)).unwrap();
    assert_eq!(function.text_length(), 19);
}

#[test]
fn parse_use() {
    parse("use foo::bar::baz;");
    parse("use foo :: bar :: baz;");
    parse("use foo::bar::baz as a;");
}
