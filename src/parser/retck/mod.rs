use std::default::Default;

use ast::Expr;
use ast::ExprType::*;
use ast::Function;
use ast::Statement;
use ast::StatementType::*;
use ast::visit::Visitor;
use ast::visit::walk_expr;

use error::ErrorCode;
use error::ParseError;

use lexer::position::Position;
use self::leave::{Leave, ReturnState};
use self::init::InitVars;

mod init;
mod leave;

pub struct ReturnCheck<'a> {
    errors: Vec<ParseError>,

    initialized_vars: InitVars<usize>,
    fct: Option<&'a Function>,
}

impl<'a> ReturnCheck<'a> {
    pub fn new() -> ReturnCheck<'a> {
        ReturnCheck {
            errors: Vec::new(),
            initialized_vars: InitVars::new(),
            fct: None,
        }
    }

    pub fn errors(self) -> Vec<ParseError> {
        self.errors
    }

    fn reset_fct(&mut self, fct: &Function) {
        // reset initialized vars with each function
        self.initialized_vars.reset();

        // parameters are always initialized
        for i in &fct.params {
            self.initialized_vars.insert(*i);
        }
    }

    fn check_block(&mut self, stmts: &Vec<Box<Statement>>) -> ReturnState {
        let mut leave = ReturnState::new();

        if stmts.len() == 0 {
            return leave;
        }

        for stmt in stmts {
            if leave.stops {
                self.errors.push(ParseError {
                    position: stmt.position,
                    code: ErrorCode::UnreachableCode,
                    message: format!("statement unreachable")
                });

                return leave;
            }

            leave.seq(self.visit_stmt(stmt));
        }

        leave
    }

    fn check_while(&mut self, cond: &Expr, block: &Statement) -> ReturnState {
        self.visit_expr(cond);

        self.initialized_vars.push();
        let ret = self.visit_stmt(block);
        self.initialized_vars.pop();

        ReturnState::for_while(ret)
    }

    fn check_if(&mut self, cond: &Expr, tblock: &Statement, eblock: &Option<Box<Statement>>) -> ReturnState {
        self.visit_expr(cond);

        self.initialized_vars.push();
        let tblock = self.visit_stmt(tblock);
        let vars_then = self.initialized_vars.pop();

        self.initialized_vars.push();
        let eblock = if let Some(eblock) = eblock.as_ref() {
            self.visit_stmt(eblock)
        } else {
            ReturnState::new()
        };
        let vars_else = self.initialized_vars.pop();

        let union = vars_then.intersection(&vars_else).cloned().collect();
        self.initialized_vars.push_hash(union);
        tblock.merge(eblock)
    }

    fn check_var(&mut self, varind: usize, expr: &Option<Box<Expr>>) -> ReturnState {
        if let Some(expr) = expr.as_ref() {
            self.visit_expr(expr);
            self.initialized_vars.insert(varind);
        }

        ReturnState::new()
    }
}

fn end_position(s: &Statement) -> Position {
    match s.stmt {
        Block(ref stmts) if stmts.len() > 0 => {
            stmts[stmts.len()-1].position
        }

        _ => s.position
    }
}

impl<'a> Visitor<'a> for ReturnCheck<'a> {
    type Returns = ReturnState;

    fn visit_fct(&mut self, fct: &'a Function) -> ReturnState {
        self.fct = Some(fct);

        // reset initialized variables for each function
        self.reset_fct(fct);

        let leave = self.visit_stmt(&fct.block);

        if !fct.return_type.is_unit() && leave.returns != Leave::Fix {
            self.errors.push(ParseError {
                position: end_position(&fct.block),
                code: ErrorCode::NoReturnValue,
                message: format!("function `{}` does not return a value in all code paths", fct.name)
            })
        }

        leave
    }

    fn visit_stmt(&mut self, s: &Statement) -> ReturnState {
        match s.stmt {
            Var(varind, _, ref expr) => self.check_var(varind, expr),

            If(ref cond, ref tblock, ref eblock) => self.check_if(cond, tblock, eblock),
            Block(ref stmts) => self.check_block(stmts),

            While(ref cond, ref block) => self.check_while(cond, block),
            Loop(ref block) => ReturnState::for_loop(self.visit_stmt(block)),

            Return(ref expr) => {
                if let Some(expr) = expr.as_ref() {
                    self.visit_expr(expr);
                }

                ReturnState::returns()
            }

            ExprStmt(ref expr) => {
                self.visit_expr(expr);

                ReturnState::new()
            }

            Break => ReturnState::breaks(),
            Continue => ReturnState::continues(),

            _ => ReturnState::new()
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> ReturnState {
        match e.expr {
            Assign(ref var, ref expr) => {
                self.visit_expr(expr);

                match var.expr {
                    Ident(varind) => {
                        self.initialized_vars.insert(varind);
                    }

                    _ => unreachable!()
                }

                Default::default()
            }

            Ident(varind) => {
                if !self.initialized_vars.contains(&varind) {
                    let fct = self.fct.unwrap();
                    let var = &fct.vars[varind];

                    self.errors.push(ParseError {
                        position: e.position,
                        code: ErrorCode::UninitializedVar,
                        message: format!("variable `{}` might not be initialized", var.name)
                    });
                }

                Default::default()
            }

            _ => walk_expr(self, e)
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::Function;

    use error::ErrorCode;
    use parser::Parser;

    fn parse(code: &'static str) -> Function {
        let mut prog = match Parser::from_str(code).parse() {
            Ok(prog) => prog,
            Err(err) => panic!("`{}` throws `{}` but no error expected", code, err),
        };

        prog.functions.swap_remove(0)
    }

    fn err(code: &'static str, error_code: ErrorCode, line: u32, col: u32) {
        let err = match Parser::from_str(code).parse() {
            Ok(_) => panic!("no error found in `{}`, expected {:?} at {}:{}",
                            code, error_code, line, col),
            Err(err) => err,
        };

        assert_eq!(error_code, err.code);
        assert_eq!(line, err.position.line);
        assert_eq!(col, err.position.column);
    }

    #[test]
    fn check_initialized() {
        parse("fn f(a: int) { a; }");
        parse("fn f { var a = 3; a; }");
        parse("fn f { var a : int; a = 3; a; }");

        err("fn f { var a : int; a; }", ErrorCode::UninitializedVar, 1, 21);
        err("fn f -> int { var a : int; return a; }", ErrorCode::UninitializedVar, 1, 35);

        parse("fn f { var a : int; if true { a = 1; } else { a = 2; } a; }");
        err("fn f { var a : int; if true {} else { a = 2; } a; }", ErrorCode::UninitializedVar, 1, 48);
        err("fn f { var a : int; if true { a = 1; } else {} a; }", ErrorCode::UninitializedVar, 1, 48);

        err("fn f { var a : bool; while a {} }", ErrorCode::UninitializedVar, 1, 28);
        err("fn f { var a : int; while true { a = 1; } a; }", ErrorCode::UninitializedVar, 1, 43);
    }

    #[test]
    fn check_return() {
        err("fn f -> int { 1; }", ErrorCode::NoReturnValue, 1, 15);
        err("fn f -> int {}", ErrorCode::NoReturnValue, 1, 13);
        err("fn f -> int { return 1; 1; }", ErrorCode::UnreachableCode, 1, 25);
        err("fn f { return; 1; }", ErrorCode::UnreachableCode, 1, 16);

        parse("fn f -> int { return 1; }");
        parse("fn f { return; }");
        parse("fn f { 1; }");
        parse("fn f {}");

        parse("fn f -> int { while true { return 1; } }");
        err("fn f -> int { while true { return 1; } return 1; }", ErrorCode::UnreachableCode, 1, 40);

        parse("fn f { while true { break; } return; }");
        parse("fn f { while true { continue; } return; }");

        parse("fn f->int {\
               if true { return 1; }\
               else { return 2; } }");
        err("fn f->int {\
             if true { return 1; } }", ErrorCode::NoReturnValue, 1, 12);
        err("fn f->int {\
             if true { return 1; } else { } }", ErrorCode::NoReturnValue, 1, 12);
        err("fn f->int {\
             if true { return 1; } else { return 2; }\
             return 3; }",
           ErrorCode::UnreachableCode, 1, 52);
    }

    #[test]
    fn check_break() {
        parse("fn x { while true { break; } return; }");
        parse("fn x { loop { break; } return; }");

        parse("fn x { loop { loop { break; } break; } }");
        parse("fn x { while true { while true { break; } break; } }");

        parse("fn x { loop { if true { break; } break; } }");
        parse("fn x { while true { if true { break; } break; } }");

        err("fn x { loop { if true { break; } else { break; } break; } }",
            ErrorCode::UnreachableCode, 1, 50);
        err("fn x { while true { if true { break; } else { break; } break; } }",
            ErrorCode::UnreachableCode, 1, 56);

        err("fn x { while true { break; return; } }", ErrorCode::UnreachableCode, 1, 28);
        err("fn x { loop { break; return; } }", ErrorCode::UnreachableCode, 1, 22);

        err("fn x { break; }", ErrorCode::MisplacedBreak, 1, 8);
        err("fn x { if true { break; } }", ErrorCode::MisplacedBreak, 1, 18);
        err("fn x { loop { if true { break; } else { return; } break; } }",
            ErrorCode::UnreachableCode, 1, 51);
    }

    #[test]
    fn check_continue() {
        parse("fn x { while true { continue; } return; }");
        err("fn x { loop { continue; } return; }",
           ErrorCode::UnreachableCode, 1, 27);

        parse("fn x { while true { while true { continue; } continue; } }");
        err("fn x { loop { loop { continue; } continue; } }",
           ErrorCode::UnreachableCode, 1, 34);

        parse("fn x { while true { if true { continue; } continue; } }");
        parse("fn x { loop { if true { continue; } continue; } }");

        err("fn x { loop { if true { continue; } else { continue; } continue; } }",
            ErrorCode::UnreachableCode, 1, 56);
        err("fn x { while true { if true { continue; } else { continue; } continue; } }",
            ErrorCode::UnreachableCode, 1, 62);

        err("fn x { while true { continue; return; } }", ErrorCode::UnreachableCode, 1, 31);
        err("fn x { loop { continue; return; } }", ErrorCode::UnreachableCode, 1, 25);

        err("fn x { continue; }", ErrorCode::MisplacedContinue, 1, 8);
        err("fn x { if true { continue; } }", ErrorCode::MisplacedContinue, 1, 18);
        err("fn x { loop { if true { continue; } else { return; } continue; } }",
            ErrorCode::UnreachableCode, 1, 54);
    }
}
