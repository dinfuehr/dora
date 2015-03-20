use std::default::Default;

use ast::Expr;
use ast::Function;
use ast::Statement;
use ast::StatementType::*;
use ast::visit::Visitor;

use error::ErrorCode;
use error::ParseError;

use lexer::position::Position;

#[derive(PartialEq,Eq,Copy)]
enum Leave {
    Fix, Maybe, Not
}

impl Leave {
    fn seq(self: Leave, other: Leave) -> Leave {
        assert!(self != Leave::Fix);

        match other {
            Leave::Fix => Leave::Fix,
            Leave::Maybe => Leave::Maybe,
            Leave::Not => self
        }
    }

    fn merge(self: Leave, other: Leave) -> Leave {
        if self == other {
            self
        } else if self == Leave::Fix || other == Leave::Fix {
            Leave::Maybe
        } else if self == Leave::Maybe || other == Leave::Maybe {
            Leave::Maybe
        } else {
            Leave::Not
        }
    }
}

struct LeaveState {
    pub method: Leave,
    pub block: Leave,
}

impl LeaveState {
    fn new() -> LeaveState {
        LeaveState { method: Leave::Not, block: Leave::Not }
    }

    fn method() -> LeaveState {
        LeaveState { method: Leave::Fix, block: Leave::Not }
    }

    fn block() -> LeaveState {
        LeaveState { method: Leave::Not, block: Leave::Fix }
    }

    fn seq(self: &mut LeaveState, o: LeaveState) {
        self.method = self.method.seq(o.method);
        self.block = self.block.seq(o.block);
    }

    fn merge(self: LeaveState, o: LeaveState) -> LeaveState {
        LeaveState {
            method: self.method.merge(o.method),
            block: self.block.merge(o.block)
        }
    }

    fn is_fix(self: &LeaveState) -> bool {
        self.method == Leave::Fix || self.block == Leave::Fix
    }
}

impl Default for LeaveState {
    fn default() -> LeaveState {
        LeaveState::new()
    }
}

pub struct ReturnCheck {
    errors: Vec<ParseError>,
}

impl ReturnCheck {
    pub fn new() -> ReturnCheck {
        ReturnCheck { errors: Vec::new() }
    }

    pub fn errors(self) -> Vec<ParseError> {
        self.errors
    }

    fn check_block(self: &mut ReturnCheck, stmts: &mut Vec<Box<Statement>>) -> LeaveState {
        let mut leave = LeaveState::new();

        if stmts.len() == 0 {
            return leave;
        }

        for stmt in stmts {
            if leave.is_fix() {
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
}

fn end_position(s: &Statement) -> Position {
    match s.stmt {
        Block(ref stmts) if stmts.len() > 0 => {
            stmts[stmts.len()-1].position
        }

        _ => s.position
    }
}

impl Visitor for ReturnCheck {
    type Returns = LeaveState;

    fn visit_fct(self: &mut ReturnCheck, fct: &mut Function) -> LeaveState {
        let leave = if fct.return_type.is_unit() {
            LeaveState::method()
        } else {
            self.visit_stmt(&mut fct.block)
        };

        if leave.method != Leave::Fix {
            self.errors.push(ParseError {
                position: end_position(&fct.block),
                code: ErrorCode::NoReturnValue,
                message: format!("function `{}` does not return a value in all code paths", fct.name)
            })
        }

        leave
    }

    fn visit_stmt(self: &mut ReturnCheck, s: &mut Statement) -> LeaveState {
        match s.stmt {
            If(_, ref mut tblock, ref mut eblock) => {
                let tblock = self.visit_stmt(tblock);
                let eblock = if eblock.is_some() {
                    self.visit_stmt(eblock.as_mut().unwrap())
                } else {
                    LeaveState::new()
                };

                tblock.merge(eblock)
            }

            Block(ref mut stmts) => {
                self.check_block(stmts)
            }

            While(_, ref mut block) => {
                let mut leave = self.visit_stmt(block);
                leave.block = Leave::Not;

                leave
            }

            Loop(ref mut block) => {
                let mut leave = self.visit_stmt(block);
                leave.block = Leave::Not;

                leave
            }

            Return(_) => LeaveState::method(),
            Break | Continue => LeaveState::block(),

            _ => LeaveState::new()
        }
    }

    fn visit_expr(self: &mut ReturnCheck, _: &mut Expr) -> LeaveState {
        LeaveState::new()
    }
}

#[cfg(test)]
mod tests {
    use ast::Function;

    use error::ErrorCode;
    use parser::Parser;

    fn parse(code: &'static str) -> Function {
        let mut prog = Parser::from_str(code).parse().unwrap();

        prog.functions.swap_remove(0)
    }

    fn err(code: &'static str, error_code: ErrorCode, line: u32, col: u32) {
        let err = Parser::from_str(code).parse().unwrap_err();

        assert_eq!(error_code, err.code);
        assert_eq!(line, err.position.line);
        assert_eq!(col, err.position.column);
    }

    #[test]
    fn check_block() {
        err("fn f->int {1;}", ErrorCode::NoReturnValue, 1, 12);
        err("fn f->int {}", ErrorCode::NoReturnValue, 1, 11);
        err("fn f->int {return 1;1;}", ErrorCode::UnreachableCode, 1, 21);
        parse("fn f->int {return 1;}");

        err("fn f {break;}", ErrorCode::MisplacedBreak, 1, 7);
        err("fn f {continue;}", ErrorCode::MisplacedContinue, 1, 7);
    }

    #[test]
    fn check_while() {
        parse("fn f->int {while true {return 1;}}");
        parse("fn f->int {while true {break;} return 1;}");
        parse("fn f->int {while true {continue;} return 1;}");
    }

    #[test]
    fn check_if() {
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
}
