use std::default::Default;

use ast::Expr;
use ast::Function;
use ast::Statement;
use ast::StatementType::*;
use ast::visit::Visitor;

use error::ErrorCode;
use error::ParseError;

use lexer::position::Position;

#[derive(PartialEq,Eq,Copy,Debug)]
enum Leave {
    Fix, Maybe, Not
}

impl Leave {
    fn seq(self: Leave, other: Leave) -> Leave {
        if self == Leave::Fix {
            return Leave::Fix;
        }

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

#[derive(Debug)]
struct ReturnState {
    pub returns: Leave,
    pub breaks: Leave,
    pub continues: Leave,

    pub stops: bool,
}

impl ReturnState {
    fn new() -> ReturnState {
        ReturnState {
            returns: Leave::Not,
            breaks: Leave::Not,
            continues: Leave::Not,
            stops: false,
        }
    }

    fn returns() -> ReturnState {
        ReturnState {
            returns: Leave::Fix,
            breaks: Leave::Not,
            continues: Leave::Not,
            stops: true,
        }
    }

    fn breaks() -> ReturnState {
        ReturnState {
            returns: Leave::Not,
            breaks: Leave::Fix,
            continues: Leave::Not,
            stops: true,
        }
    }

    fn continues() -> ReturnState {
        ReturnState {
            returns: Leave::Not,
            breaks: Leave::Not,
            continues: Leave::Fix,
            stops: true,
        }
    }

    fn for_while(s: ReturnState) -> ReturnState {
        ReturnState {
            returns: s.returns,
            breaks: Leave::Not,
            continues: Leave::Not,
            stops: s.returns == Leave::Fix,
        }
    }

    fn for_loop(s: ReturnState) -> ReturnState {
        ReturnState {
            returns: s.returns,
            breaks: Leave::Not,
            continues: Leave::Not,
            stops: s.returns == Leave::Fix || s.breaks == Leave::Not,
        }
    }

    fn seq(self: &mut ReturnState, o: ReturnState) {
        assert!(!self.stops);

        self.returns = self.returns.seq(o.returns);
        self.continues = self.continues.seq(o.continues);
        self.breaks = self.breaks.seq(o.breaks);
        self.stops = o.stops;
    }

    fn merge(self: ReturnState, o: ReturnState) -> ReturnState {
        ReturnState {
            returns: self.returns.merge(o.returns),
            breaks: self.breaks.merge(o.breaks),
            continues: self.continues.merge(o.continues),
            stops: self.stops && o.stops,
        }
    }
}

impl Default for ReturnState {
    fn default() -> ReturnState {
        ReturnState::new()
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

    fn check_block(self: &mut ReturnCheck, stmts: &mut Vec<Box<Statement>>) -> ReturnState {
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
    type Returns = ReturnState;

    fn visit_fct(self: &mut ReturnCheck, fct: &mut Function) -> ReturnState {
        let leave = self.visit_stmt(&mut fct.block);

        if !fct.return_type.is_unit() && leave.returns != Leave::Fix {
            self.errors.push(ParseError {
                position: end_position(&fct.block),
                code: ErrorCode::NoReturnValue,
                message: format!("function `{}` does not return a value in all code paths", fct.name)
            })
        }

        leave
    }

    fn visit_stmt(self: &mut ReturnCheck, s: &mut Statement) -> ReturnState {
        match s.stmt {
            If(_, ref mut tblock, ref mut eblock) => {
                let tblock = self.visit_stmt(tblock);
                let eblock = if eblock.is_some() {
                    self.visit_stmt(eblock.as_mut().unwrap())
                } else {
                    ReturnState::new()
                };

                tblock.merge(eblock)
            }

            Block(ref mut stmts) => {
                self.check_block(stmts)
            }

            While(_, ref mut block) => {
                ReturnState::for_while(self.visit_stmt(block))
            }

            Loop(ref mut block) => {
                ReturnState::for_loop(self.visit_stmt(block))
            }

            Return(_) => ReturnState::returns(),
            Break => ReturnState::breaks(),
            Continue => ReturnState::continues(),

            _ => ReturnState::new()
        }
    }

    fn visit_expr(self: &mut ReturnCheck, _: &mut Expr) -> ReturnState {
        ReturnState::new()
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
