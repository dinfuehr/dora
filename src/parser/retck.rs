use ast::Expr;
use ast::Function;
use ast::Statement;
use ast::StatementType::*;
use ast::visit::Visitor;

use error::ErrorCode;
use error::ParseError;

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

    fn check_block(self: &mut ReturnCheck, stmts: &mut Vec<Box<Statement>>) -> bool {
        if stmts.len() == 0 {
            return false;
        }

        let mut return_occured = false;

        for stmt in stmts {
            if return_occured {
                self.errors.push(ParseError {
                    position: stmt.position,
                    code: ErrorCode::UnreachableCode,
                    message: format!("statement unreachable")
                });

                return true;
            }

            return_occured = self.visit_stmt(stmt);
        }

        return_occured
    }
}

impl Visitor for ReturnCheck {
    type Returns = bool;

    fn visit_fct(self: &mut ReturnCheck, fct: &mut Function) -> bool {
        let return_occured = if fct.return_type.is_unit() {
            true
        } else {
            self.visit_stmt(&mut fct.block)
        };

        if !return_occured {
            self.errors.push(ParseError {
                position: fct.position,
                code: ErrorCode::NoReturnValue,
                message: format!("function `{}` does not return a value in all code paths", fct.name)
            })
        }

        return_occured
    }

    fn visit_stmt(self: &mut ReturnCheck, s: &mut Statement) -> bool {
        match s.stmt {
            If(_, ref mut tblock, ref mut eblock) => {
                let eblock = if eblock.is_some() {
                    self.visit_stmt(eblock.as_mut().unwrap())
                } else {
                    false
                };

                eblock && self.visit_stmt(tblock)
            }

            Block(ref mut stmts) => {
                self.check_block(stmts)
            }

            While(_, ref mut block) => {
                self.visit_stmt(block)
            }

            Loop(ref mut block) => {
                self.visit_stmt(block)
            }

            Return(_) => true,

            _ => false
        }
    }

    fn visit_expr(self: &mut ReturnCheck, _: &mut Expr) -> bool {
        false
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
        err("fn f->int {1;}", ErrorCode::NoReturnValue, 1, 1);
        err("fn f->int {}", ErrorCode::NoReturnValue, 1, 1);
        err("fn f->int {return 1;1;}", ErrorCode::UnreachableCode, 1, 21);
        parse("fn f->int {return 1;}");
    }

    #[test]
    fn check_if() {
        parse("fn f->int {if true {return 1;} else {return 2;}}");
        err("fn f->int {if true {return 1;}}", ErrorCode::NoReturnValue, 1, 1);
        err("fn f->int {if true {return 1;} else {}}", ErrorCode::NoReturnValue, 1, 1);
    }
}
