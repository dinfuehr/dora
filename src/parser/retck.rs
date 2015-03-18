use ast::Expr;
use ast::Function;
use ast::Statement;
use ast::StatementType::*;
use ast::visit::Visitor;
use ast::visit::walk_stmt;

pub struct ReturnCheck;

impl ReturnCheck {
    pub fn new() -> ReturnCheck {
        ReturnCheck
    }
}

impl Visitor for ReturnCheck {
    type Returns = bool;

    fn visit_fct(self: &mut ReturnCheck, fct: &mut Function) -> bool {
        if fct.return_type.is_unit() {
            true
        } else {
            walk_stmt(self, &mut fct.block)
        }
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
                if stmts.len() > 0 {
                    let last = stmts.len()-1;
                    self.visit_stmt(&mut stmts[last])
                } else {
                    false
                }
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
        err("fn f->int {1;}", ErrorCode::NoReturnValue, 1, 12);
        err("fn f->int {}", ErrorCode::NoReturnValue, 1, 11);
        err("fn f->int {return 1;1;}", ErrorCode::UnreachableCode, 1, 22);
        parse("fn f->int {return 1;}");
    }

    #[test]
    fn check_if() {
        parse("fn f->int {if true {return 1;} else {return 2;}}");
        err("fn f->int {if true {return 1;}}", ErrorCode::NoReturnValue, 1, 12);
        err("fn f->int {if true {return 1;} else {}}", ErrorCode::NoReturnValue, 1, 12);
    }
}
