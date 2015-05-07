use ast::Expr;
use ast::ExprType::*;
use ast::Program;
use ast::Stmt;
use ast::StmtType::*;
use ast::visit::Visitor;
use error::err;
use error::unimplemented;
use error::ErrorCode;
use error::ParseError;
use lexer::position::Position;
use parser::Parser;
use sym::Sym::*;
use sym::SymbolTable;

struct SemCheck<'a> {
    program: &'a Program,
    global: SymbolTable,
}

impl<'a> Visitor for SemCheck<'a> {
    type Returns = ();

    fn visit_stmt(&mut self, s: &Stmt) -> Result<(), ParseError> {
        match s.node {
            StmtReturn(ref expr) => {
                if expr.is_none() {
                    return err(s.pos, "no return value given".to_string(),
                        ErrorCode::ExpectedValue);
                }

                try!(self.visit_expr(&expr.as_ref().unwrap()));

                Ok(())
            }

            StmtBlock(ref stmts) => {
                for stmt in stmts {
                    try!(self.visit_stmt(stmt));
                }

                Ok(())
            }

            _ => unimplemented(s.pos)
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> Result<(), ParseError> {
        match e.node {
            ExprLitInt(val) => {
                Ok(())
            }

            _ => unimplemented(e.pos)
        }
    }
}

impl<'a> SemCheck<'a> {
    pub fn new(prog: &Program) -> SemCheck {
        SemCheck { program: prog, global: SymbolTable::new() }
    }

    pub fn check(&mut self) -> Result<(), ParseError> {
        self.check_main()
    }


    fn check_main(&mut self) -> Result<(), ParseError> {
        let fct = self.program.function("main");

        if fct.is_none() {
            return err(Position::new(1, 1), "main not found".to_string(),
                ErrorCode::MainDefinition)
        }

        let fct = fct.unwrap();

        if !fct.type_params.empty() || fct.params.len() > 0 ||
            !fct.return_type.is_int() {
            return err(fct.position, "definition of main not correct".to_string(),
                ErrorCode::MainDefinition)
        }

        try!(self.visit_stmt(&fct.block));

        Ok(())
    }
}

#[cfg(test)]
fn ck(code: &'static str) -> Result<(), ParseError> {
    let prog = Parser::from_str(code).parse().unwrap();

    SemCheck::new(&prog).check()
}

#[test]
fn test_main_undefined() {
    assert!(ck("fn foo() {}").is_err());
}

#[test]
fn test_main_definition_invalid() {
    assert!(ck("fn main(x:int) {}").is_err());
}

#[test]
fn test_main_no_return_value() {
    assert!(ck("fn main() {}").is_err());
}

#[test]
fn test_main() {
    assert!(ck("fn main() -> int { return 0; }").is_ok());
}

