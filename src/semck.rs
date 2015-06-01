use ast::Ast;
use ast::ElemType::*;
use ast::Expr;
use ast::ExprType::*;
use ast::Function;
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

type SemResult = Result<(), ParseError>;

struct SemCheck<'a> {
    global: SymbolTable,
    ast: &'a Ast,
}

impl<'a> Visitor<'a> for SemCheck<'a> {
    type Returns = ();

    fn visit_fct(&mut self, fct: &'a Function) -> SemResult {
        assert!(fct.params.len() == 0, "no fct params supported");
        assert!(fct.type_params.len() == 0, "no fct type params supported");
        assert!(fct.return_type.is_int(), "fct only allowed to return int");

        self.visit_stmt(&fct.block)
    }

    fn visit_stmt(&mut self, s: &'a Stmt) -> SemResult {
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

    fn visit_expr(&mut self, e: &Expr) -> SemResult {
        match e.node {
            ExprLitInt(_) => {
                Ok(())
            }

            _ => unimplemented(e.pos)
        }
    }
}

impl<'a> SemCheck<'a> {
    pub fn new(ast: &'a Ast) -> SemCheck {
        SemCheck {
            global: SymbolTable::new(),
            ast: ast,
        }
    }

    pub fn check(&mut self) -> SemResult {
        // check all functions
        try!(self.visit_ast(self.ast));

        // check main definition
        self.check_main_fct(self.ast)
    }

    fn check_main_fct(&mut self, prog: &Ast) -> SemResult {
        let fct = prog.function("main");

        if fct.is_none() {
            return err(Position::new(1, 1), "main function not found".to_string(),
                ErrorCode::MainDefinition)
        }

        let fct = fct.unwrap();

        if !fct.type_params.empty() {
            return err(fct.pos,
                "main function is not allowed to have any type parameters".to_string(),
                ErrorCode::MainDefinition)
        }

        if fct.params.len() > 0 {
            return err(fct.pos,
                "main function is not allowed to have any parameters".to_string(),
                ErrorCode::MainDefinition)
        }

        if !fct.return_type.is_int() {
            return err(fct.pos,
                "main function needs to return `int`".to_string(),
                ErrorCode::MainDefinition)
        }

        Ok(())
    }
}

#[cfg(test)]
fn ck(code: &'static str) -> SemResult {
    let ast = Parser::from_str(code).parse().unwrap();

    SemCheck::new(&ast).check()
}

#[test]
fn test_main_undefined() {
    assert!(ck("").is_err());
}

#[test]
fn test_main() {
    assert!(ck("fn main() -> int { return 0; }").is_ok());
}

