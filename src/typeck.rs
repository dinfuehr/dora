use ast::Program;

use error::ErrorCode;
use error::ParseError;

use lexer::position::Position;

use parser::Parser;

struct TypeCheck {
    program: Program,
}

impl TypeCheck {
    pub fn new(prog: Program) -> TypeCheck {
        TypeCheck { program: prog }
    }

    pub fn check(&self) -> Result<(), ParseError> {
        self.check_main()
    }

    fn check_main(&self) -> Result<(), ParseError> {
        let fct = self.program.get_function("main");

        if fct.is_none() {
            return Err(ParseError {
                position: Position::new(1, 1),
                message: "main not found".to_string(),
                code: ErrorCode::MainDefinition
            })
        }

        let fct = fct.unwrap();

        if !fct.type_params.empty() || fct.params.len() > 0 ||
            !fct.return_type.is_unit() {
            return Err(ParseError {
                position: fct.position,
                message: "definition of main not correct".to_string(),
                code: ErrorCode::MainDefinition
            })
        }

        Ok(())
    }
}

#[cfg(test)]
fn ck(code: &'static str) -> Result<(), ParseError> {
    let prog = Parser::from_str(code).parse().unwrap();

    TypeCheck::new(prog).check()
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
fn test_main() {
    assert!(ck("fn main() {}").is_ok());
}
