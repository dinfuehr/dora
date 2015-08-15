use ast;
use ast::Ast;
use ast::Function;
use ast::Elem::ElemFunction;

use error::ErrorCode;
use error::ParseError;

use parser::Parser;

use sym;
use sym::SymbolTable;
use sym::Sym::SymFunction;
use sym::SymFunctionType;

// Only do semantic analysis until some amount of
// errors found
static max_errors: usize = 5;

pub struct SemCheck<'a> {
    ast: &'a Ast,
    symbols: SymbolTable,
    errors: Vec<ParseError>,
}

impl<'a> SemCheck<'a> {
    pub fn new(ast: &Ast) -> SemCheck {
        SemCheck {
            ast: ast,
            symbols: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    pub fn check(&mut self) -> Result<(), ()> {
        for elem in &self.ast.elements {
            match *elem {
                ElemFunction(ref fct) => try!(self.add_function_header(fct)),
                _ => unreachable!()
            }
        }

        Ok(())
    }

    fn add_function_header(&mut self, fct: &Function) -> Result<(), ()> {
        let params = fct.params.iter().map(|p| param_header(p));
        let symfct = SymFunction(SymFunctionType {
            name: fct.name,
            return_type: fct.return_type.builtin,
            params: params.collect(),
            body: fct.block.id()
        });

        if let Err(_) = self.symbols.insert(fct.name, symfct) {
            try!(self.error(ParseError {
                code: ErrorCode::IdentifierAlreadyExists,
                message: format!("identifier {} already exists", self.ast.str(fct.name)),
                position: fct.pos
            }));
        }

        Ok(())
    }

    fn error(&mut self, error: ParseError) -> Result<(), ()> {
        self.errors.push(error);

        if self.errors.len() <= max_errors {
            Ok(())
        } else {
            Err(())
        }
    }

    fn errors(self) -> Vec<ParseError> {
        self.errors
    }

    fn check_function(&mut self, fct: &Function) {
        println!("check_function");
    }
}

fn param_header(param: &ast::Param) -> sym::Param {
    sym::Param {
        name: param.name,
        data_type: param.data_type.builtin
    }
}

#[test]
fn test_empty_file() {
    let prog = Parser::from_str("").parse().unwrap();

    let mut check = SemCheck::new(&prog);
    check.check().unwrap();
}

#[test]
fn test_function() {
    let prog = Parser::from_str("fn main() { }").parse().unwrap();
}

