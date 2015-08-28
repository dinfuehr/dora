pub mod cmd;
pub mod ctxt;

use std::cell::RefCell;
use std::collections::HashMap;

use self::ctxt::Context;
use error::diag::Diagnostic;

use parser::ast;
use parser::Parser;
use semck;
use sym::SymTable;

pub fn compile() -> i32 {
    let args = cmd::parse();

    if args.flag_version {
        println!("dora v0.01b");
        return 0;
    }

    let mut parser = match Parser::from_file(&args.arg_file) {
        Err(_) => {
            println!("unable to read file `{}`", &args.arg_file);
            return 1;
        }

        Ok(parser) => parser
    };

    let (ast, interner) = match parser.parse() {
        Ok(ret) => ret,

        Err(error) => {
            error.print();
            return 1;
        }
    };

    if args.flag_emit_ast {
        ast::dump::dump(&ast, &interner);
    }

    let ast_map = ast::map::build(&ast, &interner);

    let mut ctxt = Context {
        args: &args,
        interner: &interner,
        map: &ast_map,
        ast: &ast,
        diag: RefCell::new(Diagnostic::new()),
        sym: RefCell::new(SymTable::new()),
        types: RefCell::new(HashMap::new())
    };

    semck::check(&ctxt);

    if ctxt.diag.borrow().has_errors() {
        ctxt.diag.borrow().dump();

        println!("{} error(s) found", ctxt.diag.borrow().errors());
        return 1;
    }

    0 // success
}
