pub mod cmd;
pub mod ctxt;

use std::cell::RefCell;
use std::collections::HashMap;

use codegen::codegen::CodeGen;
use driver::ctxt::Context;
use error::diag::Diagnostic;
use error::msg::Msg;

use parser::ast::{self, Function};
use parser::Parser;
use parser::lexer::position::Position;
use semck;
use sym::*;

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
    let ctxt = Context::new(&args, &interner, &ast_map, &ast);

    semck::check(&ctxt);

    let main = find_main(&ctxt);

    if ctxt.diag.borrow().has_errors() {
        ctxt.diag.borrow().dump();

        println!("{} error(s) found", ctxt.diag.borrow().errors().len());
        return 1;
    }

    let main = main.unwrap();

    let mut cg = CodeGen::new(&ctxt, main);
    cg.generate();

    0 // success
}

fn find_main<'a, 'ast>(ctxt: &Context<'a, 'ast>) -> Option<&'ast Function> where 'a: 'ast {
    let name = ctxt.interner.intern("main");
    let fctid = match ctxt.sym.borrow().get_function(name) {
        Some(id) => id,
        None => {
            ctxt.diag.borrow_mut().report(Position::new(1, 1), Msg::MainNotFound);
            return None;
        }
    };

    let fct = ctxt.map.entry(fctid).to_fct().unwrap();
    let return_type = *ctxt.types.borrow().get(&fctid).unwrap();

    if (return_type != BuiltinType::Unit && return_type != BuiltinType::Int) ||
        fct.params.len() > 0 {
        ctxt.diag.borrow_mut().report(fct.pos, Msg::WrongMainDefinition);
        return None;
    }

    Some(fct)
}
