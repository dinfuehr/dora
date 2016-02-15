use libc;
use std;
use std::mem;

use ast::{self, Function};
use ctxt::{Context, ctxt_ptr, FctId};
use driver::cmd;
use error::msg::Msg;

use jit;
use jit::fct::JitFct;
use lexer::position::Position;
use mem::ptr::Ptr;
use os;

use parser::Parser;
use semck;
use sym::*;
use ty::BuiltinType;

pub fn start() -> i32 {
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

    let mut ctxt = Context::new(args, &ast, interner);

    semck::check(&mut ctxt);

    // register signal handler
    os::register_signals(&ctxt);

    let main = find_main(&ctxt);

    if ctxt.diag.borrow().has_errors() {
        ctxt.diag.borrow().dump(&ctxt);

        println!("{} error(s) found", ctxt.diag.borrow().errors().len());
        return 1;
    }

    let (main_id, main) = main.unwrap();
    let fct_ptr = jit::generate(&ctxt, main_id);

    let fct : extern "C" fn() -> i32 = unsafe { mem::transmute(fct_ptr) };
    let res = fct();

    // main-fct without return value exits with status 0
    if main.return_type.is_none() {
        0

    // use return value of main for exit status
    } else {
        res
    }
}

fn find_main<'ast>(ctxt: &Context<'ast>) -> Option<(FctId, &'ast Function)> {
    let name = ctxt.interner.intern("main");
    let fctid = match ctxt.sym.borrow().get_fct(name) {
        Some(id) => id,
        None => {
            ctxt.diag.borrow_mut().report(Position::new(1, 1), Msg::MainNotFound);
            return None;
        }
    };

    ctxt.fct_by_id(fctid, |fct| {
        let ret = fct.return_type;

        if (ret != BuiltinType::Unit && ret != BuiltinType::Int)
            || fct.params_types.len() > 0 {
            ctxt.diag.borrow_mut().report(fct.ast().pos, Msg::WrongMainDefinition);
            return None;
        }

        Some((fctid, fct.ast()))
    })
}
