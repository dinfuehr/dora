use std::mem;

use ast::{self, Ast};
use ctxt::{Context, FctId};
use driver::cmd::{self, Args};
use error::msg::Msg;

use interner::Interner;
use jit;
use lexer::position::Position;
use os;

use parser::Parser;
use semck;
use ty::BuiltinType;

pub fn start() -> i32 {
    let args = cmd::parse();

    if args.flag_version {
        println!("dora v0.01b");
        return 0;
    }

    let mut interner = Interner::new();
    let mut ast = Ast::new();

    if let Err(code) = parse_file(&mut ast, &args, &mut interner) {
        return code;
    }

    if args.flag_emit_ast {
        ast::dump::dump(&ast, &interner);
    }

    let mut ctxt = Context::new(args, &ast, interner);

    semck::check(&mut ctxt);

    // register signal handler
    os::register_signals(&ctxt);

    let main = find_main(&ctxt);

    if ctxt.diag.borrow().has_errors() {
        ctxt.diag.borrow().dump();
        let no_errors = ctxt.diag.borrow().errors().len();

        if no_errors == 1 {
            println!("{} error found.", no_errors);
        } else {
            println!("{} errors found.", no_errors);
        }

        return 1;
    }

    let main = main.unwrap();
    let fct_ptr = jit::generate(&ctxt, main);

    let fct : extern "C" fn() -> i32 = unsafe { mem::transmute(fct_ptr) };
    let res = fct();

    if ctxt.args.flag_gc_stats {
        let gc = ctxt.gc.lock().unwrap();

        println!("GC stats: {} ms duration", in_ms(gc.duration));
        println!("\tmalloc duration: {} ms", in_ms(gc.malloc_duration));
        println!("\tcollect duration: {} ms", in_ms(gc.collect_duration));
        println!("\tsweep duration: {} ms", in_ms(gc.sweep_duration));
        println!("\t{} collections", gc.collections);
        println!("\t{} bytes allocated", gc.total_allocated);
    }

    let is_unit = ctxt.fct_by_id(main).return_type.is_unit();

    // main-fct without return value exits with status 0
    if is_unit {
        0

    // else use return value of main for exit status
    } else {
        res
    }
}

fn in_ms(ns: u64) -> u64 {
    ns / 1000 / 1000
}

fn parse_file(ast: &mut Ast, args: &Args, interner: &mut Interner) -> Result<(), i32> {
    let mut parser = match Parser::from_file(&args.arg_file, interner) {
        Err(_) => {
            println!("unable to read file `{}`", &args.arg_file);
            return Err(1);
        }

        Ok(parser) => parser
    };

    if let Err(error) = parser.parse(ast) {
        error.print();
        return Err(1);
    }

    Ok(())
}

fn find_main<'ast>(ctxt: &Context<'ast>) -> Option<FctId> {
    let name = ctxt.interner.intern("main");
    let fctid = match ctxt.sym.borrow().get_fct(name) {
        Some(id) => id,
        None => {
            ctxt.diag.borrow_mut().report(Position::new(1, 1), Msg::MainNotFound);
            return None;
        }
    };

    let fct = ctxt.fct_by_id(fctid);
    let ret = fct.return_type;

    if (ret != BuiltinType::Unit && ret != BuiltinType::Int)
        || fct.params_types.len() > 0 {
        let pos = fct.kind.src().ast.pos;
        ctxt.diag.borrow_mut().report(pos, Msg::WrongMainDefinition);
        return None;
    }

    Some(fctid)
}
