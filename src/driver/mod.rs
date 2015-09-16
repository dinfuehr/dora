pub mod cmd;

use std::mem;
use std::cell::RefCell;
use std::collections::HashMap;

use codegen::codegen::CodeGen;
use mem::CodeMemory;
use parser::ast::ctxt::Context;
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
    let buffer = cg.generate().finish();

    if args.flag_emit_asm {
        dump_asm(&buffer, &ctxt.interner.str(main.name));
    }

    let code = CodeMemory::new(&buffer);

    let fct : extern "C" fn() -> i32 = unsafe { mem::transmute(code.ptr()) };
    let res = fct();

    // main-fct without return value exits with status 0
    if main.return_type.is_none() {
        0

    // use return value of main for exit status
    } else {
        res
    }
}

pub fn dump_asm(buf: &[u8], name: &str) {
    use capstone::*;

    let engine = Engine::new(Arch::X86, MODE_64).expect("cannot create capstone engine");
    engine.set_option(Opt::Syntax, 2); // switch to AT&T syntax
    let instrs = engine.disasm(buf, 0, buf.len()).expect("could not disassemble code");

    println!("fn {}", name);
    for instr in instrs {
        println!("  {:#06x}: {}\t\t{}", instr.addr, instr.mnemonic, instr.op_str);

        print!("\t  ");

        for byte in &instr.bytes {
            print!("{:02x} ", byte);
        }

        println!("");
    }
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

    let fct_infos = ctxt.fct_infos.borrow();
    let fct = fct_infos.get(fctid.0).unwrap();
    let return_type = fct.return_type;

    if (return_type != BuiltinType::Unit && return_type != BuiltinType::Int) ||
        fct.params_types.len() > 0 {
        ctxt.diag.borrow_mut().report(fct.ast.unwrap().pos, Msg::WrongMainDefinition);
        return None;
    }

    Some(fct.ast.unwrap())
}
