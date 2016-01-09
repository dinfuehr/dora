pub mod cmd;

use std;
use std::mem;

use ast::{self, Function};
use ctxt::Context;
use error::msg::Msg;

use jit::codegen::CodeGen;
use jit::fct::JitFct;
use lexer::position::Position;

use parser::Parser;
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

    let cg = CodeGen::new(&ctxt, main);
    let jit_fct = cg.generate();

    if args.flag_emit_asm {
        dump_asm(&jit_fct, &ctxt.interner.str(main.name));
    }

    let fct : extern "C" fn() -> i32 = unsafe { mem::transmute(jit_fct.fct_ptr()) };
    let res = fct();

    // main-fct without return value exits with status 0
    if main.return_type.is_none() {
        0

    // use return value of main for exit status
    } else {
        res
    }
}

pub fn dump_asm(jit_fct: &JitFct, name: &str) {
    use capstone::*;

    let buf: &[u8] = unsafe { std::slice::from_raw_parts(jit_fct.fct_ptr(), jit_fct.fct_len()) };

    let engine = Engine::new(Arch::X86, MODE_64).expect("cannot create capstone engine");
    engine.set_option(Opt::Syntax, 2); // switch to AT&T syntax
    let instrs = engine.disasm(buf,
        jit_fct.fct_ptr() as u64, jit_fct.fct_len()).expect("could not disassemble code");

    println!("fn {}", name);
    for instr in instrs {
        println!("  {:#06x}: {}\t\t{}", instr.addr, instr.mnemonic, instr.op_str);
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

    ctxt.fct_info_for_id(fctid, |fct_info| {
        let ret = fct_info.return_type;

        if (ret != BuiltinType::Unit && ret != BuiltinType::Int) ||
            fct_info.params_types.len() > 0 {
            ctxt.diag.borrow_mut().report(fct_info.ast.unwrap().pos, Msg::WrongMainDefinition);
            return None;
        }

        Some(fct_info.ast.unwrap())
    })
}
