use std::fs;
use std::path::Path;

use ctxt::VM;
use ctxt::{exception_get_and_clear, Fct, FctId};
use dora_parser::ast::{self, Ast};
use dora_parser::error::msg::Msg;

use dora_parser::interner::Interner;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::reader::Reader;
use driver::cmd;
use boots::bytecodegen::BytecodeGen;
use object;
use os;
use timer::Timer;

use dora_parser::parser::{NodeIdGenerator, Parser};
use semck;
use semck::specialize::specialize_class_id;
use ty::BuiltinType;

pub fn start() -> i32 {
    let args = cmd::parse();

    if args.flag_version {
        println!("dora v0.01b");
        return 0;
    }

    let mut interner = Interner::new();
    let id_generator = NodeIdGenerator::new();
    let mut ast = Ast::new();

    if let Err(code) = parse_dir("stdlib", &id_generator, &mut ast, &mut interner).and_then(|_| {
        let path = Path::new(&args.arg_file);

        if path.is_file() {
            parse_file(&args.arg_file, &id_generator, &mut ast, &mut interner)
        } else if path.is_dir() {
            parse_dir(&args.arg_file, &id_generator, &mut ast, &mut interner)
        } else {
            println!("file or directory `{}` does not exist.", &args.arg_file);
            Err(1)
        }
    }) {
        return code;
    }

    if args.flag_emit_ast {
        ast::dump::dump(&ast, &interner);
    }

    {
        let mut bytecodegen = BytecodeGen::new();
        bytecodegen.gen(&ast);
    }

    let mut vm = VM::new(args, &ast, interner);

    semck::check(&mut vm);

    // register signal handler
    os::register_signals();

    let main = if vm.args.cmd_test {
        None
    } else {
        find_main(&vm)
    };

    if vm.diag.borrow().has_errors() {
        vm.diag.borrow().dump();
        let no_errors = vm.diag.borrow().errors().len();

        if no_errors == 1 {
            println!("{} error found.", no_errors);
        } else {
            println!("{} errors found.", no_errors);
        }

        return 1;
    }

    // if --check given, stop after type/semantic check
    if vm.args.flag_check {
        return 0;
    }

    let timer = Timer::new(vm.args.flag_gc_verbose);

    let code = if vm.args.cmd_test {
        run_tests(&vm)
    } else {
        run_main(&vm, main.unwrap())
    };

    if vm.args.flag_gc_verbose {
        vm.dump_gc_summary(timer.stop());
    }

    code
}

fn run_tests<'ast>(vm: &VM<'ast>) -> i32 {
    let mut tests = 0;
    let mut passed = 0;

    for fct in vm.fcts.iter() {
        let fct = fct.borrow();

        if !is_test_fct(vm, &*fct) {
            continue;
        }

        tests += 1;

        print!("test {} ... ", vm.interner.str(fct.name));

        if run_test(vm, fct.id) {
            passed += 1;
            println!("ok");
        } else {
            println!("failed");
        }
    }

    println!(
        "{} tests executed; {} passed; {} failed.",
        tests,
        passed,
        tests - passed
    );

    // if all tests passed exit with 0, otherwise 1
    if tests == passed {
        0
    } else {
        1
    }
}

fn run_test<'ast>(vm: &VM<'ast>, fct: FctId) -> bool {
    let testing_class = vm.vips.testing_class;
    let testing_class = specialize_class_id(vm, testing_class);
    let testing = object::alloc(vm, testing_class).cast();
    vm.run_test(fct, testing);

    // see if test failed with exception
    let exception = exception_get_and_clear();

    exception.is_null() && !testing.has_failed()
}

fn is_test_fct<'ast>(vm: &VM<'ast>, fct: &Fct<'ast>) -> bool {
    // tests need to be standalone functions, with no return type and a single parameter
    if !fct.parent.is_none() || !fct.return_type.is_unit() || fct.param_types.len() != 1 {
        return false;
    }

    // parameter needs to be of type Testing
    let testing_cls = vm.cls(vm.vips.testing_class);
    if fct.param_types[0] != testing_cls {
        return false;
    }

    // the functions name needs to start with `test`
    let fct_name = vm.interner.str(fct.name);
    fct_name.starts_with("test")
}

fn run_main<'ast>(vm: &VM<'ast>, main: FctId) -> i32 {
    let res = vm.run(main);
    let is_unit = vm.fcts[main].borrow().return_type.is_unit();

    // main-fct without return value exits with status 0
    if is_unit {
        0

    // else use return value of main for exit status
    } else {
        res
    }
}

fn parse_dir(
    dirname: &str,
    id_generator: &NodeIdGenerator,
    ast: &mut Ast,
    interner: &mut Interner,
) -> Result<(), i32> {
    let path = Path::new(dirname);

    if path.is_dir() {
        for entry in fs::read_dir(path).unwrap() {
            let path = entry.unwrap().path();

            if path.is_file() && path.extension().unwrap() == "dora" {
                parse_file(path.to_str().unwrap(), id_generator, ast, interner)?;
            }
        }

        Ok(())
    } else {
        println!("directory `{}` does not exist.", dirname);

        Err(1)
    }
}

fn parse_file(
    filename: &str,
    id_generator: &NodeIdGenerator,
    ast: &mut Ast,
    interner: &mut Interner,
) -> Result<(), i32> {
    let reader = if filename == "-" {
        match Reader::from_input() {
            Err(_) => {
                println!("unable to read from stdin.");
                return Err(1);
            }

            Ok(reader) => reader,
        }
    } else {
        match Reader::from_file(filename) {
            Err(_) => {
                println!("unable to read file `{}`", filename);
                return Err(1);
            }

            Ok(reader) => reader,
        }
    };

    if let Err(error) = Parser::new(reader, id_generator, ast, interner).parse() {
        println!("{}", error);
        println!("1 error found.");
        return Err(1);
    }

    Ok(())
}

fn find_main<'ast>(vm: &VM<'ast>) -> Option<FctId> {
    let name = vm.interner.intern("main");
    let fctid = match vm.sym.borrow().get_fct(name) {
        Some(id) => id,
        None => {
            vm.diag
                .borrow_mut()
                .report(Position::new(1, 1), Msg::MainNotFound);
            return None;
        }
    };

    let fct = vm.fcts[fctid].borrow();
    let ret = fct.return_type;

    if (ret != BuiltinType::Unit && ret != BuiltinType::Int) || fct.params_without_self().len() > 0
    {
        let pos = fct.ast.pos;
        vm.diag.borrow_mut().report(pos, Msg::WrongMainDefinition);
        return None;
    }

    Some(fctid)
}
