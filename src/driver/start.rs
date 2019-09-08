use std::fs;
use std::path::Path;

use crate::vm::VM;
use crate::vm::{exception_get_and_clear, Fct, FctId, File};
use dora_parser::ast::{self, Ast};
use dora_parser::error::msg::Msg;

use crate::driver::cmd;
use crate::object;
use crate::os;
use crate::timer::Timer;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::reader::Reader;

use crate::semck;
use crate::semck::specialize::specialize_class_id;
use crate::ty::BuiltinType;
use dora_parser::parser::Parser;

pub fn start(content: Option<&str>) -> i32 {
    os::mem::init_page_size();
    let fuzzing = content.is_some();

    let args = if fuzzing {
        Default::default()
    } else {
        cmd::parse()
    };

    if args.flag_version {
        println!("dora v0.01b");
        return 0;
    }

    let mut ast = Ast::new();
    let empty = Ast::new();
    let mut vm = VM::new(args, &empty);

    if let Err(code) = parse_all_files(&mut vm, &mut ast, content) {
        return code;
    }

    vm.ast = &ast;

    if vm.args.flag_emit_ast {
        ast::dump::dump(&vm.ast, &vm.interner);
    }

    semck::check(&mut vm);

    // register signal handler
    os::register_signals();

    let main = if vm.args.cmd_test {
        None
    } else {
        find_main(&vm)
    };

    if vm.diag.lock().has_errors() {
        vm.diag.lock().dump();
        let no_errors = vm.diag.lock().errors().len();

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

    let mut timer = Timer::new(vm.args.flag_gc_stats);

    vm.threads.attach_current_thread();

    let code = if vm.args.cmd_test {
        run_tests(&vm)
    } else {
        run_main(&vm, main.unwrap())
    };

    vm.threads.detach_current_thread();
    vm.threads.join_all();

    os::unregister_signals();

    if vm.args.flag_gc_stats {
        let duration = timer.stop();
        vm.dump_gc_summary(duration);
    }

    code
}

fn parse_all_files(vm: &mut VM, ast: &mut Ast, content: Option<&str>) -> Result<(), i32> {
    let fuzzing = content.is_some();

    parse_dir("stdlib", vm, ast).and_then(|_| {
        if fuzzing {
            return parse_str(content.unwrap(), vm, ast);
        }

        let arg_file = vm.args.arg_file.clone();
        let path = Path::new(&arg_file);

        if path.is_file() {
            parse_file(&arg_file, vm, ast)
        } else if path.is_dir() {
            parse_dir(&arg_file, vm, ast)
        } else {
            println!("file or directory `{}` does not exist.", &arg_file);
            Err(1)
        }
    })
}

fn run_tests<'ast>(vm: &VM<'ast>) -> i32 {
    let mut tests = 0;
    let mut passed = 0;

    for fct in vm.fcts.iter() {
        let fct = fct.read();

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
    let fct = vm.fcts.idx(main);
    let fct = fct.read();
    let is_unit = fct.return_type.is_unit();

    // main-fct without return value exits with status 0
    if is_unit {
        0

    // else use return value of main for exit status
    } else {
        res
    }
}

fn parse_dir(dirname: &str, vm: &mut VM, ast: &mut Ast) -> Result<(), i32> {
    let path = Path::new(dirname);

    if path.is_dir() {
        for entry in fs::read_dir(path).unwrap() {
            let path = entry.unwrap().path();

            if path.is_file() && path.extension().unwrap() == "dora" {
                parse_file(path.to_str().unwrap(), vm, ast)?;
            }
        }

        Ok(())
    } else {
        println!("directory `{}` does not exist.", dirname);

        Err(1)
    }
}

fn parse_file(filename: &str, vm: &mut VM, ast: &mut Ast) -> Result<(), i32> {
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

    if let Err(error) = Parser::new(reader, &vm.id_generator, ast, &mut vm.interner).parse() {
        println!("{}", error);
        println!("1 error found.");
        return Err(1);
    }

    Ok(())
}

fn parse_str(file: &str, vm: &mut VM, ast: &mut Ast) -> Result<(), i32> {
    let reader = Reader::from_string(file);

    if let Err(error) = Parser::new(reader, &vm.id_generator, ast, &mut vm.interner).parse() {
        println!("{}", error);
        println!("1 error found.");
        return Err(1);
    }

    Ok(())
}

fn find_main<'ast>(vm: &VM<'ast>) -> Option<FctId> {
    let name = vm.interner.intern("main");
    let fctid = match vm.sym.lock().get_fct(name) {
        Some(id) => id,
        None => {
            vm.diag
                .lock()
                .report_without_path(Position::new(1, 1), Msg::MainNotFound);
            return None;
        }
    };

    let fct = vm.fcts.idx(fctid);
    let fct = fct.read();
    let ret = fct.return_type;

    if (ret != BuiltinType::Unit && ret != BuiltinType::Int) || fct.params_without_self().len() > 0
    {
        let pos = fct.ast.pos;
        vm.diag
            .lock()
            .report_without_path(pos, Msg::WrongMainDefinition);
        return None;
    }

    Some(fctid)
}
