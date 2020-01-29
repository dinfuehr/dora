use std::fs;
use std::path::Path;

use crate::error::msg::SemError;
use crate::vm::VM;
use crate::vm::{exception_get_and_clear, Fct, FctId};
use dora_parser::ast::{self, Ast};

use crate::driver::cmd;
use crate::object;
use crate::timer::Timer;
use dora_parser::lexer::reader::Reader;

use crate::semck;
use crate::semck::specialize::specialize_class_id;
use crate::ty::BuiltinType;
use dora_parser::parser::Parser;

pub fn start(content: Option<&str>) -> i32 {
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

    semck::prelude::install_conditional_intrinsics(&mut vm);

    let main = if vm.args.cmd_test {
        None
    } else {
        find_main(&vm)
    };

    if vm.diag.lock().has_errors() {
        vm.diag.lock().dump(&vm);
        let no_errors = vm.diag.lock().errors().len();

        if no_errors == 1 {
            eprintln!("{} error found.", no_errors);
        } else {
            eprintln!("{} errors found.", no_errors);
        }

        return 1;
    }

    if !vm.args.cmd_test && main.is_none() {
        println!("error: no `main` function found in the program");
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

    if vm.args.flag_gc_stats {
        let duration = timer.stop();
        vm.dump_gc_summary(duration);
    }

    code
}

fn parse_all_files(vm: &mut VM, ast: &mut Ast, content: Option<&str>) -> Result<(), i32> {
    let fuzzing = content.is_some();

    let stdlib_dir = vm.args.flag_stdlib.clone();

    if let Some(stdlib) = stdlib_dir {
        parse_dir(&stdlib, vm, ast)?;
    } else {
        parse_bundled_stdlib(vm, ast)?;
    }

    let boots_dir = vm.args.flag_boots.clone();

    if let Some(boots) = boots_dir {
        parse_dir(&boots, vm, ast)?;
    }

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

    // the function needs to be marked with the @test annotation
    fct.is_test
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
            Ok(reader) => reader,

            Err(_) => {
                println!("unable to read from stdin.");
                return Err(1);
            }
        }
    } else {
        match Reader::from_file(filename) {
            Ok(reader) => reader,

            Err(_) => {
                println!("unable to read file `{}`", filename);
                return Err(1);
            }
        }
    };

    parse_reader(reader, vm, ast)
}

const STDLIB: &[(&str, &str)] = &include!(concat!(env!("OUT_DIR"), "/dora_stdlib_bundle.rs"));

pub fn parse_bundled_stdlib(vm: &mut VM, ast: &mut Ast) -> Result<(), i32> {
    for (filename, content) in STDLIB {
        parse_bundled_stdlib_file(filename, content, vm, ast)?;
    }

    Ok(())
}

fn parse_bundled_stdlib_file(
    filename: &str,
    content: &str,
    vm: &mut VM,
    ast: &mut Ast,
) -> Result<(), i32> {
    let reader = Reader::from_string(filename, content);
    parse_reader(reader, vm, ast)
}

fn parse_str(file: &str, vm: &mut VM, ast: &mut Ast) -> Result<(), i32> {
    let reader = Reader::from_string("<<code>>", file);
    parse_reader(reader, vm, ast)
}

fn parse_reader(reader: Reader, vm: &mut VM, ast: &mut Ast) -> Result<(), i32> {
    let filename: String = reader.path().into();
    let parser = Parser::new(reader, &vm.id_generator, ast, &mut vm.interner);

    match parser.parse() {
        Ok(file) => {
            vm.files.push(file);
            assert_eq!(ast.files.len(), vm.files.len());
            Ok(())
        }

        Err(error) => {
            println!(
                "error in {} at {}: {}",
                filename,
                error.pos,
                error.error.message()
            );
            println!("error during parsing.");

            Err(1)
        }
    }
}

fn find_main<'ast>(vm: &VM<'ast>) -> Option<FctId> {
    let name = vm.interner.intern("main");
    let fctid = match vm.sym.lock().get_fct(name) {
        Some(id) => id,
        None => {
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
            .report(fct.file, pos, SemError::WrongMainDefinition);
        return None;
    }

    Some(fctid)
}
