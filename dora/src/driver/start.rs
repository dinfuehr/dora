use crate::driver::cmd::{self, Args};
use crate::timer::Timer;
use crate::vm::{clear_vm, display_fct, execute_on_main, set_vm, VM};
use dora_bytecode::{FunctionData, FunctionId, PackageId};
use dora_frontend::language;
use dora_frontend::language::error::msg::ErrorMessage;
use dora_frontend::language::sem_analysis::{SemAnalysis, SemAnalysisArgs};

pub fn start() -> i32 {
    let args = cmd::parse_arguments();

    if let Err(msg) = args {
        cmd::print_help();
        println!();
        println!("{}", msg);
        return 1;
    }

    let mut args = args.unwrap();

    if args.flag_version {
        println!("dora v0.01b");
        return 0;
    }

    if args.flag_help {
        cmd::print_help();
        return 0;
    }

    let sem_args = SemAnalysisArgs {
        arg_file: args.arg_file.clone(),
        packages: args.packages.clone(),
        test_file_as_string: None,
    };

    let mut sa = SemAnalysis::new(sem_args);

    let success = language::check(&mut sa);
    assert_eq!(success, !sa.diag.lock().has_errors());

    if report_errors(&sa) {
        return 1;
    }

    let main_fct_id = find_main(&sa, &args);

    if report_errors(&sa) {
        return 1;
    }

    if let Some(ref filter) = args.flag_emit_ast {
        language::emit_ast(&sa, filter);
    }

    language::generate_bytecode(&sa);

    if let Some(ref filter) = args.flag_emit_bytecode {
        language::emit_bytecode(&sa, filter);
    }

    // if --check given, stop after type/semantic check
    if args.flag_check {
        return 0;
    }

    if args.command.is_build() {
        unimplemented!();
    }

    // Create a serializable data structure from bytecode and metadata.
    // Here we drop the generated AST.
    let prog = language::emit_program(sa);

    let command = args.command;

    let vm_args = cmd::create_vm_args(&args);

    // Now create a VM instance from the serialized data alone.
    let program_args = std::mem::replace(&mut args.arg_argument, None).unwrap_or(Vec::new());
    let vm = VM::new(prog, vm_args, program_args);

    set_vm(&vm);

    let mut timer = Timer::new(vm.args.flag_gc_stats);

    let exit_code = if command.is_test() {
        run_tests(&vm, &args, vm.program.program_package_id)
    } else {
        run_main(&vm, main_fct_id.expect("main missing"))
    };

    vm.threads.join_all();

    if vm.args.flag_gc_stats {
        let duration = timer.stop();
        vm.dump_gc_summary(duration);
    }

    clear_vm();

    exit_code
}

fn report_errors(sa: &SemAnalysis) -> bool {
    if sa.diag.lock().has_errors() {
        sa.diag.lock().dump(&sa);
        let no_errors = sa.diag.lock().errors().len();

        if no_errors == 1 {
            eprintln!("{} error found.", no_errors);
        } else {
            eprintln!("{} errors found.", no_errors);
        }

        true
    } else {
        false
    }
}

fn run_tests(vm: &VM, args: &Args, package_id: PackageId) -> i32 {
    let mut tests = 0;
    let mut passed = 0;

    execute_on_main(|| {
        for (fct_id, fct) in vm.program.functions.iter().enumerate() {
            let fct_id = FunctionId(fct_id as u32);

            if fct.package_id != package_id
                || !is_test_fct(&*fct)
                || !test_filter_matches(vm, args, fct_id)
            {
                continue;
            }

            tests += 1;

            print!("test {} ... ", fct.name);

            run_test(vm, fct_id);
            passed += 1;
            println!("ok");
        }
    });

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

fn run_test(vm: &VM, fct: FunctionId) {
    vm.run_test(fct);
}

fn is_test_fct(fct: &FunctionData) -> bool {
    // the function needs to be marked with the @Test annotation
    fct.is_test
}

fn test_filter_matches(vm: &VM, args: &Args, fct_id: FunctionId) -> bool {
    if args.flag_test_filter.is_none() {
        return true;
    }

    let filter = args.flag_test_filter.as_ref().unwrap();
    let name = display_fct(vm, fct_id);

    name.contains(filter)
}

fn run_main(vm: &VM, main: FunctionId) -> i32 {
    let res = execute_on_main(|| vm.run(main));
    let fct = &vm.program.functions[main.0 as usize];
    let is_unit = fct.return_type.is_unit();

    // main-fct without return value exits with status 0
    if is_unit {
        0

    // else use return value of main for exit status
    } else {
        res
    }
}

fn find_main(sa: &SemAnalysis, args: &Args) -> Option<FunctionId> {
    if args.command.is_test() {
        return None;
    }

    let name = sa.interner.intern("main");
    let fctid = if let Some(id) = sa.module_table(sa.program_module_id()).read().get_fct(name) {
        id
    } else {
        return None;
    };

    let fct = sa.fcts.idx(fctid);
    let fct = fct.read();
    let ret = fct.return_type.clone();

    if (!ret.is_unit() && !ret.is_int32())
        || !fct.params_without_self().is_empty()
        || !fct.type_params.is_empty()
    {
        let pos = fct.ast.pos;
        sa.diag
            .lock()
            .report(fct.file_id, pos, ErrorMessage::WrongMainDefinition);
        return None;
    }

    Some(FunctionId(fctid.0 as u32))
}
