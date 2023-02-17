use crate::driver::cmd;
use crate::language;
use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::{
    FctDefinition, FctDefinitionId, ModuleDefinitionId, SemAnalysis,
};
use crate::timer::Timer;
use crate::vm::{clear_vm, execute_on_main, module_contains, set_vm, VM};

pub fn start() -> i32 {
    let args = cmd::parse_arguments();

    if let Err(msg) = args {
        cmd::print_help();
        println!();
        println!("{}", msg);
        return 1;
    }

    let args = args.unwrap();

    if args.flag_version {
        println!("dora v0.01b");
        return 0;
    }

    if args.flag_help {
        cmd::print_help();
        return 0;
    }

    let mut sa = SemAnalysis::new(args);

    let success = language::check(&mut sa);
    assert_eq!(success, !sa.diag.lock().has_errors());

    if report_errors(&sa) {
        return 1;
    }

    let main_fct_id = find_main(&sa);

    if report_errors(&sa) {
        return 1;
    }

    language::emit_ast(&sa);

    language::generate_bytecode(&sa);

    // if --check given, stop after type/semantic check
    if sa.args.flag_check {
        return 0;
    }

    if sa.args.command.is_build() {
        unimplemented!();
    }

    let vm = {
        let mut mutable_vm = VM::new_from_sa(sa);
        mutable_vm.setup_execution();
        mutable_vm
    };

    set_vm(&vm);

    let mut timer = Timer::new(vm.args.flag_gc_stats);

    let exit_code = if vm.args.command.is_test() {
        run_tests(&vm, vm.program_module_id())
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

fn run_tests(vm: &VM, module_id: ModuleDefinitionId) -> i32 {
    let mut tests = 0;
    let mut passed = 0;

    execute_on_main(|| {
        for fct in vm.fcts.iter() {
            let fct = fct.read();

            if !module_contains(vm, module_id, fct.module_id)
                || !is_test_fct(&*fct)
                || !test_filter_matches(vm, &*fct)
            {
                continue;
            }

            tests += 1;

            print!("test {} ... ", vm.interner.str(fct.name));

            run_test(vm, fct.id());
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

fn run_test(vm: &VM, fct: FctDefinitionId) {
    vm.run_test(fct);
}

fn is_test_fct(fct: &FctDefinition) -> bool {
    // the function needs to be marked with the @Test annotation
    fct.is_test
}

fn test_filter_matches(vm: &VM, fct: &FctDefinition) -> bool {
    if vm.args.flag_test_filter.is_none() {
        return true;
    }

    let filter = vm.args.flag_test_filter.as_ref().unwrap();
    let name = fct.display_name_vm(vm);

    name.contains(filter)
}

fn run_main(vm: &VM, main: FctDefinitionId) -> i32 {
    let res = execute_on_main(|| vm.run(main));
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

pub const STDLIB: &[(&str, &str)] = &include!(concat!(env!("OUT_DIR"), "/dora_stdlib_bundle.rs"));

fn find_main(sa: &SemAnalysis) -> Option<FctDefinitionId> {
    if sa.args.command.is_test() {
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

    Some(fctid)
}
