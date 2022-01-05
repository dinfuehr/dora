use crate::error::msg::SemError;
use crate::vm::{init_global_addresses, set_vm, Fct, FctId};
use crate::vm::{SemAnalysis, VM};

use crate::driver::cmd;
use crate::object;
use crate::timer::Timer;

use crate::semck;
use crate::semck::specialize::specialize_class_id;
use crate::vm::{execute_on_main, namespace_contains, NamespaceId};

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

    if !semck::check(&mut sa) {
        return 1;
    }

    if sa.diag.lock().has_errors() {
        sa.diag.lock().dump(&sa);
        let no_errors = sa.diag.lock().errors().len();

        if no_errors == 1 {
            eprintln!("{} error found.", no_errors);
        } else {
            eprintln!("{} errors found.", no_errors);
        }

        return 1;
    }

    semck::generate_bytecode(&sa);

    let main = if sa.args.cmd_test {
        None
    } else {
        find_main(&sa)
    };

    if !sa.args.cmd_test && main.is_none() {
        println!("error: no `main` function found in the program");
        return 1;
    }

    // if --check given, stop after type/semantic check
    if sa.args.flag_check {
        return 0;
    }

    let vm = VM::new_from_sa(sa);
    set_vm(&vm);

    let mut timer = Timer::new(vm.args.flag_gc_stats);

    init_global_addresses(&vm);

    let code = if vm.args.cmd_test {
        let namespace_id = if vm.args.flag_test_boots {
            vm.boots_namespace_id
        } else {
            vm.global_namespace_id
        };

        run_tests(&vm, namespace_id)
    } else {
        run_main(&vm, main.unwrap())
    };

    vm.threads.join_all();

    if vm.args.flag_gc_stats {
        let duration = timer.stop();
        vm.dump_gc_summary(duration);
    }

    code
}

fn run_tests(vm: &VM, namespace_id: NamespaceId) -> i32 {
    let mut tests = 0;
    let mut passed = 0;

    execute_on_main(|| {
        for fct in vm.fcts.iter() {
            let fct = fct.read();

            if !namespace_contains(vm, namespace_id, fct.namespace_id)
                || !is_test_fct(vm, &*fct)
                || !test_filter_matches(vm, &*fct)
            {
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

fn run_test(vm: &VM, fct: FctId) -> bool {
    let testing_class = vm.known.classes.testing();
    let testing_class = specialize_class_id(vm, testing_class);
    let testing = object::alloc(vm, testing_class).cast();
    vm.run_test(fct, testing);

    !testing.has_failed()
}

fn is_test_fct(vm: &VM, fct: &Fct) -> bool {
    // tests need to be standalone functions, with no return type and a single parameter
    if !fct.parent.is_none() || !fct.return_type.is_unit() || fct.param_types.len() != 1 {
        return false;
    }

    // parameter needs to be of type Testing
    let testing_cls = vm.cls(vm.known.classes.testing());
    if fct.param_types[0] != testing_cls {
        return false;
    }

    // the function needs to be marked with the @test annotation
    fct.is_test
}

fn test_filter_matches(vm: &VM, fct: &Fct) -> bool {
    if vm.args.flag_test_filter.is_none() {
        return true;
    }

    let filter = vm.args.flag_test_filter.as_ref().unwrap();
    let name = fct.name_with_params(vm);

    name.contains(filter)
}

fn run_main(vm: &VM, main: FctId) -> i32 {
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

fn find_main(vm: &VM) -> Option<FctId> {
    let name = vm.interner.intern("main");
    let fctid = if let Some(id) = vm
        .namespace_table(vm.global_namespace_id)
        .read()
        .get_fct(name)
    {
        id
    } else {
        return None;
    };

    let fct = vm.fcts.idx(fctid);
    let fct = fct.read();
    let ret = fct.return_type.clone();

    if (!ret.is_unit() && !ret.is_int32())
        || !fct.params_without_self().is_empty()
        || !fct.type_params.is_empty()
    {
        let pos = fct.ast.pos;
        vm.diag
            .lock()
            .report(fct.file_id, pos, SemError::WrongMainDefinition);
        return None;
    }

    Some(fctid)
}
