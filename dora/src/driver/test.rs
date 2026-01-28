use crate::driver::flags::{TestArgs, include_boots};
use crate::driver::start::{Result, compile_or_load, encode_and_decode_for_testing, finish_vm};
use dora_bytecode::{FunctionId, PackageId, display_fct};
use dora_runtime::{VM, execute_on_main, set_vm};

pub fn command_test(args: TestArgs) -> Result<()> {
    let file = args.file.as_ref().ok_or("missing input argument")?;

    let prog = compile_or_load(
        file,
        &args.common,
        include_boots(&args.common, &args.runtime),
    )?;
    let prog = encode_and_decode_for_testing(prog);
    let vm_flags = args.runtime.to_vm_flags();
    let vm = VM::new(prog, vm_flags, Vec::new());

    set_vm(&vm);
    vm.compile_boots_aot();

    let package_id = if args.test_boots {
        vm.program
            .boots_package_id
            .ok_or("boots package is missing")?
    } else {
        vm.program.program_package_id
    };

    let all_passed = run_tests(&vm, args.test_filter.as_deref(), package_id);

    finish_vm(&vm);

    if !all_passed {
        std::process::exit(1);
    }

    Ok(())
}

fn run_tests(vm: &VM, test_filter: Option<&str>, package_id: PackageId) -> bool {
    let mut tests = 0;
    let mut passed = 0;

    execute_on_main(|| {
        for (fct_id, fct) in vm.program.functions.iter().enumerate() {
            let fct_id: FunctionId = fct_id.into();

            if fct.package_id == package_id
                && fct.is_test
                && test_filter_matches(vm, test_filter, fct_id)
            {
                tests += 1;

                print!("test {} ... ", display_fct(&vm.program, fct_id));

                vm.run_test(fct_id);

                passed += 1;
                println!("ok");
            }
        }
    });

    println!(
        "{} tests executed; {} passed; {} failed.",
        tests,
        passed,
        tests - passed
    );

    tests == passed
}

fn test_filter_matches(vm: &VM, test_filter: Option<&str>, fct_id: FunctionId) -> bool {
    match test_filter {
        Some(filter) => {
            let name = display_fct(&vm.program, fct_id);
            name.contains(filter)
        }
        None => true,
    }
}
