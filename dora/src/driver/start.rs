use std::fs::File;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::time::Instant;

use crate::driver::cmd::{self, Args};
use dora_bytecode::{FunctionData, FunctionId, PackageId, Program};
use dora_frontend as language;
use dora_frontend::sema::{Sema, SemaArgs};
use dora_runtime::{clear_vm, display_fct, execute_on_main, set_vm, VM};

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

    if args.arg_file.is_none() {
        eprintln!("missing input argument.");
        return 1;
    }

    let file = args.arg_file.to_owned().unwrap();

    let prog = if file.ends_with(".dora-package") {
        match decode_input_program(&file) {
            Ok(prog) => prog,
            Err(_) => {
                return 1;
            }
        }
    } else {
        match compile_into_program(&args, file) {
            Ok(result) => result,
            Err(_) => {
                return 1;
            }
        }
    };

    // if --check given, stop after type/semantic check
    if args.flag_check {
        return 0;
    }

    if args.command.is_build() {
        return command_build(&args, prog);
    }

    // Now for fun encode the program into a buffer and create a new Program from the buffer.
    let prog = encode_and_decode_for_testing(prog);

    let command = args.command;

    let vm_args = cmd::create_vm_args(&args);

    // Now create a VM instance from the serialized data alone.
    let program_args = std::mem::replace(&mut args.arg_argument, None).unwrap_or(Vec::new());
    let vm = VM::new(prog, vm_args, program_args);

    set_vm(&vm);

    let timer = if vm.args.flag_gc_stats {
        Some(Instant::now())
    } else {
        None
    };

    let exit_code = if command.is_test() {
        run_tests(&vm, &args, vm.program.program_package_id)
    } else {
        if vm.program.main_fct_id.is_none() {
            eprintln!("no main method in program.");
            return 1;
        }

        let main_fct_id = vm.program.main_fct_id.expect("main missing");
        run_main(&vm, main_fct_id)
    };

    vm.threads.join_all();

    if vm.args.flag_gc_stats {
        let duration = timer.expect("missing timer").elapsed();
        vm.dump_gc_summary(duration.as_secs_f32() / 1000f32);
    }

    clear_vm();

    exit_code
}

fn compile_into_program(args: &Args, file: String) -> Result<Program, ()> {
    let sem_args = SemaArgs {
        arg_file: Some(file),
        packages: args.packages.clone(),
        test_file_as_string: None,
    };

    let mut sa = Sema::new(sem_args);

    let success = language::check_program(&mut sa);
    assert_eq!(success, !sa.diag.borrow().has_errors());

    if report_errors(&sa) {
        return Err(());
    }

    if report_errors(&sa) {
        return Err(());
    }

    if let Some(ref filter) = args.flag_emit_ast {
        language::emit_ast(&sa, filter);
    }

    language::generate_bytecode(&sa);

    if let Some(ref filter) = args.flag_emit_bytecode {
        language::emit_bytecode(&sa, filter);
    }

    // Create a serializable data structure from bytecode and metadata.
    // Here we drop the generated AST.
    let prog = language::emit_program(sa);

    Ok(prog)
}

fn command_build(args: &Args, prog: Program) -> i32 {
    if args.flag_output.is_none() {
        eprintln!("missing output file");
        return 1;
    }

    let config = bincode::config::standard();
    let encoded_program = bincode::encode_to_vec(prog, config).expect("serialization failed");

    let file = args.flag_output.as_ref().expect("missing output");

    match write_program_into_file(&encoded_program, file) {
        Ok(()) => 0,
        Err(_) => {
            eprintln!("Failed to write into output file.");
            1
        }
    }
}

fn decode_input_program(file: &str) -> Result<Program, ()> {
    let encoded_program = match read_input_file(file) {
        Ok(result) => result,
        Err(_) => {
            eprintln!("couldn't read input file.");
            return Err(());
        }
    };

    let config = bincode::config::standard();
    let (decoded_prog, decoded_len): (Program, usize) =
        bincode::decode_from_slice(&encoded_program, config).expect("serialization failure");
    assert_eq!(decoded_len, encoded_program.len());

    Ok(decoded_prog)
}

fn read_input_file(file: &str) -> Result<Vec<u8>, io::Error> {
    let path = PathBuf::from(file);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => Ok(buffer),
        Err(error) => Err(error),
    }
}

fn write_program_into_file(prog: &[u8], file: &str) -> Result<(), io::Error> {
    let path = PathBuf::from(file);

    let mut f = File::create(&path)?;
    f.write_all(prog)?;
    Ok(())
}

fn encode_and_decode_for_testing(prog: Program) -> Program {
    let config = bincode::config::standard();
    let encoded_program = bincode::encode_to_vec(prog, config).expect("serialization failed");
    let (decoded_prog, decoded_len): (Program, usize) =
        bincode::decode_from_slice(&encoded_program, config).expect("serialization failure");
    assert_eq!(decoded_len, encoded_program.len());

    decoded_prog
}

fn report_errors(sa: &Sema) -> bool {
    if sa.diag.borrow().has_errors() {
        sa.diag.borrow_mut().dump(&sa);
        let no_errors = sa.diag.borrow().errors().len();

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
