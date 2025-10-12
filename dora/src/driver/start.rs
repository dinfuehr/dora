use std::fs::File;
use std::io::{self, Read, Write};
use std::path::PathBuf;

use crate::driver::flags::{self, DriverFlags, create_sema_flags};
use dora_bytecode::{FunctionId, PackageId, Program, display_fct};
use dora_frontend as language;
use dora_frontend::sema::{FileContent, Sema, SemaCreationParams};
use dora_runtime::{VM, clear_vm, execute_on_main, set_vm};

pub fn start() -> i32 {
    let flags = flags::parse_arguments();

    if let Err(msg) = flags {
        flags::print_help();
        println!();
        println!("{}", msg);
        return 1;
    }

    let mut flags = flags.unwrap();

    if flags.version {
        println!("dora v0.01b");
        return 0;
    }

    if flags.help {
        flags::print_help();
        return 0;
    }

    if flags.arg_file.is_none() {
        eprintln!("missing input argument.");
        return 1;
    }

    let file = flags.arg_file.as_ref().expect("missing argument");

    let prog = if file.ends_with(".dora-package") {
        match decode_input_program(&file) {
            Ok(prog) => prog,
            Err(_) => {
                return 1;
            }
        }
    } else {
        if flags.separate_stdlib_check {
            if let Err(_) = compile_std_library(&flags) {
                return 1;
            }
        }

        match compile_into_program(&flags, file.clone()) {
            Ok(result) => result,
            Err(_) => {
                return 1;
            }
        }
    };

    // if --check given, stop after type/semantic check
    if flags.check {
        return 0;
    }

    if flags.command.is_build() {
        return command_build(&flags, prog);
    }

    // Now for fun encode the program into a buffer and create a new Program from the buffer.
    let prog = encode_and_decode_for_testing(prog);

    let command = flags.command;

    let vm_flags = flags::create_vm_flags(&flags);

    // Now create a VM instance from the serialized data alone.
    let program_args = std::mem::replace(&mut flags.arg_argument, None).unwrap_or(Vec::new());
    let vm = VM::new(prog, vm_flags, program_args);

    set_vm(&vm);

    vm.compile_boots_aot();

    let exit_code = if command.is_test() {
        if flags.test_boots {
            run_tests(
                &vm,
                &flags,
                vm.program
                    .boots_package_id
                    .expect("boots package is missing"),
            )
        } else {
            run_tests(&vm, &flags, vm.program.program_package_id)
        }
    } else {
        if vm.program.main_fct_id.is_none() {
            eprintln!("no main method in program.");
            return 1;
        }

        let main_fct_id = vm.program.main_fct_id.expect("main missing");
        run_main(&vm, main_fct_id)
    };

    vm.threads.join_all();
    vm.shutdown();

    if vm.flags.gc_stats {
        let duration = vm.startup_time().elapsed();
        vm.dump_gc_summary(duration.as_secs_f32() * 1000f32);
    }

    clear_vm();

    exit_code
}

fn compile_std_library(flags: &DriverFlags) -> Result<Program, ()> {
    let path = get_stdlib_path(flags).expect("standard library not found");

    let sema_flags = SemaCreationParams {
        program_file: Some(FileContent::Path(path)),
        packages: Vec::new(),
        vfs: None,
        boots: false,
        is_standard_library: true,
    };

    let mut sa = Sema::new(sema_flags);

    let success = language::check_program(&mut sa);
    assert_eq!(success, !sa.diag.borrow().has_errors());

    if report_errors(&sa, flags.report_all_warnings) {
        return Err(());
    }

    if let Some(ref filter) = flags.emit_ast {
        language::emit_ast(&sa, filter);
    }

    // Create a serializable data structure from bytecode and metadata.
    // Here we drop the generated AST.
    let prog = language::emit_program(sa);

    if let Some(ref filter) = flags.emit_bytecode {
        language::emit_bytecode(&prog, filter);
    }

    Ok(prog)
}

fn get_stdlib_path(flags: &DriverFlags) -> Option<PathBuf> {
    for (name, path) in &flags.packages {
        if name == "std" {
            return Some(path.into());
        }
    }

    let path = std::env::current_exe().expect("illegal path");
    let path = path.as_path();

    for ancestor in path.ancestors() {
        let stdlib_path = ancestor.join("pkgs/std/std.dora");

        if stdlib_path.exists() {
            return Some(stdlib_path);
        }
    }

    None
}

fn compile_into_program(flags: &DriverFlags, file: String) -> Result<Program, ()> {
    let sema_flags = create_sema_flags(flags, PathBuf::from(file));

    let mut sa = Sema::new(sema_flags);

    let success = language::check_program(&mut sa);
    assert_eq!(success, !sa.diag.borrow().has_errors());

    if report_errors(&sa, flags.report_all_warnings) {
        return Err(());
    }

    if let Some(ref filter) = flags.emit_ast {
        language::emit_ast(&sa, filter);
    }

    // Create a serializable data structure from bytecode and metadata.
    // Here we drop the generated AST.
    let prog = language::emit_program(sa);

    if let Some(ref filter) = flags.emit_bytecode {
        language::emit_bytecode(&prog, filter);
    }

    Ok(prog)
}

fn command_build(flags: &DriverFlags, prog: Program) -> i32 {
    if flags.output.is_none() {
        eprintln!("missing output file");
        return 1;
    }

    let config = bincode::config::standard();
    let encoded_program = bincode::encode_to_vec(prog, config).expect("serialization failed");

    let file = flags.output.as_ref().expect("missing output");

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

fn report_errors(sa: &Sema, report_all_warnings: bool) -> bool {
    if sa.diag.borrow().has_errors_or_warnings() {
        sa.diag.borrow_mut().dump(&sa, report_all_warnings);
        sa.diag.borrow().has_errors()
    } else {
        false
    }
}

fn run_tests(vm: &VM, flags: &DriverFlags, package_id: PackageId) -> i32 {
    let mut tests = 0;
    let mut passed = 0;

    execute_on_main(|| {
        for (fct_id, fct) in vm.program.functions.iter().enumerate() {
            let fct_id = FunctionId(fct_id as u32);

            if fct.package_id == package_id && fct.is_test && test_filter_matches(vm, flags, fct_id)
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

    // if all tests passed exit with 0, otherwise 1
    if tests == passed { 0 } else { 1 }
}

fn test_filter_matches(vm: &VM, flags: &DriverFlags, fct_id: FunctionId) -> bool {
    if flags.test_filter.is_none() {
        return true;
    }

    let filter = flags.test_filter.as_ref().unwrap();
    let name = display_fct(&vm.program, fct_id);

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
