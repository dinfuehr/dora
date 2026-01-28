use crate::driver::flags::{RunArgs, include_boots};
use crate::driver::start::{Result, compile_or_load, encode_and_decode_for_testing, finish_vm};
use dora_bytecode::FunctionId;
use dora_runtime::{VM, execute_on_main, set_vm};

pub fn command_run(args: RunArgs) -> Result<()> {
    let file = args.file.as_ref().ok_or("missing input argument")?;

    let prog = compile_or_load(
        file,
        &args.common,
        include_boots(&args.common, &args.runtime),
    )?;
    let prog = encode_and_decode_for_testing(prog);
    let vm_flags = args.runtime.to_vm_flags();
    let vm = VM::new(prog, vm_flags, args.arguments);

    set_vm(&vm);
    vm.compile_boots_aot();

    let main_fct_id = vm.program.main_fct_id.ok_or("no main method in program")?;

    let exit_code = run_main(&vm, main_fct_id);

    finish_vm(&vm);

    if exit_code != 0 {
        std::process::exit(exit_code);
    }

    Ok(())
}

fn run_main(vm: &VM, main: FunctionId) -> i32 {
    let res = execute_on_main(|| vm.run(main));
    let fct = vm.fct(main);
    let is_unit = fct.return_type.is_unit();

    if is_unit { 0 } else { res }
}
