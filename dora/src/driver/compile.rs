use std::fs::File;
use std::io::Write;

use crate::driver::flags::CompileArgs;
use crate::driver::start::{Result, compile_program, finish_vm};
use dora_runtime::{AotFunction, VM, VmFlags, compile_program_functions, execute_on_main, set_vm};

pub fn command_compile(args: CompileArgs) -> Result<()> {
    let prog = compile_program(&args.file, &args.common, true)?;

    let vm_flags = VmFlags {
        emit_asm: None,
        emit_asm_file: None,
        emit_asm_boots: false,
        emit_bytecode_compiler: None,
        emit_bytecode_boots: false,
        emit_compiler: false,
        emit_graph: None,
        emit_graph_after_each_pass: false,
        emit_stubs: false,
        enable_perf: false,
        always_boots: true,
        use_boots: None,
        omit_bounds_check: false,
        emit_debug: None,
        emit_debug_boots: false,
        emit_debug_native: false,
        emit_debug_compile: false,
        emit_debug_entry: false,
        gc_events: false,
        gc_stress: false,
        gc_stress_minor: false,
        gc_stress_in_lazy_compile: false,
        gc_stats: false,
        gc_verbose: false,
        gc_verify: false,
        gc_worker: 0,
        gc_young_size: None,
        gc_semi_ratio: None,
        gc: None,
        compiler: None,
        min_heap_size: None,
        max_heap_size: None,
        code_size: None,
        readonly_size: None,
        disable_tlab: false,
        disable_barrier: false,
        bootstrap_compiler: false,
        snapshot_on_oom: None,
    };

    let vm = VM::new(prog, vm_flags, Vec::new());
    set_vm(&vm);
    vm.compile_boots_aot();

    let functions = execute_on_main(|| compile_program_functions(&vm));

    let mut f = File::create(&args.output)?;
    write_assembly(&mut f, &functions)?;

    finish_vm(&vm);

    println!("wrote {} functions to {}", functions.len(), args.output);
    Ok(())
}

fn write_assembly(f: &mut File, functions: &[AotFunction]) -> std::io::Result<()> {
    writeln!(f, ".text")?;

    for func in functions {
        let label = mangle_name(&func.name);
        writeln!(f)?;
        writeln!(f, ".globl {}", label)?;
        writeln!(f, "{}:", label)?;

        for chunk in func.code.chunks(12) {
            let bytes: Vec<String> = chunk.iter().map(|b| format!("0x{:02x}", b)).collect();
            writeln!(f, "    .byte {}", bytes.join(", "))?;
        }

        // Emit relocations for direct call sites.
        // The offset in the relocation is the return address (after the call instruction).
        // For x64 call_rel32: the 4-byte operand starts at offset-4, so we relocate at offset-4.
        for reloc in &func.relocations {
            let target_label = mangle_name(&reloc.target);
            let reloc_offset = reloc.offset - 4;
            writeln!(
                f,
                "    .reloc {}+{}, R_X86_64_PC32, {} - 4",
                label, reloc_offset, target_label,
            )?;
        }
    }

    Ok(())
}

fn mangle_name(name: &str) -> String {
    let mut result = String::with_capacity(name.len() + 6);
    result.push_str("_dora_");
    for ch in name.chars() {
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' => result.push(ch),
            ':' | '.' | '/' => result.push('_'),
            _ => result.push('_'),
        }
    }
    result
}
