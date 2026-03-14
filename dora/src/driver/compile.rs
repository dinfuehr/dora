use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::driver::flags::CompileArgs;
use crate::driver::start::{Result, compile_program, finish_vm};
use dora_runtime::{
    AotCompilation, AotFunction, AotKnownShape, AotKnownShapeKind, AotShape, AotShapeKind, VM,
    VmFlags, VmMode, compile_program as compile_program_aot, dora_entry_trampoline,
    execute_on_main, mangle_name, set_vm,
};

struct StringSlotEntry {
    slot_label: String,
    data_label: String,
    value: String,
}

struct ShapeSlotEntry {
    slot_label: String,
    shape_id: u32,
}

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

    let vm = VM::new(VmMode::Jit, prog, vm_flags, Vec::new());
    set_vm(&vm);
    vm.compile_boots_aot();

    let trampoline = dora_entry_trampoline::generate(&vm);
    let aot = execute_on_main(|| compile_program_aot(&vm));

    let asm_file = tempfile::Builder::new().suffix(".s").tempfile()?;

    {
        let mut f = File::create(asm_file.path())?;
        write_assembly(&mut f, &aot, &trampoline.code)?;
    }

    finish_vm(&vm);

    // Find the runtime static library next to the current executable.
    let exe_dir = std::env::current_exe()?
        .parent()
        .expect("no parent directory")
        .to_path_buf();
    let runtime_lib = find_staticlib(&exe_dir, "dora_runtime")
        .ok_or_else(|| "runtime library not found in target directory".to_string())?;
    let startup_lib = find_staticlib(&exe_dir, "dora_startup")
        .ok_or_else(|| "startup library not found in target directory".to_string())?;

    let status = Command::new("gcc")
        .arg(asm_file.path())
        .arg(&startup_lib)
        .arg(&runtime_lib)
        .arg("-lpthread")
        .arg("-ldl")
        .arg("-lm")
        .arg("-o")
        .arg(&args.output)
        .status()?;

    if !status.success() {
        return Err("gcc failed".into());
    }

    Ok(())
}

fn find_staticlib(exe_dir: &Path, crate_name: &str) -> Option<PathBuf> {
    let path = exe_dir.join(format!("lib{}.a", crate_name));
    if path.exists() { Some(path) } else { None }
}

fn write_assembly(f: &mut File, aot: &AotCompilation, trampoline: &[u8]) -> std::io::Result<()> {
    let functions: &[AotFunction] = &aot.functions;
    writeln!(f, ".text")?;

    let mut string_slots = Vec::<StringSlotEntry>::new();
    let mut string_slot_map = HashMap::<String, usize>::new();
    let mut shape_slots = Vec::<ShapeSlotEntry>::new();
    let mut shape_slot_map = HashMap::<u32, usize>::new();

    for func in functions {
        let label = mangle_name(&func.name);
        writeln!(f)?;
        writeln!(f, ".globl {}", label)?;
        writeln!(f, "{}:", label)?;

        for chunk in func.code.chunks(12) {
            let bytes: Vec<String> = chunk.iter().map(|b| format!("0x{:02x}", b)).collect();
            writeln!(f, "    .byte {}", bytes.join(", "))?;
        }

        // The offset in the relocation is the return address (after the call instruction).
        // For call_rel32, the 4-byte operand starts at offset-4.
        for reloc in &func.call_relocations {
            let reloc_offset = reloc.offset - 4;
            writeln!(
                f,
                "    .reloc {}+{}, R_X86_64_PC32, {} - 4",
                label, reloc_offset, reloc.target,
            )?;
        }

        // AOT string constants are loaded through a writable data slot.
        // The machine code uses mov reg, [rip + disp32], so reloc.offset points
        // at the disp32 field and we can use an R_X86_64_PC32 relocation.
        for reloc in &func.string_relocations {
            let slot_index = if let Some(&idx) = string_slot_map.get(&reloc.value) {
                idx
            } else {
                let idx = string_slots.len();
                let slot_label = format!(".Ldora_aot_string_slot_{}", idx);
                let data_label = format!(".Ldora_aot_string_data_{}", idx);
                string_slots.push(StringSlotEntry {
                    slot_label,
                    data_label,
                    value: reloc.value.clone(),
                });
                string_slot_map.insert(reloc.value.clone(), idx);
                idx
            };

            let slot_label = &string_slots[slot_index].slot_label;
            writeln!(
                f,
                "    .reloc {}+{}, R_X86_64_PC32, {} - 4",
                label, reloc.offset, slot_label,
            )?;
        }

        // AOT shape/class pointers are loaded through a writable data slot,
        // patched at startup with the correct header word.
        for reloc in &func.shape_relocations {
            let shape_id = reloc.shape_id;

            let slot_index = if let Some(&idx) = shape_slot_map.get(&shape_id) {
                idx
            } else {
                let idx = shape_slots.len();
                let slot_label = format!(".Ldora_aot_shape_slot_{}", idx);
                shape_slots.push(ShapeSlotEntry {
                    slot_label,
                    shape_id,
                });
                shape_slot_map.insert(shape_id, idx);
                idx
            };

            let slot_label = &shape_slots[slot_index].slot_label;
            writeln!(
                f,
                "    .reloc {}+{}, R_X86_64_PC32, {} - 4",
                label, reloc.offset, slot_label,
            )?;
        }
    }

    // Writable slots for string pointers (RW).
    if !string_slots.is_empty() {
        writeln!(f)?;
        writeln!(f, ".section .dora.string_data,\"aw\",@progbits")?;
        for slot in &string_slots {
            writeln!(f, "    .p2align 3")?;
            writeln!(f, "{}:", slot.slot_label)?;
            writeln!(f, "    .quad 0")?;
        }

        // String UTF-8 payloads (R).
        writeln!(f)?;
        writeln!(f, ".section .rodata")?;
        for slot in &string_slots {
            writeln!(f, "    .p2align 3")?;
            writeln!(f, "{}:", slot.data_label)?;
            write_bytes(f, slot.value.as_bytes())?;
        }
    }

    // String metadata table (R).
    writeln!(f)?;
    writeln!(f, ".section .dora.strings,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_strings_start")?;
    writeln!(f, "_dora_aot_strings_start:")?;
    for slot in &string_slots {
        writeln!(f, "    .quad {}", slot.data_label)?;
        writeln!(f, "    .quad {}", slot.value.len())?;
    }
    writeln!(f, ".globl _dora_aot_strings_end")?;
    writeln!(f, "_dora_aot_strings_end:")?;

    // String slot relocation table (R).
    writeln!(f)?;
    writeln!(f, ".section .dora.string_slots,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_string_slots_start")?;
    writeln!(f, "_dora_aot_string_slots_start:")?;
    for (idx, slot) in string_slots.iter().enumerate() {
        writeln!(f, "    .quad {}", slot.slot_label)?;
        writeln!(f, "    .long {}", idx)?;
        writeln!(f, "    .long 0")?;
    }
    writeln!(f, ".globl _dora_aot_string_slots_end")?;
    writeln!(f, "_dora_aot_string_slots_end:")?;

    // Writable slots for shape header words (RW).
    if !shape_slots.is_empty() {
        writeln!(f)?;
        writeln!(f, ".section .dora.shape_data,\"aw\",@progbits")?;
        for slot in &shape_slots {
            writeln!(f, "    .p2align 3")?;
            writeln!(f, "{}:", slot.slot_label)?;
            writeln!(f, "    .quad 0")?;
        }
    }

    // Shape slot relocation table (R).
    writeln!(f)?;
    writeln!(f, ".section .dora.shape_slots,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_shape_slots_start")?;
    writeln!(f, "_dora_aot_shape_slots_start:")?;
    for slot in &shape_slots {
        writeln!(f, "    .quad {}", slot.slot_label)?;
        writeln!(f, "    .long {}", slot.shape_id)?;
        writeln!(f, "    .long 0")?;
    }
    writeln!(f, ".globl _dora_aot_shape_slots_end")?;
    writeln!(f, "_dora_aot_shape_slots_end:")?;

    write_shape_metadata(f, &aot.shapes, &aot.known_shapes)?;

    // Emit the dora entry trampoline (generated by dora_entry_trampoline::generate).
    // Signature: extern "C" fn(tld: usize, fct: *const u8) -> i32
    writeln!(f)?;
    writeln!(f, ".globl _dora_entry_trampoline")?;
    writeln!(f, "_dora_entry_trampoline:")?;
    for chunk in trampoline.chunks(12) {
        let bytes: Vec<String> = chunk.iter().map(|b| format!("0x{:02x}", b)).collect();
        writeln!(f, "    .byte {}", bytes.join(", "))?;
    }

    // Emit the main entry point that tail-calls dora_aot_main.
    writeln!(f)?;
    writeln!(f, ".globl main")?;
    writeln!(f, "main:")?;
    writeln!(f, "    jmp dora_aot_main")?;

    Ok(())
}

fn write_bytes(f: &mut File, data: &[u8]) -> std::io::Result<()> {
    if data.is_empty() {
        return Ok(());
    }

    for chunk in data.chunks(12) {
        let bytes: Vec<String> = chunk.iter().map(|b| format!("0x{:02x}", b)).collect();
        writeln!(f, "    .byte {}", bytes.join(", "))?;
    }

    Ok(())
}

fn write_shape_metadata(
    f: &mut File,
    shapes: &[AotShape],
    known_shapes: &[AotKnownShape],
) -> std::io::Result<()> {
    let mut refs = Vec::<i32>::new();
    let mut shape_ref_ranges = Vec::<(usize, usize)>::with_capacity(shapes.len());
    for shape in shapes {
        let start = refs.len();
        refs.extend(shape.refs.iter().copied());
        shape_ref_ranges.push((start, refs.len() - start));
    }

    writeln!(f)?;
    writeln!(f, ".section .dora.shapes,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_shapes_start")?;
    writeln!(f, "_dora_aot_shapes_start:")?;
    for (shape, (refs_start, refs_len)) in shapes.iter().zip(shape_ref_ranges.iter()) {
        writeln!(f, "    .quad {}", shape_kind_value(shape.kind))?;
        writeln!(f, "    .quad {}", shape.visitor)?;
        writeln!(f, "    .quad {}", refs_start)?;
        writeln!(f, "    .quad {}", refs_len)?;
        writeln!(f, "    .quad {}", shape.instance_size)?;
        writeln!(f, "    .quad {}", shape.element_size)?;
    }
    writeln!(f, ".globl _dora_aot_shapes_end")?;
    writeln!(f, "_dora_aot_shapes_end:")?;

    writeln!(f)?;
    writeln!(f, ".section .dora.shape_refs,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_shape_refs_start")?;
    writeln!(f, "_dora_aot_shape_refs_start:")?;
    for value in &refs {
        writeln!(f, "    .long {}", value)?;
    }
    writeln!(f, ".globl _dora_aot_shape_refs_end")?;
    writeln!(f, "_dora_aot_shape_refs_end:")?;

    writeln!(f)?;
    writeln!(f, ".section .dora.known_shapes,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_known_shapes_start")?;
    writeln!(f, "_dora_aot_known_shapes_start:")?;
    for known_shape in known_shapes {
        writeln!(f, "    .long {}", known_shape_kind_value(known_shape.kind))?;
        writeln!(f, "    .long {}", known_shape.shape_id)?;
    }
    writeln!(f, ".globl _dora_aot_known_shapes_end")?;
    writeln!(f, "_dora_aot_known_shapes_end:")?;
    writeln!(f, ".text")?;

    Ok(())
}

fn shape_kind_value(kind: AotShapeKind) -> u8 {
    match kind {
        AotShapeKind::Builtin => 0,
        AotShapeKind::String => 1,
    }
}

fn known_shape_kind_value(kind: AotKnownShapeKind) -> u8 {
    match kind {
        AotKnownShapeKind::ByteArray => 0,
        AotKnownShapeKind::Int32Array => 1,
        AotKnownShapeKind::String => 2,
        AotKnownShapeKind::Thread => 3,
        AotKnownShapeKind::FillerWord => 4,
        AotKnownShapeKind::FillerArray => 5,
        AotKnownShapeKind::FreeSpace => 6,
        AotKnownShapeKind::Code => 7,
    }
}
