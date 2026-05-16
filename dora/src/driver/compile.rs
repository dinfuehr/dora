use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::driver::flags::CompileArgs;
use crate::driver::start::{Result, compile_program, finish_vm};
use dora_bytecode::Location;
use dora_runtime::{
    AotCodeKind, AotCompilation, AotFunction, AotFunctionInfo, AotGcPoint, AotInlinedFunction,
    AotKnownShape, AotKnownShapeKind, AotLocation, AotShape, AotShapeKind, AotStringId,
    AotStringTable, CollectorName, TargetArch, VM, VmFlags, VmMode,
    compile_program as compile_program_aot, dora_entry_trampoline, execute_on_main, set_vm,
};

struct StringSlotEntry {
    slot_label: String,
    string_id: AotStringId,
}

struct ShapeSlotEntry {
    slot_label: String,
    shape_id: u32,
}

struct FunctionMetadataEntry<'a> {
    start_label: &'a str,
    end_label: String,
    fct_id: u32,
    kind: u32,
    function: &'a AotFunctionInfo,
    gcpoints: &'a [AotGcPoint],
    locations: &'a [AotLocation],
    inlined_functions: &'a [AotInlinedFunction],
}

struct FunctionMetadataLayout<'a> {
    start_label: &'a str,
    end_label: &'a str,
    fct_id: u32,
    kind: u32,
    function_info_idx: usize,
    gcpoint_start: usize,
    gcpoint_len: usize,
    location_start: usize,
    location_len: usize,
    inlined_function_start: usize,
    inlined_function_len: usize,
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
        gc: args.gc,
        compiler: None,
        min_heap_size: None,
        max_heap_size: None,
        code_size: None,
        readonly_size: None,
        disable_tlab: false,
        disable_barrier: false,
        bootstrap_compiler: false,
        snapshot_on_oom: None,
        target_arch: args.target.unwrap_or(TargetArch::host()),
    };

    let vm = VM::new(VmMode::Jit, prog, vm_flags, Vec::new());
    set_vm(&vm);
    vm.compile_boots_aot();

    let target_arch = vm.flags.target_arch;
    let trampoline = dora_entry_trampoline::generate(&vm);
    let aot = execute_on_main(|| compile_program_aot(&vm));

    let asm_file = if args.emit_asm {
        None
    } else {
        Some(tempfile::Builder::new().suffix(".s").tempfile()?)
    };

    let asm_path = match &asm_file {
        Some(tmp) => tmp.path().to_path_buf(),
        None => PathBuf::from(&args.output).with_extension("s"),
    };

    {
        let mut f = File::create(&asm_path)?;
        write_assembly(&mut f, &aot, &trampoline.code, target_arch)?;
    }

    finish_vm(&vm);

    if args.emit_asm {
        return Ok(());
    }

    // Find the runtime static library next to the current executable.
    let exe_dir = std::env::current_exe()?
        .parent()
        .expect("no parent directory")
        .to_path_buf();
    let runtime_lib = find_staticlib(&exe_dir, "dora_runtime")
        .ok_or_else(|| "runtime library not found in target directory".to_string())?;
    let startup_lib = find_staticlib(&exe_dir, "dora_startup")
        .ok_or_else(|| "startup library not found in target directory".to_string())?;

    let cc = std::env::var("CC").unwrap_or_else(|_| "gcc".to_string());
    let status = Command::new(&cc)
        .arg(&asm_path)
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

fn write_assembly(
    f: &mut File,
    aot: &AotCompilation,
    trampoline: &[u8],
    target_arch: TargetArch,
) -> std::io::Result<()> {
    let is_arm64 = target_arch.is_arm64();

    let functions: &[AotFunction] = &aot.functions;
    writeln!(f, ".text")?;

    let mut strings = aot.strings.clone();
    let mut string_slots = Vec::<StringSlotEntry>::new();
    let mut string_slot_map = HashMap::<AotStringId, usize>::new();
    let mut shape_slots = Vec::<ShapeSlotEntry>::new();
    let mut shape_slot_map = HashMap::<u32, usize>::new();
    let mut function_metadata = Vec::with_capacity(functions.len() + 1);

    for (func_idx, func) in functions.iter().enumerate() {
        let label = func.symbol_name.as_str();
        let end_label = format!(".Ldora_aot_function_end_{}", func_idx);
        writeln!(f)?;
        writeln!(f, "    .p2align 4")?;
        writeln!(f, ".globl {}", label)?;
        writeln!(f, "{}:", label)?;

        for chunk in func.code.chunks(12) {
            let bytes: Vec<String> = chunk.iter().map(|b| format!("0x{:02x}", b)).collect();
            writeln!(f, "    .byte {}", bytes.join(", "))?;
        }

        // On x86_64 the offset is the return address (position after the call
        // instruction), so we subtract 4 to reach the rel32 operand.
        // On aarch64 the offset points at the bl instruction itself.
        for reloc in &func.call_relocations {
            if is_arm64 {
                writeln!(
                    f,
                    "    .reloc {}+{}, R_AARCH64_CALL26, {}",
                    label, reloc.offset, reloc.target,
                )?;
            } else {
                let reloc_offset = reloc.offset - 4;
                writeln!(
                    f,
                    "    .reloc {}+{}, R_X86_64_PC32, {} - 4",
                    label, reloc_offset, reloc.target,
                )?;
            }
        }

        // AOT string constants are loaded through a writable data slot.
        // x86_64: mov reg, [rip + disp32] — R_X86_64_PC32.
        // aarch64: adrp+ldr sequence — not yet implemented in arm64 codegen.
        for reloc in &func.string_relocations {
            let string_id = reloc.string_id;
            let slot_index = if let Some(&idx) = string_slot_map.get(&string_id) {
                idx
            } else {
                let idx = string_slots.len();
                let slot_label = format!(".Ldora_aot_string_slot_{}", idx);
                string_slots.push(StringSlotEntry {
                    slot_label,
                    string_id,
                });
                string_slot_map.insert(string_id, idx);
                idx
            };

            let slot_label = &string_slots[slot_index].slot_label;
            if is_arm64 {
                // adrp + ldr (64-bit) sequence; offset points at the adrp.
                writeln!(
                    f,
                    "    .reloc {}+{}, R_AARCH64_ADR_PREL_PG_HI21, {}",
                    label, reloc.offset, slot_label,
                )?;
                writeln!(
                    f,
                    "    .reloc {}+{}+4, R_AARCH64_LDST64_ABS_LO12_NC, {}",
                    label, reloc.offset, slot_label,
                )?;
            } else {
                writeln!(
                    f,
                    "    .reloc {}+{}, R_X86_64_PC32, {} - 4",
                    label, reloc.offset, slot_label,
                )?;
            }
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
            if is_arm64 {
                // adrp + ldr (32-bit) sequence; offset points at the adrp.
                writeln!(
                    f,
                    "    .reloc {}+{}, R_AARCH64_ADR_PREL_PG_HI21, {}",
                    label, reloc.offset, slot_label,
                )?;
                writeln!(
                    f,
                    "    .reloc {}+{}+4, R_AARCH64_LDST32_ABS_LO12_NC, {}",
                    label, reloc.offset, slot_label,
                )?;
            } else {
                writeln!(
                    f,
                    "    .reloc {}+{}, R_X86_64_PC32, {} - 4",
                    label, reloc.offset, slot_label,
                )?;
            }
        }

        // Global variable address relocations.
        for reloc in &func.global_relocations {
            if is_arm64 {
                // adrp + add sequence; offset points at the adrp.
                writeln!(
                    f,
                    "    .reloc {}+{}, R_AARCH64_ADR_PREL_PG_HI21, _dora_global_memory+{}",
                    label, reloc.offset, reloc.global_offset,
                )?;
                writeln!(
                    f,
                    "    .reloc {}+{}+4, R_AARCH64_ADD_ABS_LO12_NC, _dora_global_memory+{}",
                    label, reloc.offset, reloc.global_offset,
                )?;
            } else {
                writeln!(
                    f,
                    "    .reloc {}+{}, R_X86_64_PC32, _dora_global_memory+{} - 4",
                    label, reloc.offset, reloc.global_offset,
                )?;
            }
        }

        writeln!(f, "{}:", end_label)?;
        function_metadata.push(FunctionMetadataEntry {
            start_label: label,
            end_label,
            fct_id: func.fct_id,
            kind: code_kind_value(func.kind),
            function: &func.function,
            gcpoints: &func.gcpoints,
            locations: &func.locations,
            inlined_functions: &func.inlined_functions,
        });
    }

    write_shape_metadata(
        f,
        &shape_slots,
        &aot.shapes,
        &aot.known_shapes,
        &mut strings,
    )?;
    write_global_metadata(f, aot)?;

    // Emit the dora entry trampoline (generated by dora_entry_trampoline::generate).
    // Signature: extern "C" fn(tld: usize, fct: *const u8) -> i32
    writeln!(f)?;
    writeln!(f, "    .p2align 4")?;
    writeln!(f, ".globl _dora_entry_trampoline")?;
    writeln!(f, ".globl _dora_entry_trampoline_end")?;
    writeln!(f, "_dora_entry_trampoline:")?;
    for chunk in trampoline.chunks(12) {
        let bytes: Vec<String> = chunk.iter().map(|b| format!("0x{:02x}", b)).collect();
        writeln!(f, "    .byte {}", bytes.join(", "))?;
    }
    writeln!(f, "_dora_entry_trampoline_end:")?;

    let dora_entry_function = AotFunctionInfo {
        name: strings.intern("_dora_entry_trampoline"),
        file: strings.intern(""),
        loc: Location::new(0, 0),
    };
    function_metadata.push(FunctionMetadataEntry {
        start_label: "_dora_entry_trampoline",
        end_label: "_dora_entry_trampoline_end".to_string(),
        fct_id: 0,
        kind: code_kind_value(AotCodeKind::DoraEntryTrampoline),
        function: &dora_entry_function,
        gcpoints: &[],
        locations: &[],
        inlined_functions: &[],
    });

    // Whether main() returns Unit (no return value).
    writeln!(f)?;
    writeln!(f, ".globl _dora_main_returns_unit")?;
    writeln!(f, "_dora_main_returns_unit:")?;
    writeln!(f, "    .byte {}", if aot.main_returns_unit { 1 } else { 0 })?;

    // Garbage collector selection.
    writeln!(f)?;
    writeln!(f, ".globl _dora_gc_collector")?;
    writeln!(f, "_dora_gc_collector:")?;
    writeln!(f, "    .byte {}", collector_name_value(aot.collector_name))?;

    // Emit the main entry point that tail-calls dora_aot_main.
    writeln!(f)?;
    writeln!(f, "    .p2align 4")?;
    writeln!(f, ".globl main")?;
    writeln!(f, "main:")?;
    if is_arm64 {
        writeln!(f, "    b dora_aot_main")?;
    } else {
        writeln!(f, "    jmp dora_aot_main")?;
    }

    write_function_metadata(f, &function_metadata)?;
    write_string_metadata(f, &string_slots, &strings)?;

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

fn write_string_metadata(
    f: &mut File,
    string_slots: &[StringSlotEntry],
    strings: &AotStringTable,
) -> std::io::Result<()> {
    // Writable slots for string pointers (RW).
    writeln!(f)?;
    writeln!(f, ".section .dora.string_data,\"aw\",@progbits")?;
    for slot in string_slots {
        writeln!(f, "    .p2align 3")?;
        writeln!(f, "{}:", slot.slot_label)?;
        writeln!(f, "    .quad 0")?;
    }

    // String UTF-8 payloads (R).
    writeln!(f)?;
    writeln!(f, ".section .rodata")?;
    for (idx, value) in strings.entries().iter().enumerate() {
        writeln!(f, "    .p2align 3")?;
        writeln!(f, ".Ldora_aot_string_data_{}:", idx)?;
        write_bytes(f, value.as_bytes())?;
    }

    // Shared string metadata table (R).
    writeln!(f)?;
    writeln!(f, ".section .dora.strings,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_strings_start")?;
    writeln!(f, "_dora_aot_strings_start:")?;
    for (idx, value) in strings.entries().iter().enumerate() {
        writeln!(f, "    .quad .Ldora_aot_string_data_{}", idx)?;
        writeln!(f, "    .quad {}", value.len())?;
    }
    writeln!(f, ".globl _dora_aot_strings_end")?;
    writeln!(f, "_dora_aot_strings_end:")?;

    // String slot relocation table (R).
    writeln!(f)?;
    writeln!(f, ".section .dora.string_slots,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_string_slots_start")?;
    writeln!(f, "_dora_aot_string_slots_start:")?;
    for slot in string_slots {
        writeln!(f, "    .quad {}", slot.slot_label)?;
        writeln!(f, "    .long {}", slot.string_id.index())?;
        writeln!(f, "    .long 0")?;
    }
    writeln!(f, ".globl _dora_aot_string_slots_end")?;
    writeln!(f, "_dora_aot_string_slots_end:")?;

    Ok(())
}

fn write_shape_metadata(
    f: &mut File,
    shape_slots: &[ShapeSlotEntry],
    shapes: &[AotShape],
    known_shapes: &[AotKnownShape],
    strings: &mut AotStringTable,
) -> std::io::Result<()> {
    // Writable slots for compressed shape pointers (RW, 4 bytes each).
    if !shape_slots.is_empty() {
        writeln!(f)?;
        writeln!(f, ".section .dora.shape_data,\"aw\",@progbits")?;
        for slot in shape_slots {
            writeln!(f, "    .p2align 2")?;
            writeln!(f, "{}:", slot.slot_label)?;
            writeln!(f, "    .long 0")?;
        }
    }

    // Shape slot relocation table (R).
    writeln!(f)?;
    writeln!(f, ".section .dora.shape_slots,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_shape_slots_start")?;
    writeln!(f, "_dora_aot_shape_slots_start:")?;
    for slot in shape_slots {
        writeln!(f, "    .quad {}", slot.slot_label)?;
        writeln!(f, "    .long {}", slot.shape_id)?;
        writeln!(f, "    .long 0")?;
    }
    writeln!(f, ".globl _dora_aot_shape_slots_end")?;
    writeln!(f, "_dora_aot_shape_slots_end:")?;

    let mut refs = Vec::<i32>::new();
    let mut shape_ref_ranges = Vec::<(usize, usize)>::with_capacity(shapes.len());
    for shape in shapes {
        let start = refs.len();
        refs.extend(shape.refs.iter().copied());
        shape_ref_ranges.push((start, refs.len() - start));
    }

    let mut vtable_entries = Vec::<Option<&str>>::new();
    let mut shape_vtable_ranges = Vec::<(usize, usize)>::with_capacity(shapes.len());
    for shape in shapes {
        let start = vtable_entries.len();
        for entry in &shape.vtable_entries {
            vtable_entries.push(entry.as_deref());
        }
        shape_vtable_ranges.push((start, vtable_entries.len() - start));
    }

    writeln!(f)?;
    writeln!(f, ".section .dora.shapes,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_shapes_start")?;
    writeln!(f, "_dora_aot_shapes_start:")?;
    for ((shape, (refs_start, refs_len)), (vtable_start, vtable_len)) in shapes
        .iter()
        .zip(shape_ref_ranges.iter())
        .zip(shape_vtable_ranges.iter())
    {
        writeln!(f, "    .quad {}", shape_kind_value(shape.kind))?;
        writeln!(f, "    .quad {}", shape.visitor)?;
        writeln!(f, "    .quad {}", refs_start)?;
        writeln!(f, "    .quad {}", refs_len)?;
        writeln!(f, "    .quad {}", shape.instance_size)?;
        writeln!(f, "    .quad {}", shape.element_size)?;
        writeln!(f, "    .quad {}", vtable_start)?;
        writeln!(f, "    .quad {}", vtable_len)?;
        writeln!(f, "    .long {}", strings.intern(&shape.name).index())?;
        writeln!(f, "    .long 0")?;
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
    writeln!(f, ".section .dora.shape_vtables,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_shape_vtables_start")?;
    writeln!(f, "_dora_aot_shape_vtables_start:")?;
    for symbol in &vtable_entries {
        match symbol {
            Some(name) => writeln!(f, "    .quad {}", name)?,
            None => writeln!(f, "    .quad 0")?,
        }
    }
    writeln!(f, ".globl _dora_aot_shape_vtables_end")?;
    writeln!(f, "_dora_aot_shape_vtables_end:")?;

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

fn write_global_metadata(f: &mut File, aot: &AotCompilation) -> std::io::Result<()> {
    writeln!(f)?;
    writeln!(f, ".bss")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_global_memory")?;
    writeln!(f, "_dora_global_memory:")?;
    writeln!(f, "    .zero {}", aot.global_layout.memory_size)?;
    writeln!(f, ".globl _dora_global_memory_end")?;
    writeln!(f, "_dora_global_memory_end:")?;

    // Emit GC reference offsets for globals.
    writeln!(f)?;
    writeln!(f, ".section .dora.global_refs,\"a\",@progbits")?;
    writeln!(f, "    .p2align 2")?;
    writeln!(f, ".globl _dora_aot_global_refs_start")?;
    writeln!(f, "_dora_aot_global_refs_start:")?;
    for offset in &aot.global_layout.references {
        writeln!(f, "    .long {}", offset)?;
    }
    writeln!(f, ".globl _dora_aot_global_refs_end")?;
    writeln!(f, "_dora_aot_global_refs_end:")?;
    writeln!(f, ".text")?;

    Ok(())
}

fn write_function_metadata(
    f: &mut File,
    functions: &[FunctionMetadataEntry<'_>],
) -> std::io::Result<()> {
    const NO_INLINED_FUNCTION_ID: u32 = u32::MAX;

    let mut gcpoint_entries = Vec::<(u32, usize, usize)>::new();
    let mut gcpoint_offsets = Vec::<i32>::new();
    let mut function_infos = Vec::<&AotFunctionInfo>::new();
    let mut location_entries = Vec::<(u32, u32, u32, u32)>::new();
    let mut inlined_function_entries = Vec::<(usize, u32, u32, u32)>::new();
    let mut function_entries = Vec::<FunctionMetadataLayout<'_>>::new();

    for metadata in functions {
        let function_info_idx = function_infos.len();
        function_infos.push(metadata.function);
        let gcpoint_start = gcpoint_entries.len();

        for gcpoint in metadata.gcpoints {
            let offsets_start = gcpoint_offsets.len();
            gcpoint_offsets.extend(gcpoint.offsets.iter().copied());
            let offsets_len = gcpoint_offsets.len() - offsets_start;
            gcpoint_entries.push((gcpoint.pc_offset, offsets_start, offsets_len));
        }

        let gcpoint_len = gcpoint_entries.len() - gcpoint_start;
        let location_start = location_entries.len();
        for location in metadata.locations {
            location_entries.push((
                location.pc_offset,
                location
                    .inlined_function_id
                    .unwrap_or(NO_INLINED_FUNCTION_ID),
                location.line,
                location.column,
            ));
        }
        let location_len = location_entries.len() - location_start;

        let inlined_function_start = inlined_function_entries.len();
        for inlined in metadata.inlined_functions {
            let function_info_idx = function_infos.len();
            function_infos.push(&inlined.function);

            inlined_function_entries.push((
                function_info_idx,
                inlined
                    .inlined_function_id
                    .unwrap_or(NO_INLINED_FUNCTION_ID),
                inlined.line,
                inlined.column,
            ));
        }
        let inlined_function_len = inlined_function_entries.len() - inlined_function_start;

        function_entries.push(FunctionMetadataLayout {
            start_label: metadata.start_label,
            end_label: metadata.end_label.as_str(),
            fct_id: metadata.fct_id,
            kind: metadata.kind,
            function_info_idx,
            gcpoint_start,
            gcpoint_len,
            location_start,
            location_len,
            inlined_function_start,
            inlined_function_len,
        });
    }

    writeln!(f)?;
    writeln!(f, ".section .dora.gcpoint_offsets,\"a\"")?;
    writeln!(f, "    .p2align 2")?;
    writeln!(f, ".globl _dora_aot_gcpoint_offsets_start")?;
    writeln!(f, "_dora_aot_gcpoint_offsets_start:")?;
    for offset in &gcpoint_offsets {
        writeln!(f, "    .long {}", offset)?;
    }
    writeln!(f, ".globl _dora_aot_gcpoint_offsets_end")?;
    writeln!(f, "_dora_aot_gcpoint_offsets_end:")?;

    writeln!(f)?;
    writeln!(f, ".section .dora.gcpoints,\"a\"")?;
    writeln!(f, "    .p2align 2")?;
    writeln!(f, ".globl _dora_aot_gcpoints_start")?;
    writeln!(f, "_dora_aot_gcpoints_start:")?;
    for (pc_offset, offsets_start, offsets_len) in &gcpoint_entries {
        writeln!(f, "    .long {}", pc_offset)?;
        writeln!(f, "    .long {}", offsets_start)?;
        writeln!(f, "    .long {}", offsets_len)?;
    }
    writeln!(f, ".globl _dora_aot_gcpoints_end")?;
    writeln!(f, "_dora_aot_gcpoints_end:")?;

    writeln!(f)?;
    writeln!(f, ".section .dora.locations,\"a\"")?;
    writeln!(f, "    .p2align 2")?;
    writeln!(f, ".globl _dora_aot_locations_start")?;
    writeln!(f, "_dora_aot_locations_start:")?;
    for (pc_offset, inlined_function_id, line, column) in &location_entries {
        writeln!(f, "    .long {}", pc_offset)?;
        writeln!(f, "    .long {}", inlined_function_id)?;
        writeln!(f, "    .long {}", line)?;
        writeln!(f, "    .long {}", column)?;
    }
    writeln!(f, ".globl _dora_aot_locations_end")?;
    writeln!(f, "_dora_aot_locations_end:")?;

    writeln!(f)?;
    writeln!(f, ".section .dora.function_info,\"a\"")?;
    writeln!(f, "    .p2align 2")?;
    writeln!(f, ".globl _dora_aot_function_info_start")?;
    writeln!(f, "_dora_aot_function_info_start:")?;
    for function_info in &function_infos {
        writeln!(f, "    .long {}", function_info.name.index())?;
        writeln!(f, "    .long {}", function_info.file.index())?;
        writeln!(f, "    .long {}", function_info.loc.line())?;
        writeln!(f, "    .long {}", function_info.loc.column())?;
    }
    writeln!(f, ".globl _dora_aot_function_info_end")?;
    writeln!(f, "_dora_aot_function_info_end:")?;

    writeln!(f)?;
    writeln!(f, ".section .dora.inlined_functions,\"a\"")?;
    writeln!(f, "    .p2align 2")?;
    writeln!(f, ".globl _dora_aot_inlined_functions_start")?;
    writeln!(f, "_dora_aot_inlined_functions_start:")?;
    for (function_info_idx, inlined_function_id, line, column) in &inlined_function_entries {
        writeln!(f, "    .long {}", function_info_idx)?;
        writeln!(f, "    .long {}", inlined_function_id)?;
        writeln!(f, "    .long {}", line)?;
        writeln!(f, "    .long {}", column)?;
    }
    writeln!(f, ".globl _dora_aot_inlined_functions_end")?;
    writeln!(f, "_dora_aot_inlined_functions_end:")?;

    writeln!(f)?;
    writeln!(f, ".section .dora.functions,\"a\"")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_functions_start")?;
    writeln!(f, "_dora_aot_functions_start:")?;
    for entry in &function_entries {
        writeln!(f, "    .quad {}", entry.start_label)?;
        writeln!(f, "    .quad {}", entry.end_label)?;
        writeln!(f, "    .long {}", entry.fct_id)?;
        writeln!(f, "    .long {}", entry.kind)?;
        writeln!(f, "    .long {}", entry.function_info_idx)?;
        writeln!(f, "    .long {}", entry.gcpoint_start)?;
        writeln!(f, "    .long {}", entry.gcpoint_len)?;
        writeln!(f, "    .long {}", entry.location_start)?;
        writeln!(f, "    .long {}", entry.location_len)?;
        writeln!(f, "    .long {}", entry.inlined_function_start)?;
        writeln!(f, "    .long {}", entry.inlined_function_len)?;
        writeln!(f, "    .long 0")?; // reserved padding
    }
    writeln!(f, ".globl _dora_aot_functions_end")?;
    writeln!(f, "_dora_aot_functions_end:")?;
    writeln!(f, ".text")?;

    Ok(())
}

fn code_kind_value(kind: AotCodeKind) -> u32 {
    match kind {
        AotCodeKind::Optimized => 0,
        AotCodeKind::RuntimeEntryTrampoline => 1,
        AotCodeKind::DoraEntryTrampoline => 2,
        AotCodeKind::AllocationFailureTrampoline => 3,
        AotCodeKind::TrapTrampoline => 4,
        AotCodeKind::SafepointTrampoline => 5,
    }
}

fn collector_name_value(name: CollectorName) -> u8 {
    match name {
        CollectorName::Zero => 0,
        CollectorName::Copy => 1,
        CollectorName::Sweep => 2,
        CollectorName::Swiper => 3,
    }
}

fn shape_kind_value(kind: AotShapeKind) -> u8 {
    match kind {
        AotShapeKind::Opaque => 0,
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
