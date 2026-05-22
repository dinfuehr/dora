use std::collections::HashMap;
use std::io::Write;

use dora_bytecode::Location;

use crate::compiler::aot::{
    AotCodeKind, AotCompilation, AotFunction, AotFunctionInfo, AotGcPoint, AotInlinedFunction,
    AotKnownShape, AotKnownShapeKind, AotLocation, AotShape, AotShapeId, AotStringId,
    AotStringTable,
};
use crate::shape::ShapeVisitor;
use crate::startup::encode_shape_kind;
use crate::vm::{CollectorName, TargetArch};

struct StringSlotEntry {
    slot_label: String,
    string_id: AotStringId,
}

struct ShapeSlotEntry {
    slot_label: String,
    shape_id: AotShapeId,
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

#[derive(Clone, Copy)]
pub enum AotAssemblyKind {
    Regular,
    CompilerImage,
}

const BOOTS_COMPILER_ENTRY_SYMBOL: &str = "_dora_boots__interface__compile";
const BOOTS_COMPILER_STARTUP_SYMBOL: &str = "dora_boots_compiler_main";

pub fn write_assembly<W: Write>(
    f: &mut W,
    aot: &AotCompilation,
    encoded_program: &[u8],
    trampoline: &[u8],
    target_arch: TargetArch,
    kind: AotAssemblyKind,
) -> std::io::Result<()> {
    let is_arm64 = target_arch.is_arm64();

    let functions: &[AotFunction] = &aot.functions;
    writeln!(f, ".text")?;

    let mut strings = aot.strings.clone();
    let mut string_slots = Vec::<StringSlotEntry>::new();
    let mut string_slot_map = HashMap::<AotStringId, usize>::new();
    let mut shape_slots = Vec::<ShapeSlotEntry>::new();
    let mut shape_slot_map = HashMap::<AotShapeId, usize>::new();
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
        // x86_64: mov reg, [rip + disp32] - R_X86_64_PC32.
        // aarch64: adrp+ldr sequence - not yet implemented in arm64 codegen.
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
    write_program_metadata(f, encoded_program)?;

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

    // Garbage collector selection.
    writeln!(f)?;
    writeln!(f, ".globl _dora_gc_collector")?;
    writeln!(f, "_dora_gc_collector:")?;
    writeln!(f, "    .byte {}", collector_name_value(aot.collector_name))?;

    match kind {
        AotAssemblyKind::Regular => write_regular_main(f, target_arch)?,
        AotAssemblyKind::CompilerImage => write_compiler_image_main(f, target_arch)?,
    }

    write_function_metadata(f, &function_metadata)?;
    write_string_metadata(f, &string_slots, &strings)?;

    Ok(())
}

fn write_regular_main<W: Write>(f: &mut W, target_arch: TargetArch) -> std::io::Result<()> {
    // Emit the main entry point that tail-calls dora_aot_main.
    writeln!(f)?;
    writeln!(f, "    .p2align 4")?;
    writeln!(f, ".globl main")?;
    writeln!(f, "main:")?;
    if target_arch.is_arm64() {
        writeln!(f, "    b dora_aot_main")?;
    } else {
        writeln!(f, "    jmp dora_aot_main")?;
    }

    Ok(())
}

fn write_compiler_image_main<W: Write>(f: &mut W, target_arch: TargetArch) -> std::io::Result<()> {
    // dora-startup also contains the normal AOT startup path, which links
    // against _dora_main. Compiler images enter through a different startup
    // symbol, so provide an unreachable definition only to satisfy the static
    // library reference.
    writeln!(f)?;
    writeln!(f, "    .p2align 4")?;
    writeln!(f, ".globl _dora_main")?;
    writeln!(f, "_dora_main:")?;
    if target_arch.is_arm64() {
        writeln!(f, "    brk #0")?;
    } else {
        writeln!(f, "    ud2")?;
    }

    // The executable entry enters Rust startup first. The compiled entry
    // symbol is passed as a third C argument.
    writeln!(f)?;
    writeln!(f, "    .p2align 4")?;
    writeln!(f, ".globl main")?;
    writeln!(f, "main:")?;
    if target_arch.is_arm64() {
        writeln!(f, "    adrp x2, {}", BOOTS_COMPILER_ENTRY_SYMBOL)?;
        writeln!(f, "    add x2, x2, :lo12:{}", BOOTS_COMPILER_ENTRY_SYMBOL)?;
        writeln!(f, "    b {}", BOOTS_COMPILER_STARTUP_SYMBOL)?;
    } else {
        writeln!(f, "    leaq {}(%rip), %rdx", BOOTS_COMPILER_ENTRY_SYMBOL)?;
        writeln!(f, "    jmp {}", BOOTS_COMPILER_STARTUP_SYMBOL)?;
    }

    Ok(())
}

fn write_program_metadata<W: Write>(f: &mut W, encoded_program: &[u8]) -> std::io::Result<()> {
    writeln!(f)?;
    writeln!(f, ".section .dora.program,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_program_start")?;
    writeln!(f, "_dora_aot_program_start:")?;
    write_bytes(f, encoded_program)?;
    writeln!(f, ".globl _dora_aot_program_end")?;
    writeln!(f, "_dora_aot_program_end:")?;
    writeln!(f, ".text")?;

    Ok(())
}

fn write_bytes<W: Write>(f: &mut W, data: &[u8]) -> std::io::Result<()> {
    if data.is_empty() {
        return Ok(());
    }

    for chunk in data.chunks(12) {
        let bytes: Vec<String> = chunk.iter().map(|b| format!("0x{:02x}", b)).collect();
        writeln!(f, "    .byte {}", bytes.join(", "))?;
    }

    Ok(())
}

fn write_string_metadata<W: Write>(
    f: &mut W,
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

fn write_shape_metadata<W: Write>(
    f: &mut W,
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
        writeln!(f, "    .long {}", slot.shape_id.0)?;
        writeln!(f, "    .long 0")?;
    }
    writeln!(f, ".globl _dora_aot_shape_slots_end")?;
    writeln!(f, "_dora_aot_shape_slots_end:")?;

    let mut refs = Vec::<i32>::new();
    let mut shape_ref_ranges = Vec::<(usize, usize)>::with_capacity(shapes.len());
    let mut shape_kinds = Vec::<u8>::new();
    let mut shape_kind_ranges = Vec::<(usize, usize)>::with_capacity(shapes.len());
    let mut shape_fields = Vec::<u8>::new();
    let mut shape_field_ranges = Vec::<(usize, usize)>::with_capacity(shapes.len());
    for shape in shapes {
        let start = refs.len();
        refs.extend(shape.refs.iter().copied());
        shape_ref_ranges.push((start, refs.len() - start));

        let start = shape_kinds.len();
        let kind = encode_shape_kind(&shape.kind);
        shape_kinds.extend(kind.iter().copied());
        shape_kind_ranges.push((start, shape_kinds.len() - start));

        let start = shape_fields.len();
        shape_fields.extend(shape.fields.iter().copied());
        shape_field_ranges.push((start, shape_fields.len() - start));
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
    for (
        (((shape, (refs_start, refs_len)), (kind_start, kind_len)), (fields_start, fields_len)),
        (vtable_start, vtable_len),
    ) in shapes
        .iter()
        .zip(shape_ref_ranges.iter())
        .zip(shape_kind_ranges.iter())
        .zip(shape_field_ranges.iter())
        .zip(shape_vtable_ranges.iter())
    {
        writeln!(f, "    .quad {}", kind_start)?;
        writeln!(f, "    .quad {}", kind_len)?;
        writeln!(f, "    .quad {}", shape_visitor_value(shape.visitor))?;
        writeln!(f, "    .quad {}", refs_start)?;
        writeln!(f, "    .quad {}", refs_len)?;
        writeln!(f, "    .quad {}", fields_start)?;
        writeln!(f, "    .quad {}", fields_len)?;
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
    writeln!(f, ".section .dora.shape_kinds,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_shape_kinds_start")?;
    writeln!(f, "_dora_aot_shape_kinds_start:")?;
    write_bytes(f, &shape_kinds)?;
    writeln!(f, ".globl _dora_aot_shape_kinds_end")?;
    writeln!(f, "_dora_aot_shape_kinds_end:")?;

    writeln!(f)?;
    writeln!(f, ".section .dora.shape_fields,\"a\",@progbits")?;
    writeln!(f, "    .p2align 3")?;
    writeln!(f, ".globl _dora_aot_shape_fields_start")?;
    writeln!(f, "_dora_aot_shape_fields_start:")?;
    write_bytes(f, &shape_fields)?;
    writeln!(f, ".globl _dora_aot_shape_fields_end")?;
    writeln!(f, "_dora_aot_shape_fields_end:")?;

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
        writeln!(f, "    .long {}", known_shape.shape_id.0)?;
    }
    writeln!(f, ".globl _dora_aot_known_shapes_end")?;
    writeln!(f, "_dora_aot_known_shapes_end:")?;
    writeln!(f, ".text")?;

    Ok(())
}

fn write_global_metadata<W: Write>(f: &mut W, aot: &AotCompilation) -> std::io::Result<()> {
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

fn write_function_metadata<W: Write>(
    f: &mut W,
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

fn shape_visitor_value(visitor: ShapeVisitor) -> u8 {
    match visitor {
        ShapeVisitor::Regular => 0,
        ShapeVisitor::PointerArray => 1,
        ShapeVisitor::RecordArray => 2,
        ShapeVisitor::None => 3,
        ShapeVisitor::Invalid => 4,
    }
}
