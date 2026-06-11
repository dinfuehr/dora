use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

use dora_bytecode::Location;
use dora_symbol::mangle_name;

use crate::{
    AOT_CODE_KIND_ALLOCATION_FAILURE_TRAMPOLINE, AOT_CODE_KIND_DORA_ENTRY_TRAMPOLINE,
    AOT_CODE_KIND_FATAL_ERROR_TRAMPOLINE, AOT_CODE_KIND_OPTIMIZED,
    AOT_CODE_KIND_RUNTIME_ENTRY_TRAMPOLINE, AOT_CODE_KIND_SAFEPOINT_TRAMPOLINE,
    AOT_CODE_KIND_STACK_OVERFLOW_TRAMPOLINE, AOT_CODE_KIND_TRAP_TRAMPOLINE,
    AOT_CODE_KIND_UNREACHABLE_TRAMPOLINE, AOT_SHAPE_VISITOR_INVALID, AOT_SHAPE_VISITOR_NONE,
    AOT_SHAPE_VISITOR_POINTER_ARRAY, AOT_SHAPE_VISITOR_RECORD_ARRAY, AOT_SHAPE_VISITOR_REGULAR,
    AotCodeKind, AotCompilation, AotFunction, AotFunctionInfo, AotGcPoint,
    AotGlobalRelocationTarget, AotInlinedFunction, AotKnownShape, AotKnownShapeKind, AotLocation,
    AotRelocationTarget, AotShape, AotShapeId, AotStringId, AotStringTable, CollectorName,
    ShapeVisitor, TargetArch, encode_shape_kind,
};

mod coff;
mod elf;
mod macho;

struct StringSlotEntry {
    slot_label: String,
    string_id: AotStringId,
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
    Test,
    CompilerImage,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ObjectFormat {
    Elf,
    MachO,
    Coff,
}

#[derive(Clone, Copy)]
enum SectionKind {
    ReadOnly,
    Writable,
}

struct AssemblySyntax {
    f: BufWriter<File>,
    format: ObjectFormat,
}

impl AssemblySyntax {
    fn new(f: File) -> AssemblySyntax {
        let format = if cfg!(target_os = "windows") {
            ObjectFormat::Coff
        } else if cfg!(target_os = "macos") {
            ObjectFormat::MachO
        } else {
            ObjectFormat::Elf
        };

        AssemblySyntax {
            f: BufWriter::new(f),
            format,
        }
    }

    fn is_macho(&self) -> bool {
        self.format == ObjectFormat::MachO
    }

    fn is_coff(&self) -> bool {
        self.format == ObjectFormat::Coff
    }

    fn symbol(&self, symbol: &str) -> String {
        debug_assert!(!symbol.starts_with(".L"));
        debug_assert!(!symbol.starts_with('_'));

        if self.is_macho() {
            format!("_{}", symbol)
        } else {
            symbol.to_string()
        }
    }

    fn symbol_ref(&self, symbol: &str) -> String {
        if symbol.starts_with(".L") {
            self.local_symbol_ref(symbol)
        } else {
            self.symbol(symbol)
        }
    }

    fn local_symbol_ref(&self, symbol: &str) -> String {
        debug_assert!(symbol.starts_with(".L"));

        if self.is_coff() {
            format!("L${}", &symbol[2..])
        } else {
            symbol.to_string()
        }
    }

    fn write_line(&mut self, args: fmt::Arguments<'_>) {
        self.f.write_fmt(args).expect("failed to write assembly");
        self.f.write_all(b"\n").expect("failed to write assembly");
    }

    fn write_indented_line(&mut self, args: fmt::Arguments<'_>) {
        self.f.write_all(b"    ").expect("failed to write assembly");
        self.write_line(args)
    }

    fn write_preamble(&mut self) {
        if self.is_coff() {
            self.write_line(format_args!("OPTION CASEMAP:NONE"));
        }
    }

    fn write_text(&mut self) {
        if self.is_coff() {
            self.write_line(format_args!(".code"))
        } else {
            self.write_line(format_args!(".text"))
        }
    }

    fn write_rodata(&mut self) {
        match self.format {
            ObjectFormat::Elf => self.write_line(format_args!(".section .rodata")),
            ObjectFormat::MachO => self.write_line(format_args!(".section __TEXT,__const")),
            ObjectFormat::Coff => self.write_line(format_args!(".const")),
        }
    }

    fn write_data_section(
        &mut self,
        elf_section_name: &str,
        macho_section_name: &str,
        kind: SectionKind,
    ) {
        match self.format {
            ObjectFormat::Elf => {
                let flags = match kind {
                    SectionKind::ReadOnly => "a",
                    SectionKind::Writable => "aw",
                };
                self.write_line(format_args!(
                    ".section {elf_section_name},\"{flags}\",@progbits"
                ))
            }
            ObjectFormat::MachO => {
                assert!(macho_section_name.len() <= 16);
                self.write_line(format_args!(".section __DATA,{macho_section_name}"))
            }
            ObjectFormat::Coff => match kind {
                SectionKind::ReadOnly => self.write_line(format_args!(".const")),
                SectionKind::Writable => self.write_line(format_args!(".data")),
            },
        }
    }

    fn write_bss(&mut self) {
        match self.format {
            ObjectFormat::Elf => self.write_line(format_args!(".bss")),
            ObjectFormat::MachO => self.write_line(format_args!(".section __DATA,__bss")),
            ObjectFormat::Coff => self.write_line(format_args!(".data?")),
        }
    }

    fn write_global(&mut self, symbol: &str) {
        let symbol = self.symbol(symbol);
        if self.is_coff() {
            self.write_line(format_args!("PUBLIC {symbol}"))
        } else {
            self.write_line(format_args!(".globl {symbol}"))
        }
    }

    fn write_extern_proc(&mut self, symbol: &str) {
        debug_assert!(self.is_coff());

        let symbol = self.symbol(symbol);
        self.write_line(format_args!("EXTERN {symbol}:PROC"));
    }

    fn write_label(&mut self, symbol: &str) {
        let symbol = self.symbol(symbol);
        self.write_line(format_args!("{symbol}:"))
    }

    fn write_local_symbol(&mut self, symbol: &str) {
        let symbol = self.local_symbol_ref(symbol);
        self.write_line(format_args!("{symbol}:"))
    }

    fn write_newline(&mut self) {
        self.write_line(format_args!(""))
    }

    fn write_p2_align(&mut self, alignment: u32) {
        if self.is_coff() {
            self.write_indented_line(format_args!("ALIGN {}", 1_u32 << alignment))
        } else {
            self.write_indented_line(format_args!(".p2align {alignment}"))
        }
    }

    fn write_align16(&mut self) {
        self.write_p2_align(4)
    }

    fn write_align8(&mut self) {
        self.write_p2_align(3)
    }

    fn write_align4(&mut self) {
        self.write_p2_align(2)
    }

    fn write_bytes(&mut self, data: &[u8]) {
        for chunk in data.chunks(12) {
            if self.is_coff() {
                let bytes = chunk
                    .iter()
                    .map(|b| b.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                self.write_indented_line(format_args!("DB {bytes}"));
            } else {
                let bytes = chunk
                    .iter()
                    .map(|b| format!("0x{:02x}", b))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.write_indented_line(format_args!(".byte {bytes}"));
            }
        }
    }

    fn write_byte(&mut self, value: impl fmt::Display) {
        if self.is_coff() {
            self.write_indented_line(format_args!("DB {value}"))
        } else {
            self.write_indented_line(format_args!(".byte {value}"))
        }
    }

    fn write_quad(&mut self, value: impl fmt::Display) {
        if self.is_coff() {
            self.write_indented_line(format_args!("DQ {value}"))
        } else {
            self.write_indented_line(format_args!(".quad {value}"))
        }
    }

    fn write_quad_symbol(&mut self, symbol: &str) {
        let symbol = self.symbol_ref(symbol);
        self.write_quad(symbol)
    }

    fn write_quad_symbol_offset(&mut self, symbol: &str, offset: usize) {
        let symbol = self.symbol_ref(symbol);
        if offset == 0 {
            self.write_quad(symbol)
        } else {
            self.write_quad(format_args!("{symbol}+{offset}"))
        }
    }

    fn write_long(&mut self, value: impl fmt::Display) {
        if self.is_coff() {
            self.write_indented_line(format_args!("DD {value}"))
        } else {
            self.write_indented_line(format_args!(".long {value}"))
        }
    }

    fn write_zero(&mut self, size: impl fmt::Display) {
        if self.is_coff() {
            self.write_indented_line(format_args!("DB {size} DUP (?)"))
        } else {
            self.write_indented_line(format_args!(".zero {size}"))
        }
    }

    fn write_end(&mut self) {
        if self.is_coff() {
            self.write_line(format_args!("END"));
        }
    }

    fn flush(&mut self) {
        self.f.flush().expect("failed to write assembly");
    }
}

fn global_target_label(target: AotGlobalRelocationTarget) -> String {
    match target {
        AotGlobalRelocationTarget::State(global_id) => {
            format!(".Ldora_global_{}_state", global_id.index())
        }
        AotGlobalRelocationTarget::Value(global_id) => {
            format!(".Ldora_global_{}_value", global_id.index())
        }
    }
}

pub fn write_assembly(
    output_path: impl AsRef<Path>,
    aot: &AotCompilation,
    encoded_program: &[u8],
    trampoline: &[u8],
    target_arch: TargetArch,
    kind: AotAssemblyKind,
) {
    let output_path = output_path.as_ref();
    let output = File::create(output_path).unwrap_or_else(|err| {
        panic!(
            "failed to create assembly output '{}': {err}",
            output_path.display()
        )
    });
    let mut syntax = AssemblySyntax::new(output);

    if syntax.is_macho() && !target_arch.is_arm64() {
        panic!("Mach-O AOT assembly is only supported on arm64");
    }

    if syntax.is_coff() && target_arch.is_arm64() {
        panic!("COFF AOT assembly is only supported on x64");
    }

    let functions: &[AotFunction] = &aot.functions;
    syntax.write_preamble();
    write_masm_extern_procs(&mut syntax, kind);
    syntax.write_text();

    let mut strings = aot.strings.clone();
    let mut string_slots = Vec::<StringSlotEntry>::new();
    let mut string_slot_map = HashMap::<AotStringId, usize>::new();
    let mut function_metadata = Vec::with_capacity(functions.len() + 1);

    for (func_idx, func) in functions.iter().enumerate() {
        let label = func.symbol_name.as_str();
        let end_label = format!(".Ldora_aot_function_end_{}", func_idx);
        write_masm_extern_proc_for_function(&mut syntax, func);
        syntax.write_newline();
        syntax.write_align16();
        syntax.write_global(label);
        syntax.write_label(label);

        if syntax.is_macho() {
            macho::write_function_body(&mut syntax, func, &mut string_slots, &mut string_slot_map);
        } else if syntax.is_coff() {
            coff::write_function_body(&mut syntax, func, &mut string_slots, &mut string_slot_map);
        } else {
            elf::write_function_body(&mut syntax, func, &mut string_slots, &mut string_slot_map);
        }

        syntax.write_local_symbol(&end_label);
        function_metadata.push(FunctionMetadataEntry {
            start_label: label,
            end_label,
            fct_id: func.fct_id,
            kind: code_kind_value(&func.kind),
            function: &func.function,
            gcpoints: &func.gcpoints,
            locations: &func.locations,
            inlined_functions: &func.inlined_functions,
        });
    }

    write_shape_metadata(&mut syntax, &aot.shapes, &aot.known_shapes);
    write_global_metadata(&mut syntax, aot);
    write_program_metadata(&mut syntax, encoded_program);

    // Emit the dora entry trampoline (generated by dora_entry_trampoline::generate).
    // Signature: extern "C" fn(tld: usize, fct: *const u8) -> i32
    syntax.write_newline();
    syntax.write_align16();
    let dora_entry_trampoline = "dora_entry_trampoline";
    let dora_entry_trampoline_end = ".Ldora_entry_trampoline_end";
    syntax.write_global(dora_entry_trampoline);
    syntax.write_label(dora_entry_trampoline);
    syntax.write_bytes(trampoline);
    syntax.write_local_symbol(dora_entry_trampoline_end);

    let dora_entry_function = AotFunctionInfo {
        name: strings.intern(dora_entry_trampoline),
        file: strings.intern(""),
        loc: Location::new(0, 0),
    };
    function_metadata.push(FunctionMetadataEntry {
        start_label: dora_entry_trampoline,
        end_label: dora_entry_trampoline_end.to_string(),
        fct_id: 0,
        kind: code_kind_value(&AotCodeKind::DoraEntryTrampoline),
        function: &dora_entry_function,
        gcpoints: &[],
        locations: &[],
        inlined_functions: &[],
    });

    // Garbage collector selection.
    syntax.write_newline();
    syntax.write_global("dora_gc_collector");
    syntax.write_label("dora_gc_collector");
    syntax.write_byte(collector_name_value(aot.collector_name));

    write_test_metadata(&mut syntax, aot);

    match kind {
        AotAssemblyKind::Regular => write_regular_main(&mut syntax, target_arch),
        AotAssemblyKind::Test => write_test_main(&mut syntax, target_arch),
        AotAssemblyKind::CompilerImage => write_compiler_image_main(&mut syntax, target_arch),
    }

    write_function_metadata(&mut syntax, &function_metadata);
    write_string_metadata(&mut syntax, &string_slots, &strings);

    syntax.write_end();
    syntax.flush();
}

fn write_masm_extern_procs(syntax: &mut AssemblySyntax, kind: AotAssemblyKind) {
    if !syntax.is_coff() {
        return;
    }

    syntax.write_extern_proc("dora_aot_write_barrier_slow_path");
    syntax.write_extern_proc(match kind {
        AotAssemblyKind::Regular => "dora_aot_main",
        AotAssemblyKind::Test => "dora_aot_test_main",
        AotAssemblyKind::CompilerImage => "dora_boots_compiler_main",
    });

    syntax.write_newline();
}

fn write_masm_extern_proc_for_function(syntax: &mut AssemblySyntax, function: &AotFunction) {
    if !syntax.is_coff() {
        return;
    }

    match &function.kind {
        AotCodeKind::RuntimeEntryTrampoline { native_target } => {
            syntax.write_extern_proc(native_target);
        }
        AotCodeKind::AllocationFailureTrampoline => {
            syntax.write_extern_proc("dora_native_gc_alloc");
        }
        AotCodeKind::TrapTrampoline => {
            syntax.write_extern_proc("dora_native_trap");
        }
        AotCodeKind::StackOverflowTrampoline => {
            syntax.write_extern_proc("dora_native_stack_overflow");
        }
        AotCodeKind::SafepointTrampoline => {
            syntax.write_extern_proc("dora_native_safepoint_slow");
        }
        AotCodeKind::UnreachableTrampoline => {
            syntax.write_extern_proc("dora_native_unreachable");
        }
        AotCodeKind::FatalErrorTrampoline => {
            syntax.write_extern_proc("dora_native_fatal_error");
        }
        AotCodeKind::Optimized | AotCodeKind::DoraEntryTrampoline => {}
    }
}

fn relocation_target_symbol(
    syntax: &AssemblySyntax,
    target: &AotRelocationTarget,
    string_slots: &mut Vec<StringSlotEntry>,
    string_slot_map: &mut HashMap<AotStringId, usize>,
) -> String {
    let symbol = match target {
        AotRelocationTarget::Call(target) => target.clone(),
        AotRelocationTarget::StringSlot(string_id) => {
            string_slot_label(string_slots, string_slot_map, *string_id)
        }
        AotRelocationTarget::ShapeAddress(shape_id) => shape_descriptor_label(*shape_id),
        AotRelocationTarget::ShapeBase => "dora_aot_shape_base".to_string(),
        AotRelocationTarget::Global(target) => global_target_label(*target),
    };

    syntax.symbol_ref(&symbol)
}

fn string_slot_label(
    string_slots: &mut Vec<StringSlotEntry>,
    string_slot_map: &mut HashMap<AotStringId, usize>,
    string_id: AotStringId,
) -> String {
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

    string_slots[slot_index].slot_label.clone()
}

fn shape_descriptor_label(shape_id: AotShapeId) -> String {
    format!(".Ldora_aot_shape_{}", shape_id.0)
}

fn write_test_metadata(syntax: &mut AssemblySyntax, aot: &AotCompilation) {
    use SectionKind::ReadOnly;

    syntax.write_newline();
    syntax.write_data_section(".dora.tests", "__dora_tests", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_tests_start");
    syntax.write_label("dora_aot_tests_start");
    for test in &aot.test_functions {
        syntax.write_quad_symbol(&test.symbol_name);
        syntax.write_long(test.fct_id);
        syntax.write_long(0);
    }
    syntax.write_global("dora_aot_tests_end");
    syntax.write_label("dora_aot_tests_end");
    syntax.write_text();
}

fn write_regular_main(syntax: &mut AssemblySyntax, target_arch: TargetArch) {
    // Pass the compiled program entry as a third argument to startup.
    let main_symbol = syntax.symbol(&mangle_name("main"));
    let startup_symbol = syntax.symbol("dora_aot_main");

    syntax.write_newline();
    syntax.write_align16();
    syntax.write_global("main");
    syntax.write_label("main");
    if target_arch.is_arm64() {
        if syntax.is_macho() {
            syntax.write_indented_line(format_args!("adrp x2, {main_symbol}@PAGE"));
            syntax.write_indented_line(format_args!("add x2, x2, {main_symbol}@PAGEOFF"));
        } else {
            syntax.write_indented_line(format_args!("adrp x2, {main_symbol}"));
            syntax.write_indented_line(format_args!("add x2, x2, :lo12:{main_symbol}"));
        }
        syntax.write_indented_line(format_args!("b {startup_symbol}"));
    } else {
        if syntax.is_coff() {
            syntax.write_indented_line(format_args!("lea r8, [{main_symbol}]"));
        } else {
            syntax.write_indented_line(format_args!("leaq {main_symbol}(%rip), %rdx"));
        }
        syntax.write_indented_line(format_args!("jmp {startup_symbol}"));
    }
}

fn write_test_main(syntax: &mut AssemblySyntax, target_arch: TargetArch) {
    let startup_symbol = syntax.symbol("dora_aot_test_main");

    syntax.write_newline();
    syntax.write_align16();
    syntax.write_global("main");
    syntax.write_label("main");
    if target_arch.is_arm64() {
        syntax.write_indented_line(format_args!("b {startup_symbol}"));
    } else {
        syntax.write_indented_line(format_args!("jmp {startup_symbol}"));
    }
}

fn write_compiler_image_main(syntax: &mut AssemblySyntax, target_arch: TargetArch) {
    // The executable entry enters Rust startup first. The compiled entry
    // symbol is passed as a third C argument.
    let compiler_entry_symbol = syntax.symbol(&mangle_name("interface::compile"));
    let startup_symbol = syntax.symbol("dora_boots_compiler_main");

    syntax.write_newline();
    syntax.write_align16();
    syntax.write_global("main");
    syntax.write_label("main");
    if target_arch.is_arm64() {
        if syntax.is_macho() {
            syntax.write_indented_line(format_args!("adrp x2, {compiler_entry_symbol}@PAGE"));
            syntax.write_indented_line(format_args!("add x2, x2, {compiler_entry_symbol}@PAGEOFF"));
        } else {
            syntax.write_indented_line(format_args!("adrp x2, {compiler_entry_symbol}"));
            syntax.write_indented_line(format_args!("add x2, x2, :lo12:{compiler_entry_symbol}"));
        }
        syntax.write_indented_line(format_args!("b {startup_symbol}"));
    } else {
        if syntax.is_coff() {
            syntax.write_indented_line(format_args!("lea r8, [{compiler_entry_symbol}]"));
        } else {
            syntax.write_indented_line(format_args!("leaq {compiler_entry_symbol}(%rip), %rdx"));
        }
        syntax.write_indented_line(format_args!("jmp {startup_symbol}"));
    }
}

fn write_program_metadata(syntax: &mut AssemblySyntax, encoded_program: &[u8]) {
    use SectionKind::ReadOnly;

    syntax.write_newline();
    syntax.write_data_section(".dora.program", "__dora_program", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_program_start");
    syntax.write_label("dora_aot_program_start");
    syntax.write_bytes(encoded_program);
    syntax.write_global("dora_aot_program_end");
    syntax.write_label("dora_aot_program_end");
    syntax.write_text();
}

fn write_string_metadata(
    syntax: &mut AssemblySyntax,
    string_slots: &[StringSlotEntry],
    strings: &AotStringTable,
) {
    use SectionKind::{ReadOnly, Writable};

    // Writable slots for string pointers (RW).
    syntax.write_newline();
    syntax.write_data_section(".dora.string_data", "__dora_strdata", Writable);
    for slot in string_slots {
        syntax.write_align8();
        syntax.write_local_symbol(&slot.slot_label);
        syntax.write_quad(0);
    }

    // String UTF-8 payloads (R).
    syntax.write_newline();
    syntax.write_rodata();
    for (idx, value) in strings.entries().iter().enumerate() {
        syntax.write_align8();
        syntax.write_local_symbol(&format!(".Ldora_aot_string_data_{}", idx));
        syntax.write_bytes(value.as_bytes());
    }

    // Shared string metadata table (R).
    syntax.write_newline();
    syntax.write_data_section(".dora.strings", "__dora_strings", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_strings_start");
    syntax.write_label("dora_aot_strings_start");
    for (idx, value) in strings.entries().iter().enumerate() {
        syntax.write_quad_symbol(&format!(".Ldora_aot_string_data_{}", idx));
        syntax.write_quad(value.len());
    }
    syntax.write_global("dora_aot_strings_end");
    syntax.write_label("dora_aot_strings_end");

    // String slot relocation table (R).
    syntax.write_newline();
    syntax.write_data_section(".dora.string_slots", "__dora_strslots", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_string_slots_start");
    syntax.write_label("dora_aot_string_slots_start");
    for slot in string_slots {
        syntax.write_quad_symbol(&slot.slot_label);
        syntax.write_long(slot.string_id.index());
        syntax.write_long(0);
    }
    syntax.write_global("dora_aot_string_slots_end");
    syntax.write_label("dora_aot_string_slots_end");
}

fn write_shape_metadata(
    syntax: &mut AssemblySyntax,
    shapes: &[AotShape],
    known_shapes: &[AotKnownShape],
) {
    use SectionKind::ReadOnly;

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

    syntax.write_newline();
    syntax.write_data_section(".dora.shapes", "__dora_shapes", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_shape_base");
    syntax.write_label("dora_aot_shape_base");
    syntax.write_global("dora_aot_shapes_start");
    syntax.write_label("dora_aot_shapes_start");
    // Descriptor field order is the runtime Shape ABI. Keep it in sync with
    // dora_runtime::shape::Shape and dora_compiler::abi::ShapeLayout.
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
        syntax.write_align8();
        syntax.write_local_symbol(&shape_descriptor_label(AotShapeId(shape.id)));
        syntax.write_quad(shape_visitor_value(shape.visitor));
        syntax.write_quad_symbol_offset("dora_aot_shape_refs_start", *refs_start * 4);
        syntax.write_quad(refs_len);
        syntax.write_quad(shape.instance_size);
        syntax.write_quad(shape.element_size);
        syntax.write_quad(vtable_len);
        syntax.write_quad_symbol_offset("dora_aot_shape_kinds_start", *kind_start);
        syntax.write_quad(kind_len);
        syntax.write_quad_symbol_offset("dora_aot_shape_fields_start", *fields_start);
        syntax.write_quad(fields_len);

        for symbol in &vtable_entries[*vtable_start..*vtable_start + *vtable_len] {
            match symbol {
                Some(name) => syntax.write_quad_symbol(name),
                None => syntax.write_quad(0),
            }
        }
    }
    syntax.write_global("dora_aot_shapes_end");
    syntax.write_label("dora_aot_shapes_end");

    syntax.write_newline();
    syntax.write_data_section(".dora.shape_refs", "__dora_shprefs", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_shape_refs_start");
    syntax.write_label("dora_aot_shape_refs_start");
    for value in &refs {
        syntax.write_long(value);
    }
    syntax.write_global("dora_aot_shape_refs_end");
    syntax.write_label("dora_aot_shape_refs_end");

    syntax.write_newline();
    syntax.write_data_section(".dora.shape_kinds", "__dora_shpkinds", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_shape_kinds_start");
    syntax.write_label("dora_aot_shape_kinds_start");
    syntax.write_bytes(&shape_kinds);
    syntax.write_global("dora_aot_shape_kinds_end");
    syntax.write_label("dora_aot_shape_kinds_end");

    syntax.write_newline();
    syntax.write_data_section(".dora.shape_fields", "__dora_shpflds", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_shape_fields_start");
    syntax.write_label("dora_aot_shape_fields_start");
    syntax.write_bytes(&shape_fields);
    syntax.write_global("dora_aot_shape_fields_end");
    syntax.write_label("dora_aot_shape_fields_end");

    syntax.write_newline();
    syntax.write_data_section(".dora.known_shapes", "__dora_knownshp", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_known_shapes_start");
    syntax.write_label("dora_aot_known_shapes_start");
    for known_shape in known_shapes {
        syntax.write_long(known_shape_kind_value(known_shape.kind));
        syntax.write_long(0);
        syntax.write_quad_symbol(&shape_descriptor_label(known_shape.shape_id));
    }
    syntax.write_global("dora_aot_known_shapes_end");
    syntax.write_label("dora_aot_known_shapes_end");
    syntax.write_text();
}

fn write_global_metadata(syntax: &mut AssemblySyntax, aot: &AotCompilation) {
    use SectionKind::ReadOnly;

    syntax.write_newline();
    syntax.write_bss();
    syntax.write_align8();
    syntax.write_global("dora_global_memory");
    syntax.write_label("dora_global_memory");
    let mut current_offset = 0;
    let align_global_memory_to_offset =
        |syntax: &mut AssemblySyntax, current_offset: &mut usize, offset: usize| {
            assert!(offset >= *current_offset);

            if offset > *current_offset {
                syntax.write_zero(offset - *current_offset);
                *current_offset = offset;
            }
        };

    for (global_idx, global) in aot.global_layout.globals.iter().enumerate() {
        align_global_memory_to_offset(syntax, &mut current_offset, global.state_offset);
        syntax.write_local_symbol(&format!(".Ldora_global_{}_state", global_idx));

        align_global_memory_to_offset(syntax, &mut current_offset, global.value_offset);
        syntax.write_local_symbol(&format!(".Ldora_global_{}_value", global_idx));
    }

    align_global_memory_to_offset(syntax, &mut current_offset, aot.global_layout.memory_size);
    syntax.write_global("dora_global_memory_end");
    syntax.write_label("dora_global_memory_end");

    // Emit GC reference offsets for globals.
    syntax.write_newline();
    syntax.write_data_section(".dora.global_refs", "__dora_globrefs", ReadOnly);
    syntax.write_align4();
    syntax.write_global("dora_aot_global_refs_start");
    syntax.write_label("dora_aot_global_refs_start");
    for offset in &aot.global_layout.references {
        syntax.write_long(offset);
    }
    syntax.write_global("dora_aot_global_refs_end");
    syntax.write_label("dora_aot_global_refs_end");
    syntax.write_text();
}

fn write_function_metadata(syntax: &mut AssemblySyntax, functions: &[FunctionMetadataEntry<'_>]) {
    use SectionKind::ReadOnly;

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

    syntax.write_newline();
    syntax.write_data_section(".dora.gcpoint_offsets", "__dora_gcpoffs", ReadOnly);
    syntax.write_align4();
    syntax.write_global("dora_aot_gcpoint_offsets_start");
    syntax.write_label("dora_aot_gcpoint_offsets_start");
    for offset in &gcpoint_offsets {
        syntax.write_long(offset);
    }
    syntax.write_global("dora_aot_gcpoint_offsets_end");
    syntax.write_label("dora_aot_gcpoint_offsets_end");

    syntax.write_newline();
    syntax.write_data_section(".dora.gcpoints", "__dora_gcpoints", ReadOnly);
    syntax.write_align4();
    syntax.write_global("dora_aot_gcpoints_start");
    syntax.write_label("dora_aot_gcpoints_start");
    for (pc_offset, offsets_start, offsets_len) in &gcpoint_entries {
        syntax.write_long(pc_offset);
        syntax.write_long(offsets_start);
        syntax.write_long(offsets_len);
    }
    syntax.write_global("dora_aot_gcpoints_end");
    syntax.write_label("dora_aot_gcpoints_end");

    syntax.write_newline();
    syntax.write_data_section(".dora.locations", "__dora_locs", ReadOnly);
    syntax.write_align4();
    syntax.write_global("dora_aot_locations_start");
    syntax.write_label("dora_aot_locations_start");
    for (pc_offset, inlined_function_id, line, column) in &location_entries {
        syntax.write_long(pc_offset);
        syntax.write_long(inlined_function_id);
        syntax.write_long(line);
        syntax.write_long(column);
    }
    syntax.write_global("dora_aot_locations_end");
    syntax.write_label("dora_aot_locations_end");

    syntax.write_newline();
    syntax.write_data_section(".dora.function_info", "__dora_fctinfo", ReadOnly);
    syntax.write_align4();
    syntax.write_global("dora_aot_function_info_start");
    syntax.write_label("dora_aot_function_info_start");
    for function_info in &function_infos {
        syntax.write_long(function_info.name.index());
        syntax.write_long(function_info.file.index());
        syntax.write_long(function_info.loc.line());
        syntax.write_long(function_info.loc.column());
    }
    syntax.write_global("dora_aot_function_info_end");
    syntax.write_label("dora_aot_function_info_end");

    syntax.write_newline();
    syntax.write_data_section(".dora.inlined_functions", "__dora_inlfcts", ReadOnly);
    syntax.write_align4();
    syntax.write_global("dora_aot_inlined_functions_start");
    syntax.write_label("dora_aot_inlined_functions_start");
    for (function_info_idx, inlined_function_id, line, column) in &inlined_function_entries {
        syntax.write_long(function_info_idx);
        syntax.write_long(inlined_function_id);
        syntax.write_long(line);
        syntax.write_long(column);
    }
    syntax.write_global("dora_aot_inlined_functions_end");
    syntax.write_label("dora_aot_inlined_functions_end");

    syntax.write_newline();
    syntax.write_data_section(".dora.functions", "__dora_fcts", ReadOnly);
    syntax.write_align8();
    syntax.write_global("dora_aot_functions_start");
    syntax.write_label("dora_aot_functions_start");
    for entry in &function_entries {
        syntax.write_quad_symbol(entry.start_label);
        syntax.write_quad_symbol(entry.end_label);
        syntax.write_long(entry.fct_id);
        syntax.write_long(entry.kind);
        syntax.write_long(entry.function_info_idx);
        syntax.write_long(entry.gcpoint_start);
        syntax.write_long(entry.gcpoint_len);
        syntax.write_long(entry.location_start);
        syntax.write_long(entry.location_len);
        syntax.write_long(entry.inlined_function_start);
        syntax.write_long(entry.inlined_function_len);
        syntax.write_long(0); // reserved padding
    }
    syntax.write_global("dora_aot_functions_end");
    syntax.write_label("dora_aot_functions_end");
    syntax.write_text();
}

fn code_kind_value(kind: &AotCodeKind) -> u32 {
    match kind {
        AotCodeKind::Optimized => AOT_CODE_KIND_OPTIMIZED,
        AotCodeKind::RuntimeEntryTrampoline { .. } => AOT_CODE_KIND_RUNTIME_ENTRY_TRAMPOLINE,
        AotCodeKind::DoraEntryTrampoline => AOT_CODE_KIND_DORA_ENTRY_TRAMPOLINE,
        AotCodeKind::AllocationFailureTrampoline => AOT_CODE_KIND_ALLOCATION_FAILURE_TRAMPOLINE,
        AotCodeKind::TrapTrampoline => AOT_CODE_KIND_TRAP_TRAMPOLINE,
        AotCodeKind::SafepointTrampoline => AOT_CODE_KIND_SAFEPOINT_TRAMPOLINE,
        AotCodeKind::UnreachableTrampoline => AOT_CODE_KIND_UNREACHABLE_TRAMPOLINE,
        AotCodeKind::FatalErrorTrampoline => AOT_CODE_KIND_FATAL_ERROR_TRAMPOLINE,
        AotCodeKind::StackOverflowTrampoline => AOT_CODE_KIND_STACK_OVERFLOW_TRAMPOLINE,
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
        ShapeVisitor::Regular => AOT_SHAPE_VISITOR_REGULAR as u8,
        ShapeVisitor::PointerArray => AOT_SHAPE_VISITOR_POINTER_ARRAY as u8,
        ShapeVisitor::RecordArray => AOT_SHAPE_VISITOR_RECORD_ARRAY as u8,
        ShapeVisitor::None => AOT_SHAPE_VISITOR_NONE as u8,
        ShapeVisitor::Invalid => AOT_SHAPE_VISITOR_INVALID as u8,
    }
}
