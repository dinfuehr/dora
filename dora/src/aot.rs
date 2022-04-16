use std::fs::File;
use std::io::{self, Write};

use crate::cannon::{self, CompilationFlags};
use crate::compiler::dora_entry_stub;
use crate::language::sem_analysis::{FctDefinition, FctDefinitionId};
use crate::language::ty::SourceTypeArray;
use crate::masm::CodeDescriptor;
use crate::vm::SemAnalysis;

pub fn build(sa: &SemAnalysis, main_fct_id: FctDefinitionId) {
    let reachable_fcts = discover_reachable_functions(main_fct_id);

    write_file(sa, &reachable_fcts).expect("write failed");
}

fn write_file(
    sa: &SemAnalysis,
    reachable_fcts: &[(FctDefinitionId, SourceTypeArray)],
) -> io::Result<()> {
    let mut file = File::create("program.s")?;
    writeln!(&mut file, "\t.text")?;

    write_builtin_functions(sa, &mut file)?;
    write_reachable_functions(sa, &mut file, reachable_fcts)?;

    Ok(())
}

fn write_builtin_functions(sa: &SemAnalysis, file: &mut File) -> io::Result<()> {
    write_main(file)?;

    let code_descriptor = dora_entry_stub::generate(sa);
    write_fct(file, "_dora_entry_stub", code_descriptor)?;

    Ok(())
}

fn write_main(file: &mut File) -> io::Result<()> {
    writeln!(file, "\t.globl _main")?;
    writeln!(file, "\t.p2align 2")?;
    writeln!(file, "_main:")?;
    writeln!(file, "\t.cfi_startproc")?;
    writeln!(file, "\tstp x29, x30, [sp, #-16]!")?;
    writeln!(file, "\tmov x29, sp")?;
    writeln!(file, "\tmov x29, sp")?;
    writeln!(file, "\t.cfi_def_cfa w29, 16")?;
    writeln!(file, "\t.cfi_offset w30, -8")?;
    writeln!(file, "\t.cfi_offset w29, -16")?;
    writeln!(file, "\tbl _run_aot_program")?;
    writeln!(file, "\tmov w0, #0")?;
    writeln!(file, "\tldp	x29, x30, [sp], #16")?;
    writeln!(file, "\tret")?;
    writeln!(file, "\t.cfi_endproc")?;

    Ok(())
}

fn write_reachable_functions(
    sa: &SemAnalysis,
    file: &mut File,
    reachable_fcts: &[(FctDefinitionId, SourceTypeArray)],
) -> io::Result<()> {
    for (fct_id, type_params) in reachable_fcts {
        let fct = sa.fcts.idx(*fct_id);
        let fct = fct.read();
        let symbol = fct_symbol(sa, &*fct);

        let code_descriptor = cannon::compile(sa, &*fct, type_params, CompilationFlags::aot());
        write_fct(file, &symbol, code_descriptor)?;
    }

    Ok(())
}

fn fct_symbol(sa: &SemAnalysis, fct: &FctDefinition) -> String {
    let name = sa.interner.str(fct.name);
    format!("_dora_uf_{}", name)
}

fn write_fct(file: &mut File, name: &str, code_descriptor: CodeDescriptor) -> io::Result<()> {
    writeln!(file, "\t.globl {}", name)?;
    writeln!(file, "\t.p2align 4")?;
    writeln!(file, "{}:", name)?;
    writeln!(file, "\t.cfi_startproc")?;
    write_binary_buffer(file, &code_descriptor.code)?;
    writeln!(file, "\t.cfi_endproc")?;

    Ok(())
}

fn write_binary_buffer(file: &mut File, buffer: &[u8]) -> io::Result<()> {
    write!(
        file,
        "\t.byte 0x{:x}",
        buffer.first().expect("empty buffer")
    )?;

    for &value in buffer.iter().skip(1) {
        write!(file, ", 0x{:x}", value)?;
    }

    writeln!(file)?;

    Ok(())
}

fn discover_reachable_functions(
    main_fct_id: FctDefinitionId,
) -> Vec<(FctDefinitionId, SourceTypeArray)> {
    vec![(main_fct_id, SourceTypeArray::empty())]
}
