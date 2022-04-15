use std::fs::File;
use std::io::{self, Write};

use crate::cannon::{self, CompilationFlags};
use crate::compiler::dora_entry_stub;
use crate::language::sem_analysis::{FctDefinition, FctDefinitionId};
use crate::language::ty::SourceTypeArray;
use crate::masm::CodeDescriptor;
use crate::vm::SemAnalysis;

pub fn build(sa: &SemAnalysis, main_fct_id: FctDefinitionId) {
    let fct = sa.fcts.idx(main_fct_id);
    let fct = fct.read();

    write(sa, &*fct).expect("write failed");
}

fn write(sa: &SemAnalysis, fct: &FctDefinition) -> io::Result<()> {
    let mut file = File::create("program.s")?;
    writeln!(&mut file, "\t.text")?;

    write_main(&mut file)?;

    let code_descriptor = cannon::compile(
        sa,
        &*fct,
        &SourceTypeArray::empty(),
        CompilationFlags::aot(),
    );
    write_fct(&mut file, "_dora_main", code_descriptor)?;

    let code_descriptor = dora_entry_stub::generate(sa);
    write_fct(&mut file, "_dora_entry_stub", code_descriptor)?;

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
