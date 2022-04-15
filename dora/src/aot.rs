use std::fs::File;
use std::io::{self, Write};

use crate::cannon::{self, CompilationFlags};
use crate::compiler::dora_entry_stub;
use crate::language::sem_analysis::{FctDefinition, FctDefinitionId};
use crate::language::ty::SourceTypeArray;
use crate::vm::SemAnalysis;

pub fn build(sa: &SemAnalysis, main_fct_id: FctDefinitionId) {
    let fct = sa.fcts.idx(main_fct_id);
    let fct = fct.read();

    write(sa, &*fct).expect("write failed");
}

fn write(sa: &SemAnalysis, fct: &FctDefinition) -> io::Result<()> {
    let mut file = File::create("program.s")?;
    writeln!(&mut file, ".text")?;

    writeln!(&mut file, "\t.globl _dora_main")?;
    writeln!(&mut file, "\t.p2align 4")?;
    writeln!(&mut file, "_dora_main:")?;
    let code_descriptor = cannon::compile(
        sa,
        &*fct,
        &SourceTypeArray::empty(),
        CompilationFlags::aot(),
    );
    write_binary_buffer(&mut file, &code_descriptor.code)?;

    writeln!(&mut file, "\t.globl _dora_entry_stub")?;
    writeln!(&mut file, "\t.p2align 4")?;
    writeln!(&mut file, "_dora_entry_stub:")?;
    let code_descriptor = dora_entry_stub::generate(sa);
    write_binary_buffer(&mut file, &code_descriptor.code)?;

    Ok(())
}

fn write_binary_buffer(file: &mut File, buffer: &[u8]) -> io::Result<()> {
    write!(file, ".byte {:x}", buffer.first().expect("empty buffer"))?;

    for &value in buffer.iter().skip(1) {
        write!(file, ", 0x{:x}", value)?;
    }

    writeln!(file)?;

    Ok(())
}
