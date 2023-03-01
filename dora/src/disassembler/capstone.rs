use libc;

use std::fs::OpenOptions;
use std::io::{self, BufWriter, Write};
use std::slice;

use capstone::prelude::*;

use crate::bytecode::BytecodeTypeArray;
use crate::driver::cmd::AsmSyntax;
use crate::language::sem_analysis::FctDefinition;
use crate::vm::{display_fct, display_ty, Code, VM};

pub fn supported() -> bool {
    true
}

pub fn disassemble(
    vm: &VM,
    fct: &FctDefinition,
    type_params: &BytecodeTypeArray,
    code: &Code,
    asm_syntax: AsmSyntax,
) {
    let instruction_length = code.instruction_end().offset_from(code.instruction_start());
    let buf: &[u8] =
        unsafe { slice::from_raw_parts(code.instruction_start().to_ptr(), instruction_length) };

    let engine = get_engine(asm_syntax).expect("cannot create capstone engine");

    let mut w: Box<dyn Write> = if vm.args.flag_emit_asm_file {
        let pid = unsafe { libc::getpid() };
        let name = format!("code-{}.asm", pid);
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&name)
            .expect("couldn't append to asm file");

        Box::new(BufWriter::new(file))
    } else {
        Box::new(io::stdout())
    };

    let start_addr = code.instruction_start().to_usize() as u64;
    let end_addr = code.instruction_end().to_usize() as u64;

    let instrs = engine
        .disasm_all(buf, start_addr)
        .expect("could not disassemble code");

    let name = display_fct(vm, fct.id());

    let type_params = if !type_params.is_empty() {
        let mut ty_names = Vec::new();

        for ty in type_params.iter() {
            ty_names.push(display_ty(vm, &ty));
        }

        format!(" [{}]", ty_names.join(", "))
    } else {
        "".into()
    };

    writeln!(
        &mut w,
        "fn {}{} {:#x} {:#x}",
        &name, type_params, start_addr, end_addr
    )
    .unwrap();

    for instr in instrs.iter() {
        let addr = (instr.address() - start_addr) as u32;

        if let Some(gc_point) = code.gcpoint_for_offset(addr) {
            write!(&mut w, "\t\t  ; gc point = (").unwrap();
            let mut first = true;

            for &offset in &gc_point.offsets {
                if !first {
                    write!(&mut w, ", ").unwrap();
                }

                if offset < 0 {
                    write!(&mut w, "-").unwrap();
                }

                write!(&mut w, "0x{:x}", offset.abs()).unwrap();
                first = false;
            }

            writeln!(&mut w, ")").unwrap();
        }

        for comment in code.comments_for_offset(addr as u32) {
            writeln!(&mut w, "\t\t  // {}", comment).unwrap();
        }

        writeln!(
            &mut w,
            "  {:#06x}: {}\t\t{}",
            instr.address(),
            instr.mnemonic().expect("no mnmemonic found"),
            instr.op_str().expect("no op_str found"),
        )
        .unwrap();
    }

    writeln!(&mut w).unwrap();
}

#[cfg(target_arch = "x86_64")]
fn get_engine(asm_syntax: AsmSyntax) -> CsResult<Capstone> {
    let arch_syntax = match asm_syntax {
        AsmSyntax::Intel => arch::x86::ArchSyntax::Intel,
        AsmSyntax::Att => arch::x86::ArchSyntax::Att,
    };

    Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch_syntax)
        .build()
}

#[cfg(target_arch = "aarch64")]
fn get_engine(_asm_syntax: AsmSyntax) -> CsResult<Capstone> {
    Capstone::new()
        .arm64()
        .mode(arch::arm64::ArchMode::Arm)
        .build()
}
