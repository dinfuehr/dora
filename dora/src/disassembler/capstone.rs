use libc;

use std::fs::OpenOptions;
use std::io::{self, BufWriter, Write};
use std::slice;

use capstone::prelude::*;

use crate::compiler::Code;
use crate::driver::cmd::AsmSyntax;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

pub fn supported() -> bool {
    true
}

pub fn disassemble<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    type_params: &TypeList,
    code: &Code,
    fct_src: Option<&FctSrc>,
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

    let name = fct.full_name(vm);

    let type_params = if !type_params.is_empty() {
        let mut ty_names = Vec::new();

        for ty in type_params.iter() {
            ty_names.push(ty.name_fct(vm, fct));
        }

        format!(" [{}]", ty_names.join(", "))
    } else {
        "".into()
    };

    writeln!(
        &mut w,
        "fun {}{} {:#x} {:#x}",
        &name, type_params, start_addr, end_addr
    )
    .unwrap();

    if let Some(fct_src) = fct_src {
        for var in &fct_src.vars {
            let name = vm.interner.str(var.name);
            writeln!(
                &mut w,
                "  var `{}`: type {}",
                name,
                var.ty.name_fct(vm, fct)
            )
            .unwrap();
        }

        if fct_src.vars.len() > 0 {
            writeln!(&mut w).unwrap();
        }
    }

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

        if let Some(comment) = code.comment_for_offset(addr as u32) {
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
