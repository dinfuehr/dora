use std::fs::OpenOptions;
use std::io::{self, BufWriter, Write};
use std::slice;

use capstone::prelude::*;

use crate::vm::{Code, VM};
use dora_bytecode::{BytecodeTypeArray, FunctionId, display_fct, display_ty};

pub fn supported() -> bool {
    true
}

pub fn disassemble(vm: &VM, fct_id: FunctionId, type_params: &BytecodeTypeArray, code: &Code) {
    let instruction_length = code.instruction_end().offset_from(code.instruction_start());
    let buf: &[u8] =
        unsafe { slice::from_raw_parts(code.instruction_start().to_ptr(), instruction_length) };

    let engine = get_engine().expect("cannot create capstone engine");

    let mut w: Box<dyn Write> = if let Some(ref file) = vm.flags.emit_asm_file {
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(file)
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

    let name = display_fct(&vm.program, fct_id);

    let type_params = if !type_params.is_empty() {
        let mut ty_names = Vec::new();

        for ty in type_params.iter() {
            ty_names.push(display_ty(&vm.program, &ty));
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
        let offset = (instr.address() - start_addr) as u32;

        if let Some(gc_point) = code.gcpoint_for_offset(offset) {
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

        let prefix = format!("  {:#06x}: ", instr.address());
        let alignment = prefix.len();

        for comment in code.comments_for_offset(offset as u32) {
            for _ in 0..alignment {
                write!(&mut w, " ").expect("write() failed");
            }
            writeln!(&mut w, "// {}", comment).expect("write() failed");
        }

        writeln!(
            &mut w,
            "{}{}\t\t{}",
            prefix,
            instr.mnemonic().expect("no mnmemonic found"),
            instr.op_str().expect("no op_str found"),
        )
        .unwrap();
    }

    writeln!(&mut w).unwrap();
}

#[cfg(target_arch = "x86_64")]
fn get_engine() -> CsResult<Capstone> {
    Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Intel)
        .build()
}

#[cfg(target_arch = "aarch64")]
fn get_engine() -> CsResult<Capstone> {
    Capstone::new()
        .arm64()
        .mode(arch::arm64::ArchMode::Arm)
        .build()
}
