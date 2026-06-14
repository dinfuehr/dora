use crate::{AotRelocationTarget, RelocationForm};

use super::AssemblySyntax;

pub(super) fn write_relocation(
    syntax: &mut AssemblySyntax,
    target_kind: &AotRelocationTarget,
    form: RelocationForm,
    target: &str,
) {
    match (target_kind, form) {
        (AotRelocationTarget::Call(_), RelocationForm::X64CallRel32) => {
            syntax.write_indented_line(format_args!("call {target}"));
        }
        (
            AotRelocationTarget::StringSlot(_),
            RelocationForm::X64RipRelativeLoad64 { dst_reg, .. },
        ) => {
            let dst_reg = x64_reg64(dst_reg);
            syntax.write_indented_line(format_args!("mov {dst_reg}, QWORD PTR [{target}]"));
        }
        (
            AotRelocationTarget::ShapeAddress(_)
            | AotRelocationTarget::ShapeBase
            | AotRelocationTarget::Global(_)
            | AotRelocationTarget::JumpTable { .. },
            RelocationForm::X64RipRelativeLea { dst_reg, .. },
        ) => {
            let dst_reg = x64_reg64(dst_reg);
            syntax.write_indented_line(format_args!("lea {dst_reg}, [{target}]"));
        }
        _ => panic!(
            "unexpected MASM relocation target/form combination {:?}",
            form
        ),
    }
}

fn x64_reg64(reg: u8) -> &'static str {
    match reg {
        0 => "rax",
        1 => "rcx",
        2 => "rdx",
        3 => "rbx",
        4 => "rsp",
        5 => "rbp",
        6 => "rsi",
        7 => "rdi",
        8 => "r8",
        9 => "r9",
        10 => "r10",
        11 => "r11",
        12 => "r12",
        13 => "r13",
        14 => "r14",
        15 => "r15",
        _ => panic!("invalid x64 register {reg}"),
    }
}
