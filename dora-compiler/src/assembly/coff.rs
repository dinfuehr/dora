use crate::{AotRelocationTarget, Arm64LoadWidth, RelocationForm};

use super::AssemblySyntax;

pub(super) fn write_relocation(
    syntax: &mut AssemblySyntax,
    target_kind: &AotRelocationTarget,
    form: RelocationForm,
    target: &str,
) {
    match (target_kind, form) {
        (AotRelocationTarget::Call(_), RelocationForm::Arm64Branch26) => {
            syntax.write_indented_line(format_args!("bl {target}"));
        }
        (AotRelocationTarget::Call(_), RelocationForm::X64CallRel32) => {
            syntax.write_indented_line(format_args!("call {target}"));
        }
        (
            AotRelocationTarget::StringSlot(_),
            RelocationForm::Arm64AdrpLdr {
                page_reg,
                base_reg,
                dst_reg,
                width,
            },
        ) => {
            let page_reg = arm64_x_reg(page_reg);
            let base_reg = arm64_x_reg(base_reg);
            let dst_reg = match width {
                Arm64LoadWidth::U32 => arm64_w_reg(dst_reg),
                Arm64LoadWidth::U64 => arm64_x_reg(dst_reg),
            };
            syntax.write_indented_line(format_args!("adrp {page_reg}, {target}"));
            syntax.write_indented_line(format_args!("ldr {dst_reg}, [{base_reg}, {target}]"));
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
        (
            AotRelocationTarget::StringSlot(_)
            | AotRelocationTarget::ShapeAddress(_)
            | AotRelocationTarget::ShapeBase
            | AotRelocationTarget::Global(_)
            | AotRelocationTarget::JumpTable { .. },
            RelocationForm::Arm64AdrpAdd {
                page_reg,
                base_reg,
                dst_reg,
            },
        ) => {
            let page_reg = arm64_x_reg(page_reg);
            let base_reg = arm64_x_reg(base_reg);
            let dst_reg = arm64_x_reg(dst_reg);
            syntax.write_indented_line(format_args!("adrp {page_reg}, {target}"));
            syntax.write_indented_line(format_args!("add {dst_reg}, {base_reg}, {target}"));
        }
        _ => panic!(
            "unexpected COFF relocation target/form combination {:?}",
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

fn arm64_x_reg(reg: u8) -> String {
    format!("x{reg}")
}

fn arm64_w_reg(reg: u8) -> String {
    format!("w{reg}")
}
