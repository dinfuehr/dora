use crate::{Arm64LoadWidth, RelocationForm};

use super::AssemblySyntax;

pub(super) fn write_relocation(syntax: &mut AssemblySyntax, form: RelocationForm, target: &str) {
    match form {
        RelocationForm::Arm64Branch26 => {
            syntax.write_indented_line(format_args!("bl {target}"));
        }
        RelocationForm::Arm64AdrpLdr {
            page_reg,
            base_reg,
            dst_reg,
            width,
        } => {
            let page_reg = arm64_x_reg(page_reg);
            let base_reg = arm64_x_reg(base_reg);
            let dst_reg = match width {
                Arm64LoadWidth::U32 => arm64_w_reg(dst_reg),
                Arm64LoadWidth::U64 => arm64_x_reg(dst_reg),
            };
            syntax.write_indented_line(format_args!("adrp {page_reg}, {target}@PAGE"));
            syntax.write_indented_line(format_args!(
                "ldr {dst_reg}, [{base_reg}, {target}@PAGEOFF]"
            ));
        }
        RelocationForm::Arm64AdrpAdd {
            page_reg,
            base_reg,
            dst_reg,
        } => {
            let page_reg = arm64_x_reg(page_reg);
            let base_reg = arm64_x_reg(base_reg);
            let dst_reg = arm64_x_reg(dst_reg);
            syntax.write_indented_line(format_args!("adrp {page_reg}, {target}@PAGE"));
            syntax.write_indented_line(format_args!("add {dst_reg}, {base_reg}, {target}@PAGEOFF"));
        }
        _ => panic!("unexpected Mach-O relocation form {:?}", form),
    }
}

fn arm64_x_reg(reg: u8) -> String {
    format!("x{reg}")
}

fn arm64_w_reg(reg: u8) -> String {
    format!("w{reg}")
}
