use std::collections::{BTreeMap, HashMap};

use crate::{AotFunction, AotRelocationTarget, AotStringId, Arm64LoadWidth, RelocationForm};

use super::{
    AssemblySyntax, StringSlotEntry, relocation_target_symbol, write_bytes_with_code_offset_labels,
    write_code_offset_label_at_cursor,
};

pub(super) fn write_function_body(
    syntax: &mut AssemblySyntax,
    func: &AotFunction,
    code_offset_labels: &BTreeMap<u32, String>,
    string_slots: &mut Vec<StringSlotEntry>,
    string_slot_map: &mut HashMap<AotStringId, usize>,
) {
    let mut cursor = 0;
    for reloc in &func.relocations {
        let start = reloc.offset as usize;
        assert!(start >= cursor, "overlapping Mach-O relocation patches");

        if matches!(
            (&reloc.target, reloc.form),
            (
                AotRelocationTarget::CodeOffset { .. },
                RelocationForm::AbsoluteAddress
            )
        ) {
            write_bytes_with_code_offset_labels(
                syntax,
                &func.code,
                &mut cursor,
                start,
                code_offset_labels,
            );
            write_code_offset_label_at_cursor(syntax, cursor, code_offset_labels);

            let target =
                relocation_target_symbol(syntax, &reloc.target, string_slots, string_slot_map);
            syntax.write_quad(target);
            cursor = start + 8;
            continue;
        }

        let end = start + reloc.form.instruction_sequence_len();
        write_bytes_with_code_offset_labels(
            syntax,
            &func.code,
            &mut cursor,
            start,
            code_offset_labels,
        );
        write_code_offset_label_at_cursor(syntax, cursor, code_offset_labels);

        write_relocation(
            syntax,
            &reloc.target,
            reloc.form,
            string_slots,
            string_slot_map,
        );
        cursor = end;
    }

    write_bytes_with_code_offset_labels(
        syntax,
        &func.code,
        &mut cursor,
        func.code.len(),
        code_offset_labels,
    );
}

fn write_relocation(
    syntax: &mut AssemblySyntax,
    target_kind: &AotRelocationTarget,
    form: RelocationForm,
    string_slots: &mut Vec<StringSlotEntry>,
    string_slot_map: &mut HashMap<AotStringId, usize>,
) {
    let target = relocation_target_symbol(syntax, target_kind, string_slots, string_slot_map);

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
