use std::collections::HashMap;

use crate::{
    AotFunction, AotRelocationTarget, AotShapeId, AotStringId, Arm64LoadWidth, RelocationForm,
};

use super::{AssemblySyntax, ShapeDataEntry, StringSlotEntry, relocation_target_symbol};

pub(super) fn write_function_body(
    syntax: &mut AssemblySyntax,
    func: &AotFunction,
    string_slots: &mut Vec<StringSlotEntry>,
    string_slot_map: &mut HashMap<AotStringId, usize>,
    shape_data_entries: &mut Vec<ShapeDataEntry>,
    shape_data_entry_map: &mut HashMap<AotShapeId, usize>,
) {
    let mut cursor = 0;
    for reloc in &func.relocations {
        let start = reloc.offset as usize;
        assert!(start >= cursor, "overlapping Mach-O relocation patches");

        let end = start + reloc.form.instruction_sequence_len();
        syntax.write_bytes(&func.code[cursor..start]);

        write_relocation(
            syntax,
            &reloc.target,
            reloc.form,
            string_slots,
            string_slot_map,
            shape_data_entries,
            shape_data_entry_map,
        );
        cursor = end;
    }

    syntax.write_bytes(&func.code[cursor..]);
}

fn write_relocation(
    syntax: &mut AssemblySyntax,
    target_kind: &AotRelocationTarget,
    form: RelocationForm,
    string_slots: &mut Vec<StringSlotEntry>,
    string_slot_map: &mut HashMap<AotStringId, usize>,
    shape_data_entries: &mut Vec<ShapeDataEntry>,
    shape_data_entry_map: &mut HashMap<AotShapeId, usize>,
) {
    let target = relocation_target_symbol(
        syntax,
        target_kind,
        string_slots,
        string_slot_map,
        shape_data_entries,
        shape_data_entry_map,
    );

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
