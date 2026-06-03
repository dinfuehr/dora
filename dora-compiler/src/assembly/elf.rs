use std::collections::HashMap;

use crate::{
    AotFunction, AotRelocationTarget, AotShapeId, AotStringId, Arm64LoadWidth, RelocationForm,
};

use super::{AssemblySyntax, ShapeSlotEntry, StringSlotEntry, relocation_target_symbol};

pub(super) fn write_function_body(
    syntax: &mut AssemblySyntax,
    func: &AotFunction,
    string_slots: &mut Vec<StringSlotEntry>,
    string_slot_map: &mut HashMap<AotStringId, usize>,
    shape_slots: &mut Vec<ShapeSlotEntry>,
    shape_slot_map: &mut HashMap<AotShapeId, usize>,
) {
    let label = func.symbol_name.as_str();

    syntax.write_bytes(&func.code);

    for reloc in &func.relocations {
        let target = relocation_target_symbol(
            syntax,
            &reloc.target,
            string_slots,
            string_slot_map,
            shape_slots,
            shape_slot_map,
        );
        write_relocation(
            syntax,
            label,
            reloc.offset,
            &reloc.target,
            reloc.form,
            &target,
        );
    }
}

fn write_relocation(
    syntax: &mut AssemblySyntax,
    label: &str,
    offset: u32,
    target_kind: &AotRelocationTarget,
    form: RelocationForm,
    target: &str,
) {
    match (target_kind, form) {
        (AotRelocationTarget::Call(_), RelocationForm::Arm64Branch26) => {
            syntax.write_indented_line(format_args!(
                ".reloc {label}+{offset}, R_AARCH64_CALL26, {target}"
            ));
        }
        (AotRelocationTarget::Call(_), RelocationForm::X64CallRel32) => {
            syntax.write_indented_line(format_args!(
                ".reloc {}+{}, R_X86_64_PC32, {} - 4",
                label,
                offset + 1,
                target,
            ));
        }
        (
            AotRelocationTarget::StringSlot(_),
            RelocationForm::Arm64AdrpLdr {
                width: Arm64LoadWidth::U64,
                ..
            },
        ) => write_arm64_adrp_ldr_relocation(
            syntax,
            label,
            offset,
            target,
            "R_AARCH64_LDST64_ABS_LO12_NC",
        ),
        (
            AotRelocationTarget::ShapeSlot(_),
            RelocationForm::Arm64AdrpLdr {
                width: Arm64LoadWidth::U32,
                ..
            },
        ) => write_arm64_adrp_ldr_relocation(
            syntax,
            label,
            offset,
            target,
            "R_AARCH64_LDST32_ABS_LO12_NC",
        ),
        (
            AotRelocationTarget::StringSlot(_)
            | AotRelocationTarget::ShapeSlot(_)
            | AotRelocationTarget::Global(_),
            RelocationForm::X64RipRelative32 { disp_offset },
        ) => {
            syntax.write_indented_line(format_args!(
                ".reloc {}+{}, R_X86_64_PC32, {} - 4",
                label,
                offset + u32::from(disp_offset),
                target,
            ));
        }
        (AotRelocationTarget::Global(_), RelocationForm::Arm64AdrpAdd { .. }) => {
            syntax.write_indented_line(format_args!(
                ".reloc {label}+{offset}, R_AARCH64_ADR_PREL_PG_HI21, {target}"
            ));
            syntax.write_indented_line(format_args!(
                ".reloc {label}+{offset}+4, R_AARCH64_ADD_ABS_LO12_NC, {target}"
            ));
        }
        _ => panic!("unexpected relocation target/form combination {:?}", form),
    }
}

fn write_arm64_adrp_ldr_relocation(
    syntax: &mut AssemblySyntax,
    label: &str,
    offset: u32,
    target: &str,
    lo12_relocation: &str,
) {
    syntax.write_indented_line(format_args!(
        ".reloc {label}+{offset}, R_AARCH64_ADR_PREL_PG_HI21, {target}"
    ));
    syntax.write_indented_line(format_args!(
        ".reloc {label}+{offset}+4, {lo12_relocation}, {target}"
    ));
}
