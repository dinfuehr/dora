use std::collections::BTreeMap;
use std::collections::HashMap;

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
    let label = func.symbol_name.as_str();

    let mut cursor = 0;

    for reloc in &func.relocations {
        if matches!(
            (&reloc.target, reloc.form),
            (
                AotRelocationTarget::CodeOffset { .. },
                RelocationForm::AbsoluteAddress
            )
        ) {
            let start = reloc.offset as usize;
            assert!(start >= cursor, "overlapping ELF relocation patches");

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
        }
    }

    write_bytes_with_code_offset_labels(
        syntax,
        &func.code,
        &mut cursor,
        func.code.len(),
        code_offset_labels,
    );

    for reloc in &func.relocations {
        if matches!(
            (&reloc.target, reloc.form),
            (
                AotRelocationTarget::CodeOffset { .. },
                RelocationForm::AbsoluteAddress
            )
        ) {
            continue;
        }

        let target = relocation_target_symbol(syntax, &reloc.target, string_slots, string_slot_map);
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
            AotRelocationTarget::StringSlot(_)
            | AotRelocationTarget::Global(_)
            | AotRelocationTarget::JumpTable { .. },
            RelocationForm::X64RipRelativeLoad64 { disp_offset, .. }
            | RelocationForm::X64RipRelativeLoad32 { disp_offset, .. }
            | RelocationForm::X64RipRelativeLea { disp_offset, .. },
        ) => {
            syntax.write_indented_line(format_args!(
                ".reloc {}+{}, R_X86_64_PC32, {} - 4",
                label,
                offset + u32::from(disp_offset),
                target,
            ));
        }
        (
            AotRelocationTarget::ShapeAddress(_) | AotRelocationTarget::ShapeBase,
            RelocationForm::X64RipRelativeLea { disp_offset, .. },
        ) => {
            syntax.write_indented_line(format_args!(
                ".reloc {}+{}, R_X86_64_PC32, {} - 4",
                label,
                offset + u32::from(disp_offset),
                target,
            ));
        }
        (
            AotRelocationTarget::ShapeAddress(_)
            | AotRelocationTarget::ShapeBase
            | AotRelocationTarget::Global(_)
            | AotRelocationTarget::JumpTable { .. },
            RelocationForm::Arm64AdrpAdd { .. },
        ) => {
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
