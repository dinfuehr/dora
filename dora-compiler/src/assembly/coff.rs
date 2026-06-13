use std::collections::{BTreeMap, HashMap};

use crate::{AotFunction, AotRelocationTarget, AotStringId, RelocationForm};

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
        assert!(start >= cursor, "overlapping MASM relocation patches");

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
        assert!(
            end <= func.code.len(),
            "MASM relocation exceeds function body"
        );

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
            | AotRelocationTarget::Global(_),
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
