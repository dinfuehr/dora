use std::collections::HashMap;

use crate::{AotFunction, AotRelocationTarget, AotShapeId, AotStringId, RelocationForm};

use super::{AssemblySyntax, ShapeSlotEntry, StringSlotEntry, relocation_target_symbol};

pub(super) fn write_function_body(
    syntax: &mut AssemblySyntax,
    func: &AotFunction,
    string_slots: &mut Vec<StringSlotEntry>,
    string_slot_map: &mut HashMap<AotStringId, usize>,
    shape_slots: &mut Vec<ShapeSlotEntry>,
    shape_slot_map: &mut HashMap<AotShapeId, usize>,
) {
    let mut cursor = 0;

    for reloc in &func.relocations {
        let start = reloc.offset as usize;
        assert!(start >= cursor, "overlapping MASM relocation patches");

        let end = start + reloc.form.instruction_sequence_len();
        assert!(
            end <= func.code.len(),
            "MASM relocation exceeds function body"
        );

        syntax.write_bytes(&func.code[cursor..start]);
        write_relocation(
            syntax,
            &reloc.target,
            reloc.form,
            string_slots,
            string_slot_map,
            shape_slots,
            shape_slot_map,
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
    shape_slots: &mut Vec<ShapeSlotEntry>,
    shape_slot_map: &mut HashMap<AotShapeId, usize>,
) {
    let target = relocation_target_symbol(
        syntax,
        target_kind,
        string_slots,
        string_slot_map,
        shape_slots,
        shape_slot_map,
    );

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
            AotRelocationTarget::ShapeSlot(_),
            RelocationForm::X64RipRelativeLoad32 { dst_reg, .. },
        ) => {
            let dst_reg = x64_reg32(dst_reg);
            syntax.write_indented_line(format_args!("mov {dst_reg}, DWORD PTR [{target}]"));
        }
        (AotRelocationTarget::Global(_), RelocationForm::X64RipRelativeLea { dst_reg, .. }) => {
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

fn x64_reg32(reg: u8) -> &'static str {
    match reg {
        0 => "eax",
        1 => "ecx",
        2 => "edx",
        3 => "ebx",
        4 => "esp",
        5 => "ebp",
        6 => "esi",
        7 => "edi",
        8 => "r8d",
        9 => "r9d",
        10 => "r10d",
        11 => "r11d",
        12 => "r12d",
        13 => "r13d",
        14 => "r14d",
        15 => "r15d",
        _ => panic!("invalid x64 register {reg}"),
    }
}
