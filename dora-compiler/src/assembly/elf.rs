use crate::{AotRelocationTarget, Arm64LoadWidth, RelocationForm};

use super::AssemblySyntax;

pub(super) fn write_relocation(
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
            AotRelocationTarget::StringSlot(_)
            | AotRelocationTarget::ShapeAddress(_)
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
