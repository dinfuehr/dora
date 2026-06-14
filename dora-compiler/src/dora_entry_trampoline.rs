use crate::{
    CODE_ALIGNMENT, CodeDescriptor, CommentTable, GcPointTable, LocationTable, RelocationTable,
    TargetArch, align_i32, ptr_width,
};

pub fn generate() -> CodeDescriptor {
    generate_code()
}

pub fn generate_aot(target_arch: TargetArch) -> CodeDescriptor {
    match target_arch {
        TargetArch::X64 => x64::generate(),
        TargetArch::Arm64 => arm64::generate(),
    }
}

#[cfg(target_arch = "x86_64")]
fn generate_code() -> CodeDescriptor {
    x64::generate()
}

#[cfg(target_arch = "aarch64")]
fn generate_code() -> CodeDescriptor {
    arm64::generate()
}

const FP_CALLER_FP_OFFSET: i32 = 0;

fn code_descriptor(code: Vec<u8>) -> CodeDescriptor {
    CodeDescriptor {
        code,
        jump_tables: Vec::new(),
        gcpoints: GcPointTable::new(),
        comments: CommentTable::new(),
        positions: LocationTable::new(),
        relocations: RelocationTable::new(),
        inlined_functions: Vec::new(),
    }
}

mod x64 {
    use super::*;
    use crate::cpu::x64 as cpu;
    use dora_asm::x64::{Address as AsmAddress, AssemblerX64 as Assembler, Immediate, XmmRegister};

    const FRAME_CALLEE_REGS_SIZE: i32 = (cpu::CALLEE_SAVED_REGS.len() as i32) * ptr_width();
    const FP_CALLEE_REGS_OFFSET: i32 = FP_CALLER_FP_OFFSET - FRAME_CALLEE_REGS_SIZE;
    const FRAME_CALLEE_FREGS_SIZE: i32 = (cpu::CALLEE_SAVED_FREGS.len() as i32) * ptr_width();
    const FP_CALLEE_FREGS_OFFSET: i32 = FP_CALLEE_REGS_OFFSET - FRAME_CALLEE_FREGS_SIZE;
    const UNALIGNED_FRAME_SIZE: i32 = FP_CALLER_FP_OFFSET - FP_CALLEE_FREGS_OFFSET;
    const FRAME_SIZE: i32 = align_i32(UNALIGNED_FRAME_SIZE, cpu::STACK_FRAME_ALIGNMENT as i32);

    pub(super) fn generate() -> CodeDescriptor {
        let has_avx2 = cpu::has_avx2();
        let mut asm = Assembler::new(has_avx2);

        asm.pushq_r(cpu::REG_FP.into());
        asm.movq_rr(cpu::REG_FP.into(), cpu::REG_SP.into());
        debug_assert!(FRAME_SIZE as usize % cpu::STACK_FRAME_ALIGNMENT == 0);

        if FRAME_SIZE > 0 {
            asm.subq_ri(cpu::REG_SP.into(), Immediate(FRAME_SIZE as i64));
        }

        save_callee_saved_regs(&mut asm, has_avx2);

        asm.movq_rr(cpu::REG_THREAD.into(), cpu::CCALL_REG_PARAMS[0].into());
        asm.movq_rr(cpu::REG_TMP1.into(), cpu::CCALL_REG_PARAMS[1].into());
        asm.movq_rr(cpu::REG_PARAMS[0].into(), cpu::CCALL_REG_PARAMS[2].into());
        asm.call_r(cpu::REG_TMP1.into());

        load_callee_saved_regs(&mut asm, has_avx2);
        asm.movq_rr(cpu::REG_SP.into(), cpu::REG_FP.into());
        asm.popq_r(cpu::REG_FP.into());
        asm.retq();

        code_descriptor(asm.finalize(CODE_ALIGNMENT).code())
    }

    fn save_callee_saved_regs(asm: &mut Assembler, has_avx2: bool) {
        for (idx, &reg) in cpu::CALLEE_SAVED_REGS.iter().enumerate() {
            let offset = FP_CALLEE_REGS_OFFSET + (idx as i32) * ptr_width();
            asm.movq_ar(AsmAddress::offset(cpu::REG_FP.into(), offset), reg.into());
        }

        for (idx, &reg) in cpu::CALLEE_SAVED_FREGS.iter().enumerate() {
            let offset = FP_CALLEE_FREGS_OFFSET + (idx as i32) * ptr_width();
            let addr = AsmAddress::offset(cpu::REG_FP.into(), offset);
            let reg = XmmRegister::new(reg.0);
            if has_avx2 {
                asm.vmovsd_ar(addr, reg);
            } else {
                asm.movsd_ar(addr, reg);
            }
        }
    }

    fn load_callee_saved_regs(asm: &mut Assembler, has_avx2: bool) {
        for (idx, &reg) in cpu::CALLEE_SAVED_REGS.iter().enumerate() {
            let offset = FP_CALLEE_REGS_OFFSET + (idx as i32) * ptr_width();
            asm.movq_ra(reg.into(), AsmAddress::offset(cpu::REG_FP.into(), offset));
        }

        for (idx, &reg) in cpu::CALLEE_SAVED_FREGS.iter().enumerate() {
            let offset = FP_CALLEE_FREGS_OFFSET + (idx as i32) * ptr_width();
            let addr = AsmAddress::offset(cpu::REG_FP.into(), offset);
            let reg = XmmRegister::new(reg.0);
            if has_avx2 {
                asm.vmovsd_ra(reg, addr);
            } else {
                asm.movsd_ra(reg, addr);
            }
        }
    }
}

mod arm64 {
    use super::*;
    use crate::cpu::arm64 as cpu;
    use dora_asm::arm64::{self as asm, AssemblerArm64 as Assembler, NeonRegister};

    const FRAME_CALLEE_REGS_SIZE: i32 = (cpu::CALLEE_SAVED_REGS.len() as i32) * ptr_width();
    const FP_CALLEE_REGS_OFFSET: i32 = FP_CALLER_FP_OFFSET - FRAME_CALLEE_REGS_SIZE;
    const FRAME_CALLEE_FREGS_SIZE: i32 = (cpu::CALLEE_SAVED_FREGS.len() as i32) * ptr_width();
    const FP_CALLEE_FREGS_OFFSET: i32 = FP_CALLEE_REGS_OFFSET - FRAME_CALLEE_FREGS_SIZE;
    const UNALIGNED_FRAME_SIZE: i32 = FP_CALLER_FP_OFFSET - FP_CALLEE_FREGS_OFFSET;
    const FRAME_SIZE: i32 = align_i32(UNALIGNED_FRAME_SIZE, cpu::STACK_FRAME_ALIGNMENT as i32);

    pub(super) fn generate() -> CodeDescriptor {
        let mut asm = Assembler::new();

        asm.stp_pre(
            cpu::REG_FP.into(),
            cpu::REG_LR.into(),
            cpu::REG_SP.into(),
            -2,
        );
        asm.add(cpu::REG_FP.into(), cpu::REG_SP.into(), cpu::REG_ZERO.into());

        if FRAME_SIZE > 0 {
            debug_assert!(asm::fits_addsub_imm(FRAME_SIZE as u32));
            asm.sub_imm(cpu::REG_SP.into(), cpu::REG_SP.into(), FRAME_SIZE as u32);
        }

        save_callee_saved_regs(&mut asm);

        asm.mov(cpu::REG_THREAD.into(), cpu::CCALL_REG_PARAMS[0].into());
        asm.mov(cpu::REG_TMP1.into(), cpu::CCALL_REG_PARAMS[1].into());
        asm.mov(cpu::REG_PARAMS[0].into(), cpu::CCALL_REG_PARAMS[2].into());
        asm.bl_r(cpu::REG_TMP1.into());

        load_callee_saved_regs(&mut asm);
        asm.add(cpu::REG_SP.into(), cpu::REG_FP.into(), cpu::REG_ZERO.into());
        asm.ldp_post(
            cpu::REG_FP.into(),
            cpu::REG_LR.into(),
            cpu::REG_SP.into(),
            2,
        );
        asm.ret(cpu::REG_LR.into());

        code_descriptor(asm.finalize(CODE_ALIGNMENT).code())
    }

    fn save_callee_saved_regs(asm: &mut Assembler) {
        for (idx, &reg) in cpu::CALLEE_SAVED_REGS.iter().enumerate() {
            let offset = FP_CALLEE_REGS_OFFSET + (idx as i32) * ptr_width();
            debug_assert!(asm::fits_ldst_unscaled(offset));
            asm.stur(reg.into(), cpu::REG_FP.into(), offset);
        }

        for (idx, &reg) in cpu::CALLEE_SAVED_FREGS.iter().enumerate() {
            let offset = FP_CALLEE_FREGS_OFFSET + (idx as i32) * ptr_width();
            debug_assert!(asm::fits_ldst_unscaled(offset));
            asm.stur_d(NeonRegister::new(reg.0), cpu::REG_FP.into(), offset);
        }
    }

    fn load_callee_saved_regs(asm: &mut Assembler) {
        for (idx, &reg) in cpu::CALLEE_SAVED_REGS.iter().enumerate() {
            let offset = FP_CALLEE_REGS_OFFSET + (idx as i32) * ptr_width();
            debug_assert!(asm::fits_ldst_unscaled(offset));
            asm.ldur(reg.into(), cpu::REG_FP.into(), offset);
        }

        for (idx, &reg) in cpu::CALLEE_SAVED_FREGS.iter().enumerate() {
            let offset = FP_CALLEE_FREGS_OFFSET + (idx as i32) * ptr_width();
            debug_assert!(asm::fits_ldst_unscaled(offset));
            asm.ldur_d(NeonRegister::new(reg.0), cpu::REG_FP.into(), offset);
        }
    }
}
