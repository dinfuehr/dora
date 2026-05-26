use std::sync::Arc;

use crate::cpu::{
    CALLEE_SAVED_FREGS, CALLEE_SAVED_REGS, CCALL_REG_PARAMS, REG_FP, REG_PARAMS, REG_SP,
    REG_THREAD, REG_TMP1, STACK_FRAME_ALIGNMENT,
};
use crate::mem;
use crate::vm::{
    CODE_ALIGNMENT, Code, CodeDescriptor, CodeKind, CommentTable, GcPointTable,
    LazyCompilationData, LocationTable, RelocationTable, VM, install_code_stub,
};

pub fn install<'a>(vm: &'a VM) -> Arc<Code> {
    let code_descriptor = generate(vm);
    install_code_stub(vm, code_descriptor, CodeKind::DoraEntryTrampoline)
}

pub fn generate<'a>(vm: &'a VM) -> CodeDescriptor {
    generate_code(vm.flags.emit_debug_entry)
}

pub fn generate_aot() -> CodeDescriptor {
    generate_code(false)
}

#[cfg(target_arch = "x86_64")]
fn generate_code(dbg: bool) -> CodeDescriptor {
    x64::generate(dbg)
}

#[cfg(target_arch = "aarch64")]
fn generate_code(dbg: bool) -> CodeDescriptor {
    arm64::generate(dbg)
}

const FP_CALLER_PC_OFFSET: i32 = FP_CALLER_FP_OFFSET + mem::ptr_width();
const FP_CALLER_FP_OFFSET: i32 = 0;

const FRAME_CALLEE_REGS_SIZE: i32 = (CALLEE_SAVED_REGS.len() as i32) * mem::ptr_width();
const FP_CALLEE_REGS_OFFSET: i32 = FP_CALLER_FP_OFFSET - FRAME_CALLEE_REGS_SIZE;
const FRAME_CALLEE_FREGS_SIZE: i32 = (CALLEE_SAVED_FREGS.len() as i32) * mem::ptr_width();
const FP_CALLEE_FREGS_OFFSET: i32 = FP_CALLEE_REGS_OFFSET - FRAME_CALLEE_FREGS_SIZE;

const UNALIGNED_FRAME_SIZE: i32 = FP_CALLER_FP_OFFSET - FP_CALLEE_FREGS_OFFSET;
const FRAME_SIZE: i32 = mem::align_i32(UNALIGNED_FRAME_SIZE, STACK_FRAME_ALIGNMENT as i32);

fn code_descriptor(code: Vec<u8>) -> CodeDescriptor {
    CodeDescriptor {
        code,
        lazy_compilation: LazyCompilationData::new(),
        gcpoints: GcPointTable::new(),
        comments: CommentTable::new(),
        positions: LocationTable::new(),
        relocations: RelocationTable::new(),
        inlined_functions: Vec::new(),
    }
}

#[cfg(target_arch = "x86_64")]
mod x64 {
    use super::*;
    use crate::cpu::has_avx2;
    use dora_asm::x64::{Address as AsmAddress, AssemblerX64 as Assembler, Immediate};

    pub(super) fn generate(dbg: bool) -> CodeDescriptor {
        let has_avx2 = has_avx2();
        let mut asm = Assembler::new(has_avx2);

        if dbg {
            asm.int3();
        }

        asm.pushq_r(REG_FP.into());
        asm.movq_rr(REG_FP.into(), REG_SP.into());
        debug_assert!(FRAME_SIZE as usize % STACK_FRAME_ALIGNMENT == 0);

        if FRAME_SIZE > 0 {
            asm.subq_ri(REG_SP.into(), Immediate(FRAME_SIZE as i64));
        }

        save_callee_saved_regs(&mut asm, has_avx2);

        asm.movq_rr(REG_THREAD.into(), CCALL_REG_PARAMS[0].into());
        asm.movq_rr(REG_TMP1.into(), CCALL_REG_PARAMS[1].into());
        asm.movq_rr(REG_PARAMS[0].into(), CCALL_REG_PARAMS[2].into());
        asm.call_r(REG_TMP1.into());

        load_callee_saved_regs(&mut asm, has_avx2);
        asm.movq_rr(REG_SP.into(), REG_FP.into());
        asm.popq_r(REG_FP.into());
        asm.retq();

        code_descriptor(asm.finalize(CODE_ALIGNMENT).code())
    }

    fn save_callee_saved_regs(asm: &mut Assembler, has_avx2: bool) {
        for (idx, &reg) in CALLEE_SAVED_REGS.iter().enumerate() {
            let offset = FP_CALLEE_REGS_OFFSET + (idx as i32) * mem::ptr_width();
            asm.movq_ar(AsmAddress::offset(REG_FP.into(), offset), reg.into());
        }

        for (idx, &reg) in CALLEE_SAVED_FREGS.iter().enumerate() {
            let offset = FP_CALLEE_FREGS_OFFSET + (idx as i32) * mem::ptr_width();
            let addr = AsmAddress::offset(REG_FP.into(), offset);
            if has_avx2 {
                asm.vmovsd_ar(addr, reg.into());
            } else {
                asm.movsd_ar(addr, reg.into());
            }
        }
    }

    fn load_callee_saved_regs(asm: &mut Assembler, has_avx2: bool) {
        for (idx, &reg) in CALLEE_SAVED_REGS.iter().enumerate() {
            let offset = FP_CALLEE_REGS_OFFSET + (idx as i32) * mem::ptr_width();
            asm.movq_ra(reg.into(), AsmAddress::offset(REG_FP.into(), offset));
        }

        for (idx, &reg) in CALLEE_SAVED_FREGS.iter().enumerate() {
            let offset = FP_CALLEE_FREGS_OFFSET + (idx as i32) * mem::ptr_width();
            let addr = AsmAddress::offset(REG_FP.into(), offset);
            if has_avx2 {
                asm.vmovsd_ra(reg.into(), addr);
            } else {
                asm.movsd_ra(reg.into(), addr);
            }
        }
    }
}

#[cfg(target_arch = "aarch64")]
mod arm64 {
    use super::*;
    use crate::cpu::{REG_LR, REG_ZERO};
    use dora_asm::arm64::{self as asm, AssemblerArm64 as Assembler};

    pub(super) fn generate(dbg: bool) -> CodeDescriptor {
        let mut asm = Assembler::new();

        if dbg {
            asm.brk(0xF000);
        }

        asm.stp_pre(REG_FP.into(), REG_LR.into(), REG_SP.into(), -2);
        asm.add(REG_FP.into(), REG_SP.into(), REG_ZERO.into());

        if FRAME_SIZE > 0 {
            debug_assert!(asm::fits_addsub_imm(FRAME_SIZE as u32));
            asm.sub_imm(REG_SP.into(), REG_SP.into(), FRAME_SIZE as u32);
        }

        save_callee_saved_regs(&mut asm);

        asm.mov(REG_THREAD.into(), CCALL_REG_PARAMS[0].into());
        asm.mov(REG_TMP1.into(), CCALL_REG_PARAMS[1].into());
        asm.mov(REG_PARAMS[0].into(), CCALL_REG_PARAMS[2].into());
        asm.bl_r(REG_TMP1.into());

        load_callee_saved_regs(&mut asm);
        asm.add(REG_SP.into(), REG_FP.into(), REG_ZERO.into());
        asm.ldp_post(REG_FP.into(), REG_LR.into(), REG_SP.into(), 2);
        asm.ret(REG_LR.into());

        code_descriptor(asm.finalize(CODE_ALIGNMENT).code())
    }

    fn save_callee_saved_regs(asm: &mut Assembler) {
        for (idx, &reg) in CALLEE_SAVED_REGS.iter().enumerate() {
            let offset = FP_CALLEE_REGS_OFFSET + (idx as i32) * mem::ptr_width();
            debug_assert!(asm::fits_ldst_unscaled(offset));
            asm.stur(reg.into(), REG_FP.into(), offset);
        }

        for (idx, &reg) in CALLEE_SAVED_FREGS.iter().enumerate() {
            let offset = FP_CALLEE_FREGS_OFFSET + (idx as i32) * mem::ptr_width();
            debug_assert!(asm::fits_ldst_unscaled(offset));
            asm.stur_d(reg.into(), REG_FP.into(), offset);
        }
    }

    fn load_callee_saved_regs(asm: &mut Assembler) {
        for (idx, &reg) in CALLEE_SAVED_REGS.iter().enumerate() {
            let offset = FP_CALLEE_REGS_OFFSET + (idx as i32) * mem::ptr_width();
            debug_assert!(asm::fits_ldst_unscaled(offset));
            asm.ldur(reg.into(), REG_FP.into(), offset);
        }

        for (idx, &reg) in CALLEE_SAVED_FREGS.iter().enumerate() {
            let offset = FP_CALLEE_FREGS_OFFSET + (idx as i32) * mem::ptr_width();
            debug_assert!(asm::fits_ldst_unscaled(offset));
            asm.ldur_d(reg.into(), REG_FP.into(), offset);
        }
    }
}
