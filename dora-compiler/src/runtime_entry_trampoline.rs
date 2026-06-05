use std::mem::size_of;

use crate::{
    CODE_ALIGNMENT, CodeDescriptor, CommentTable, DoraToNativeInfo, FReg, GcPoint, GcPointTable,
    LocationTable, MachineMode, Reg, RelocationForm, RelocationKind, RelocationTable, TargetArch,
    align_i32, ptr_width, thread_local_dtn_offset,
};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, FunctionId};

#[derive(Clone)]
pub enum NativeFctKind {
    RuntimeEntryTrampoline(FunctionId),
    GcAllocationTrampoline,
    TrapTrampoline,
    StackOverflowTrampoline,
    SafepointTrampoline,
    UnreachableTrampoline,
    FatalErrorTrampoline,
}

pub enum NativeTarget {
    Address(usize),
    Symbol(String),
}

pub struct NativeFct {
    pub target: NativeTarget,
    pub args: BytecodeTypeArray,
    pub return_type: BytecodeType,
    pub desc: NativeFctKind,
}

pub fn generate_aot(target_arch: TargetArch, fct: NativeFct, dbg: bool) -> CodeDescriptor {
    match target_arch {
        TargetArch::X64 => x64::generate(fct, dbg),
        TargetArch::Arm64 => arm64::generate(fct, dbg),
    }
}

fn code_descriptor(
    code: Vec<u8>,
    gcpoints: GcPointTable,
    relocations: RelocationTable,
) -> CodeDescriptor {
    CodeDescriptor {
        code,
        gcpoints,
        comments: CommentTable::new(),
        positions: LocationTable::new(),
        relocations,
        inlined_functions: Vec::new(),
    }
}

mod x64 {
    use super::*;
    use crate::cpu::x64 as cpu;
    use dora_asm::x64::{Address as AsmAddress, AssemblerX64 as Assembler, Immediate, XmmRegister};

    pub(super) fn generate(fct: NativeFct, dbg: bool) -> CodeDescriptor {
        NativeGen {
            asm: Assembler::new(cpu::has_avx2()),
            fct,
            dbg,
            gcpoints: GcPointTable::new(),
            relocations: RelocationTable::new(),
        }
        .generate()
    }

    struct NativeGen {
        asm: Assembler,
        fct: NativeFct,
        dbg: bool,
        gcpoints: GcPointTable,
        relocations: RelocationTable,
    }

    impl NativeGen {
        fn generate(mut self) -> CodeDescriptor {
            let temp_reg = cpu::SCRATCH[0];
            let has_avx2 = cpu::has_avx2();

            let save_return = self.fct.return_type.is_unit();
            let dtn_size = size_of::<DoraToNativeInfo>() as i32;

            let (stack_args, temporaries, temporaries_desc, args_desc) = analyze(
                &self.fct.args,
                &cpu::REG_PARAMS,
                &cpu::FREG_PARAMS,
                &cpu::CCALL_REG_PARAMS,
                &cpu::CCALL_FREG_PARAMS,
            );

            let offset_args = 0;
            let offset_temporaries = offset_args + stack_args as i32 * ptr_width();
            let offset_dtn = offset_temporaries + temporaries as i32 * ptr_width();
            let offset_return = offset_dtn + dtn_size;
            let framesize = offset_return + if save_return { ptr_width() } else { 0 };
            let framesize = align_i32(framesize, cpu::STACK_FRAME_ALIGNMENT as i32);

            if self.dbg {
                self.asm.int3();
            }

            self.asm.pushq_r(cpu::REG_FP.into());
            self.asm.movq_rr(cpu::REG_FP.into(), cpu::REG_SP.into());
            debug_assert!(framesize as usize % cpu::STACK_FRAME_ALIGNMENT == 0);

            if framesize > 0 {
                self.asm
                    .subq_ri(cpu::REG_SP.into(), Immediate(framesize as i64));
            }

            for desc in temporaries_desc {
                match desc {
                    TemporaryStore::Register(mode, reg, offset) => {
                        store_reg(
                            &mut self.asm,
                            mode,
                            cpu::REG_SP,
                            offset_temporaries + offset as i32 * ptr_width(),
                            reg,
                        );
                    }

                    TemporaryStore::FloatRegister(mode, reg, offset) => {
                        store_freg(
                            &mut self.asm,
                            has_avx2,
                            mode,
                            cpu::REG_SP,
                            offset_temporaries + offset as i32 * ptr_width(),
                            reg,
                        );
                    }
                }
            }

            load_reg(
                &mut self.asm,
                MachineMode::Ptr,
                temp_reg,
                cpu::REG_THREAD,
                thread_local_dtn_offset(),
            );
            store_reg(
                &mut self.asm,
                MachineMode::Ptr,
                cpu::REG_SP,
                offset_dtn + DoraToNativeInfo::last_offset(),
                temp_reg,
            );
            store_reg(
                &mut self.asm,
                MachineMode::Ptr,
                cpu::REG_SP,
                offset_dtn + DoraToNativeInfo::fp_offset(),
                cpu::REG_FP,
            );

            self.asm.lea(temp_reg.into(), AsmAddress::rip(0));
            store_reg(
                &mut self.asm,
                MachineMode::Ptr,
                cpu::REG_SP,
                offset_dtn + DoraToNativeInfo::pc_offset(),
                temp_reg,
            );

            self.asm.movq_rr(temp_reg.into(), cpu::REG_SP.into());
            if offset_dtn != 0 {
                self.asm
                    .addq_ri(temp_reg.into(), Immediate(offset_dtn as i64));
            }

            store_reg(
                &mut self.asm,
                MachineMode::Ptr,
                cpu::REG_THREAD,
                thread_local_dtn_offset(),
                temp_reg,
            );

            let mut offsets = Vec::new();

            for desc in args_desc {
                let sp_offset = match desc.0 {
                    ArgumentSource::CallerArg(offset) => {
                        framesize + cpu::PARAM_OFFSET + offset as i32 * ptr_width()
                    }
                    ArgumentSource::Temporary(offset) => {
                        offset_temporaries + offset as i32 * ptr_width()
                    }
                };

                match desc.1 {
                    ArgumentDestination::FloatRegister(mode, reg) => {
                        load_freg(&mut self.asm, has_avx2, mode, reg, cpu::REG_SP, sp_offset);
                    }
                    ArgumentDestination::Register(mode, reg) => {
                        load_reg(&mut self.asm, mode, reg, cpu::REG_SP, sp_offset);
                    }
                    ArgumentDestination::Offset(mode, offset) => {
                        let dest_offset = offset as i32 * ptr_width();
                        if mode.is_float() {
                            load_freg(
                                &mut self.asm,
                                has_avx2,
                                mode,
                                cpu::FREG_TMP1,
                                cpu::REG_SP,
                                sp_offset,
                            );
                            store_freg(
                                &mut self.asm,
                                has_avx2,
                                mode,
                                cpu::REG_SP,
                                dest_offset,
                                cpu::FREG_TMP1,
                            );
                        } else {
                            load_reg(&mut self.asm, mode, temp_reg, cpu::REG_SP, sp_offset);
                            store_reg(&mut self.asm, mode, cpu::REG_SP, dest_offset, temp_reg);
                        }
                    }
                    ArgumentDestination::HandleRegister(reg) => {
                        offsets.push(sp_offset - framesize);
                        self.asm.lea(reg.into(), stack_address(sp_offset));
                    }
                    ArgumentDestination::HandleOffset(offset) => {
                        offsets.push(sp_offset - framesize);
                        self.asm.lea(temp_reg.into(), stack_address(sp_offset));
                        store_reg(
                            &mut self.asm,
                            MachineMode::Ptr,
                            cpu::REG_SP,
                            offset as i32 * ptr_width(),
                            temp_reg,
                        );
                    }
                }
            }

            match &self.fct.target {
                NativeTarget::Address(addr) => {
                    self.asm
                        .movq_ri(cpu::REG_RESULT.into(), Immediate(*addr as i64));
                    self.asm.call_r(cpu::REG_RESULT.into());
                }
                NativeTarget::Symbol(sym) => {
                    let pos = self.asm.position() as u32;
                    self.asm.call_rel32(0);
                    self.relocations.insert(
                        pos,
                        RelocationKind::NativeCall(sym.clone()),
                        RelocationForm::X64CallRel32,
                    );
                }
            }
            self.gcpoints.insert(0, GcPoint::from_offsets(offsets));

            load_reg(
                &mut self.asm,
                MachineMode::Ptr,
                temp_reg,
                cpu::REG_SP,
                offset_dtn + DoraToNativeInfo::last_offset(),
            );
            store_reg(
                &mut self.asm,
                MachineMode::Ptr,
                cpu::REG_THREAD,
                thread_local_dtn_offset(),
                temp_reg,
            );

            self.asm.movq_rr(cpu::REG_SP.into(), cpu::REG_FP.into());
            self.asm.popq_r(cpu::REG_FP.into());
            self.asm.retq();
            self.asm.nop();

            code_descriptor(
                self.asm.finalize(CODE_ALIGNMENT).code(),
                self.gcpoints,
                self.relocations,
            )
        }
    }

    fn stack_address(offset: i32) -> AsmAddress {
        AsmAddress::offset(cpu::REG_SP.into(), offset)
    }

    fn address(base: Reg, offset: i32) -> AsmAddress {
        AsmAddress::offset(base.into(), offset)
    }

    fn load_reg(asm: &mut Assembler, mode: MachineMode, dest: Reg, base: Reg, offset: i32) {
        match mode {
            MachineMode::Int8 => asm.movb_ra(dest.into(), address(base, offset)),
            MachineMode::Int32 => asm.movl_ra(dest.into(), address(base, offset)),
            MachineMode::Int64 | MachineMode::Ptr | MachineMode::IntPtr => {
                asm.movq_ra(dest.into(), address(base, offset))
            }
            MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
        }
    }

    fn store_reg(asm: &mut Assembler, mode: MachineMode, base: Reg, offset: i32, src: Reg) {
        match mode {
            MachineMode::Int8 => asm.movb_ar(address(base, offset), src.into()),
            MachineMode::Int32 => asm.movl_ar(address(base, offset), src.into()),
            MachineMode::Int64 | MachineMode::Ptr | MachineMode::IntPtr => {
                asm.movq_ar(address(base, offset), src.into())
            }
            MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
        }
    }

    fn load_freg(
        asm: &mut Assembler,
        has_avx2: bool,
        mode: MachineMode,
        dest: FReg,
        base: Reg,
        offset: i32,
    ) {
        let dest = XmmRegister::new(dest.0);
        match mode {
            MachineMode::Float32 => {
                if has_avx2 {
                    asm.vmovss_ra(dest, address(base, offset));
                } else {
                    asm.movss_ra(dest, address(base, offset));
                }
            }
            MachineMode::Float64 => {
                if has_avx2 {
                    asm.vmovsd_ra(dest, address(base, offset));
                } else {
                    asm.movsd_ra(dest, address(base, offset));
                }
            }
            _ => unreachable!(),
        }
    }

    fn store_freg(
        asm: &mut Assembler,
        has_avx2: bool,
        mode: MachineMode,
        base: Reg,
        offset: i32,
        src: FReg,
    ) {
        let src = XmmRegister::new(src.0);
        match mode {
            MachineMode::Float32 => {
                if has_avx2 {
                    asm.vmovss_ar(address(base, offset), src);
                } else {
                    asm.movss_ar(address(base, offset), src);
                }
            }
            MachineMode::Float64 => {
                if has_avx2 {
                    asm.vmovsd_ar(address(base, offset), src);
                } else {
                    asm.movsd_ar(address(base, offset), src);
                }
            }
            _ => unreachable!(),
        }
    }
}

mod arm64 {
    use super::*;
    use crate::cpu::arm64 as cpu;
    use dora_asm::arm64::{self as asm, AssemblerArm64 as Assembler, MemOperand, NeonRegister};

    pub(super) fn generate(fct: NativeFct, dbg: bool) -> CodeDescriptor {
        NativeGen {
            asm: Assembler::new(),
            fct,
            dbg,
            gcpoints: GcPointTable::new(),
            relocations: RelocationTable::new(),
        }
        .generate()
    }

    struct NativeGen {
        asm: Assembler,
        fct: NativeFct,
        dbg: bool,
        gcpoints: GcPointTable,
        relocations: RelocationTable,
    }

    impl NativeGen {
        fn generate(mut self) -> CodeDescriptor {
            let temp_reg = cpu::SCRATCH[0];
            let addr_scratch = cpu::SCRATCH[1];

            let save_return = self.fct.return_type.is_unit();
            let dtn_size = size_of::<DoraToNativeInfo>() as i32;

            let (stack_args, temporaries, temporaries_desc, args_desc) = analyze(
                &self.fct.args,
                &cpu::REG_PARAMS,
                &cpu::FREG_PARAMS,
                &cpu::CCALL_REG_PARAMS,
                &cpu::CCALL_FREG_PARAMS,
            );

            let offset_args = 0;
            let offset_temporaries = offset_args + stack_args as i32 * ptr_width();
            let offset_dtn = offset_temporaries + temporaries as i32 * ptr_width();
            let offset_return = offset_dtn + dtn_size;
            let framesize = offset_return + if save_return { ptr_width() } else { 0 };
            let framesize = align_i32(framesize, cpu::STACK_FRAME_ALIGNMENT as i32);

            if self.dbg {
                self.asm.brk(0xF000);
            }

            self.asm.stp_pre(
                cpu::REG_FP.into(),
                cpu::REG_LR.into(),
                cpu::REG_SP.into(),
                -2,
            );
            self.asm
                .add(cpu::REG_FP.into(), cpu::REG_SP.into(), cpu::REG_ZERO.into());
            debug_assert!(framesize as usize % cpu::STACK_FRAME_ALIGNMENT == 0);

            if framesize > 0 {
                self.asm.mov_imm(temp_reg.into(), framesize as i64);
                self.asm
                    .sub(cpu::REG_SP.into(), cpu::REG_SP.into(), temp_reg.into());
            }

            for desc in temporaries_desc {
                match desc {
                    TemporaryStore::Register(mode, reg, offset) => {
                        let opnd = MemOperand::new(
                            cpu::REG_SP.into(),
                            (offset_temporaries + offset as i32 * ptr_width()) as i64,
                        );
                        match mode {
                            MachineMode::Int8 => {
                                self.asm.str_mem_b(reg.into(), opnd, addr_scratch.into())
                            }
                            MachineMode::Int32 => {
                                self.asm.str_mem_w(reg.into(), opnd, addr_scratch.into())
                            }
                            MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => {
                                self.asm.str_mem_x(reg.into(), opnd, addr_scratch.into())
                            }
                            MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
                        }
                    }

                    TemporaryStore::FloatRegister(mode, reg, offset) => {
                        let offset = offset_temporaries + offset as i32 * ptr_width();
                        match mode {
                            MachineMode::Float32 => self.asm.str_mem_s(
                                NeonRegister::new(reg.0),
                                MemOperand::new(cpu::REG_SP.into(), offset as i64),
                                addr_scratch.into(),
                            ),
                            MachineMode::Float64 => self.asm.str_mem_d(
                                NeonRegister::new(reg.0),
                                MemOperand::new(cpu::REG_SP.into(), offset as i64),
                                addr_scratch.into(),
                            ),
                            _ => unreachable!(),
                        }
                    }
                }
            }

            self.asm.ldr_mem_x(
                temp_reg.into(),
                MemOperand::new(cpu::REG_THREAD.into(), thread_local_dtn_offset() as i64),
                addr_scratch.into(),
            );
            self.asm.str_mem_x(
                temp_reg.into(),
                MemOperand::new(
                    cpu::REG_SP.into(),
                    (offset_dtn + DoraToNativeInfo::last_offset()) as i64,
                ),
                addr_scratch.into(),
            );
            self.asm.str_mem_x(
                cpu::REG_FP.into(),
                MemOperand::new(
                    cpu::REG_SP.into(),
                    (offset_dtn + DoraToNativeInfo::fp_offset()) as i64,
                ),
                addr_scratch.into(),
            );

            self.asm.adr_imm(temp_reg.into(), 0);
            self.asm.str_mem_x(
                temp_reg.into(),
                MemOperand::new(
                    cpu::REG_SP.into(),
                    (offset_dtn + DoraToNativeInfo::pc_offset()) as i64,
                ),
                addr_scratch.into(),
            );

            self.asm.mov(temp_reg.into(), cpu::REG_SP.into());
            if offset_dtn != 0 {
                add_offset(&mut self.asm, temp_reg, temp_reg, offset_dtn, addr_scratch);
            }

            self.asm.str_mem_x(
                temp_reg.into(),
                MemOperand::new(cpu::REG_THREAD.into(), thread_local_dtn_offset() as i64),
                addr_scratch.into(),
            );

            let mut offsets = Vec::new();

            for desc in args_desc {
                let sp_offset = match desc.0 {
                    ArgumentSource::CallerArg(offset) => {
                        framesize + cpu::PARAM_OFFSET + offset as i32 * ptr_width()
                    }
                    ArgumentSource::Temporary(offset) => {
                        offset_temporaries + offset as i32 * ptr_width()
                    }
                };

                match desc.1 {
                    ArgumentDestination::FloatRegister(mode, reg) => match mode {
                        MachineMode::Float32 => self.asm.ldr_mem_s(
                            NeonRegister::new(reg.0),
                            MemOperand::new(cpu::REG_SP.into(), sp_offset as i64),
                            addr_scratch.into(),
                        ),
                        MachineMode::Float64 => self.asm.ldr_mem_d(
                            NeonRegister::new(reg.0),
                            MemOperand::new(cpu::REG_SP.into(), sp_offset as i64),
                            addr_scratch.into(),
                        ),
                        _ => unreachable!(),
                    },
                    ArgumentDestination::Register(mode, reg) => {
                        let opnd = MemOperand::new(cpu::REG_SP.into(), sp_offset as i64);
                        match mode {
                            MachineMode::Int8 => {
                                self.asm.ldr_mem_b(reg.into(), opnd, addr_scratch.into())
                            }
                            MachineMode::Int32 => {
                                self.asm.ldr_mem_w(reg.into(), opnd, addr_scratch.into())
                            }
                            MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => {
                                self.asm.ldr_mem_x(reg.into(), opnd, addr_scratch.into())
                            }
                            MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
                        }
                    }
                    ArgumentDestination::Offset(mode, offset) => {
                        let dest_offset = offset as i32 * ptr_width();
                        if mode.is_float() {
                            match mode {
                                MachineMode::Float32 => {
                                    self.asm.ldr_mem_s(
                                        NeonRegister::new(cpu::FREG_TMP1.0),
                                        MemOperand::new(cpu::REG_SP.into(), sp_offset as i64),
                                        addr_scratch.into(),
                                    );
                                    self.asm.str_mem_s(
                                        NeonRegister::new(cpu::FREG_TMP1.0),
                                        MemOperand::new(cpu::REG_SP.into(), dest_offset as i64),
                                        addr_scratch.into(),
                                    );
                                }
                                MachineMode::Float64 => {
                                    self.asm.ldr_mem_d(
                                        NeonRegister::new(cpu::FREG_TMP1.0),
                                        MemOperand::new(cpu::REG_SP.into(), sp_offset as i64),
                                        addr_scratch.into(),
                                    );
                                    self.asm.str_mem_d(
                                        NeonRegister::new(cpu::FREG_TMP1.0),
                                        MemOperand::new(cpu::REG_SP.into(), dest_offset as i64),
                                        addr_scratch.into(),
                                    );
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            let src = MemOperand::new(cpu::REG_SP.into(), sp_offset as i64);
                            let dest = MemOperand::new(cpu::REG_SP.into(), dest_offset as i64);
                            match mode {
                                MachineMode::Int8 => {
                                    self.asm
                                        .ldr_mem_b(temp_reg.into(), src, addr_scratch.into());
                                    self.asm
                                        .str_mem_b(temp_reg.into(), dest, addr_scratch.into());
                                }
                                MachineMode::Int32 => {
                                    self.asm
                                        .ldr_mem_w(temp_reg.into(), src, addr_scratch.into());
                                    self.asm
                                        .str_mem_w(temp_reg.into(), dest, addr_scratch.into());
                                }
                                MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => {
                                    self.asm
                                        .ldr_mem_x(temp_reg.into(), src, addr_scratch.into());
                                    self.asm
                                        .str_mem_x(temp_reg.into(), dest, addr_scratch.into());
                                }
                                MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
                            }
                        }
                    }
                    ArgumentDestination::HandleRegister(reg) => {
                        offsets.push(sp_offset - framesize);
                        add_offset(&mut self.asm, reg, cpu::REG_SP, sp_offset, addr_scratch);
                    }
                    ArgumentDestination::HandleOffset(offset) => {
                        offsets.push(sp_offset - framesize);
                        add_offset(
                            &mut self.asm,
                            temp_reg,
                            cpu::REG_SP,
                            sp_offset,
                            addr_scratch,
                        );
                        self.asm.str_mem_x(
                            temp_reg.into(),
                            MemOperand::new(
                                cpu::REG_SP.into(),
                                (offset as i32 * ptr_width()) as i64,
                            ),
                            addr_scratch.into(),
                        );
                    }
                }
            }

            match &self.fct.target {
                NativeTarget::Address(addr) => {
                    self.asm.mov_imm(temp_reg.into(), *addr as i64);
                    self.asm.bl_r(temp_reg.into());
                }
                NativeTarget::Symbol(sym) => {
                    let pos = self.asm.position() as u32;
                    self.asm.bl_imm(0);
                    self.relocations.insert(
                        pos,
                        RelocationKind::NativeCall(sym.clone()),
                        RelocationForm::Arm64Branch26,
                    );
                }
            }
            self.gcpoints.insert(0, GcPoint::from_offsets(offsets));

            self.asm.ldr_mem_x(
                temp_reg.into(),
                MemOperand::new(
                    cpu::REG_SP.into(),
                    (offset_dtn + DoraToNativeInfo::last_offset()) as i64,
                ),
                addr_scratch.into(),
            );
            self.asm.str_mem_x(
                temp_reg.into(),
                MemOperand::new(cpu::REG_THREAD.into(), thread_local_dtn_offset() as i64),
                addr_scratch.into(),
            );

            self.asm
                .add(cpu::REG_SP.into(), cpu::REG_FP.into(), cpu::REG_ZERO.into());
            self.asm.ldp_post(
                cpu::REG_FP.into(),
                cpu::REG_LR.into(),
                cpu::REG_SP.into(),
                2,
            );
            self.asm.ret(cpu::REG_LR.into());
            self.asm.nop();

            code_descriptor(
                self.asm.finalize(CODE_ALIGNMENT).code(),
                self.gcpoints,
                self.relocations,
            )
        }
    }

    fn add_offset(asm: &mut Assembler, dest: Reg, base: Reg, offset: i32, scratch: Reg) {
        if offset >= 0 && asm::fits_addsub_imm(offset as u32) {
            asm.add_imm(dest.into(), base.into(), offset as u32);
        } else {
            asm.mov_imm(scratch.into(), offset as i64);
            asm.add(dest.into(), base.into(), scratch.into());
        }
    }
}

fn analyze(
    args: &BytecodeTypeArray,
    reg_params: &[Reg],
    freg_params: &[FReg],
    ccall_reg_params: &[Reg],
    ccall_freg_params: &[FReg],
) -> (
    u32,
    u32,
    Vec<TemporaryStore>,
    Vec<(ArgumentSource, ArgumentDestination)>,
) {
    let mut stack_args = 0;
    let mut temporaries = 0;

    let mut save_temporaries: Vec<TemporaryStore> = Vec::new();
    let mut load_params: Vec<(ArgumentSource, ArgumentDestination)> = Vec::new();

    let mut reg_idx = 0;
    let mut freg_idx = 0;
    let mut stack_idx = 0;

    for ty in args.iter() {
        let mode = mode(ty.clone());

        if ty.is_any_float() {
            let source = if freg_idx < freg_params.len() {
                save_temporaries.push(TemporaryStore::FloatRegister(
                    mode,
                    freg_params[freg_idx],
                    temporaries,
                ));
                temporaries += 1;
                ArgumentSource::Temporary(temporaries - 1)
            } else {
                stack_idx += 1;
                ArgumentSource::CallerArg(stack_idx - 1)
            };

            let destination = if freg_idx < ccall_freg_params.len() {
                // argument still fits into register
                ArgumentDestination::FloatRegister(mode, ccall_freg_params[freg_idx])
            } else {
                stack_args += 1;
                ArgumentDestination::Offset(mode, stack_args - 1)
            };

            load_params.push((source, destination));
            freg_idx += 1;
        } else {
            let source = if reg_idx < reg_params.len() {
                save_temporaries.push(TemporaryStore::Register(
                    mode,
                    reg_params[reg_idx],
                    temporaries,
                ));
                temporaries += 1;
                ArgumentSource::Temporary(temporaries - 1)
            } else {
                stack_idx += 1;
                ArgumentSource::CallerArg(stack_idx - 1)
            };

            let destination = if reg_idx < ccall_reg_params.len() {
                // argument still fits into register
                if cfg!(target_family = "windows") {
                    stack_args += 1;
                }

                if ty.is_reference_type() {
                    ArgumentDestination::HandleRegister(ccall_reg_params[reg_idx])
                } else {
                    ArgumentDestination::Register(mode, ccall_reg_params[reg_idx])
                }
            } else {
                stack_args += 1;

                if ty.is_reference_type() {
                    ArgumentDestination::HandleOffset(stack_args - 1)
                } else {
                    ArgumentDestination::Offset(mode, stack_args - 1)
                }
            };

            load_params.push((source, destination));
            reg_idx += 1;
        }
    }

    if cfg!(target_family = "windows") && stack_args < 4 {
        stack_args = 4;
    }

    (stack_args, temporaries, save_temporaries, load_params)
}

fn mode(ty: BytecodeType) -> MachineMode {
    match ty {
        BytecodeType::Bool | BytecodeType::UInt8 => MachineMode::Int8,
        BytecodeType::Char | BytecodeType::Int32 => MachineMode::Int32,
        BytecodeType::Int64 => MachineMode::Int64,
        BytecodeType::Float32 => MachineMode::Float32,
        BytecodeType::Float64 => MachineMode::Float64,
        BytecodeType::Ptr
        | BytecodeType::Address
        | BytecodeType::TraitObject(..)
        | BytecodeType::Class(..)
        | BytecodeType::Lambda(..)
        | BytecodeType::Ref(..) => MachineMode::Ptr,
        BytecodeType::Enum(..)
        | BytecodeType::TypeAlias(..)
        | BytecodeType::Assoc { .. }
        | BytecodeType::Tuple(_)
        | BytecodeType::TypeParam(_)
        | BytecodeType::This
        | BytecodeType::Struct(_, _)
        | BytecodeType::Unit => {
            panic!("unexpected native trampoline argument type {:?}", ty)
        }
    }
}

enum TemporaryStore {
    Register(MachineMode, Reg, u32),
    FloatRegister(MachineMode, FReg, u32),
}

enum ArgumentSource {
    Temporary(u32),
    CallerArg(u32),
}

enum ArgumentDestination {
    Offset(MachineMode, u32),
    Register(MachineMode, Reg),
    FloatRegister(MachineMode, FReg),
    HandleOffset(u32),
    HandleRegister(Reg),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn native_fct() -> NativeFct {
        NativeFct {
            target: NativeTarget::Symbol("dora_native_test".to_string()),
            args: BytecodeTypeArray::new(vec![
                BytecodeType::Ptr,
                BytecodeType::Int64,
                BytecodeType::Float64,
            ]),
            return_type: BytecodeType::Unit,
            desc: NativeFctKind::GcAllocationTrampoline,
        }
    }

    fn assert_generate_aot(target_arch: TargetArch) {
        let code = generate_aot(target_arch, native_fct(), false);

        assert!(!code.code.is_empty());
        assert_eq!(code.code.len() % CODE_ALIGNMENT, 0);
        assert_eq!(code.gcpoints.entries().len(), 1);
        assert_eq!(code.relocations.entries.len(), 1);
        assert!(matches!(
            &code.relocations.entries[0].target,
            RelocationKind::NativeCall(symbol) if symbol == "dora_native_test"
        ));
    }

    #[test]
    fn generate_aot_x64() {
        assert_generate_aot(TargetArch::X64);
    }

    #[test]
    fn generate_aot_arm64() {
        assert_generate_aot(TargetArch::Arm64);
    }
}
