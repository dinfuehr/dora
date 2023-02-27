use std::collections::hash_map::HashMap;
use std::mem::size_of;
use std::sync::Arc;

use crate::bytecode::{BytecodeType, BytecodeTypeArray};
use crate::cannon::codegen::mode;
use crate::compiler::codegen::AnyReg;
use crate::cpu::{
    FReg, Reg, CCALL_FREG_PARAMS, CCALL_REG_PARAMS, FREG_PARAMS, FREG_TMP1, PARAM_OFFSET, REG_FP,
    REG_PARAMS, REG_RESULT, REG_SP, REG_THREAD, REG_TMP1,
};
use crate::gc::Address;
use crate::language::sem_analysis::FctDefinitionId;
use crate::masm::{MacroAssembler, Mem};
use crate::mem;
use crate::mode::MachineMode;
use crate::stack::DoraToNativeInfo;
use crate::threads::ThreadLocalData;
use crate::vm::install_code_stub;
use crate::vm::{Code, CodeKind, GcPoint, VM};

pub struct NativeStubs {
    map: HashMap<Address, Address>,
}

impl NativeStubs {
    pub fn new() -> NativeStubs {
        NativeStubs {
            map: HashMap::new(),
        }
    }

    pub fn find_fct(&self, key: Address) -> Option<Address> {
        self.map.get(&key).map(|&code_id| code_id)
    }

    pub fn insert_fct(&mut self, key: Address, stub: Address) {
        self.map.entry(key).or_insert(stub);
    }
}

#[derive(Clone)]
pub enum NativeFctKind {
    NativeStub(FctDefinitionId),
    AllocStub,
    VerifyStub,
    TrapStub,
    GuardCheckStub,
    SafepointStub,
}

pub struct NativeFct {
    pub fctptr: Address,
    pub args: BytecodeTypeArray,
    pub return_type: BytecodeType,
    pub desc: NativeFctKind,
}

pub fn generate<'a>(vm: &'a VM, fct: NativeFct, dbg: bool) -> Arc<Code> {
    let ngen = NativeGen {
        vm,
        masm: MacroAssembler::new(),
        fct,
        dbg,
    };

    ngen.generate()
}

struct NativeGen<'a> {
    vm: &'a VM,
    masm: MacroAssembler,

    fct: NativeFct,
    dbg: bool,
}

impl<'a> NativeGen<'a> {
    pub fn generate(mut self) -> Arc<Code> {
        let save_return = self.fct.return_type.is_unit();
        let dtn_size = size_of::<DoraToNativeInfo>() as i32;

        let (stack_args, temporaries, temporaries_desc, args_desc) =
            analyze(self.vm, &self.fct.args);

        let offset_args = 0;
        let offset_temporaries = offset_args + stack_args as i32 * mem::ptr_width();
        let offset_dtn = offset_temporaries + temporaries as i32 * mem::ptr_width();
        let offset_return = offset_dtn + dtn_size;
        let framesize = offset_return + if save_return { mem::ptr_width() } else { 0 };
        let framesize = mem::align_i32(framesize, 16);

        if self.dbg || self.vm.args.flag_emit_debug_native {
            self.masm.debug();
        }

        self.masm.prolog(framesize);

        for desc in temporaries_desc {
            match desc {
                TemporaryStore::Register(mode, reg, offset) => {
                    self.masm.store_mem(
                        mode,
                        Mem::Base(
                            REG_SP,
                            offset_temporaries + offset as i32 * mem::ptr_width(),
                        ),
                        reg.into(),
                    );
                }

                TemporaryStore::FloatRegister(mode, reg, offset) => {
                    self.masm.store_mem(
                        mode,
                        Mem::Base(
                            REG_SP,
                            offset_temporaries + offset as i32 * mem::ptr_width(),
                        ),
                        reg.into(),
                    );
                }
            }
        }

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::last_offset()),
            REG_TMP1.into(),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::fp_offset()),
            REG_FP.into(),
        );

        self.masm.copy_pc(REG_TMP1);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::pc_offset()),
            REG_TMP1.into(),
        );

        self.masm.copy_reg(MachineMode::Ptr, REG_TMP1, REG_SP);
        if offset_dtn != 0 {
            self.masm
                .int_add_imm(MachineMode::Ptr, REG_TMP1, REG_TMP1, offset_dtn as i64);
        }

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_TMP1.into(),
        );

        let mut offsets = Vec::new();

        for desc in args_desc {
            let sp_offset = match desc.0 {
                ArgumentSource::CallerArg(offset) => {
                    framesize + PARAM_OFFSET + offset as i32 * mem::ptr_width()
                }
                ArgumentSource::Temporary(offset) => {
                    offset_temporaries + offset as i32 * mem::ptr_width()
                }
            };

            match desc.1 {
                ArgumentDestination::FloatRegister(mode, reg) => {
                    self.masm
                        .load_mem(mode, reg.into(), Mem::Base(REG_SP, sp_offset));
                }
                ArgumentDestination::Register(mode, reg) => {
                    self.masm
                        .load_mem(mode, reg.into(), Mem::Base(REG_SP, sp_offset));
                }
                ArgumentDestination::Offset(mode, offset) => {
                    let reg: AnyReg = if mode.is_float() {
                        FREG_TMP1.into()
                    } else {
                        REG_TMP1.into()
                    };

                    self.masm
                        .load_mem(mode, reg.into(), Mem::Base(REG_SP, sp_offset));
                    self.masm.store_mem(
                        mode,
                        Mem::Base(REG_SP, offset as i32 * mem::ptr_width()),
                        reg.into(),
                    );
                }
                ArgumentDestination::HandleRegister(reg) => {
                    offsets.push(sp_offset - framesize);
                    self.masm.lea(reg, Mem::Base(REG_SP, sp_offset));
                }
                ArgumentDestination::HandleOffset(offset) => {
                    offsets.push(sp_offset - framesize);
                    self.masm.lea(REG_TMP1, Mem::Base(REG_SP, sp_offset));
                    self.masm.store_mem(
                        MachineMode::Ptr,
                        Mem::Base(REG_SP, offset as i32 * mem::ptr_width()),
                        REG_TMP1.into(),
                    );
                }
            }
        }

        self.masm.raw_call(self.fct.fctptr);
        self.masm.emit_only_gcpoint(GcPoint::from_offsets(offsets));

        if !self.fct.return_type.is_unit() {
            let mode = mode(self.vm, self.fct.return_type);
            self.masm.fix_result(REG_RESULT, mode);
        }

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::last_offset()),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_TMP1.into(),
        );

        self.masm.epilog();
        self.masm.nop();

        let kind = match self.fct.desc {
            NativeFctKind::NativeStub(fid) => CodeKind::NativeStub(fid),
            NativeFctKind::AllocStub => CodeKind::AllocStub,
            NativeFctKind::VerifyStub => CodeKind::VerifyStub,
            NativeFctKind::TrapStub => CodeKind::TrapStub,
            NativeFctKind::GuardCheckStub => CodeKind::GuardCheckStub,
            NativeFctKind::SafepointStub => CodeKind::SafepointStub,
        };

        let code_descriptor = self.masm.code();
        install_code_stub(self.vm, code_descriptor, kind)
    }
}

fn analyze(
    vm: &VM,
    args: &BytecodeTypeArray,
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
        let mode = mode(vm, ty.clone());

        if ty.is_any_float() {
            let source = if freg_idx < FREG_PARAMS.len() {
                save_temporaries.push(TemporaryStore::FloatRegister(
                    mode,
                    FREG_PARAMS[freg_idx],
                    temporaries,
                ));
                temporaries += 1;
                ArgumentSource::Temporary(temporaries - 1)
            } else {
                stack_idx += 1;
                ArgumentSource::CallerArg(stack_idx - 1)
            };

            let destination = if freg_idx < CCALL_FREG_PARAMS.len() {
                // argument still fits into register
                ArgumentDestination::FloatRegister(mode, CCALL_FREG_PARAMS[freg_idx])
            } else {
                stack_args += 1;
                ArgumentDestination::Offset(mode, stack_args - 1)
            };

            load_params.push((source, destination));
            freg_idx += 1;
        } else {
            let source = if reg_idx < REG_PARAMS.len() {
                save_temporaries.push(TemporaryStore::Register(
                    mode,
                    REG_PARAMS[reg_idx],
                    temporaries,
                ));
                temporaries += 1;
                ArgumentSource::Temporary(temporaries - 1)
            } else {
                stack_idx += 1;
                ArgumentSource::CallerArg(stack_idx - 1)
            };

            let destination = if reg_idx < CCALL_REG_PARAMS.len() {
                // argument still fits into register
                if cfg!(target_family = "windows") {
                    stack_args += 1;
                }

                if ty.is_reference_type() {
                    ArgumentDestination::HandleRegister(CCALL_REG_PARAMS[reg_idx])
                } else {
                    ArgumentDestination::Register(mode, CCALL_REG_PARAMS[reg_idx])
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
