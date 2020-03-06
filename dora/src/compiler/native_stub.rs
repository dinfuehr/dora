use std::collections::hash_map::HashMap;
use std::mem::size_of;

use dora_parser::lexer::position::Position;

use crate::compiler::codegen::AnyReg;
use crate::compiler::CodeDescriptor;
use crate::compiler::{Code, GcPoint, JitDescriptor, JitFct, JitFctId};
use crate::cpu::{
    FReg, Mem, Reg, CCALL_FREG_PARAMS, CCALL_REG_PARAMS, FREG_PARAMS, FREG_TMP1, PARAM_OFFSET,
    REG_FP, REG_PARAMS, REG_RESULT, REG_SP, REG_THREAD, REG_TMP1,
};
use crate::exception::DoraToNativeInfo;
use crate::gc::Address;
use crate::masm::MacroAssembler;
use crate::mem;
use crate::threads::ThreadLocalData;
use crate::ty::{BuiltinType, MachineMode};
use crate::vm::FctId;
use crate::vm::VM;

pub struct NativeStubs {
    map: HashMap<Address, JitFctId>,
}

impl NativeStubs {
    pub fn new() -> NativeStubs {
        NativeStubs {
            map: HashMap::new(),
        }
    }

    pub fn find_fct(&self, ptr: Address) -> Option<JitFctId> {
        self.map.get(&ptr).map(|&jit_fct_id| jit_fct_id)
    }

    pub fn insert_fct(&mut self, ptr: Address, fct: JitFctId) {
        self.map.entry(ptr).or_insert(fct);
    }
}

#[derive(Clone)]
pub enum NativeFctDescriptor {
    NativeStub(FctId),
    AllocStub,
    VerifyStub,
    TrapStub,
    GuardCheckStub,
}

pub struct NativeFct<'a> {
    pub ptr: Address,
    pub args: &'a [BuiltinType],
    pub return_type: BuiltinType,
    pub throws: bool,
    pub desc: NativeFctDescriptor,
}

pub fn generate<'a, 'ast: 'a>(vm: &'a VM<'ast>, fct: NativeFct, dbg: bool) -> JitFctId {
    let fct_desc = fct.desc.clone();

    let ngen = NativeGen {
        vm,
        masm: MacroAssembler::new(),
        fct,
        dbg,
    };

    let jit_fct = ngen.generate();
    let jit_start = jit_fct.ptr_start();
    let jit_end = jit_fct.ptr_end();
    let jit_fct_id: JitFctId = vm.jit_fcts.push(JitFct::Compiled(jit_fct)).into();

    let code_desc = match fct_desc {
        NativeFctDescriptor::NativeStub(_) => CodeDescriptor::NativeStub(jit_fct_id),
        NativeFctDescriptor::TrapStub => CodeDescriptor::TrapStub,
        NativeFctDescriptor::VerifyStub => CodeDescriptor::VerifyStub,
        NativeFctDescriptor::AllocStub => CodeDescriptor::AllocStub,
        NativeFctDescriptor::GuardCheckStub => CodeDescriptor::GuardCheckStub,
    };

    vm.insert_code_map(jit_start, jit_end, code_desc);

    jit_fct_id
}

struct NativeGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    masm: MacroAssembler,

    fct: NativeFct<'a>,
    dbg: bool,
}

impl<'a, 'ast> NativeGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> Code {
        let save_return = self.fct.return_type != BuiltinType::Unit;
        let dtn_size = size_of::<DoraToNativeInfo>() as i32;

        let (stack_args, temporaries, temporaries_desc, args_desc) = analyze(self.fct.args);

        let offset_args = 0;
        let offset_temporaries = offset_args + stack_args as i32 * mem::ptr_width();
        let offset_dtn = offset_temporaries + temporaries as i32 * mem::ptr_width();
        let offset_return = offset_dtn + dtn_size;
        let framesize = offset_return + if save_return { mem::ptr_width() } else { 0 };
        let framesize = mem::align_i32(framesize, 16);

        if self.dbg || self.vm.args.flag_emit_debug_native {
            self.masm.debug();
        }

        self.masm.prolog_size(framesize);

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

        self.masm.raw_call(self.fct.ptr.to_ptr());
        self.masm.emit_only_gcpoint(GcPoint::from_offsets(offsets));

        if !self.fct.return_type.is_unit() {
            self.masm
                .fix_result(REG_RESULT, self.fct.return_type.mode());
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

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::exception_object_offset()),
        );

        let lbl_exception = self.masm.test_if_not_nil(REG_TMP1);

        self.masm.epilog();

        self.masm.bind_label(lbl_exception);
        self.masm.throw(REG_TMP1, Position::new(1, 1));
        self.masm.nop();

        let desc = match self.fct.desc {
            NativeFctDescriptor::NativeStub(fid) => JitDescriptor::NativeStub(fid),
            NativeFctDescriptor::AllocStub => JitDescriptor::AllocStub,
            NativeFctDescriptor::VerifyStub => JitDescriptor::VerifyStub,
            NativeFctDescriptor::TrapStub => JitDescriptor::TrapStub,
            NativeFctDescriptor::GuardCheckStub => JitDescriptor::GuardCheckStub,
        };

        self.masm.jit(self.vm, framesize, desc, self.fct.throws)
    }
}

fn analyze(
    args: &[BuiltinType],
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

    for &ty in args {
        if ty.is_float() {
            let source = if freg_idx < FREG_PARAMS.len() {
                save_temporaries.push(TemporaryStore::FloatRegister(
                    ty.mode(),
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
                ArgumentDestination::FloatRegister(ty.mode(), CCALL_FREG_PARAMS[freg_idx])
            } else {
                stack_args += 1;
                ArgumentDestination::Offset(ty.mode(), stack_args - 1)
            };

            load_params.push((source, destination));
            freg_idx += 1;
        } else {
            let source = if reg_idx < REG_PARAMS.len() {
                save_temporaries.push(TemporaryStore::Register(
                    ty.mode(),
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

                if ty.reference_type() {
                    ArgumentDestination::HandleRegister(CCALL_REG_PARAMS[reg_idx])
                } else {
                    ArgumentDestination::Register(ty.mode(), CCALL_REG_PARAMS[reg_idx])
                }
            } else {
                stack_args += 1;

                if ty.reference_type() {
                    ArgumentDestination::HandleOffset(stack_args - 1)
                } else {
                    ArgumentDestination::Offset(ty.mode(), stack_args - 1)
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
