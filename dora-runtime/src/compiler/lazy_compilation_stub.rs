use std::mem::size_of;
use std::sync::Arc;

use crate::cannon::codegen::result_passed_as_argument;
use crate::compiler;
use crate::cpu::{
    CCALL_REG_PARAMS, FREG_PARAMS, REG_FP, REG_PARAMS, REG_RESULT, REG_THREAD, REG_TMP1,
    STACK_FRAME_ALIGNMENT,
};
use crate::gc::{Address, Slot};
use crate::handle::{Handle, handle_scope};
use crate::masm::{MacroAssembler, Mem};
use crate::mem;
use crate::mirror::Object;
use crate::mode::MachineMode;
use crate::os;
use crate::stack::DoraToNativeInfo;
use crate::threads::{ThreadLocalData, current_thread};
use crate::vm::{
    BytecodeTypeExt, Code, CodeKind, EnumLayout, LazyCompilationSite, ShapeKind, VM,
    create_enum_instance, get_vm, install_code_stub,
};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, FunctionId};

// This code generates the compiler stub, there should only be one instance
// of this function be used in Dora. It is necessary for lazy compilation, where
// functions are only compiled on their first invocation. The compiler can use
// the address of this stub for invocations of functions that have not been compiled
// yet. The stub compiles the function and patches the call site to invoke the
// now-compiled function directly on the next invocation. In the end the function is
// executed.

pub fn generate<'a>(vm: &'a VM) -> Arc<Code> {
    let ngen = DoraCompileGen {
        vm,
        masm: MacroAssembler::new(),
        dbg: vm.flags.emit_debug_compile,
    };

    ngen.generate()
}

const FP_FIRST_STACK_ARG_OFFSET: i32 = FP_CALLER_PC_OFFSET + mem::ptr_width();
const FP_CALLER_PC_OFFSET: i32 = FP_CALLER_FP_OFFSET + mem::ptr_width();
const FP_CALLER_FP_OFFSET: i32 = 0;
const FRAME_SHADOW_STACK_SIZE: i32 = if cfg!(target_family = "windows") {
    32
} else {
    0
};
const FP_SHADOW_STACK_OFFSET: i32 = FP_CALLER_FP_OFFSET - FRAME_SHADOW_STACK_SIZE;

const FRAME_DTN_SIZE: i32 = size_of::<DoraToNativeInfo>() as i32;
const FP_DTN_OFFSET: i32 = FP_SHADOW_STACK_OFFSET - FRAME_DTN_SIZE;

const FRAME_REG_PARAMS_SIZE: i32 = REG_PARAMS.len() as i32 * mem::ptr_width();
const FP_REG_PARAMS_OFFSET: i32 = FP_DTN_OFFSET - FRAME_REG_PARAMS_SIZE;

const FRAME_FREG_PARAMS_SIZE: i32 = FREG_PARAMS.len() as i32 * mem::ptr_width();
const FP_FREG_PARAMS_OFFSET: i32 = FP_REG_PARAMS_OFFSET - FRAME_FREG_PARAMS_SIZE;

const UNALIGNED_FRAME_SIZE: i32 = FP_CALLER_FP_OFFSET - FP_FREG_PARAMS_OFFSET;
const FRAME_SIZE: i32 = mem::align_i32(UNALIGNED_FRAME_SIZE, STACK_FRAME_ALIGNMENT as i32);

struct DoraCompileGen<'a> {
    vm: &'a VM,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a> DoraCompileGen<'a> {
    pub fn generate(mut self) -> Arc<Code> {
        if self.dbg {
            self.masm.debug();
        }

        // the return address is the call-site we need to patch
        self.masm.prolog(FRAME_SIZE);

        // store params passed in registers on the stack
        self.store_params();

        // prepare the native call
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_FP, FP_DTN_OFFSET + DoraToNativeInfo::last_offset()),
            REG_TMP1.into(),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_FP, FP_DTN_OFFSET + DoraToNativeInfo::fp_offset()),
            REG_FP.into(),
        );

        self.masm.copy_pc(REG_TMP1);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_FP, FP_DTN_OFFSET + DoraToNativeInfo::pc_offset()),
            REG_TMP1.into(),
        );

        self.masm.copy_reg(MachineMode::Ptr, REG_TMP1, REG_FP);
        self.masm
            .int_add_imm(MachineMode::Ptr, REG_TMP1, REG_TMP1, FP_DTN_OFFSET as i64);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_TMP1.into(),
        );

        // invoke the compiler for the call site
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[0].into(),
            Mem::Base(REG_FP, mem::ptr_width()),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[1].into(),
            Mem::Base(REG_FP, FP_REG_PARAMS_OFFSET),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[2].into(),
            Mem::Base(REG_FP, FP_REG_PARAMS_OFFSET + mem::ptr_width()),
        );
        self.masm
            .raw_call(Address::from_ptr(lazy_compile as *const u8));

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_FP, FP_DTN_OFFSET + DoraToNativeInfo::last_offset()),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_TMP1.into(),
        );

        self.masm.copy_reg(MachineMode::Ptr, REG_TMP1, REG_RESULT);

        // restore argument registers from the stack
        self.load_params();

        // remove the stack frame
        self.masm.epilog_without_return();

        // jump to compiled function
        self.masm.jump_reg(REG_TMP1);

        let code_descriptor = self.masm.code();
        install_code_stub(self.vm, code_descriptor, CodeKind::LazyCompilationStub)
    }

    fn store_params(&mut self) {
        for (idx, &reg) in REG_PARAMS.iter().enumerate() {
            self.masm.store_mem(
                MachineMode::Ptr,
                Mem::Base(
                    REG_FP,
                    FP_REG_PARAMS_OFFSET + (idx as i32) * mem::ptr_width(),
                ),
                reg.into(),
            );
        }

        for (idx, &reg) in FREG_PARAMS.iter().enumerate() {
            self.masm.store_mem(
                MachineMode::Float64,
                Mem::Base(
                    REG_FP,
                    FP_FREG_PARAMS_OFFSET + (idx as i32) * mem::ptr_width(),
                ),
                reg.into(),
            );
        }
    }

    fn load_params(&mut self) {
        for (idx, &reg) in REG_PARAMS.iter().enumerate() {
            self.masm.load_mem(
                MachineMode::Ptr,
                reg.into(),
                Mem::Base(
                    REG_FP,
                    FP_REG_PARAMS_OFFSET + (idx as i32) * mem::ptr_width(),
                ),
            );
        }

        for (idx, &reg) in FREG_PARAMS.iter().enumerate() {
            self.masm.load_mem(
                MachineMode::Float64,
                reg.into(),
                Mem::Base(
                    REG_FP,
                    FP_FREG_PARAMS_OFFSET + (idx as i32) * mem::ptr_width(),
                ),
            );
        }
    }
}

pub fn iterate_roots<F>(
    vm: &VM,
    fp: Address,
    params: &BytecodeTypeArray,
    is_variadic: bool,
    return_type: BytecodeType,
    mut callback: F,
) where
    F: FnMut(Slot),
{
    let mut reg_idx = 0;
    let mut freg_idx = 0;
    let mut stack_address = fp.ioffset(FP_FIRST_STACK_ARG_OFFSET as isize);

    if result_passed_as_argument(return_type) {
        reg_offset(&mut reg_idx, &mut stack_address);
    }

    for (idx, param_ty) in params.iter().enumerate() {
        assert!(param_ty.is_concrete_type());

        let param_ty = if idx == params.len() - 1 && is_variadic {
            BytecodeType::Ptr
        } else if param_ty.is_unit() {
            continue;
        } else {
            param_ty
        };

        match param_ty.clone() {
            BytecodeType::Tuple(..) => {
                reg_offset(&mut reg_idx, &mut stack_address);
            }

            BytecodeType::Struct(..) => {
                reg_offset(&mut reg_idx, &mut stack_address);
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let enum_instance_id = create_enum_instance(vm, enum_id, type_params);
                let enum_instance = vm.enum_instances.idx(enum_instance_id);

                match enum_instance.layout {
                    EnumLayout::Int => reg_offset(&mut reg_idx, &mut stack_address),
                    EnumLayout::Tagged | EnumLayout::Ptr => {
                        reg_offset_pointer(&mut reg_idx, fp, &mut stack_address, &mut callback)
                    }
                }
            }

            BytecodeType::Float32 | BytecodeType::Float64 => {
                freg_offset(&mut freg_idx, &mut stack_address);
            }

            BytecodeType::UInt8
            | BytecodeType::Bool
            | BytecodeType::Char
            | BytecodeType::Int32
            | BytecodeType::Int64 => {
                reg_offset(&mut reg_idx, &mut stack_address);
            }

            BytecodeType::Ptr
            | BytecodeType::Class(..)
            | BytecodeType::TraitObject(..)
            | BytecodeType::Lambda(_, _) => {
                reg_offset_pointer(&mut reg_idx, fp, &mut stack_address, &mut callback);
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::GenericAssoc { .. }
            | BytecodeType::TypeParam(_)
            | BytecodeType::Unit
            | BytecodeType::This => {
                unreachable!()
            }
        }
    }
}

fn reg_offset(reg_idx: &mut usize, stack_address: &mut Address) {
    if *reg_idx < REG_PARAMS.len() {
        *reg_idx += 1;
    } else {
        *stack_address = stack_address.add_ptr(1);
    }
}

fn reg_offset_pointer<F>(
    reg_idx: &mut usize,
    fp: Address,
    stack_address: &mut Address,
    callback: &mut F,
) where
    F: FnMut(Slot),
{
    if *reg_idx < REG_PARAMS.len() {
        let fp_offset = FP_REG_PARAMS_OFFSET + *reg_idx as i32 * mem::ptr_width();
        callback(Slot::at(fp.ioffset(fp_offset as isize)));
        *reg_idx += 1;
    } else {
        let slot_address = *stack_address;
        callback(Slot::at(slot_address));
        *stack_address = stack_address.add_ptr(1);
    }
}

fn freg_offset(freg_idx: &mut usize, stack_address: &mut Address) {
    if *freg_idx < FREG_PARAMS.len() {
        *freg_idx += 1;
    } else {
        *stack_address = stack_address.add_ptr(1);
    }
}

fn lazy_compile(ra: usize, receiver1: Address, receiver2: Address) -> Address {
    let vm = get_vm();

    let code_id = vm
        .code_map
        .get(ra.into())
        .expect("return address not found");

    let code = vm.code_objects.get(code_id);

    let fct = vm.fct(code.fct_id());
    assert_ne!(vm.program.boots_package_id, Some(fct.package_id));

    let offset = ra - code.instruction_start().to_usize();

    let lazy_compilation_site = code
        .lazy_for_offset(offset as u32)
        .expect("lazy compilation site not found")
        .clone();

    handle_scope(|| {
        let receiver = match lazy_compilation_site {
            LazyCompilationSite::Direct { .. } => None,
            LazyCompilationSite::Virtual {
                receiver_is_first, ..
            }
            | LazyCompilationSite::Lambda {
                receiver_is_first, ..
            } => {
                let right_receiver = if receiver_is_first {
                    receiver1
                } else {
                    receiver2
                };

                let handle: Handle<Object> = current_thread()
                    .handles
                    .create_handle(right_receiver.into());

                Some(handle)
            }
        };

        if vm.flags.gc_stress_in_lazy_compile {
            vm.gc.force_collect(vm, crate::gc::GcReason::Stress);
        }

        match lazy_compilation_site {
            LazyCompilationSite::Direct {
                fct_id,
                ref type_params,
                const_pool_offset_from_ra,
            } => patch_direct_call(vm, ra, fct_id, type_params, const_pool_offset_from_ra),

            LazyCompilationSite::Virtual {
                trait_object_ty,
                vtable_index,
                ..
            } => {
                let trait_id = trait_object_ty.trait_id().expect("trait expected");
                let trait_fct_id = vm.trait_(trait_id).virtual_methods[vtable_index as usize];

                patch_virtual_call(
                    vm,
                    receiver.expect("missing handle"),
                    trait_fct_id,
                    trait_object_ty,
                    vtable_index,
                )
            }

            LazyCompilationSite::Lambda { .. } => {
                patch_lambda_call(vm, receiver.expect("missing handle"))
            }
        }
    })
}

fn patch_lambda_call(vm: &VM, receiver: Handle<Object>) -> Address {
    let shape = receiver.header().shape(vm.meta_space_start());

    let (lambda_id, type_params) = match shape.kind() {
        ShapeKind::Lambda(lambda_id, type_params) => (*lambda_id, type_params.clone()),
        _ => unreachable!(),
    };

    let fct_ptr = compiler::compile_fct_jit(vm, lambda_id, &type_params);
    shape.set_method_table_entry(0, fct_ptr);

    fct_ptr
}

fn patch_virtual_call(
    vm: &VM,
    receiver: Handle<Object>,
    trait_fct_id: FunctionId,
    trait_object_ty: BytecodeType,
    vtable_index: u32,
) -> Address {
    let shape = receiver.header().shape(vm.meta_space_start());

    let fct_ptr = match shape.kind() {
        ShapeKind::TraitObject {
            actual_object_ty, ..
        } => compiler::trait_object_thunk::ensure_compiled_jit(
            vm,
            trait_fct_id,
            trait_object_ty,
            actual_object_ty.clone(),
        ),

        _ => unreachable!(),
    };

    shape.set_method_table_entry(vtable_index as usize, fct_ptr);

    fct_ptr
}

fn patch_direct_call(
    vm: &VM,
    ra: usize,
    fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
    const_pool_offset_from_ra: i32,
) -> Address {
    let fct_ptr = compiler::compile_fct_jit(vm, fct_id, type_params);
    let fct_addr: *mut usize = (ra as isize + const_pool_offset_from_ra as isize) as *mut _;

    // update function pointer in data segment
    os::jit_writable();
    unsafe {
        *fct_addr = fct_ptr.to_usize();
    }
    os::jit_executable();

    fct_ptr
}
