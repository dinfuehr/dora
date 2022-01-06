use std::collections::HashSet;
use std::mem::size_of;

use crate::bytecode::{self, BytecodeBuilder, BytecodeFunction, BytecodeType, Register};
use crate::compiler;
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::fct::{Code, FctDescriptor, LazyCompilationSite};
use crate::compiler::map::CodeDescriptor;
use crate::cpu::{
    CCALL_REG_PARAMS, FREG_PARAMS, REG_FP, REG_PARAMS, REG_RESULT, REG_SP, REG_THREAD, REG_TMP1,
};
use crate::gc::Address;
use crate::masm::{MacroAssembler, Mem};
use crate::mem;
use crate::object::Obj;
use crate::os;
use crate::stack::DoraToNativeInfo;
use crate::threads::ThreadLocalData;
use crate::ty::{MachineMode, SourceType, SourceTypeArray};
use crate::vm::{
    find_trait_impl, get_vm, AnalysisData, ClassInstanceId, FctDefinition, FctDefinitionId,
    FctParent, TypeParam, TypeParamId, VM,
};

// This code generates the compiler stub, there should only be one instance
// of this function be used in Dora. It is necessary for lazy compilation, where
// functions are only compiled on their first invocation. The compiler can use
// the address of this stub for invocations of functions that have not been compiled
// yet. The stub compiles the function and patches the call site to invoke the
// now-compiled function directly on the next invocation. In the end the function is
// executed.

pub fn generate<'a>(vm: &'a VM) -> Address {
    let ngen = DoraCompileGen {
        vm,
        masm: MacroAssembler::new(),
        dbg: vm.args.flag_emit_debug_compile,
    };

    let code = ngen.generate();
    let addr = code.instruction_start();
    vm.insert_code_map(
        code.ptr_start(),
        code.ptr_end(),
        CodeDescriptor::CompileStub,
    );
    vm.code.push(code);

    addr
}

struct DoraCompileGen<'a> {
    vm: &'a VM,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a> DoraCompileGen<'a> {
    pub fn generate(mut self) -> Code {
        let offset_shadow_stack = 0;
        let offset_dtn = offset_shadow_stack
            + if cfg!(target_family = "windows") {
                32
            } else {
                0
            };
        let offset_params = offset_dtn + size_of::<DoraToNativeInfo>() as i32;
        let offset_thread =
            offset_params + (FREG_PARAMS.len() + REG_PARAMS.len()) as i32 * mem::ptr_width();
        let framesize = mem::align_i32(offset_thread + mem::ptr_width(), 16) as i32;

        if self.dbg {
            self.masm.debug();
        }

        // the return address is the call-site we need to patch
        self.masm.prolog_size(framesize);

        // store params passed in registers on the stack
        self.store_params(offset_params);

        // prepare the native call
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

        // invoke the compiler for the call site
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[0].into(),
            Mem::Base(REG_FP, mem::ptr_width()),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[1].into(),
            Mem::Base(REG_SP, offset_params),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[2].into(),
            Mem::Base(REG_SP, offset_params + mem::ptr_width()),
        );
        self.masm.raw_call(compile_request as *const u8);

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

        self.masm.copy_reg(MachineMode::Ptr, REG_TMP1, REG_RESULT);

        // restore argument registers from the stack
        self.load_params(offset_params);

        // remove the stack frame
        self.masm.epilog_without_return();

        // jump to compiled function
        self.masm.jump_reg(REG_TMP1);

        self.masm
            .code(self.vm, framesize, FctDescriptor::CompileStub)
    }

    fn store_params(&mut self, mut offset: i32) {
        for reg in &REG_PARAMS {
            self.masm
                .store_mem(MachineMode::Ptr, Mem::Base(REG_SP, offset), (*reg).into());
            offset += mem::ptr_width();
        }

        for reg in &FREG_PARAMS {
            self.masm.store_mem(
                MachineMode::Float64,
                Mem::Base(REG_SP, offset),
                (*reg).into(),
            );
            offset += mem::ptr_width();
        }
    }

    fn load_params(&mut self, mut offset: i32) {
        for reg in &REG_PARAMS {
            self.masm
                .load_mem(MachineMode::Ptr, (*reg).into(), Mem::Base(REG_SP, offset));
            offset += mem::ptr_width();
        }

        for reg in &FREG_PARAMS {
            self.masm.load_mem(
                MachineMode::Float64,
                (*reg).into(),
                Mem::Base(REG_SP, offset),
            );
            offset += mem::ptr_width();
        }
    }
}

fn compile_request(ra: usize, receiver1: Address, receiver2: Address) -> Address {
    let vm = get_vm();

    let lazy_compilation_site = {
        let data = {
            let code_map = vm.code_map.lock();
            code_map.get(ra.into()).expect("return address not found")
        };

        let fct_id = match data {
            CodeDescriptor::DoraFct(fct_id) => fct_id,
            _ => panic!("expected function for code"),
        };

        let code = vm.code.idx(fct_id);

        let offset = ra - code.instruction_start().to_usize();
        code.lazy_for_offset(offset as u32)
            .expect("lazy compilation site not found")
            .clone()
    };

    match lazy_compilation_site {
        LazyCompilationSite::Direct(fct_id, disp, ref type_params) => {
            patch_direct_call(vm, ra, fct_id, type_params, disp)
        }

        LazyCompilationSite::Virtual(receiver_is_first, fct_id, vtable_index, ref type_params) => {
            patch_virtual_call(
                vm,
                receiver_is_first,
                receiver1,
                receiver2,
                fct_id,
                vtable_index,
                type_params,
            )
        }
    }
}

fn patch_virtual_call(
    vm: &VM,
    receiver_is_first: bool,
    receiver1: Address,
    receiver2: Address,
    trait_fct_id: FctDefinitionId,
    vtable_index: u32,
    type_params: &SourceTypeArray,
) -> Address {
    let receiver = if receiver_is_first {
        receiver1
    } else {
        receiver2
    };

    let obj = unsafe { &mut *receiver.to_mut_ptr::<Obj>() };
    let vtable = obj.header().vtbl();
    let cls_def = vtable.class_def();

    let fct_ptr = if let Some(cls_id) = cls_def.cls_id {
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();

        let fct_id = cls.virtual_fcts[vtable_index as usize];
        compiler::generate(vm, fct_id, type_params)
    } else {
        let object_ty = cls_def.trait_object.clone().expect("trait object expected");
        let all_type_params = type_params.connect_single(object_ty.clone());
        let thunk_fct_id =
            ensure_thunk(vm, cls_def.id, trait_fct_id, type_params.clone(), object_ty);

        compiler::generate(vm, thunk_fct_id, &all_type_params)
    };

    let methodtable = vtable.table_mut();
    methodtable[vtable_index as usize] = fct_ptr.to_usize();

    fct_ptr
}

fn patch_direct_call(
    vm: &VM,
    ra: usize,
    fct_id: FctDefinitionId,
    type_params: &SourceTypeArray,
    disp: i32,
) -> Address {
    let fct_ptr = compiler::generate(vm, fct_id, type_params);
    let fct_addr: *mut usize = (ra as isize - disp as isize) as *mut _;

    // update function pointer in data segment
    os::jit_writable();
    unsafe {
        *fct_addr = fct_ptr.to_usize();
    }
    os::jit_executable();

    fct_ptr
}

fn ensure_thunk(
    vm: &VM,
    cls_def_id: ClassInstanceId,
    fct_id: FctDefinitionId,
    type_params: SourceTypeArray,
    object_ty: SourceType,
) -> FctDefinitionId {
    let fct = vm.fcts.idx(fct_id);
    let fct = fct.read();

    let trait_id = fct.parent.trait_id().expect("expected trait");

    let thunk_id = fct.thunk_id.write();

    if let Some(thunk_id) = thunk_id.clone() {
        return thunk_id;
    }

    let callee_id = find_trait_impl(vm, fct_id, trait_id, object_ty.clone());

    let mut thunk_fct =
        FctDefinition::new(vm, fct.file_id, fct.namespace_id, &fct.ast, FctParent::None);
    thunk_fct.type_params = fct.type_params.clone();
    let mut traits = HashSet::new();
    traits.insert(trait_id);
    thunk_fct.type_params.push(TypeParam {
        name: vm.interner.intern("new_self"),
        trait_bounds: traits,
    });
    thunk_fct.bytecode = Some(generate_bytecode_for_thunk(
        vm,
        cls_def_id,
        &*fct,
        &mut thunk_fct,
        callee_id,
        object_ty,
    ));
    thunk_fct.analysis = Some(AnalysisData::new());

    let list_id = vm.source_type_arrays.lock().insert(type_params);
    let mut param_types: Vec<SourceType> = vec![SourceType::Trait(trait_id, list_id)];
    param_types.extend_from_slice(fct.params_without_self());
    thunk_fct.param_types = param_types;
    thunk_fct.return_type = fct.return_type.clone();
    let thunk_fct_id = vm.add_fct(thunk_fct);

    {
        let thunk_fct = vm.fcts.idx(thunk_fct_id);
        let thunk_fct = thunk_fct.read();

        if should_emit_bytecode(vm, &*fct) {
            bytecode::dump(vm, Some(&*fct), thunk_fct.bytecode.as_ref().unwrap());
        }
    }

    thunk_fct_id
}

fn generate_bytecode_for_thunk(
    vm: &VM,
    cls_def_id: ClassInstanceId,
    trait_fct: &FctDefinition,
    thunk_fct: &mut FctDefinition,
    _callee_id: FctDefinitionId,
    object_ty: SourceType,
) -> BytecodeFunction {
    let mut gen = BytecodeBuilder::new(&vm.args);
    gen.push_scope();
    gen.alloc_var(BytecodeType::Ptr);

    for param_ty in trait_fct.params_without_self() {
        if !param_ty.is_unit() {
            let ty = BytecodeType::from_ty(vm, param_ty.clone());
            gen.alloc_var(ty);
        }
    }

    gen.set_arguments(trait_fct.params_with_self().len() as u32);

    if !object_ty.is_unit() {
        let ty = BytecodeType::from_ty(vm, object_ty.clone());
        let new_self_reg = gen.alloc_var(ty);
        let field_idx = gen.add_const_field_fixed(cls_def_id, 0.into());
        gen.emit_load_field(new_self_reg, Register(0), field_idx, trait_fct.pos);
        gen.emit_push_register(new_self_reg);
    }

    for (idx, _) in trait_fct.params_without_self().iter().enumerate() {
        gen.emit_push_register(Register(1 + idx));
    }

    let type_param_id = TypeParamId(thunk_fct.type_params.len() - 1);
    let target_fct_idx =
        gen.add_const_generic(type_param_id, trait_fct.id, SourceTypeArray::empty());

    if !trait_fct.return_type.is_unit() {
        let ty = BytecodeType::from_ty(vm, trait_fct.return_type.clone());
        let result_reg = gen.alloc_var(ty);
        gen.emit_invoke_generic_direct(result_reg, target_fct_idx, trait_fct.pos);
        gen.emit_ret(result_reg);
    } else {
        gen.emit_invoke_generic_direct_void(target_fct_idx, trait_fct.pos);
        gen.emit_ret_void();
    }

    gen.pop_scope();
    gen.generate(vm)
}
