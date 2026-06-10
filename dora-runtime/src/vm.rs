use dora_bytecode::{
    ClassData, ConstData, ConstId, EnumData, ExtensionData, ExtensionId, FunctionData, ImplData,
    ImplId, ModuleData, SourceFileData, SourceFileId, StructData, TraitData,
};
use dora_bytecode::{GlobalData, GlobalId};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::collections::HashMap;
use std::ptr;
use std::sync::atomic::{AtomicU8, Ordering};
use std::time::Instant;

use crate::Shape;
use crate::gc::{Address, Gc};
use crate::threads::ManagedThread;
use crate::threads::{
    DoraThread, STACK_SIZE, Threads, current_thread, deinit_current_thread, init_current_thread,
};

use dora_bytecode::{
    AliasData, AliasId, ClassId, EnumId, FunctionId, ModuleId, Program, StructId, TraitId,
};
pub use dora_compiler::{
    AotShapeKey, CodeDescriptor, CollectorName, CommentTable, FieldInstance, GcPoint, GcPointTable,
    InlinedFunction, InlinedFunctionId, InlinedLocation, LocationTable, RelocationKind,
    RelocationTable, RuntimeFunction, ShapeKind, TargetArch, ThreadState, Trap,
};

pub use self::code::{
    CODE_ALIGNMENT, Code, CodeId, CodeKind, CodeMap, FunctionInfoAot, InlinedFunctionAot,
    install_external_code_stub,
};
pub use self::extensions::{block_matches_ty, block_matches_ty_in_program};
pub use self::flags::{Compiler, MemSize, VmFlags, parse_collector, parse_target_arch};
pub use self::globals::GlobalVariableMemory;
pub use self::globals::{INITIALIZED, RUNNING, UNINITIALIZED};
pub use self::impls::{
    bounds_for_tp, find_impl, find_impl_in_program, find_trait_impl, find_trait_impl_in_program,
    find_trait_ty_impl, find_trait_ty_impl_in_program, tp_implements_trait, ty_implements_trait,
    ty_implements_trait_in_program,
};
pub use self::known::Intrinsic;
use self::known::KnownElements;
pub use self::specialize::{
    specialize_bty, specialize_bty_array, specialize_bty_for_trait_object,
    specialize_trait_ty_in_program, specialize_ty_array_in_program, specialize_ty_in_program,
};
pub use self::ty::BytecodeTypeExt;
pub use self::waitlists::{ManagedCondition, ManagedMutex, WaitLists};

mod code;
mod extensions;
mod flags;
mod globals;
pub mod impls;
mod known;
mod specialize;
mod ty;
mod waitlists;

static mut VM_GLOBAL: *const u8 = ptr::null();

pub fn get_vm() -> &'static VM {
    unsafe {
        debug_assert!(!VM_GLOBAL.is_null());
        &*(VM_GLOBAL as *const VM)
    }
}

pub fn set_vm(vm: &VM) {
    unsafe {
        debug_assert!(VM_GLOBAL.is_null());
        VM_GLOBAL = vm as *const _ as *const u8;
    }
}

pub fn clear_vm() {
    unsafe {
        debug_assert!(!VM_GLOBAL.is_null());
        VM_GLOBAL = ptr::null();
    }
}

#[inline(never)]
pub fn stack_pointer() -> Address {
    let local: i32 = 0;
    Address::from_ptr(&local as *const i32)
}

#[derive(TryFromPrimitive, IntoPrimitive, PartialEq, Eq, Copy, Clone, Debug)]
#[repr(u8)]
pub enum VmState {
    Running,
    Safepoint,
}

impl VmState {
    pub fn in_running(&self) -> bool {
        match self {
            VmState::Running => true,
            _ => false,
        }
    }

    pub fn in_safepoint(&self) -> bool {
        match self {
            VmState::Safepoint => true,
            _ => false,
        }
    }
}

pub struct VM {
    pub flags: VmFlags,
    pub program_args: Vec<String>,
    pub program: Program,
    pub known: KnownElements,
    pub code_map: CodeMap, // stores all compiled functions
    pub global_variable_memory: Option<GlobalVariableMemory>,
    pub gc: Gc, // garbage collector
    shape_base: Address,
    shape_size: usize,
    pub dora_entry_trampoline: Option<Address>,
    pub intrinsics: HashMap<FunctionId, Intrinsic>,
    pub threads: Threads,
    pub wait_lists: WaitLists,
    pub state: AtomicU8,
    pub startup_time: Instant,
}

impl VM {
    pub fn new(program: Program, flags: VmFlags, program_args: Vec<String>) -> Box<VM> {
        let gc = Gc::new(&flags);

        let vm = Box::new(VM {
            flags,
            program_args,
            program,
            global_variable_memory: None,
            known: KnownElements::new(),
            gc,
            shape_base: Address::null(),
            shape_size: 0,
            code_map: CodeMap::new(),
            dora_entry_trampoline: None,
            intrinsics: HashMap::new(),
            threads: Threads::new(),
            wait_lists: WaitLists::new(),
            state: AtomicU8::new(VmState::Running.into()),
            startup_time: Instant::now(),
        });

        vm
    }

    pub fn shutdown(&self) {
        self.gc.shutdown(self);
    }

    pub fn boots_compile_fct_address(&self) -> *const u8 {
        self.known.boots_compile_fct_address().to_ptr()
    }

    pub fn state(&self) -> VmState {
        let state = self.state.load(Ordering::Relaxed);
        VmState::try_from(state).expect("invalid state")
    }

    pub fn set_state(&self, new_state: VmState) -> VmState {
        let old_state = self.state.swap(new_state.into(), Ordering::Relaxed);
        VmState::try_from(old_state).expect("invalid state")
    }

    pub fn startup_time(&self) -> Instant {
        self.startup_time.clone()
    }

    pub fn gc_epoch(&self) -> usize {
        self.gc.epoch()
    }

    pub fn dump_gc_summary(&self, runtime: f32) {
        self.gc.dump_summary(runtime);
    }

    pub fn add_code(&mut self, code: Code) -> CodeId {
        self.code_map.add(code)
    }

    pub fn program_module_id(&self) -> ModuleId {
        self.program.program_module_id()
    }

    pub fn set_shape_space(&mut self, base: Address, size: usize) {
        assert!(base.is_non_null());
        assert_eq!(
            base.to_usize() % std::mem::align_of::<Shape>(),
            0,
            "shape space base must be Shape-aligned"
        );
        assert!(size > 0);
        self.shape_base = base;
        self.shape_size = size;
    }

    pub fn shape_base(&self) -> Address {
        assert!(self.shape_base.is_non_null(), "shape space not initialized");
        self.shape_base
    }

    pub fn shape_size(&self) -> usize {
        assert!(self.shape_size > 0, "shape space not initialized");
        self.shape_size
    }

    pub fn alias(&self, id: AliasId) -> &AliasData {
        self.program.alias(id)
    }

    pub fn class(&self, id: ClassId) -> &ClassData {
        self.program.class(id)
    }

    pub fn const_(&self, id: ConstId) -> &ConstData {
        self.program.const_(id)
    }

    pub fn enum_(&self, id: EnumId) -> &EnumData {
        self.program.enum_(id)
    }

    pub fn extension(&self, id: ExtensionId) -> &ExtensionData {
        self.program.extension(id)
    }

    pub fn fct(&self, id: FunctionId) -> &FunctionData {
        self.program.fct(id)
    }

    pub fn file(&self, id: SourceFileId) -> &SourceFileData {
        self.program.file(id)
    }

    pub fn global(&self, id: GlobalId) -> &GlobalData {
        self.program.global(id)
    }

    pub fn impl_(&self, id: ImplId) -> &ImplData {
        self.program.impl_(id)
    }

    pub fn module(&self, id: ModuleId) -> &ModuleData {
        self.program.module(id)
    }

    pub fn struct_(&self, id: StructId) -> &StructData {
        self.program.struct_(id)
    }

    pub fn trait_(&self, id: TraitId) -> &TraitData {
        self.program.trait_(id)
    }
}

unsafe impl Sync for VM {}

pub fn execute_on_main<F, R>(callback: F) -> R
where
    F: FnOnce() -> R,
{
    let vm = get_vm();

    let native_thread = DoraThread::new(vm, ThreadState::Running);
    init_current_thread(native_thread.clone());

    vm.threads.add_main_thread(native_thread.clone());

    let mut managed_thread = ManagedThread::alloc(vm);
    managed_thread.install_native_thread(&native_thread);

    let managed_thread_handle = native_thread.handles.create_handle(managed_thread);

    native_thread
        .tld
        .set_managed_thread_handle(managed_thread_handle.location());

    let stack_top = stack_pointer();
    let stack_limit = stack_top.sub(STACK_SIZE);

    let thread = current_thread();
    thread.tld.set_stack_limit(stack_limit);

    let result = callback();

    vm.threads.remove_current_thread();
    deinit_current_thread();

    result
}
