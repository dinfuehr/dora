use dora_bytecode::{
    ClassData, ConstData, ConstId, EnumData, ExtensionData, ExtensionId, FunctionData, ImplData,
    ImplId, ModuleData, SourceFileData, SourceFileId, StructData, TraitData,
};
use dora_bytecode::{GlobalData, GlobalId};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::collections::HashMap;
use std::ptr;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::Instant;

use crate::gc::{Address, Gc};
use crate::threads::ManagedThread;
use crate::threads::{
    DoraThread, STACK_SIZE, ThreadState, Threads, current_thread, deinit_current_thread,
    init_current_thread,
};

use dora_bytecode::{
    AliasData, AliasId, ClassId, EnumId, FunctionId, ModuleId, Program, StructId, TraitId,
};

pub use self::classes::{FieldInstance, ShapeKind};
pub use self::code::{
    AotShapeKey, CODE_ALIGNMENT, Code, CodeDescriptor, CodeId, CodeKind, CodeObjects, CommentTable,
    FunctionInfoAot, GcPoint, GcPointTable, InlinedFunction, InlinedFunctionAot, InlinedFunctionId,
    InlinedLocation, LocationTable, ManagedCodeHeader, RelocationKind, RelocationTable,
    RuntimeFunction, install_code, install_code_stub, install_external_code_stub,
};
pub use self::code_map::CodeMap;
pub use self::extensions::{block_matches_ty, block_matches_ty_in_program};
pub use self::flags::{
    CollectorName, Compiler, MemSize, TargetArch, VmFlags, parse_collector, parse_target_arch,
};
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
    specialize_bty, specialize_bty_array, specialize_bty_for_trait_object, specialize_trait_ty,
    specialize_trait_ty_in_program, specialize_ty, specialize_ty_array,
    specialize_ty_array_in_program, specialize_ty_in_program,
};
pub(crate) use self::stdlib_lookup::native_function_symbol;
pub use self::ty::BytecodeTypeExt;
pub use self::waitlists::{ManagedCondition, ManagedMutex, WaitLists};

mod classes;
mod code;
mod code_map;
mod extensions;
mod flags;
mod globals;
pub mod impls;
mod known;
mod specialize;
mod stdlib_lookup;
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
    pub code_objects: CodeObjects,
    pub code_map: CodeMap, // stores all compiled functions
    pub global_variable_memory: Option<GlobalVariableMemory>,
    pub gc: Gc, // garbage collector
    pub dora_entry_trampoline: Option<Address>,
    pub intrinsics: HashMap<FunctionId, Intrinsic>,
    pub threads: Threads,
    pub wait_lists: WaitLists,
    pub state: AtomicU8,
    pub startup_time: OnceLock<Instant>,
}

impl VM {
    pub fn new(program: Program, flags: VmFlags, program_args: Vec<String>) -> Box<VM> {
        let gc = Gc::new(&flags);

        let mut vm = Box::new(VM {
            flags,
            program_args,
            program,
            global_variable_memory: None,
            known: KnownElements::new(),
            gc,
            code_objects: CodeObjects::new(),
            code_map: CodeMap::new(),
            dora_entry_trampoline: None,
            intrinsics: HashMap::new(),
            threads: Threads::new(),
            wait_lists: WaitLists::new(),
            state: AtomicU8::new(VmState::Running.into()),
            startup_time: OnceLock::new(),
        });

        vm.setup();

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
        self.startup_time.get().expect("time missing").clone()
    }

    fn setup(&mut self) {
        self.startup_time
            .set(Instant::now())
            .expect("already initialized");
    }

    pub fn gc_epoch(&self) -> usize {
        self.gc.epoch()
    }

    pub fn dump_gc_summary(&self, runtime: f32) {
        self.gc.dump_summary(runtime);
    }

    pub fn add_code(&self, code: Arc<Code>) -> CodeId {
        let code_start = code.object_start();
        let code_end = code.object_end();

        let code_id = self.code_objects.add(code);

        self.code_map.insert(code_start, code_end, code_id);

        code_id
    }

    pub fn program_module_id(&self) -> ModuleId {
        self.program.program_module_id()
    }

    pub fn meta_space_start(&self) -> Address {
        self.gc.meta_space_start()
    }

    pub fn meta_space_size(&self) -> usize {
        self.gc.meta_space_size()
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

    pub fn has_boots(&self) -> bool {
        self.program.boots_package_id.is_some()
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        self.gc
            .drop_all_native_code_objects(self.meta_space_start());
    }
}

unsafe impl Sync for VM {}

#[derive(Copy, Clone, Debug, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Trap {
    DIV0,
    ASSERT,
    INDEX_OUT_OF_BOUNDS,
    NIL,
    CAST,
    OOM,
    STACK_OVERFLOW,
    ILLEGAL,
    OVERFLOW,
    SHIFT,
}

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
