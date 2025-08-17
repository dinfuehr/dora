use dora_bytecode::{
    ClassData, ConstData, ConstId, EnumData, ExtensionData, ExtensionId, FunctionData, ImplData,
    ImplId, ModuleData, SourceFileData, SourceFileId, StructData, TraitData,
};
use dora_bytecode::{GlobalData, GlobalId};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use parking_lot::RwLock;
use std::collections::HashMap;
use std::mem;
use std::ptr;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::Instant;

use crate::Shape;
use crate::compiler;
use crate::gc::{Address, Gc};
use crate::mirror::Str;
use crate::threads::ManagedThread;
use crate::threads::{
    DoraThread, STACK_SIZE, ThreadState, Threads, current_thread, deinit_current_thread,
    init_current_thread,
};
use crate::utils::GrowableVecNonIter;

use dora_bytecode::{
    AliasData, AliasId, BytecodeType, BytecodeTypeArray, ClassId, EnumId, FunctionId, ModuleId,
    Program, StructId, TraitId,
};

pub use self::classes::{FieldInstance, ShapeKind, create_shape};
pub use self::code::{
    CODE_ALIGNMENT, Code, CodeDescriptor, CodeId, CodeKind, CodeObjects, CommentTable, GcPoint,
    GcPointTable, InlinedFunction, InlinedFunctionId, InlinedLocation, LazyCompilationData,
    LazyCompilationSite, LocationTable, ManagedCodeHeader, RelocationKind, RelocationTable,
    install_code, install_code_stub,
};
pub use self::code_map::CodeMap;
pub use self::compilation::CompilationDatabase;
pub use self::enums::{EnumInstance, EnumInstanceId, EnumLayout, enum_definition_name};
pub use self::extensions::block_matches_ty;
pub use self::flags::{CollectorName, Compiler, MemSize, VmFlags};
use self::globals::GlobalVariableMemory;
pub use self::globals::{INITIALIZED, RUNNING, UNINITIALIZED};
pub use self::impls::{
    bounds_for_tp, find_impl, find_trait_impl, find_trait_ty_impl, tp_implements_trait,
    ty_implements_trait,
};
pub use self::known::Intrinsic;
use self::known::KnownElements;
pub use self::natives::{NativeMethods, setup_builtin_natives};
pub use self::specialize::{
    add_ref_fields, compute_vtable_index, create_enum_instance, create_shape_for_class,
    create_struct_instance, ensure_shape_for_enum_variant, ensure_shape_for_lambda,
    ensure_shape_for_trait_object, specialize_bty, specialize_bty_array,
    specialize_bty_for_trait_object, specialize_ty, specialize_ty_array,
};
pub use self::stdlib_lookup::FctImplementation;
pub use self::structs::{StructInstance, StructInstanceField, StructInstanceId};
pub use self::tuples::{ConcreteTuple, get_concrete_tuple_bty, get_concrete_tuple_bty_array};
pub use self::ty::BytecodeTypeExt;
pub use self::waitlists::{ManagedCondition, ManagedMutex, WaitLists};

mod classes;
mod code;
mod code_map;
mod compilation;
mod enums;
mod extensions;
mod flags;
mod globals;
pub mod impls;
mod initialize;
mod known;
mod natives;
mod specialize;
mod stdlib_lookup;
mod structs;
mod tuples;
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
    pub struct_specializations: RwLock<HashMap<(StructId, BytecodeTypeArray), StructInstanceId>>,
    pub struct_instances: GrowableVecNonIter<StructInstance>, // stores all struct definitions
    pub class_shapes: RwLock<HashMap<(ClassId, BytecodeTypeArray), *const Shape>>,
    pub code_objects: CodeObjects,
    pub compilation_database: CompilationDatabase,
    pub enum_specializations: RwLock<HashMap<(EnumId, BytecodeTypeArray), EnumInstanceId>>,
    pub enum_instances: GrowableVecNonIter<EnumInstance>, // stores all enum definitions
    pub trait_shapes: RwLock<HashMap<(BytecodeType, BytecodeType), *const Shape>>,
    pub lambda_shapes: RwLock<HashMap<(FunctionId, BytecodeTypeArray), *const Shape>>,
    pub code_map: CodeMap, // stores all compiled functions
    pub global_variable_memory: Option<GlobalVariableMemory>,
    pub gc: Gc, // garbage collector
    pub native_methods: NativeMethods,
    pub intrinsics: HashMap<FunctionId, Intrinsic>,
    pub threads: Threads,
    pub wait_lists: WaitLists,
    pub state: AtomicU8,
    pub startup_time: OnceLock<Instant>,
    pub string_table: RwLock<HashMap<String, Address>>,
}

impl VM {
    pub fn new(program: Program, flags: VmFlags, program_args: Vec<String>) -> Box<VM> {
        let gc = Gc::new(&flags);

        let mut vm = Box::new(VM {
            flags,
            program_args,
            program,
            struct_specializations: RwLock::new(HashMap::new()),
            struct_instances: GrowableVecNonIter::new(),
            class_shapes: RwLock::new(HashMap::new()),
            enum_specializations: RwLock::new(HashMap::new()),
            enum_instances: GrowableVecNonIter::new(),
            trait_shapes: RwLock::new(HashMap::new()),
            lambda_shapes: RwLock::new(HashMap::new()),
            global_variable_memory: None,
            known: KnownElements::new(),
            gc,
            compilation_database: CompilationDatabase::new(),
            code_objects: CodeObjects::new(),
            code_map: CodeMap::new(),
            native_methods: NativeMethods::new(),
            intrinsics: HashMap::new(),
            threads: Threads::new(),
            wait_lists: WaitLists::new(),
            state: AtomicU8::new(VmState::Running.into()),
            startup_time: OnceLock::new(),
            string_table: RwLock::new(HashMap::new()),
        });

        vm.setup();

        vm
    }

    pub fn shutdown(&self) {
        self.gc.shutdown(self);
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

    pub fn internalize_string_constant(&self, str: &str) -> Address {
        let mut lock = self.string_table.write();
        if let Some(address) = lock.get(str) {
            *address
        } else {
            let handle = Str::from_buffer_in_perm(self, str.as_bytes());
            let address = handle.address();
            lock.insert(str.to_owned(), address);
            address
        }
    }

    fn setup(&mut self) {
        // ensure this data is only created during execution
        assert!(self.compilation_database.is_empty());

        self.startup_time
            .set(Instant::now())
            .expect("already initialized");

        initialize::setup(self);

        globals::init_global_addresses(self);

        self.gc.setup(self);
    }

    pub fn gc_epoch(&self) -> usize {
        self.gc.epoch()
    }

    pub fn run(&self, fct_id: FunctionId) -> i32 {
        let tld = current_thread().tld_address();
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.native_methods.dora_entry_trampoline();
        let fct: extern "C" fn(Address, Address) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr)
    }

    pub fn run_test(&self, fct_id: FunctionId) {
        let tld = current_thread().tld_address();
        let address = self
            .known
            .boots_test_addresses
            .get()
            .expect("missing tests")
            .get(&fct_id)
            .cloned()
            .unwrap_or_else(|| self.ensure_compiled(fct_id));
        let dora_stub_address = self.native_methods.dora_entry_trampoline();
        let fct: extern "C" fn(Address, Address) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, address);
    }

    pub fn ensure_compiled(&self, fct_id: FunctionId) -> Address {
        let type_params = BytecodeTypeArray::empty();
        compiler::compile_fct_jit(self, fct_id, &type_params)
    }

    pub fn compile_boots_aot(&self) {
        compiler::aot::compile_boots_aot(self);
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

    pub fn shape_for_class(&self, class_id: ClassId, type_params: &BytecodeTypeArray) -> &Shape {
        let shape = create_shape_for_class(self, class_id, type_params);
        unsafe { &*shape }
    }

    pub fn shape_for_enum_variant(
        &self,
        enum_instance: &EnumInstance,
        enum_: &EnumData,
        variant_id: u32,
    ) -> &Shape {
        let shape = ensure_shape_for_enum_variant(self, enum_instance, enum_, variant_id);
        unsafe { &*shape }
    }

    pub fn shape_for_lambda(&self, fct_id: FunctionId, type_params: BytecodeTypeArray) -> &Shape {
        let shape = ensure_shape_for_lambda(self, fct_id, type_params);
        unsafe { &*shape }
    }

    pub fn shape_for_trait_object(
        &self,
        trait_ty: BytecodeType,
        actual_object_ty: BytecodeType,
    ) -> &Shape {
        let shape = ensure_shape_for_trait_object(self, trait_ty, actual_object_ty);
        unsafe { &*shape }
    }

    pub fn alias(&self, id: AliasId) -> &AliasData {
        &self.program.aliases[id.0 as usize]
    }

    pub fn class(&self, id: ClassId) -> &ClassData {
        &self.program.classes[id.0 as usize]
    }

    pub fn const_(&self, id: ConstId) -> &ConstData {
        &self.program.consts[id.0 as usize]
    }

    pub fn enum_(&self, id: EnumId) -> &EnumData {
        &self.program.enums[id.0 as usize]
    }

    pub fn extension(&self, id: ExtensionId) -> &ExtensionData {
        &self.program.extensions[id.0 as usize]
    }

    pub fn fct(&self, id: FunctionId) -> &FunctionData {
        &self.program.functions[id.0 as usize]
    }

    pub fn file(&self, id: SourceFileId) -> &SourceFileData {
        &self.program.source_files[id.0 as usize]
    }

    pub fn global(&self, id: GlobalId) -> &GlobalData {
        &self.program.globals[id.0 as usize]
    }

    pub fn impl_(&self, id: ImplId) -> &ImplData {
        &self.program.impls[id.0 as usize]
    }

    pub fn module(&self, id: ModuleId) -> &ModuleData {
        &self.program.modules[id.0 as usize]
    }

    pub fn struct_(&self, id: StructId) -> &StructData {
        &self.program.structs[id.0 as usize]
    }

    pub fn trait_(&self, id: TraitId) -> &TraitData {
        &self.program.traits[id.0 as usize]
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
