use num_enum::{IntoPrimitive, TryFromPrimitive};
use parking_lot::RwLock;
use std::collections::HashMap;
use std::mem;
use std::ptr;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Arc;

use crate::compiler;
use crate::gc::{Address, Gc};
use crate::stack::DoraToNativeInfo;
use crate::threads::ManagedThread;
use crate::threads::{
    current_thread, deinit_current_thread, init_current_thread, DoraThread, ThreadState, Threads,
    STACK_SIZE,
};
use crate::utils::GrowableVecNonIter;
use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ClassId, EnumId, FunctionId, ModuleId, Program, StructId,
    TraitId,
};

pub use self::classes::{
    create_class_instance_with_vtable, ClassInstance, ClassInstanceId, FieldInstance, ShapeKind,
};
pub use self::code::{
    install_code, install_code_stub, Code, CodeDescriptor, CodeId, CodeKind, CodeObjects,
    CommentTable, ConstPool, ConstPoolEntry, ConstPoolValue, GcPoint, GcPointTable,
    LazyCompilationData, LazyCompilationSite, LocationTable, ManagedCodeHeader, RelocationTable,
    CODE_ALIGNMENT,
};
pub use self::code_map::CodeMap;
pub use self::compilation::CompilationDatabase;
pub use self::enums::{enum_definition_name, EnumInstance, EnumInstanceId, EnumLayout};
pub use self::extensions::block_matches_ty;
pub use self::flags::{CollectorName, CompilerName, Flags, MemSize};
pub use self::functions::display_fct;
use self::globals::GlobalVariableMemory;
pub use self::globals::{INITIALIZED, RUNNING, UNINITIALIZED};
pub use self::impls::{bounds_for_tp, find_trait_impl, tp_implements_trait, ty_implements_trait};
use self::known::KnownElements;
pub use self::modules::{module_path, module_path_name};
pub use self::natives::{setup_builtin_natives, NativeMethods};
pub use self::specialize::{
    add_ref_fields, create_class_instance, create_enum_instance, create_struct_instance,
    ensure_class_instance_for_enum_variant, ensure_class_instance_for_lambda,
    ensure_class_instance_for_trait_object, specialize_bty, specialize_bty_array,
};
pub use self::structs::{StructInstance, StructInstanceField, StructInstanceId};
pub use self::tuples::{get_concrete_tuple_bty, get_concrete_tuple_bty_array, ConcreteTuple};
pub use self::ty::{display_ty, BytecodeTypeExt};
pub use self::waitlists::{ManagedCondition, ManagedMutex, WaitLists};

mod classes;
mod code;
mod code_map;
mod compilation;
mod enums;
mod extensions;
mod flags;
mod functions;
mod globals;
mod impls;
mod initialize;
mod known;
mod modules;
mod natives;
mod specialize;
mod stdlib;
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
    pub flags: Flags,
    pub program_args: Vec<String>,
    pub program: Program,
    pub known: KnownElements,
    pub struct_specializations: RwLock<HashMap<(StructId, BytecodeTypeArray), StructInstanceId>>,
    pub struct_instances: GrowableVecNonIter<StructInstance>, // stores all struct definitions
    pub class_specializations: RwLock<HashMap<(ClassId, BytecodeTypeArray), ClassInstanceId>>,
    pub class_instances: GrowableVecNonIter<ClassInstance>, // stores all class definitions
    pub code_objects: CodeObjects,
    pub compilation_database: CompilationDatabase,
    pub enum_specializations: RwLock<HashMap<(EnumId, BytecodeTypeArray), EnumInstanceId>>,
    pub enum_instances: GrowableVecNonIter<EnumInstance>, // stores all enum definitions
    pub trait_vtables: RwLock<HashMap<(TraitId, BytecodeTypeArray), ClassInstanceId>>,
    pub code_map: CodeMap, // stores all compiled functions
    pub global_variable_memory: Option<GlobalVariableMemory>,
    pub gc: Gc, // garbage collector
    pub native_methods: NativeMethods,
    pub threads: Threads,
    pub wait_lists: WaitLists,
    pub state: AtomicU8,
}

impl VM {
    pub fn new(program: Program, args: Flags, program_args: Vec<String>) -> Box<VM> {
        let gc = Gc::new(&args);

        let mut vm = Box::new(VM {
            flags: args,
            program_args,
            program,
            struct_specializations: RwLock::new(HashMap::new()),
            struct_instances: GrowableVecNonIter::new(),
            class_specializations: RwLock::new(HashMap::new()),
            class_instances: GrowableVecNonIter::new(),
            enum_specializations: RwLock::new(HashMap::new()),
            enum_instances: GrowableVecNonIter::new(),
            trait_vtables: RwLock::new(HashMap::new()),
            global_variable_memory: None,
            known: KnownElements::new(),
            gc,
            compilation_database: CompilationDatabase::new(),
            code_objects: CodeObjects::new(),
            code_map: CodeMap::new(),
            native_methods: NativeMethods::new(),
            threads: Threads::new(),
            wait_lists: WaitLists::new(),
            state: AtomicU8::new(VmState::Running.into()),
        });

        vm.setup();

        vm
    }

    pub fn state(&self) -> VmState {
        let state = self.state.load(Ordering::Relaxed);
        VmState::try_from(state).expect("invalid state")
    }

    pub fn set_state(&self, new_state: VmState) -> VmState {
        let old_state = self.state.swap(new_state.into(), Ordering::Relaxed);
        VmState::try_from(old_state).expect("invalid state")
    }

    fn setup(&mut self) {
        // ensure this data is only created during execution
        assert!(self.compilation_database.is_empty());

        initialize::setup(self);

        globals::init_global_addresses(self);
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
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.native_methods.dora_entry_trampoline();
        let fct: extern "C" fn(Address, Address) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr);
    }

    pub fn ensure_compiled(&self, fct_id: FunctionId) -> Address {
        let mut dtn = DoraToNativeInfo::new();
        let type_params = BytecodeTypeArray::empty();

        current_thread().use_dtn(&mut dtn, || {
            compiler::generate_fct(self, fct_id, &type_params)
        })
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
        let pkg_id = self.program.program_package_id.0 as usize;
        let pkg = &self.program.packages[pkg_id];
        pkg.root_module_id
    }

    pub fn byte_array(&self) -> ClassInstanceId {
        let mut byte_array_def = self.known.byte_array_class_instance_id.lock();

        if let Some(cls_id) = *byte_array_def {
            cls_id
        } else {
            let type_args = BytecodeTypeArray::one(BytecodeType::UInt8);
            let cls_id = create_class_instance(self, self.known.array_class_id(), &type_args);
            *byte_array_def = Some(cls_id);
            cls_id
        }
    }

    pub fn int_array(&self) -> ClassInstanceId {
        let mut int_array_def = self.known.int_array_class_instance_id.lock();

        if let Some(cls_id) = *int_array_def {
            cls_id
        } else {
            let type_args = BytecodeTypeArray::one(BytecodeType::Int32);
            let cls_id = create_class_instance(self, self.known.array_class_id(), &type_args);
            *int_array_def = Some(cls_id);
            cls_id
        }
    }

    pub fn str(&self) -> ClassInstanceId {
        let mut str_class_def = self.known.string_class_instance_id.lock();

        if let Some(cls_id) = *str_class_def {
            cls_id
        } else {
            let cls_id = create_class_instance(
                self,
                self.known.string_class_id(),
                &BytecodeTypeArray::empty(),
            );
            *str_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn stack_trace_element(&self) -> ClassInstanceId {
        let mut ste_class_def = self.known.ste_class_instance_id.lock();

        if let Some(cls_id) = *ste_class_def {
            cls_id
        } else {
            let cls_id = create_class_instance(
                self,
                self.known.stacktrace_element_class_id(),
                &BytecodeTypeArray::empty(),
            );
            *ste_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn thread_class_instance(&self) -> ClassInstanceId {
        create_class_instance(
            self,
            self.known.thread_class_id(),
            &BytecodeTypeArray::empty(),
        )
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        self.gc.drop_all_native_code_objects();
    }
}

unsafe impl Sync for VM {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

impl Trap {
    pub fn int(self) -> u32 {
        match self {
            Trap::DIV0 => 1,
            Trap::ASSERT => 2,
            Trap::INDEX_OUT_OF_BOUNDS => 3,
            Trap::NIL => 4,
            Trap::CAST => 5,
            Trap::OOM => 6,
            Trap::STACK_OVERFLOW => 7,
            Trap::ILLEGAL => 8,
            Trap::OVERFLOW => 9,
        }
    }

    pub fn from(value: u32) -> Option<Trap> {
        match value {
            1 => Some(Trap::DIV0),
            2 => Some(Trap::ASSERT),
            3 => Some(Trap::INDEX_OUT_OF_BOUNDS),
            4 => Some(Trap::NIL),
            5 => Some(Trap::CAST),
            6 => Some(Trap::OOM),
            7 => Some(Trap::STACK_OVERFLOW),
            8 => Some(Trap::ILLEGAL),
            9 => Some(Trap::OVERFLOW),
            _ => None,
        }
    }
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
