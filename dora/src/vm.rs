use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::mem;
use std::ptr;
use std::sync::Arc;

use crate::compiler;
use crate::compiler::dora_exit_stubs::NativeStubs;
use crate::driver::cmd::Args;
use crate::gc::{Address, Gc};
use crate::language::error::diag::Diagnostic;
use crate::language::sem_analysis::{
    AnnotationDefinition, AnnotationDefinitionId, ClassDefinition, ClassDefinitionId,
    ConstDefinition, EnumDefinition, EnumDefinitionId, ExtensionDefinition, FctDefinition,
    FctDefinitionId, GlobalDefinition, ImplDefinition, ModuleDefinition, ModuleDefinitionId,
    PackageDefinition, PackageDefinitionId, SourceFile, StructDefinition, StructDefinitionId,
    TraitDefinition, TraitDefinitionId, UseDefinition,
};
use crate::language::ty::SourceTypeArray;
use crate::stack::DoraToNativeInfo;
use crate::threads::ManagedThread;
use crate::threads::{
    current_thread, deinit_current_thread, init_current_thread, DoraThread, ThreadState, Threads,
    STACK_SIZE,
};
use crate::utils::GrowableVecNonIter;
use crate::utils::{GrowableVec, MutableVec};

use dora_parser::interner::*;

pub use self::classes::{
    create_class_instance_with_vtable, ClassInstance, ClassInstanceId, FieldInstance, ShapeKind,
};
pub use self::code::{
    install_code, install_code_stub, Code, CodeId, CodeKind, CodeObjects, CommentTable, GcPoint,
    GcPointTable, LazyCompilationData, LazyCompilationSite, ManagedCodeHeader, PositionTable,
    RelocationTable, CODE_ALIGNMENT,
};
pub use self::code_map::CodeMap;
pub use self::compilation::CompilationDatabase;
pub use self::enums::{EnumInstance, EnumInstanceId, EnumLayout};
use self::globals::GlobalVariableMemory;
pub use self::known::{
    KnownAnnotations, KnownClasses, KnownElements, KnownEnums, KnownFunctions, KnownStructs,
    KnownTraits,
};
pub use self::specialize::{
    add_ref_fields, replace_type_param, specialize_class_id, specialize_class_id_params,
    specialize_enum_class, specialize_enum_id_params, specialize_lambda,
    specialize_struct_id_params, specialize_trait_object, specialize_tuple_array,
    specialize_tuple_bty, specialize_tuple_ty, specialize_type, specialize_type_list,
};
pub use self::structs::{StructInstance, StructInstanceField, StructInstanceId};
pub use self::stubs::{setup_stubs, Stubs};
pub use self::tuples::{
    get_concrete_tuple_array, get_concrete_tuple_bytecode_ty, get_concrete_tuple_ty, ConcreteTuple,
};
pub use self::waitlists::{ManagedCondition, ManagedMutex, WaitLists};

mod classes;
mod code;
mod code_map;
mod compilation;
mod enums;
mod globals;
mod initialize;
mod known;
mod specialize;
mod structs;
mod stubs;
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

pub struct FullSemAnalysis {
    pub args: Args,
    pub test_file_as_string: Option<&'static str>,
    pub interner: Interner,
    pub source_files: Vec<SourceFile>,
    pub diag: Mutex<Diagnostic>,
    pub known: KnownElements,
    pub consts: MutableVec<ConstDefinition>, // stores all const definitions
    pub structs: MutableVec<StructDefinition>, // stores all struct source definitions
    pub classes: MutableVec<ClassDefinition>, // stores all class source definitions
    pub extensions: MutableVec<ExtensionDefinition>, // stores all extension definitions
    pub annotations: MutableVec<AnnotationDefinition>, // stores all annotation source definitions
    pub modules: MutableVec<ModuleDefinition>, // stores all module definitions
    pub fcts: GrowableVec<RwLock<FctDefinition>>, // stores all function source definitions
    pub enums: MutableVec<EnumDefinition>,   // stores all enum source definitions
    pub traits: MutableVec<TraitDefinition>, // stores all trait definitions
    pub impls: MutableVec<ImplDefinition>,   // stores all impl definitions
    pub globals: MutableVec<GlobalDefinition>, // stores all global variables
    pub uses: Vec<UseDefinition>,            // stores all uses
    pub native_stubs: Mutex<NativeStubs>,
    pub packages: MutableVec<PackageDefinition>,
    pub package_names: HashMap<Name, PackageDefinitionId>,
    pub prelude_module_id: Option<ModuleDefinitionId>,
    pub stdlib_module_id: Option<ModuleDefinitionId>,
    pub program_module_id: Option<ModuleDefinitionId>,
    pub stdlib_package_id: Option<PackageDefinitionId>,
    pub program_package_id: Option<PackageDefinitionId>,
}

impl FullSemAnalysis {
    pub fn new(args: Args) -> Box<FullSemAnalysis> {
        let sa = Box::new(FullSemAnalysis {
            args,
            test_file_as_string: None,
            source_files: Vec::new(),
            consts: MutableVec::new(),
            structs: MutableVec::new(),
            classes: MutableVec::new(),
            extensions: MutableVec::new(),
            annotations: MutableVec::new(),
            modules: MutableVec::new(),
            enums: MutableVec::new(),
            traits: MutableVec::new(),
            impls: MutableVec::new(),
            globals: MutableVec::new(),
            uses: Vec::new(),
            interner: Interner::new(),
            known: KnownElements::new(),
            diag: Mutex::new(Diagnostic::new()),
            fcts: GrowableVec::new(),
            native_stubs: Mutex::new(NativeStubs::new()),
            packages: MutableVec::new(),
            package_names: HashMap::new(),
            prelude_module_id: None,
            stdlib_module_id: None,
            program_module_id: None,
            stdlib_package_id: None,
            program_package_id: None,
        });

        sa
    }

    pub fn new_from_sa(sa: Box<FullSemAnalysis>) -> Box<VM> {
        VM::new_from_full_sa(sa)
    }

    pub fn prelude_module_id(&self) -> ModuleDefinitionId {
        self.prelude_module_id.expect("uninitialized module id")
    }

    pub fn stdlib_module_id(&self) -> ModuleDefinitionId {
        self.stdlib_module_id.expect("uninitialized module id")
    }

    pub fn program_module_id(&self) -> ModuleDefinitionId {
        self.program_module_id.expect("uninitialized module id")
    }
}

pub struct VM {
    pub args: Args,
    pub test_file_as_string: Option<&'static str>,
    pub interner: Interner,
    pub source_files: Vec<SourceFile>,
    pub diag: Mutex<Diagnostic>,
    pub known: KnownElements,
    pub consts: MutableVec<ConstDefinition>, // stores all const definitions
    pub structs: MutableVec<StructDefinition>, // stores all struct source definitions
    pub struct_specializations:
        RwLock<HashMap<(StructDefinitionId, SourceTypeArray), StructInstanceId>>,
    pub struct_instances: GrowableVecNonIter<StructInstance>, // stores all struct definitions
    pub classes: MutableVec<ClassDefinition>,                 // stores all class source definitions
    pub class_specializations:
        RwLock<HashMap<(ClassDefinitionId, SourceTypeArray), ClassInstanceId>>,
    pub class_instances: GrowableVecNonIter<ClassInstance>, // stores all class definitions
    pub extensions: MutableVec<ExtensionDefinition>,        // stores all extension definitions
    pub annotations: MutableVec<AnnotationDefinition>, // stores all annotation source definitions
    pub modules: MutableVec<ModuleDefinition>,         // stores all module definitions
    pub fcts: GrowableVec<RwLock<FctDefinition>>,      // stores all function source definitions
    pub code_objects: CodeObjects,
    pub compilation_database: CompilationDatabase,
    pub enums: MutableVec<EnumDefinition>, // store all enum source definitions
    pub enum_specializations: RwLock<HashMap<(EnumDefinitionId, SourceTypeArray), EnumInstanceId>>,
    pub enum_instances: GrowableVecNonIter<EnumInstance>, // stores all enum definitions
    pub traits: MutableVec<TraitDefinition>,              // stores all trait definitions
    pub trait_vtables: RwLock<HashMap<(TraitDefinitionId, SourceTypeArray), ClassInstanceId>>,
    pub impls: MutableVec<ImplDefinition>, // stores all impl definitions
    pub code_map: CodeMap,                 // stores all compiled functions
    pub globals: MutableVec<GlobalDefinition>, // stores all global variables
    pub global_variable_memory: Option<GlobalVariableMemory>,
    pub uses: Vec<UseDefinition>, // stores all uses
    pub gc: Gc,                   // garbage collector
    pub native_stubs: Mutex<NativeStubs>,
    pub stubs: Stubs,
    pub threads: Threads,
    pub packages: MutableVec<PackageDefinition>,
    pub package_names: HashMap<Name, PackageDefinitionId>,
    pub prelude_module_id: Option<ModuleDefinitionId>,
    pub stdlib_module_id: Option<ModuleDefinitionId>,
    pub program_module_id: Option<ModuleDefinitionId>,
    pub stdlib_package_id: Option<PackageDefinitionId>,
    pub program_package_id: Option<PackageDefinitionId>,
    pub wait_lists: WaitLists,
}

impl VM {
    pub fn new(args: Args) -> Box<VM> {
        let gc = Gc::new(&args);

        let vm = Box::new(VM {
            args,
            test_file_as_string: None,
            source_files: Vec::new(),
            consts: MutableVec::new(),
            structs: MutableVec::new(),
            struct_specializations: RwLock::new(HashMap::new()),
            struct_instances: GrowableVecNonIter::new(),
            classes: MutableVec::new(),
            class_specializations: RwLock::new(HashMap::new()),
            class_instances: GrowableVecNonIter::new(),
            extensions: MutableVec::new(),
            annotations: MutableVec::new(),
            modules: MutableVec::new(),
            enums: MutableVec::new(),
            enum_specializations: RwLock::new(HashMap::new()),
            enum_instances: GrowableVecNonIter::new(),
            traits: MutableVec::new(),
            trait_vtables: RwLock::new(HashMap::new()),
            impls: MutableVec::new(),
            globals: MutableVec::new(),
            global_variable_memory: None,
            uses: Vec::new(),
            interner: Interner::new(),
            known: KnownElements::new(),
            gc,
            diag: Mutex::new(Diagnostic::new()),
            fcts: GrowableVec::new(),
            compilation_database: CompilationDatabase::new(),
            code_objects: CodeObjects::new(),
            code_map: CodeMap::new(),
            native_stubs: Mutex::new(NativeStubs::new()),
            stubs: Stubs::new(),
            threads: Threads::new(),
            packages: MutableVec::new(),
            prelude_module_id: None,
            stdlib_module_id: None,
            program_module_id: None,
            stdlib_package_id: None,
            program_package_id: None,
            package_names: HashMap::new(),
            wait_lists: WaitLists::new(),
        });

        vm
    }

    pub fn new_from_sa(sa: Box<VM>) -> Box<VM> {
        sa
    }

    pub fn new_from_full_sa(sa: Box<FullSemAnalysis>) -> Box<VM> {
        let gc = Gc::new(&sa.args);

        let vm = Box::new(VM {
            args: sa.args,
            test_file_as_string: sa.test_file_as_string,
            source_files: sa.source_files,
            consts: sa.consts,
            structs: sa.structs,
            struct_specializations: RwLock::new(HashMap::new()),
            struct_instances: GrowableVecNonIter::new(),
            classes: sa.classes,
            class_specializations: RwLock::new(HashMap::new()),
            class_instances: GrowableVecNonIter::new(),
            extensions: sa.extensions,
            annotations: sa.annotations,
            modules: sa.modules,
            enums: sa.enums,
            enum_specializations: RwLock::new(HashMap::new()),
            enum_instances: GrowableVecNonIter::new(),
            traits: sa.traits,
            trait_vtables: RwLock::new(HashMap::new()),
            impls: sa.impls,
            globals: sa.globals,
            global_variable_memory: None,
            uses: sa.uses,
            interner: sa.interner,
            known: sa.known,
            gc,
            diag: sa.diag,
            fcts: sa.fcts,
            compilation_database: CompilationDatabase::new(),
            code_objects: CodeObjects::new(),
            code_map: CodeMap::new(),
            native_stubs: sa.native_stubs,
            stubs: Stubs::new(),
            threads: Threads::new(),
            packages: sa.packages,
            prelude_module_id: sa.prelude_module_id,
            stdlib_module_id: sa.stdlib_module_id,
            program_module_id: sa.program_module_id,
            stdlib_package_id: sa.stdlib_package_id,
            program_package_id: sa.program_package_id,
            package_names: sa.package_names,
            wait_lists: WaitLists::new(),
        });

        vm
    }

    pub fn setup_execution(&mut self) {
        // ensure this data is only created during execution
        assert!(self.compilation_database.is_empty());

        initialize::setup(self);

        globals::init_global_addresses(self);
    }

    pub fn gc_epoch(&self) -> usize {
        self.gc.epoch()
    }

    pub fn run(&self, fct_id: FctDefinitionId) -> i32 {
        let tld = current_thread().tld_address();
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.stubs.dora_entry();
        let fct: extern "C" fn(Address, Address) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr)
    }

    pub fn run_test(&self, fct_id: FctDefinitionId) {
        let tld = current_thread().tld_address();
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.stubs.dora_entry();
        let fct: extern "C" fn(Address, Address) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr);
    }

    pub fn ensure_compiled(&self, fct_id: FctDefinitionId) -> Address {
        let mut dtn = DoraToNativeInfo::new();
        let type_params = SourceTypeArray::empty();

        current_thread().use_dtn(&mut dtn, || compiler::generate(self, fct_id, &type_params))
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

    pub fn prelude_module_id(&self) -> ModuleDefinitionId {
        self.prelude_module_id.expect("uninitialized module id")
    }

    pub fn stdlib_module_id(&self) -> ModuleDefinitionId {
        self.stdlib_module_id.expect("uninitialized module id")
    }

    pub fn program_module_id(&self) -> ModuleDefinitionId {
        self.program_module_id.expect("uninitialized module id")
    }

    pub fn stdlib_package_id(&self) -> PackageDefinitionId {
        self.stdlib_package_id.expect("uninitialized package id")
    }

    pub fn program_package_id(&self) -> PackageDefinitionId {
        self.program_package_id.expect("uninitialized package id")
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

    let managed_thread_handle = native_thread.handles.handle(managed_thread);

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
