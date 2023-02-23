use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::mem;
use std::ptr;
use std::sync::Arc;

use crate::compiler;
use crate::compiler::dora_exit_stubs::NativeStubs;
use crate::driver::cmd::Args;
use crate::gc::{Address, Gc};
use crate::language::sem_analysis::{
    AnnotationDefinition, ClassDefinition, ClassDefinitionId, EnumDefinition, EnumDefinitionId,
    ExtensionDefinition, FctDefinition, FctDefinitionId, GlobalDefinition, ImplDefinition,
    KnownElements, ModuleDefinition, ModuleDefinitionId, PackageDefinition, PackageDefinitionId,
    SemAnalysis, SourceFile, SourceFileId, StructDefinition, StructDefinitionId, TraitDefinition,
    TraitDefinitionId,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::stack::DoraToNativeInfo;
use crate::threads::ManagedThread;
use crate::threads::{
    current_thread, deinit_current_thread, init_current_thread, DoraThread, ThreadState, Threads,
    STACK_SIZE,
};
use crate::utils::GrowableVecNonIter;
use dora_frontend::{GrowableVec, MutableVec};

use dora_parser::interner::*;

pub use self::classes::{
    class_definition_name, class_definition_name_with_params, create_class_instance_with_vtable,
    ClassInstance, ClassInstanceId, FieldInstance, ShapeKind,
};
pub use self::code::{
    install_code, install_code_stub, Code, CodeId, CodeKind, CodeObjects, CommentTable, GcPoint,
    GcPointTable, LazyCompilationData, LazyCompilationSite, ManagedCodeHeader, PositionTable,
    RelocationTable, CODE_ALIGNMENT,
};
pub use self::code_map::CodeMap;
pub use self::compilation::CompilationDatabase;
pub use self::enums::{
    enum_definition_name, enum_definition_name_with_params, EnumInstance, EnumInstanceId,
    EnumLayout,
};
pub use self::extensions::extension_matches_ty;
use self::globals::GlobalVariableMemory;
pub use self::impls::{find_trait_impl, implements_trait};
use self::known::KnownInstances;
pub use self::modules::{module_contains, module_path};
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
pub use self::ty::path_for_type;
pub use self::waitlists::{ManagedCondition, ManagedMutex, WaitLists};

mod classes;
mod code;
mod code_map;
mod compilation;
mod enums;
mod extensions;
mod functions;
mod globals;
mod impls;
mod initialize;
mod known;
mod modules;
mod specialize;
mod stdlib;
mod structs;
mod stubs;
mod traits;
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

pub struct VM {
    pub args: Args,
    pub interner: Interner,
    pub source_files: Vec<SourceFile>,
    pub known: KnownElements,
    pub known_instances: KnownInstances,
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
    pub gc: Gc, // garbage collector
    pub native_stubs: Mutex<NativeStubs>,
    pub native_implementations: HashMap<FctDefinitionId, Address>,
    pub stubs: Stubs,
    pub threads: Threads,
    pub packages: MutableVec<PackageDefinition>,
    pub package_names: HashMap<Name, PackageDefinitionId>,
    pub prelude_module_id: Option<ModuleDefinitionId>,
    pub stdlib_module_id: Option<ModuleDefinitionId>,
    pub program_module_id: Option<ModuleDefinitionId>,
    pub boots_module_id: Option<ModuleDefinitionId>,
    pub stdlib_package_id: Option<PackageDefinitionId>,
    pub program_package_id: Option<PackageDefinitionId>,
    pub boots_package_id: Option<PackageDefinitionId>,
    pub wait_lists: WaitLists,
}

impl VM {
    pub fn new_from_sa(sa: Box<SemAnalysis>, args: Args) -> Box<VM> {
        let gc = Gc::new(&args);

        let vm = Box::new(VM {
            args,
            source_files: sa.source_files,
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
            interner: sa.interner,
            known: sa.known,
            known_instances: KnownInstances::new(),
            gc,
            fcts: sa.fcts,
            compilation_database: CompilationDatabase::new(),
            code_objects: CodeObjects::new(),
            code_map: CodeMap::new(),
            native_stubs: Mutex::new(NativeStubs::new()),
            native_implementations: HashMap::new(),
            stubs: Stubs::new(),
            threads: Threads::new(),
            packages: sa.packages,
            prelude_module_id: sa.prelude_module_id,
            stdlib_module_id: sa.stdlib_module_id,
            program_module_id: sa.program_module_id,
            boots_module_id: sa.boots_module_id,
            stdlib_package_id: sa.stdlib_package_id,
            program_package_id: sa.program_package_id,
            boots_package_id: sa.boots_package_id,
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

    pub fn boots_module_id(&self) -> ModuleDefinitionId {
        self.boots_module_id.expect("uninitialized module id")
    }

    pub fn program_module_id(&self) -> ModuleDefinitionId {
        self.program_module_id.expect("uninitialized module id")
    }

    pub fn stdlib_package_id(&self) -> PackageDefinitionId {
        self.stdlib_package_id.expect("uninitialized package id")
    }

    pub fn boots_package_id(&self) -> PackageDefinitionId {
        self.boots_package_id.expect("uninitialized package id")
    }

    pub fn program_package_id(&self) -> PackageDefinitionId {
        self.program_package_id.expect("uninitialized package id")
    }

    pub fn source_file(&self, idx: SourceFileId) -> &SourceFile {
        &self.source_files[idx.to_usize()]
    }

    pub fn add_fct(&self, mut fct: FctDefinition) -> FctDefinitionId {
        let mut fcts = self.fcts.lock();
        let fctid = FctDefinitionId(fcts.len());

        fct.id = Some(fctid);

        fcts.push(Arc::new(RwLock::new(fct)));

        fctid
    }

    pub fn byte_array(&self) -> ClassInstanceId {
        let mut byte_array_def = self.known_instances.byte_array_class_instance.lock();

        if let Some(cls_id) = *byte_array_def {
            cls_id
        } else {
            let type_args = SourceTypeArray::single(SourceType::UInt8);
            let cls_id = specialize_class_id_params(self, self.known.classes.array(), &type_args);
            *byte_array_def = Some(cls_id);
            cls_id
        }
    }

    pub fn int_array(&self) -> ClassInstanceId {
        let mut int_array_def = self.known_instances.int_array_class_instance.lock();

        if let Some(cls_id) = *int_array_def {
            cls_id
        } else {
            let type_args = SourceTypeArray::single(SourceType::Int32);
            let cls_id = specialize_class_id_params(self, self.known.classes.array(), &type_args);
            *int_array_def = Some(cls_id);
            cls_id
        }
    }

    pub fn str(&self) -> ClassInstanceId {
        let mut str_class_def = self.known_instances.str_class_instance.lock();

        if let Some(cls_id) = *str_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(self, self.known.classes.string());
            *str_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn stack_trace_element(&self) -> ClassInstanceId {
        let mut ste_class_def = self.known_instances.ste_class_instance.lock();

        if let Some(cls_id) = *ste_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(self, self.known.classes.stacktrace_element());
            *ste_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn thread_class_instance(&self) -> ClassInstanceId {
        specialize_class_id(self, self.known.classes.thread())
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
