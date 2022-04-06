use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::hash::Hash;
use std::mem;
use std::path::PathBuf;
use std::ptr;
use std::sync::Arc;

use crate::compiler;
use crate::compiler::compile_stub;
use crate::compiler::dora_stub;
use crate::compiler::native_stub::{self, NativeFct, NativeFctKind, NativeStubs};
use crate::driver::cmd::Args;
use crate::gc::{Address, Gc};
use crate::language::error::diag::Diagnostic;
use crate::language::sem_analysis::{
    ClassDefinition, ConstDefinition, GlobalDefinition, NamespaceData, NamespaceId,
    StructDefinition, StructDefinitionId, StructInstance,
};
use crate::language::ty::{LambdaTypes, SourceType, SourceTypeArray};
use crate::object::{Ref, Testing};
use crate::safepoint;
use crate::stack::DoraToNativeInfo;
use crate::stdlib;
use crate::threads::{
    current_thread, deinit_current_thread, init_current_thread, DoraThread, ThreadState, Threads,
    STACK_SIZE,
};
use crate::utils::GrowableVec;

use dora_parser::ast;
use dora_parser::interner::*;
use dora_parser::parser::NodeIdGenerator;

pub use self::annotations::{AnnotationDefinition, AnnotationDefinitionId};
pub use self::classes::{ClassInstance, ClassInstanceId, Field, FieldDef, FieldId};
pub use self::code::{
    install_code, install_code_stub, Code, CodeId, CodeKind, CommentTable, GcPoint, GcPointTable,
    LazyCompilationData, LazyCompilationSite, PositionTable,
};
pub use self::code_map::CodeMap;
pub use self::enums::{
    find_methods_in_enum, EnumDefinition, EnumDefinitionId, EnumInstance, EnumInstanceId,
    EnumLayout, EnumVariant,
};
pub use self::extensions::{extension_matches, extension_matches_ty, ExtensionData, ExtensionId};
pub use self::functions::{FctDefinition, FctDefinitionId, FctParent, Intrinsic};
pub use self::globals::init_global_addresses;
pub use self::impls::{find_trait_impl, impl_matches, ImplData, ImplId};
pub use self::imports::ImportData;
pub use self::known::{
    KnownAnnotations, KnownClasses, KnownElements, KnownEnums, KnownFunctions, KnownStructs,
    KnownTraits,
};
pub use self::modules::{find_methods_in_module, Module, ModuleDefId, ModuleId, ModuleInstance};
pub use self::specialize::{
    add_ref_fields, ensure_display, replace_type_param, specialize_class_id,
    specialize_class_id_params, specialize_enum_class, specialize_enum_id_params,
    specialize_struct_id_params, specialize_trait_object, specialize_tuple, specialize_type,
    specialize_type_list,
};
pub use self::traits::{TraitDefinition, TraitDefinitionId};
pub use self::tuples::{ensure_tuple, TupleId, Tuples};
pub use self::waitlists::{ManagedCondition, ManagedMutex, WaitLists};

mod annotations;
mod classes;
mod code;
mod code_map;
mod enums;
mod extensions;
mod functions;
mod globals;
mod impls;
mod imports;
mod known;
mod modules;
mod specialize;
mod stdlib_setup;
mod traits;
mod tuples;
mod waitlists;

static mut VM_GLOBAL: *const u8 = ptr::null();

pub fn get_vm() -> &'static VM {
    unsafe { &*(VM_GLOBAL as *const VM) }
}

pub fn set_vm(vm: &VM) {
    let ptr = vm as *const _ as *const u8;

    unsafe {
        VM_GLOBAL = ptr;
    }
}

#[inline(never)]
pub fn stack_pointer() -> Address {
    let local: i32 = 0;
    Address::from_ptr(&local as *const i32)
}

pub struct File {
    pub id: FileId,
    pub path: Option<PathBuf>,
    pub namespace_id: NamespaceId,
    pub ast: Arc<ast::File>,
}

pub struct FullSemAnalysis {
    pub args: Args,
    pub interner: Interner,
    pub id_generator: NodeIdGenerator,
    pub files: Arc<RwLock<Vec<File>>>,
    pub diag: Mutex<Diagnostic>,
    pub known: KnownElements,
    pub consts: GrowableVec<RwLock<ConstDefinition>>, // stores all const definitions
    pub structs: GrowableVec<RwLock<StructDefinition>>, // stores all struct source definitions
    pub struct_defs: GrowableVec<StructInstance>,     // stores all struct definitions
    pub classes: GrowableVec<RwLock<ClassDefinition>>, // stores all class source definitions
    pub class_defs: GrowableVec<ClassInstance>,       // stores all class definitions
    pub extensions: Vec<RwLock<ExtensionData>>,       // stores all extension definitions
    pub tuples: Mutex<Tuples>,                        // stores all tuple definitions
    pub modules: GrowableVec<RwLock<Module>>,         // stores all module source definitions
    pub module_defs: GrowableVec<RwLock<ModuleInstance>>, // stores all module definitions
    pub annotations: GrowableVec<RwLock<AnnotationDefinition>>, // stores all annotation source definitions
    pub namespaces: Vec<RwLock<NamespaceData>>,                 // stores all namespace definitions
    pub fcts: GrowableVec<RwLock<FctDefinition>>, // stores all function source definitions
    pub enums: Vec<RwLock<EnumDefinition>>,       // stores all enum source definitions
    pub enum_defs: GrowableVec<EnumInstance>,     // stores all enum definitions
    pub traits: Vec<RwLock<TraitDefinition>>,     // stores all trait definitions
    pub impls: Vec<RwLock<ImplData>>,             // stores all impl definitions
    pub globals: GrowableVec<RwLock<GlobalDefinition>>, // stores all global variables
    pub imports: Vec<ImportData>,                 // stores all imports
    pub native_stubs: Mutex<NativeStubs>,
    pub lambda_types: Mutex<LambdaTypes>,
    pub parse_arg_file: bool,
    pub prelude_namespace_id: NamespaceId,
    pub stdlib_namespace_id: NamespaceId,
    pub global_namespace_id: NamespaceId,
    pub boots_namespace_id: NamespaceId,
}

impl FullSemAnalysis {
    pub fn new(args: Args) -> Box<FullSemAnalysis> {
        let empty_class_def_id: ClassInstanceId = 0.into();
        let empty_trait_id: TraitDefinitionId = 0.into();
        let empty_fct_id: FctDefinitionId = 0.into();
        let empty_enum_id: EnumDefinitionId = 0.into();
        let empty_struct_id: StructDefinitionId = 0.into();
        let empty_annotation_id: AnnotationDefinitionId = 0.into();

        let prelude_namespace_id = NamespaceId(0);
        let stdlib_namespace_id = NamespaceId(1);
        let global_namespace_id = NamespaceId(2);
        let boots_namespace_id = NamespaceId(3);

        let interner = Interner::new();
        let stdlib_name = interner.intern("std");
        let boots_name = interner.intern("boots");

        let namespaces = vec![
            RwLock::new(NamespaceData::predefined(prelude_namespace_id, None)),
            RwLock::new(NamespaceData::predefined(
                stdlib_namespace_id,
                Some(stdlib_name),
            )),
            RwLock::new(NamespaceData::predefined(global_namespace_id, None)),
            RwLock::new(NamespaceData::predefined(
                boots_namespace_id,
                Some(boots_name),
            )),
        ];

        let sa = Box::new(FullSemAnalysis {
            args,
            files: Arc::new(RwLock::new(Vec::new())),
            consts: GrowableVec::new(),
            structs: GrowableVec::new(),
            struct_defs: GrowableVec::new(),
            classes: GrowableVec::new(),
            class_defs: GrowableVec::new(),
            extensions: Vec::new(),
            tuples: Mutex::new(Tuples::new()),
            modules: GrowableVec::new(),
            module_defs: GrowableVec::new(),
            annotations: GrowableVec::new(),
            namespaces,
            enums: Vec::new(),
            enum_defs: GrowableVec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
            globals: GrowableVec::new(),
            imports: Vec::new(),
            interner,
            known: KnownElements {
                classes: KnownClasses::new(),

                functions: KnownFunctions {
                    string_buffer_empty: empty_fct_id,
                    string_buffer_append: empty_fct_id,
                    string_buffer_to_string: empty_fct_id,
                },

                traits: KnownTraits {
                    equals: empty_trait_id,
                    comparable: empty_trait_id,
                    stringable: empty_trait_id,
                    iterator: empty_trait_id,
                    zero: empty_trait_id,
                },

                enums: KnownEnums {
                    option: empty_enum_id,
                },

                annotations: KnownAnnotations {
                    abstract_: empty_annotation_id,
                    final_: empty_annotation_id,
                    internal: empty_annotation_id,
                    override_: empty_annotation_id,
                    open: empty_annotation_id,
                    pub_: empty_annotation_id,
                    static_: empty_annotation_id,

                    test: empty_annotation_id,

                    cannon: empty_annotation_id,
                    optimize_immediately: empty_annotation_id,
                },

                structs: KnownStructs {
                    bool: empty_struct_id,
                    uint8: empty_struct_id,
                    char: empty_struct_id,
                    int32: empty_struct_id,
                    int64: empty_struct_id,
                    float32: empty_struct_id,
                    float64: empty_struct_id,
                },

                byte_array_def: Mutex::new(None),
                int_array_def: Mutex::new(None),
                str_class_def: Mutex::new(None),
                obj_class_def: Mutex::new(None),
                ste_class_def: Mutex::new(None),
                ex_class_def: Mutex::new(None),

                free_object_class_def: empty_class_def_id,
                free_array_class_def: empty_class_def_id,
                code_class_def: empty_class_def_id,
            },
            id_generator: NodeIdGenerator::new(),
            diag: Mutex::new(Diagnostic::new()),
            fcts: GrowableVec::new(),
            lambda_types: Mutex::new(LambdaTypes::new()),
            native_stubs: Mutex::new(NativeStubs::new()),
            parse_arg_file: true,
            prelude_namespace_id,
            stdlib_namespace_id,
            global_namespace_id,
            boots_namespace_id,
        });

        sa
    }

    pub fn new_from_sa(sa: Box<FullSemAnalysis>) -> Box<VM> {
        VM::new_from_full_sa(sa)
    }
}

pub type SemAnalysis = VM;

pub struct VM {
    pub args: Args,
    pub interner: Interner,
    pub id_generator: NodeIdGenerator,
    pub files: Arc<RwLock<Vec<File>>>,
    pub diag: Mutex<Diagnostic>,
    pub known: KnownElements,
    pub consts: GrowableVec<RwLock<ConstDefinition>>, // stores all const definitions
    pub structs: GrowableVec<RwLock<StructDefinition>>, // stores all struct source definitions
    pub struct_defs: GrowableVec<StructInstance>,     // stores all struct definitions
    pub classes: GrowableVec<RwLock<ClassDefinition>>, // stores all class source definitions
    pub class_defs: GrowableVec<ClassInstance>,       // stores all class definitions
    pub extensions: Vec<RwLock<ExtensionData>>,       // stores all extension definitions
    pub tuples: Mutex<Tuples>,                        // stores all tuple definitions
    pub modules: GrowableVec<RwLock<Module>>,         // stores all module source definitions
    pub module_instances: GrowableVec<RwLock<ModuleInstance>>, // stores all module definitions
    pub annotations: GrowableVec<RwLock<AnnotationDefinition>>, // stores all annotation source definitions
    pub namespaces: Vec<RwLock<NamespaceData>>,                 // stores all namespace definitions
    pub fcts: GrowableVec<RwLock<FctDefinition>>, // stores all function source definitions
    pub compiled_fcts: RwLock<HashMap<(FctDefinitionId, SourceTypeArray), CodeId>>,
    pub code: GrowableVec<Code>, // stores all function implementations
    pub enums: Vec<RwLock<EnumDefinition>>, // store all enum source definitions
    pub enum_defs: GrowableVec<EnumInstance>, // stores all enum definitions
    pub traits: Vec<RwLock<TraitDefinition>>, // stores all trait definitions
    pub impls: Vec<RwLock<ImplData>>, // stores all impl definitions
    pub code_map: Mutex<CodeMap>, // stores all compiled functions
    pub globals: GrowableVec<RwLock<GlobalDefinition>>, // stores all global variables
    pub imports: Vec<ImportData>, // stores all imports
    pub gc: Gc,                  // garbage collector
    pub native_stubs: Mutex<NativeStubs>,
    pub lambda_types: Mutex<LambdaTypes>,
    pub compile_stub: Mutex<Address>,
    pub dora_stub: Mutex<Address>,
    pub trap_stub: Mutex<Address>,
    pub stack_overflow_stub: Mutex<Address>,
    pub safepoint_stub: Mutex<Address>,
    pub threads: Threads,
    pub parse_arg_file: bool,
    pub prelude_namespace_id: NamespaceId,
    pub stdlib_namespace_id: NamespaceId,
    pub global_namespace_id: NamespaceId,
    pub boots_namespace_id: NamespaceId,
    pub wait_lists: WaitLists,
}

impl VM {
    pub fn new(args: Args) -> Box<VM> {
        let empty_class_def_id: ClassInstanceId = 0.into();
        let empty_trait_id: TraitDefinitionId = 0.into();
        let empty_fct_id: FctDefinitionId = 0.into();
        let empty_enum_id: EnumDefinitionId = 0.into();
        let empty_struct_id = 0.into();
        let empty_annotation_id: AnnotationDefinitionId = 0.into();
        let gc = Gc::new(&args);

        let prelude_namespace_id = NamespaceId(0);
        let stdlib_namespace_id = NamespaceId(1);
        let global_namespace_id = NamespaceId(2);
        let boots_namespace_id = NamespaceId(3);

        let interner = Interner::new();
        let stdlib_name = interner.intern("std");
        let boots_name = interner.intern("boots");

        let namespaces = vec![
            RwLock::new(NamespaceData::predefined(prelude_namespace_id, None)),
            RwLock::new(NamespaceData::predefined(
                stdlib_namespace_id,
                Some(stdlib_name),
            )),
            RwLock::new(NamespaceData::predefined(global_namespace_id, None)),
            RwLock::new(NamespaceData::predefined(
                boots_namespace_id,
                Some(boots_name),
            )),
        ];

        let vm = Box::new(VM {
            args,
            files: Arc::new(RwLock::new(Vec::new())),
            consts: GrowableVec::new(),
            structs: GrowableVec::new(),
            struct_defs: GrowableVec::new(),
            classes: GrowableVec::new(),
            class_defs: GrowableVec::new(),
            extensions: Vec::new(),
            tuples: Mutex::new(Tuples::new()),
            modules: GrowableVec::new(),
            module_instances: GrowableVec::new(),
            annotations: GrowableVec::new(),
            namespaces,
            enums: Vec::new(),
            enum_defs: GrowableVec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
            globals: GrowableVec::new(),
            imports: Vec::new(),
            interner,
            known: KnownElements {
                classes: KnownClasses::new(),

                functions: KnownFunctions {
                    string_buffer_empty: empty_fct_id,
                    string_buffer_append: empty_fct_id,
                    string_buffer_to_string: empty_fct_id,
                },

                traits: KnownTraits {
                    equals: empty_trait_id,
                    comparable: empty_trait_id,
                    stringable: empty_trait_id,
                    iterator: empty_trait_id,
                    zero: empty_trait_id,
                },

                enums: KnownEnums {
                    option: empty_enum_id,
                },

                annotations: KnownAnnotations {
                    abstract_: empty_annotation_id,
                    final_: empty_annotation_id,
                    internal: empty_annotation_id,
                    override_: empty_annotation_id,
                    open: empty_annotation_id,
                    pub_: empty_annotation_id,
                    static_: empty_annotation_id,

                    test: empty_annotation_id,

                    cannon: empty_annotation_id,
                    optimize_immediately: empty_annotation_id,
                },

                structs: KnownStructs {
                    bool: empty_struct_id,
                    uint8: empty_struct_id,
                    char: empty_struct_id,
                    int32: empty_struct_id,
                    int64: empty_struct_id,
                    float32: empty_struct_id,
                    float64: empty_struct_id,
                },

                byte_array_def: Mutex::new(None),
                int_array_def: Mutex::new(None),
                str_class_def: Mutex::new(None),
                obj_class_def: Mutex::new(None),
                ste_class_def: Mutex::new(None),
                ex_class_def: Mutex::new(None),

                free_object_class_def: empty_class_def_id,
                free_array_class_def: empty_class_def_id,
                code_class_def: empty_class_def_id,
            },
            gc,
            id_generator: NodeIdGenerator::new(),
            diag: Mutex::new(Diagnostic::new()),
            fcts: GrowableVec::new(),
            compiled_fcts: RwLock::new(HashMap::new()),
            code: GrowableVec::new(),
            code_map: Mutex::new(CodeMap::new()),
            lambda_types: Mutex::new(LambdaTypes::new()),
            native_stubs: Mutex::new(NativeStubs::new()),
            compile_stub: Mutex::new(Address::null()),
            dora_stub: Mutex::new(Address::null()),
            trap_stub: Mutex::new(Address::null()),
            stack_overflow_stub: Mutex::new(Address::null()),
            safepoint_stub: Mutex::new(Address::null()),
            threads: Threads::new(),
            parse_arg_file: true,
            prelude_namespace_id,
            stdlib_namespace_id,
            global_namespace_id,
            boots_namespace_id,
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
            files: sa.files,
            consts: sa.consts,
            structs: sa.structs,
            struct_defs: sa.struct_defs,
            classes: sa.classes,
            class_defs: sa.class_defs,
            extensions: sa.extensions,
            tuples: sa.tuples,
            modules: sa.modules,
            module_instances: sa.module_defs,
            annotations: sa.annotations,
            namespaces: sa.namespaces,
            enums: sa.enums,
            enum_defs: sa.enum_defs,
            traits: sa.traits,
            impls: sa.impls,
            globals: sa.globals,
            imports: sa.imports,
            interner: sa.interner,
            known: sa.known,
            gc,
            id_generator: sa.id_generator,
            diag: sa.diag,
            fcts: sa.fcts,
            compiled_fcts: RwLock::new(HashMap::new()),
            code: GrowableVec::new(),
            code_map: Mutex::new(CodeMap::new()),
            lambda_types: sa.lambda_types,
            native_stubs: sa.native_stubs,
            compile_stub: Mutex::new(Address::null()),
            dora_stub: Mutex::new(Address::null()),
            trap_stub: Mutex::new(Address::null()),
            stack_overflow_stub: Mutex::new(Address::null()),
            safepoint_stub: Mutex::new(Address::null()),
            threads: Threads::new(),
            parse_arg_file: sa.parse_arg_file,
            prelude_namespace_id: sa.prelude_namespace_id,
            stdlib_namespace_id: sa.stdlib_namespace_id,
            global_namespace_id: sa.global_namespace_id,
            boots_namespace_id: sa.boots_namespace_id,
            wait_lists: WaitLists::new(),
        });

        vm
    }

    pub fn setup_execution(&mut self) {
        // ensure this data is only created during execution
        assert!(self.compiled_fcts.read().is_empty());
        assert!(self.class_defs.len() == 0);

        stdlib_setup::setup(self);
    }

    pub fn gc_epoch(&self) -> usize {
        self.gc.epoch()
    }

    pub fn run(&self, fct_id: FctDefinitionId) -> i32 {
        let tld = current_thread().tld_address();
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.dora_stub();
        let fct: extern "C" fn(Address, Address) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr)
    }

    pub fn run_test(&self, fct_id: FctDefinitionId, testing: Ref<Testing>) {
        let tld = current_thread().tld_address();
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.dora_stub();
        let fct: extern "C" fn(Address, Address, Ref<Testing>) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr, testing);
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

        let code_id = {
            let mut code_vec = self.code.lock();
            let code_id = code_vec.len().into();
            code_vec.push(code);
            code_id
        };

        let mut code_map = self.code_map.lock();
        code_map.insert(code_start, code_end, code_id);

        code_id
    }

    pub fn insert_code_map(&self, start: Address, end: Address, desc: CodeId) {
        let mut code_map = self.code_map.lock();
        code_map.insert(start, end, desc);
    }

    pub fn dora_stub(&self) -> Address {
        let mut dora_stub_address = self.dora_stub.lock();

        if dora_stub_address.is_null() {
            *dora_stub_address = dora_stub::generate(self).instruction_start();
        }

        *dora_stub_address
    }

    pub fn compile_stub(&self) -> Address {
        let mut compile_stub_address = self.compile_stub.lock();

        if compile_stub_address.is_null() {
            *compile_stub_address = compile_stub::generate(self).instruction_start();
        }

        *compile_stub_address
    }

    pub fn trap_stub(&self) -> Address {
        let mut trap_stub_address = self.trap_stub.lock();

        if trap_stub_address.is_null() {
            let ifct = NativeFct {
                fctptr: Address::from_ptr(stdlib::trap as *const u8),
                args: &[SourceType::Int32],
                return_type: SourceType::Unit,
                desc: NativeFctKind::TrapStub,
            };
            let code = native_stub::generate(self, ifct, false);
            *trap_stub_address = code.instruction_start();
        }

        *trap_stub_address
    }

    pub fn stack_overflow_stub(&self) -> Address {
        let mut stack_overflow_stub_address = self.stack_overflow_stub.lock();

        if stack_overflow_stub_address.is_null() {
            let ifct = NativeFct {
                fctptr: Address::from_ptr(safepoint::stack_overflow as *const u8),
                args: &[],
                return_type: SourceType::Unit,
                desc: NativeFctKind::GuardCheckStub,
            };
            let code = native_stub::generate(self, ifct, false);
            *stack_overflow_stub_address = code.instruction_start();
        }

        *stack_overflow_stub_address
    }

    pub fn safepoint_stub(&self) -> Address {
        let mut safepoint_stub_address = self.safepoint_stub.lock();

        if safepoint_stub_address.is_null() {
            let ifct = NativeFct {
                fctptr: Address::from_ptr(safepoint::safepoint_slow as *const u8),
                args: &[],
                return_type: SourceType::Unit,
                desc: NativeFctKind::SafepointStub,
            };
            let code = native_stub::generate(self, ifct, false);
            *safepoint_stub_address = code.instruction_start();
        }

        *safepoint_stub_address
    }
}

unsafe impl Sync for VM {}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileId(u32);

impl From<u32> for FileId {
    fn from(data: u32) -> FileId {
        FileId(data)
    }
}

impl FileId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

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
    let thread = DoraThread::new(vm, ThreadState::Running);
    init_current_thread(thread.clone());
    vm.threads.attach_thread(thread);

    let stack_top = stack_pointer();
    let stack_limit = stack_top.sub(STACK_SIZE);

    let thread = current_thread();
    thread.tld.set_stack_limit(stack_limit);

    let result = callback();

    vm.threads.detach_current_thread();
    deinit_current_thread();

    result
}
