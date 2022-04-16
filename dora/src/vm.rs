use parking_lot::{Mutex, RwLock};
use std::mem;
use std::ptr;
use std::sync::Arc;

use crate::compiler;
use crate::compiler::dora_entry_stub;
use crate::compiler::dora_exit_stubs::{self, NativeFct, NativeFctKind, NativeStubs};
use crate::compiler::lazy_compile_stub;
use crate::driver::cmd::Args;
use crate::gc::{Address, Gc};
use crate::language::error::diag::Diagnostic;
use crate::language::sem_analysis::{
    get_tuple_subtypes, AnnotationDefinition, AnnotationDefinitionId, ClassDefinition,
    ConstDefinition, EnumDefinition, EnumDefinitionId, ExtensionDefinition, FctDefinition,
    FctDefinitionId, GlobalDefinition, ImplDefinition, ModuleDefinition, ModuleDefinitionId,
    SourceFile, StructDefinition, StructDefinitionId, StructInstance, TraitDefinition,
    TraitDefinitionId, Tuples, UseDefinition,
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
use crate::utils::{GrowableVec, MutableVec};

use dora_parser::interner::*;
use dora_parser::parser::NodeIdGenerator;

pub use self::classes::{ClassInstance, ClassInstanceId, Field, FieldDef, FieldId};
pub use self::code::{
    install_code, install_code_stub, Code, CodeId, CodeKind, CodeObjects, CommentTable, GcPoint,
    GcPointTable, LazyCompilationData, LazyCompilationSite, ManagedCodeHeader, PositionTable,
    RelocationTable, CODE_ALIGNMENT,
};
pub use self::code_map::CodeMap;
pub use self::compilation::CompilationDatabase;
pub use self::enums::{EnumInstance, EnumInstanceId, EnumLayout};
pub use self::globals::init_global_addresses;
pub use self::known::{
    KnownAnnotations, KnownClasses, KnownElements, KnownEnums, KnownFunctions, KnownStructs,
    KnownTraits,
};
pub use self::specialize::{
    add_ref_fields, ensure_display, replace_type_param, specialize_class_id,
    specialize_class_id_params, specialize_enum_class, specialize_enum_id_params,
    specialize_struct_id_params, specialize_trait_object, specialize_tuple, specialize_type,
    specialize_type_list,
};
pub use self::tuples::{get_concrete_tuple, ConcreteTuple};
pub use self::waitlists::{ManagedCondition, ManagedMutex, WaitLists};

mod classes;
mod code;
mod code_map;
mod compilation;
mod enums;
mod globals;
mod known;
mod specialize;
mod stdlib_setup;
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

pub struct FullSemAnalysis {
    pub args: Args,
    pub test_file_as_string: Option<&'static str>,
    pub interner: Interner,
    pub id_generator: NodeIdGenerator,
    pub source_files: Vec<SourceFile>,
    pub diag: Mutex<Diagnostic>,
    pub known: KnownElements,
    pub consts: MutableVec<ConstDefinition>, // stores all const definitions
    pub structs: MutableVec<StructDefinition>, // stores all struct source definitions
    pub struct_defs: GrowableVec<StructInstance>, // stores all struct definitions
    pub classes: MutableVec<ClassDefinition>, // stores all class source definitions
    pub class_defs: GrowableVec<ClassInstance>, // stores all class definitions
    pub extensions: MutableVec<ExtensionDefinition>, // stores all extension definitions
    pub tuples: Mutex<Tuples>,               // stores all tuple definitions
    pub annotations: MutableVec<AnnotationDefinition>, // stores all annotation source definitions
    pub modules: MutableVec<ModuleDefinition>, // stores all module definitions
    pub fcts: GrowableVec<RwLock<FctDefinition>>, // stores all function source definitions
    pub enums: MutableVec<EnumDefinition>,   // stores all enum source definitions
    pub enum_defs: GrowableVec<EnumInstance>, // stores all enum definitions
    pub traits: MutableVec<TraitDefinition>, // stores all trait definitions
    pub impls: MutableVec<ImplDefinition>,   // stores all impl definitions
    pub globals: MutableVec<GlobalDefinition>, // stores all global variables
    pub uses: Vec<UseDefinition>,            // stores all uses
    pub native_stubs: Mutex<NativeStubs>,
    pub lambda_types: Mutex<LambdaTypes>,
    pub parse_arg_file: bool,
    pub prelude_module_id: ModuleDefinitionId,
    pub stdlib_module_id: ModuleDefinitionId,
    pub program_module_id: ModuleDefinitionId,
    pub boots_module_id: ModuleDefinitionId,
}

impl FullSemAnalysis {
    pub fn new(args: Args) -> Box<FullSemAnalysis> {
        let empty_class_def_id: ClassInstanceId = 0.into();
        let empty_trait_id: TraitDefinitionId = 0.into();
        let empty_fct_id: FctDefinitionId = 0.into();
        let empty_enum_id: EnumDefinitionId = 0.into();
        let empty_struct_id: StructDefinitionId = 0.into();
        let empty_annotation_id: AnnotationDefinitionId = 0.into();

        let interner = Interner::new();
        let stdlib_name = interner.intern("std");
        let boots_name = interner.intern("boots");

        let mut modules = MutableVec::new();
        let prelude_module_id = modules.push(ModuleDefinition::predefined(None));
        let stdlib_module_id = modules.push(ModuleDefinition::predefined(Some(stdlib_name)));
        let program_module_id = modules.push(ModuleDefinition::predefined(None));
        let boots_module_id = modules.push(ModuleDefinition::predefined(Some(boots_name)));

        let sa = Box::new(FullSemAnalysis {
            args,
            test_file_as_string: None,
            source_files: Vec::new(),
            consts: MutableVec::new(),
            structs: MutableVec::new(),
            struct_defs: GrowableVec::new(),
            classes: MutableVec::new(),
            class_defs: GrowableVec::new(),
            extensions: MutableVec::new(),
            tuples: Mutex::new(Tuples::new()),
            annotations: MutableVec::new(),
            modules,
            enums: MutableVec::new(),
            enum_defs: GrowableVec::new(),
            traits: MutableVec::new(),
            impls: MutableVec::new(),
            globals: MutableVec::new(),
            uses: Vec::new(),
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
            prelude_module_id,
            stdlib_module_id,
            program_module_id,
            boots_module_id,
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
    pub test_file_as_string: Option<&'static str>,
    pub interner: Interner,
    pub id_generator: NodeIdGenerator,
    pub source_files: Vec<SourceFile>,
    pub diag: Mutex<Diagnostic>,
    pub known: KnownElements,
    pub consts: MutableVec<ConstDefinition>, // stores all const definitions
    pub structs: MutableVec<StructDefinition>, // stores all struct source definitions
    pub struct_defs: GrowableVec<StructInstance>, // stores all struct definitions
    pub classes: MutableVec<ClassDefinition>, // stores all class source definitions
    pub class_defs: GrowableVec<ClassInstance>, // stores all class definitions
    pub extensions: MutableVec<ExtensionDefinition>, // stores all extension definitions
    pub tuples: Mutex<Tuples>,               // stores all tuple definitions
    pub annotations: MutableVec<AnnotationDefinition>, // stores all annotation source definitions
    pub modules: MutableVec<ModuleDefinition>, // stores all module definitions
    pub fcts: GrowableVec<RwLock<FctDefinition>>, // stores all function source definitions
    pub code_objects: CodeObjects,
    pub compilation_database: CompilationDatabase,
    pub enums: MutableVec<EnumDefinition>, // store all enum source definitions
    pub enum_defs: GrowableVec<EnumInstance>, // stores all enum definitions
    pub traits: MutableVec<TraitDefinition>, // stores all trait definitions
    pub impls: MutableVec<ImplDefinition>, // stores all impl definitions
    pub code_map: CodeMap,                 // stores all compiled functions
    pub globals: MutableVec<GlobalDefinition>, // stores all global variables
    pub uses: Vec<UseDefinition>,          // stores all uses
    pub gc: Gc,                            // garbage collector
    pub native_stubs: Mutex<NativeStubs>,
    pub lambda_types: Mutex<LambdaTypes>,
    pub compile_stub: Mutex<Address>,
    pub dora_stub: Mutex<Address>,
    pub trap_stub: Mutex<Address>,
    pub stack_overflow_stub: Mutex<Address>,
    pub safepoint_stub: Mutex<Address>,
    pub threads: Threads,
    pub parse_arg_file: bool,
    pub prelude_module_id: ModuleDefinitionId,
    pub stdlib_module_id: ModuleDefinitionId,
    pub program_module_id: ModuleDefinitionId,
    pub boots_module_id: ModuleDefinitionId,
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

        let interner = Interner::new();
        let stdlib_name = interner.intern("std");
        let boots_name = interner.intern("boots");

        let mut modules = MutableVec::new();
        let prelude_module_id = modules.push(ModuleDefinition::predefined(None));
        let stdlib_module_id = modules.push(ModuleDefinition::predefined(Some(stdlib_name)));
        let program_module_id = modules.push(ModuleDefinition::predefined(None));
        let boots_module_id = modules.push(ModuleDefinition::predefined(Some(boots_name)));

        let vm = Box::new(VM {
            args,
            test_file_as_string: None,
            source_files: Vec::new(),
            consts: MutableVec::new(),
            structs: MutableVec::new(),
            struct_defs: GrowableVec::new(),
            classes: MutableVec::new(),
            class_defs: GrowableVec::new(),
            extensions: MutableVec::new(),
            tuples: Mutex::new(Tuples::new()),
            annotations: MutableVec::new(),
            modules,
            enums: MutableVec::new(),
            enum_defs: GrowableVec::new(),
            traits: MutableVec::new(),
            impls: MutableVec::new(),
            globals: MutableVec::new(),
            uses: Vec::new(),
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
            compilation_database: CompilationDatabase::new(),
            code_objects: CodeObjects::new(),
            code_map: CodeMap::new(),
            lambda_types: Mutex::new(LambdaTypes::new()),
            native_stubs: Mutex::new(NativeStubs::new()),
            compile_stub: Mutex::new(Address::null()),
            dora_stub: Mutex::new(Address::null()),
            trap_stub: Mutex::new(Address::null()),
            stack_overflow_stub: Mutex::new(Address::null()),
            safepoint_stub: Mutex::new(Address::null()),
            threads: Threads::new(),
            parse_arg_file: true,
            prelude_module_id,
            stdlib_module_id,
            program_module_id,
            boots_module_id,
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
            struct_defs: sa.struct_defs,
            classes: sa.classes,
            class_defs: sa.class_defs,
            extensions: sa.extensions,
            tuples: sa.tuples,
            annotations: sa.annotations,
            modules: sa.modules,
            enums: sa.enums,
            enum_defs: sa.enum_defs,
            traits: sa.traits,
            impls: sa.impls,
            globals: sa.globals,
            uses: sa.uses,
            interner: sa.interner,
            known: sa.known,
            gc,
            id_generator: sa.id_generator,
            diag: sa.diag,
            fcts: sa.fcts,
            compilation_database: CompilationDatabase::new(),
            code_objects: CodeObjects::new(),
            code_map: CodeMap::new(),
            lambda_types: sa.lambda_types,
            native_stubs: sa.native_stubs,
            compile_stub: Mutex::new(Address::null()),
            dora_stub: Mutex::new(Address::null()),
            trap_stub: Mutex::new(Address::null()),
            stack_overflow_stub: Mutex::new(Address::null()),
            safepoint_stub: Mutex::new(Address::null()),
            threads: Threads::new(),
            parse_arg_file: sa.parse_arg_file,
            prelude_module_id: sa.prelude_module_id,
            stdlib_module_id: sa.stdlib_module_id,
            program_module_id: sa.program_module_id,
            boots_module_id: sa.boots_module_id,
            wait_lists: WaitLists::new(),
        });

        vm
    }

    pub fn setup_execution(&mut self) {
        // ensure this data is only created during execution
        assert!(self.compilation_database.is_empty());
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

        let code_id = self.code_objects.add(code);

        self.code_map.insert(code_start, code_end, code_id);

        code_id
    }

    pub fn dora_stub(&self) -> Address {
        let mut dora_stub_address = self.dora_stub.lock();

        if dora_stub_address.is_null() {
            *dora_stub_address = dora_entry_stub::install(self).instruction_start();
        }

        *dora_stub_address
    }

    pub fn compile_stub(&self) -> Address {
        let mut compile_stub_address = self.compile_stub.lock();

        if compile_stub_address.is_null() {
            *compile_stub_address = lazy_compile_stub::generate(self).instruction_start();
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
            let code = dora_exit_stubs::generate(self, ifct, false);
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
            let code = dora_exit_stubs::generate(self, ifct, false);
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
            let code = dora_exit_stubs::generate(self, ifct, false);
            *safepoint_stub_address = code.instruction_start();
        }

        *safepoint_stub_address
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
