use parking_lot::{Mutex, RwLock};
use std::mem;
use std::path::PathBuf;
use std::ptr;
use std::sync::Arc;

use crate::compiler;
use crate::compiler::compile_stub;
use crate::compiler::dora_stub;
use crate::compiler::fct::JitFct;
use crate::compiler::map::{CodeDescriptor, CodeMap};
use crate::compiler::native_stub::{self, NativeFct, NativeFctDescriptor, NativeStubs};
use crate::driver::cmd::Args;
use crate::error::diag::Diagnostic;
use crate::gc::{Address, Gc};
use crate::object::{Ref, Testing};
use crate::safepoint;
use crate::stack::DoraToNativeInfo;
use crate::stdlib;
use crate::sym::{NestedSymTable, SymTable};
use crate::threads::{
    current_thread, deinit_current_thread, init_current_thread, DoraThread, ThreadState, Threads,
    STACK_SIZE,
};
use crate::ty::{LambdaTypes, SourceType, SourceTypeArray, SourceTypeArrays};
use crate::utils::GrowableVec;

use dora_parser::ast;
use dora_parser::interner::*;
use dora_parser::parser::NodeIdGenerator;

pub use self::classes::{
    class_accessible_from, class_field_accessible_from, find_field_in_class, find_method_in_class,
    find_methods_in_class, method_accessible_from, Candidate, Class, ClassDef, ClassDefId, ClassId,
    Field, FieldDef, FieldId, TypeParam, TypeParamDefinition, TypeParamId,
};
pub use self::consts::{const_accessible_from, ConstData, ConstId, ConstValue};
pub use self::enums::{
    enum_accessible_from, find_methods_in_enum, EnumData, EnumDef, EnumDefId, EnumId, EnumLayout,
    EnumVariant,
};
pub use self::extensions::{extension_matches, extension_matches_ty, ExtensionData, ExtensionId};
pub use self::functions::{fct_accessible_from, Fct, FctId, FctParent, Intrinsic};
pub use self::globals::{global_accessible_from, init_global_addresses, GlobalData, GlobalId};
pub use self::impls::{find_trait_impl, impl_matches, ImplData, ImplId};
pub use self::imports::ImportData;
pub use self::known::{
    KnownClasses, KnownElements, KnownEnums, KnownFunctions, KnownStructs, KnownTraits,
};
pub use self::modules::{
    find_methods_in_module, module_accessible_from, Module, ModuleDef, ModuleDefId, ModuleId,
};
pub use self::mutex::{ManagedMutex, MutexMap};
pub use self::namespaces::{
    accessible_from, namespace_accessible_from, namespace_contains, namespace_package,
    namespace_path, NamespaceData, NamespaceId,
};
pub use self::src::{
    AnalysisData, CallType, ConvInfo, ForTypeInfo, IdentType, NodeMap, Var, VarId,
};
pub use self::structs::{
    find_methods_in_struct, struct_accessible_from, struct_field_accessible_from, StructData,
    StructDef, StructDefId, StructFieldData, StructFieldDef, StructFieldId, StructId,
};
pub use self::traits::{trait_accessible_from, TraitData, TraitId};
pub use self::tuples::{ensure_tuple, TupleId, Tuples};

mod annotations;
mod classes;
mod consts;
mod enums;
mod extensions;
mod functions;
mod globals;
mod impls;
mod imports;
mod known;
mod modules;
mod mutex;
mod namespaces;
mod src;
mod structs;
mod traits;
mod tuples;

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

pub struct VM {
    pub args: Args,
    pub interner: Interner,
    pub id_generator: NodeIdGenerator,
    pub files: Arc<RwLock<Vec<File>>>,
    pub diag: Mutex<Diagnostic>,
    pub known: KnownElements,
    pub consts: GrowableVec<RwLock<ConstData>>, // stores all const definitions
    pub structs: GrowableVec<RwLock<StructData>>, // stores all struct source definitions
    pub struct_defs: GrowableVec<StructDef>,    // stores all struct definitions
    pub classes: GrowableVec<RwLock<Class>>,    // stores all class source definitions
    pub class_defs: GrowableVec<ClassDef>,      // stores all class definitions
    pub extensions: Vec<RwLock<ExtensionData>>, // stores all extension definitions
    pub tuples: Mutex<Tuples>,                  // stores all tuple definitions
    pub modules: GrowableVec<RwLock<Module>>,   // stores all module source definitions
    pub module_defs: GrowableVec<RwLock<ModuleDef>>, // stores all module definitions
    pub namespaces: Vec<NamespaceData>,         // storer all namespace definitions
    pub fcts: GrowableVec<RwLock<Fct>>,         // stores all function source definitions
    pub jit_fcts: GrowableVec<JitFct>,          // stores all function implementations
    pub enums: Vec<RwLock<EnumData>>,           // store all enum source definitions
    pub enum_defs: GrowableVec<EnumDef>,        // stores all enum definitions
    pub traits: Vec<RwLock<TraitData>>,         // stores all trait definitions
    pub impls: Vec<RwLock<ImplData>>,           // stores all impl definitions
    pub code_map: Mutex<CodeMap>,               // stores all compiled functions
    pub globals: GrowableVec<RwLock<GlobalData>>, // stores all global variables
    pub imports: Vec<ImportData>,               // stores all imports
    pub gc: Gc,                                 // garbage collector
    pub native_stubs: Mutex<NativeStubs>,
    pub source_type_arrays: Mutex<SourceTypeArrays>,
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
    pub mutex_map: MutexMap,
}

impl VM {
    pub fn new(args: Args) -> Box<VM> {
        let empty_class_def_id: ClassDefId = 0.into();
        let empty_trait_id: TraitId = 0.into();
        let empty_fct_id: FctId = 0.into();
        let empty_enum_id: EnumId = 0.into();
        let empty_struct_id = 0.into();
        let gc = Gc::new(&args);

        let prelude_namespace_id = NamespaceId(0);
        let stdlib_namespace_id = NamespaceId(1);
        let global_namespace_id = NamespaceId(2);
        let boots_namespace_id = NamespaceId(3);

        let interner = Interner::new();
        let stdlib_name = interner.intern("std");
        let boots_name = interner.intern("boots");

        let namespaces = vec![
            NamespaceData::predefined(prelude_namespace_id, None),
            NamespaceData::predefined(stdlib_namespace_id, Some(stdlib_name)),
            NamespaceData::predefined(global_namespace_id, None),
            NamespaceData::predefined(boots_namespace_id, Some(boots_name)),
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
            module_defs: GrowableVec::new(),
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
            },
            gc,
            id_generator: NodeIdGenerator::new(),
            diag: Mutex::new(Diagnostic::new()),
            fcts: GrowableVec::new(),
            jit_fcts: GrowableVec::new(),
            code_map: Mutex::new(CodeMap::new()),
            source_type_arrays: Mutex::new(SourceTypeArrays::new()),
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
            mutex_map: MutexMap::new(),
        });

        set_vm(&vm);

        vm
    }

    pub fn gc_epoch(&self) -> usize {
        self.gc.epoch()
    }

    pub fn run(&self, fct_id: FctId) -> i32 {
        let tld = current_thread().tld_address();
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.dora_stub();
        let fct: extern "C" fn(Address, Address) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr)
    }

    pub fn run_test(&self, fct_id: FctId, testing: Ref<Testing>) {
        let tld = current_thread().tld_address();
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.dora_stub();
        let fct: extern "C" fn(Address, Address, Ref<Testing>) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr, testing);
    }

    pub fn add_file(&self, path: Option<PathBuf>, namespace_id: NamespaceId, ast: Arc<ast::File>) {
        let mut files = self.files.write();
        let file_id = (files.len() as u32).into();
        files.push(File {
            id: file_id,
            path,
            namespace_id,
            ast,
        });
    }

    pub fn ensure_compiled(&self, fct_id: FctId) -> Address {
        let mut dtn = DoraToNativeInfo::new();
        let type_params = SourceTypeArray::empty();

        current_thread().use_dtn(&mut dtn, || compiler::generate(self, fct_id, &type_params))
    }

    pub fn dump_gc_summary(&self, runtime: f32) {
        self.gc.dump_summary(runtime);
    }

    pub fn insert_code_map(&self, start: Address, end: Address, desc: CodeDescriptor) {
        let mut code_map = self.code_map.lock();
        code_map.insert(start, end, desc);
    }

    pub fn add_fct(&self, mut fct: Fct) -> FctId {
        let mut fcts = self.fcts.lock();
        let fctid = FctId(fcts.len());

        fct.id = fctid;

        fcts.push(Arc::new(RwLock::new(fct)));

        fctid
    }

    pub fn namespace_table(&self, namespace_id: NamespaceId) -> Arc<RwLock<SymTable>> {
        self.namespaces[namespace_id.to_usize()].table.clone()
    }

    pub fn stdlib_namespace(&self) -> Arc<RwLock<SymTable>> {
        self.namespaces[self.stdlib_namespace_id.to_usize()]
            .table
            .clone()
    }

    pub fn prelude_namespace(&self) -> Arc<RwLock<SymTable>> {
        self.namespaces[self.prelude_namespace_id.to_usize()]
            .table
            .clone()
    }

    #[cfg(test)]
    pub fn cls_by_name(&self, name: &'static str) -> ClassId {
        let name = self.interner.intern(name);

        NestedSymTable::new(self, self.global_namespace_id)
            .get_class(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn struct_by_name(&self, name: &'static str) -> StructId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.global_namespace_id)
            .get_struct(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn enum_by_name(&self, name: &'static str) -> EnumId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.global_namespace_id)
            .get_enum(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn const_by_name(&self, name: &'static str) -> ConstId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.global_namespace_id)
            .get_const(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn cls_method_by_name(
        &self,
        class_name: &'static str,
        function_name: &'static str,
        is_static: bool,
    ) -> Option<FctId> {
        let class_name = self.interner.intern(class_name);
        let function_name = self.interner.intern(function_name);

        let cls_id = NestedSymTable::new(self, self.global_namespace_id)
            .get_class(class_name)
            .expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        let candidates = find_methods_in_class(
            self,
            cls.ty(),
            &cls.type_params,
            None,
            function_name,
            is_static,
        );
        if candidates.len() == 1 {
            Some(candidates[0].fct_id)
        } else {
            None
        }
    }

    #[cfg(test)]
    pub fn struct_method_by_name(
        &self,
        struct_name: &'static str,
        function_name: &'static str,
        is_static: bool,
    ) -> Option<FctId> {
        let struct_name = self.interner.intern(struct_name);
        let function_name = self.interner.intern(function_name);

        let struct_id = NestedSymTable::new(self, self.global_namespace_id)
            .get_struct(struct_name)
            .expect("struct not found");
        let xstruct = self.structs.idx(struct_id);
        let xstruct = xstruct.read();

        let candidates = find_methods_in_struct(
            self,
            xstruct.ty(self),
            &xstruct.type_params,
            None,
            function_name,
            is_static,
        );

        if candidates.len() == 1 {
            Some(candidates[0].fct_id)
        } else {
            None
        }
    }

    pub fn cls_def_by_name(&self, namespace_id: NamespaceId, name: &'static str) -> ClassDefId {
        use crate::semck::specialize::specialize_class_id;

        let name = self.interner.intern(name);
        let cls_id = NestedSymTable::new(self, namespace_id)
            .get_class(name)
            .expect("class not found");

        specialize_class_id(self, cls_id)
    }

    pub fn struct_def_by_name(&self, namespace_id: NamespaceId, name: &'static str) -> StructDefId {
        use crate::semck::specialize::specialize_struct_id;

        let name = self.interner.intern(name);
        let struct_id = NestedSymTable::new(self, namespace_id)
            .get_struct(name)
            .expect("struct not found");

        specialize_struct_id(self, struct_id)
    }

    pub fn cls_def_by_name_with_type_params(
        &self,
        name: &'static str,
        type_params: SourceTypeArray,
    ) -> ClassDefId {
        use crate::semck::specialize::specialize_class_id_params;

        let name = self.interner.intern(name);
        let cls_id = NestedSymTable::new(self, self.global_namespace_id)
            .get_class(name)
            .expect("class not found");

        specialize_class_id_params(self, cls_id, &type_params)
    }

    pub fn field_in_class(&self, cls_def_id: ClassDefId, name: &'static str) -> FieldId {
        let cls_def = self.class_defs.idx(cls_def_id);

        let cls_id = cls_def.cls_id.unwrap();
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        let name = self.interner.intern(name);
        cls.field_by_name(name)
    }

    #[cfg(test)]
    pub fn field_by_name(
        &self,
        class_name: &'static str,
        field_name: &'static str,
    ) -> (ClassId, FieldId) {
        let class_name = self.interner.intern(class_name);
        let field_name = self.interner.intern(field_name);

        let cls_id = NestedSymTable::new(self, self.global_namespace_id)
            .get_class(class_name)
            .expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();
        let field_id = cls.field_by_name(field_name);

        (cls_id, field_id)
    }

    pub fn fct_by_name(&self, name: &str) -> Option<FctId> {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.global_namespace_id).get_fct(name)
    }

    pub fn fct_by_name_and_namespace(
        &self,
        name: &str,
        namespace_id: NamespaceId,
    ) -> Option<FctId> {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, namespace_id).get_fct(name)
    }

    #[cfg(test)]
    pub fn ctor_by_name(&self, name: &str) -> FctId {
        let name = self.interner.intern(name);
        let cls_id = NestedSymTable::new(self, self.global_namespace_id)
            .get_class(name)
            .expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        cls.constructor.expect("no ctor found")
    }

    #[cfg(test)]
    pub fn trait_by_name(&self, name: &str) -> TraitId {
        let name = self.interner.intern(name);
        let trait_id = NestedSymTable::new(self, self.global_namespace_id)
            .get_trait(name)
            .expect("class not found");

        trait_id
    }

    #[cfg(test)]
    pub fn trait_method_by_name(&self, trait_name: &str, method_name: &str) -> FctId {
        let trait_id = self.trait_by_name(trait_name);
        let method_name = self.interner.intern(method_name);

        let xtrait = self.traits[trait_id].read();

        xtrait
            .instance_names
            .get(&method_name)
            .cloned()
            .expect("method not found")
    }

    #[cfg(test)]
    pub fn global_by_name(&self, name: &str) -> GlobalId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.global_namespace_id)
            .get_global(name)
            .expect("global not found")
    }

    pub fn cls(&self, cls_id: ClassId) -> SourceType {
        let list_id = self
            .source_type_arrays
            .lock()
            .insert(SourceTypeArray::empty());
        SourceType::Class(cls_id, list_id)
    }

    pub fn cls_with_type_params(
        &self,
        cls_id: ClassId,
        type_params: Vec<SourceType>,
    ) -> SourceType {
        let list = SourceTypeArray::with(type_params);
        let list_id = self.source_type_arrays.lock().insert(list);
        SourceType::Class(cls_id, list_id)
    }

    pub fn cls_with_type_list(&self, cls_id: ClassId, type_list: SourceTypeArray) -> SourceType {
        let list_id = self.source_type_arrays.lock().insert(type_list);
        SourceType::Class(cls_id, list_id)
    }

    pub fn modu(&self, mod_id: ModuleId) -> SourceType {
        SourceType::Module(mod_id)
    }

    pub fn dora_stub(&self) -> Address {
        let mut dora_stub_address = self.dora_stub.lock();

        if dora_stub_address.is_null() {
            *dora_stub_address = dora_stub::generate(self);
        }

        *dora_stub_address
    }

    pub fn compile_stub(&self) -> Address {
        let mut compile_stub_address = self.compile_stub.lock();

        if compile_stub_address.is_null() {
            *compile_stub_address = compile_stub::generate(self);
        }

        *compile_stub_address
    }

    pub fn trap_stub(&self) -> Address {
        let mut trap_stub_address = self.trap_stub.lock();

        if trap_stub_address.is_null() {
            let ifct = NativeFct {
                ptr: Address::from_ptr(stdlib::trap as *const u8),
                args: &[SourceType::Int32],
                return_type: SourceType::Unit,
                desc: NativeFctDescriptor::TrapStub,
            };
            let jit_fct_id = native_stub::generate(self, ifct, false);
            let jit_fct = self.jit_fcts.idx(jit_fct_id);
            let fct_ptr = jit_fct.instruction_start();
            *trap_stub_address = fct_ptr;
        }

        *trap_stub_address
    }

    pub fn stack_overflow_stub(&self) -> Address {
        let mut stack_overflow_stub_address = self.stack_overflow_stub.lock();

        if stack_overflow_stub_address.is_null() {
            let ifct = NativeFct {
                ptr: Address::from_ptr(safepoint::stack_overflow as *const u8),
                args: &[],
                return_type: SourceType::Unit,
                desc: NativeFctDescriptor::GuardCheckStub,
            };
            let jit_fct_id = native_stub::generate(self, ifct, false);
            let jit_fct = self.jit_fcts.idx(jit_fct_id);
            let fct_ptr = jit_fct.instruction_start();
            *stack_overflow_stub_address = fct_ptr;
        }

        *stack_overflow_stub_address
    }

    pub fn safepoint_stub(&self) -> Address {
        let mut safepoint_stub_address = self.safepoint_stub.lock();

        if safepoint_stub_address.is_null() {
            let ifct = NativeFct {
                ptr: Address::from_ptr(safepoint::safepoint_slow as *const u8),
                args: &[],
                return_type: SourceType::Unit,
                desc: NativeFctDescriptor::SafepointStub,
            };
            let jit_fct_id = native_stub::generate(self, ifct, false);
            let jit_fct = self.jit_fcts.idx(jit_fct_id);
            let fct_ptr = jit_fct.instruction_start();
            *safepoint_stub_address = fct_ptr;
        }

        *safepoint_stub_address
    }

    pub fn file(&self, idx: FileId) -> Arc<ast::File> {
        self.files.read().get(idx.to_usize()).unwrap().ast.clone()
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
