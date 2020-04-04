use parking_lot::{Mutex, RwLock};
use std::mem;
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
use crate::sym::TermSym::SymFct;
use crate::sym::{SymTable, TermSym};
use crate::threads::{Threads, STACK_SIZE, THREAD};
use crate::ty::{BuiltinType, LambdaTypes, TypeList, TypeLists, TypeParamId};
use crate::utils::GrowableVec;
use crate::vm::module::{Module, ModuleDef, ModuleId};

use dora_parser::ast;
use dora_parser::interner::*;
use dora_parser::lexer::File;
use dora_parser::parser::NodeIdGenerator;

pub use self::class::{
    find_field_in_class, find_method_in_class, find_methods_in_class, Class, ClassDef, ClassDefId,
    ClassId, TypeParam,
};
pub use self::cnst::{ConstData, ConstId, ConstValue};
pub use self::enums::{
    find_methods_in_enum, EnumData, EnumDef, EnumDefId, EnumId, EnumLayout, EnumVariant,
};
pub use self::extension::{ExtensionData, ExtensionId};
pub use self::fct::{Fct, FctDef, FctDefId, FctId, FctKind, FctParent, Intrinsic};
pub use self::field::{Field, FieldDef, FieldId};
pub use self::global::{GlobalData, GlobalId};
pub use self::impls::{ImplData, ImplId};
pub use self::src::{CallType, ConvInfo, FctSrc, ForTypeInfo, IdentType, NodeMap, Var, VarId};
pub use self::strct::{
    StructData, StructDef, StructDefId, StructFieldData, StructFieldDef, StructId,
};
pub use self::traits::{TraitData, TraitId};
pub use self::tuple::{ensure_tuple, TupleId, Tuples};
pub use self::vip::{KnownClasses, KnownElements, KnownFunctions};

pub mod class;
mod cnst;
mod enums;
mod extension;
mod fct;
mod field;
mod global;
mod impls;
pub mod module;
mod src;
mod strct;
mod traits;
mod tuple;
mod vip;

static mut VM_GLOBAL: *const u8 = ptr::null();

pub fn get_vm() -> &'static VM<'static> {
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

pub struct VM<'ast> {
    pub args: Args,
    pub interner: Interner,
    pub ast: &'ast ast::Ast,
    pub id_generator: NodeIdGenerator,
    pub files: Vec<File>,
    pub diag: Mutex<Diagnostic>,
    pub sym: Mutex<SymTable>,
    pub vips: KnownElements,
    pub consts: GrowableVec<Mutex<ConstData>>, // stores all const definitions
    pub structs: GrowableVec<Mutex<StructData>>, // stores all struct source definitions
    pub struct_defs: GrowableVec<Mutex<StructDef>>, // stores all struct definitions
    pub classes: GrowableVec<RwLock<Class>>,   // stores all class source definitions
    pub class_defs: GrowableVec<RwLock<ClassDef>>, // stores all class definitions
    pub extensions: Vec<RwLock<ExtensionData>>, // stores all extension definitions
    pub tuples: Mutex<Tuples>,                 // stores all tuple definitions
    pub modules: GrowableVec<RwLock<Module>>,  // stores all module source definitions
    pub module_defs: GrowableVec<RwLock<ModuleDef>>, // stores all module definitions
    pub fcts: GrowableVec<RwLock<Fct<'ast>>>,  // stores all function source definitions
    pub jit_fcts: GrowableVec<JitFct>,         // stores all function implementations
    pub fct_defs: GrowableVec<RwLock<FctDef>>, // stores all function definitions
    pub enums: Vec<RwLock<EnumData>>,          // store all enum source definitions
    pub enum_defs: GrowableVec<RwLock<EnumDef>>, // stores all enum definitions
    pub traits: Vec<RwLock<TraitData>>,        // stores all trait definitions
    pub impls: Vec<RwLock<ImplData>>,          // stores all impl definitions
    pub code_map: Mutex<CodeMap>,              // stores all compiled functions
    pub globals: GrowableVec<RwLock<GlobalData>>, // stores all global variables
    pub gc: Gc,                                // garbage collector
    pub native_stubs: Mutex<NativeStubs>,
    pub lists: Mutex<TypeLists>,
    pub lambda_types: Mutex<LambdaTypes>,
    pub compile_stub: Mutex<Address>,
    pub dora_stub: Mutex<Address>,
    pub trap_stub: Mutex<Address>,
    pub guard_check_stub: Mutex<Address>,
    pub threads: Threads,
}

impl<'ast> VM<'ast> {
    pub fn new(args: Args, ast: &'ast ast::Ast) -> Box<VM<'ast>> {
        let empty_class_id: ClassId = 0.into();
        let empty_class_def_id: ClassDefId = 0.into();
        let empty_trait_id: TraitId = 0.into();
        let empty_fct_id: FctId = 0.into();
        let gc = Gc::new(&args);

        let vm = Box::new(VM {
            args,
            files: Vec::new(),
            consts: GrowableVec::new(),
            structs: GrowableVec::new(),
            struct_defs: GrowableVec::new(),
            classes: GrowableVec::new(),
            class_defs: GrowableVec::new(),
            extensions: Vec::new(),
            tuples: Mutex::new(Tuples::new()),
            modules: GrowableVec::new(),
            module_defs: GrowableVec::new(),
            enums: Vec::new(),
            enum_defs: GrowableVec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
            globals: GrowableVec::new(),
            interner: Interner::new(),
            vips: KnownElements {
                unit_class: empty_class_id,
                bool_class: empty_class_id,
                byte_class: empty_class_id,
                char_class: empty_class_id,
                int_class: empty_class_id,
                long_class: empty_class_id,
                float_class: empty_class_id,
                double_class: empty_class_id,
                object_class: empty_class_id,
                string_class: empty_class_id,

                array_class: empty_class_id,

                cls: KnownClasses {
                    string_buffer: empty_class_id,
                },

                fct: KnownFunctions {
                    string_buffer_empty: empty_fct_id,
                    string_buffer_append: empty_fct_id,
                    string_buffer_to_string: empty_fct_id,
                },

                testing_class: empty_class_id,
                stacktrace_class: empty_class_id,
                stacktrace_element_class: empty_class_id,

                equals_trait: empty_trait_id,
                comparable_trait: empty_trait_id,
                stringable_trait: empty_trait_id,
                iterator_trait: Mutex::new(None),
                zero_trait: empty_trait_id,

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
            ast,
            id_generator: NodeIdGenerator::new(),
            diag: Mutex::new(Diagnostic::new()),
            sym: Mutex::new(SymTable::new()),
            fcts: GrowableVec::new(),
            jit_fcts: GrowableVec::new(),
            fct_defs: GrowableVec::new(),
            code_map: Mutex::new(CodeMap::new()),
            lists: Mutex::new(TypeLists::new()),
            lambda_types: Mutex::new(LambdaTypes::new()),
            native_stubs: Mutex::new(NativeStubs::new()),
            compile_stub: Mutex::new(Address::null()),
            dora_stub: Mutex::new(Address::null()),
            trap_stub: Mutex::new(Address::null()),
            guard_check_stub: Mutex::new(Address::null()),
            threads: Threads::new(),
        });

        set_vm(&vm);

        vm
    }

    pub fn run(&self, fct_id: FctId) -> i32 {
        let stack_top = stack_pointer();
        let stack_limit = stack_top.sub(STACK_SIZE);

        THREAD.with(|thread| {
            thread.borrow().tld.set_stack_limit(stack_limit);
        });

        let tld = THREAD.with(|thread| {
            let thread = thread.borrow();
            let ptr = &thread.tld;

            Address::from_ptr(ptr as *const _)
        });
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.dora_stub();
        let fct: extern "C" fn(Address, Address) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr)
    }

    pub fn run_test(&self, fct_id: FctId, testing: Ref<Testing>) {
        let tld = THREAD.with(|thread| {
            let thread = thread.borrow();
            let ptr = &thread.tld;

            Address::from_ptr(ptr as *const _)
        });
        let ptr = self.ensure_compiled(fct_id);
        let dora_stub_address = self.dora_stub();
        let fct: extern "C" fn(Address, Address, Ref<Testing>) -> i32 =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, ptr, testing);
    }

    pub fn ensure_compiled(&self, fct_id: FctId) -> Address {
        let mut dtn = DoraToNativeInfo::new();
        let type_params = TypeList::empty();

        THREAD.with(|thread| {
            thread.borrow().use_dtn(&mut dtn, || {
                compiler::generate(self, fct_id, &type_params, &type_params)
            })
        })
    }

    pub fn dump_gc_summary(&self, runtime: f32) {
        self.gc.dump_summary(runtime);
    }

    pub fn insert_code_map(&self, start: Address, end: Address, desc: CodeDescriptor) {
        let mut code_map = self.code_map.lock();
        code_map.insert(start, end, desc);
    }

    pub fn add_fct(&mut self, mut fct: Fct<'ast>) -> FctId {
        let mut fcts = self.fcts.lock();
        let fctid = FctId(fcts.len());

        fct.id = fctid;

        fcts.push(Arc::new(RwLock::new(fct)));

        fctid
    }

    pub fn add_fct_to_sym(&mut self, fct: Fct<'ast>) -> Result<FctId, TermSym> {
        let name = fct.name;
        let fctid = self.add_fct(fct);

        let mut sym = self.sym.lock();

        match sym.get_term(name) {
            Some(sym) => Err(sym),
            None => {
                assert!(sym.insert_term(name, SymFct(fctid)).is_none());

                Ok(fctid)
            }
        }
    }

    pub fn add_fct_def(&self, mut fct_def: FctDef) -> FctDefId {
        let mut fct_defs = self.fct_defs.lock();
        let fid = FctDefId(fct_defs.len());

        fct_def.id = fid;

        fct_defs.push(Arc::new(RwLock::new(fct_def)));

        fid
    }

    #[cfg(test)]
    pub fn cls_by_name(&self, name: &'static str) -> ClassId {
        let name = self.interner.intern(name);
        self.sym.lock().get_class(name).expect("class not found")
    }

    #[cfg(test)]
    pub fn const_by_name(&self, name: &'static str) -> ConstId {
        let name = self.interner.intern(name);
        self.sym.lock().get_const(name).expect("class not found")
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

        let cls_id = self
            .sym
            .lock()
            .get_class(class_name)
            .expect("class not found");
        let cls = self.cls(cls_id);

        let candidates = find_methods_in_class(self, cls, function_name, is_static);
        if candidates.len() == 1 {
            Some(candidates[0].1)
        } else {
            None
        }
    }

    #[cfg(test)]
    pub fn cls_method_def_by_name(
        &self,
        class_name: &'static str,
        function_name: &'static str,
        is_static: bool,
    ) -> Option<FctDefId> {
        let class_name = self.interner.intern(class_name);
        let function_name = self.interner.intern(function_name);

        let cls_id = self
            .sym
            .lock()
            .get_class(class_name)
            .expect("class not found");
        let cls = self.cls(cls_id);

        let candidates = find_methods_in_class(self, cls, function_name, is_static);
        if candidates.len() == 1 {
            let fct_id = candidates[0].1;
            let fct = self.fcts.idx(fct_id);
            let fct = fct.read();
            let fct_def = fct
                .specializations
                .read()
                .get(&(TypeList::Empty, TypeList::Empty))
                .and_then(|fct_def_id| Some(*fct_def_id));

            fct_def
        } else {
            None
        }
    }

    pub fn cls_def_by_name(&self, name: &'static str) -> ClassDefId {
        use crate::semck::specialize::specialize_class_id;

        let name = self.interner.intern(name);
        let cls_id = self.sym.lock().get_class(name).expect("class not found");

        specialize_class_id(self, cls_id)
    }

    pub fn cls_def_by_name_with_type_params(
        &self,
        name: &'static str,
        cls_type_params: TypeList,
    ) -> ClassDefId {
        use crate::semck::specialize::specialize_class_id_params;

        let name = self.interner.intern(name);
        let cls_id = self.sym.lock().get_class(name).expect("class not found");

        specialize_class_id_params(self, cls_id, &cls_type_params)
    }

    pub fn field_in_class(&self, cls_def_id: ClassDefId, name: &'static str) -> FieldId {
        let cls_def = self.class_defs.idx(cls_def_id);
        let cls_def = cls_def.read();

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
    ) -> (ClassDefId, FieldId) {
        use crate::semck::specialize;

        let class_name = self.interner.intern(class_name);
        let field_name = self.interner.intern(field_name);

        let cls_id = self
            .sym
            .lock()
            .get_class(class_name)
            .expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();
        let field_id = cls.field_by_name(field_name);
        let cls_id = specialize::specialize_class_ty(self, cls.ty);

        (cls_id, field_id)
    }

    pub fn fct_by_name(&self, name: &str) -> Option<FctId> {
        let name = self.interner.intern(name);
        self.sym.lock().get_fct(name)
    }

    #[cfg(test)]
    pub fn fct_def_by_name(&self, name: &str) -> Option<FctDefId> {
        let fct_id = self.fct_by_name(name).expect("function not found");
        let fct = self.fcts.idx(fct_id);
        let fct = fct.read();
        let fct_def = fct
            .specializations
            .read()
            .get(&(TypeList::Empty, TypeList::Empty))
            .and_then(|fct_def_id| Some(*fct_def_id));

        fct_def
    }

    #[cfg(test)]
    pub fn ctor_by_name(&self, name: &str) -> FctId {
        let name = self.interner.intern(name);
        let cls_id = self.sym.lock().get_class(name).expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        cls.constructor.expect("no ctor found")
    }

    #[cfg(test)]
    pub fn ctor_def_by_name(&self, name: &str) -> FctDefId {
        let name = self.interner.intern(name);
        let cls_id = self.sym.lock().get_class(name).expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        let fct_id = cls.constructor.expect("no ctor found");
        let fct = self.fcts.idx(fct_id);
        let fct = fct.read();
        let fct_def = fct
            .specializations
            .read()
            .get(&(TypeList::Empty, TypeList::Empty))
            .and_then(|fct_def_id| Some(*fct_def_id))
            .expect("no ctor definition found");

        fct_def
    }

    #[cfg(test)]
    pub fn ctor_def_by_name_with_type_params(
        &self,
        name: &str,
        cls_type_params: TypeList,
    ) -> FctDefId {
        let name = self.interner.intern(name);
        let cls_id = self.sym.lock().get_class(name).expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        let fct_id = cls.constructor.expect("no ctor found");
        let fct = self.fcts.idx(fct_id);
        let fct = fct.read();
        let fct_def = fct
            .specializations
            .read()
            .get(&(cls_type_params, TypeList::empty()))
            .and_then(|fct_def_id| Some(*fct_def_id))
            .expect("no ctor definition found");

        fct_def
    }

    #[cfg(test)]
    pub fn global_by_name(&self, name: &str) -> GlobalId {
        let name = self.interner.intern(name);
        self.sym.lock().get_global(name).expect("global not found")
    }

    pub fn cls(&self, cls_id: ClassId) -> BuiltinType {
        let list_id = self.lists.lock().insert(TypeList::empty());
        BuiltinType::Class(cls_id, list_id)
    }

    pub fn cls_with_type_params(
        &self,
        cls_id: ClassId,
        type_params: Vec<BuiltinType>,
    ) -> BuiltinType {
        let list = TypeList::with(type_params);
        let list_id = self.lists.lock().insert(list);
        BuiltinType::Class(cls_id, list_id)
    }

    pub fn cls_with_type_list(&self, cls_id: ClassId, type_list: TypeList) -> BuiltinType {
        let list_id = self.lists.lock().insert(type_list);
        BuiltinType::Class(cls_id, list_id)
    }

    pub fn modu(&self, mod_id: ModuleId) -> BuiltinType {
        BuiltinType::Module(mod_id)
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
                args: &[BuiltinType::Int],
                return_type: BuiltinType::Unit,
                desc: NativeFctDescriptor::TrapStub,
            };
            let jit_fct_id = native_stub::generate(self, ifct, false);
            let jit_fct = self.jit_fcts.idx(jit_fct_id);
            let fct_ptr = jit_fct.instruction_start();
            *trap_stub_address = fct_ptr;
        }

        *trap_stub_address
    }

    pub fn guard_check_stub(&self) -> Address {
        let mut guard_check_stub_address = self.guard_check_stub.lock();

        if guard_check_stub_address.is_null() {
            let ifct = NativeFct {
                ptr: Address::from_ptr(safepoint::guard_check as *const u8),
                args: &[],
                return_type: BuiltinType::Unit,
                desc: NativeFctDescriptor::GuardCheckStub,
            };
            let jit_fct_id = native_stub::generate(self, ifct, false);
            let jit_fct = self.jit_fcts.idx(jit_fct_id);
            let fct_ptr = jit_fct.instruction_start();
            *guard_check_stub_address = fct_ptr;
        }

        *guard_check_stub_address
    }

    pub fn file(&self, idx: FileId) -> &File {
        &self.files[idx.0 as usize]
    }
}

unsafe impl<'ast> Sync for VM<'ast> {}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileId(u32);

impl From<u32> for FileId {
    fn from(data: u32) -> FileId {
        FileId(data)
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
            _ => None,
        }
    }
}
