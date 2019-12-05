use parking_lot::{Mutex, RwLock};
use std::mem;
use std::ptr;
use std::sync::Arc;

use crate::compiler;
use crate::compiler::dora_compile;
use crate::compiler::dora_entry;
use crate::compiler::dora_native::{self, InternalFct, InternalFctDescriptor, NativeThunks};
use crate::compiler::dora_throw;
use crate::compiler::fct::JitFct;
use crate::compiler::map::{CodeDescriptor, CodeMap};
use crate::driver::cmd::Args;
use crate::error::diag::Diagnostic;
use crate::exception::DoraToNativeInfo;
use crate::gc::{Address, Gc};
use crate::object::{Ref, Testing};
use crate::safepoint::{PollingPage, Safepoint};
use crate::stdlib;
use crate::sym::Sym::*;
use crate::sym::*;
use crate::threads::{Threads, STACK_SIZE, THREAD};
use crate::ty::{BuiltinType, LambdaTypes, TypeList, TypeLists, TypeParamId};
use crate::utils::GrowableVec;

use dora_parser::ast;
use dora_parser::interner::*;
use dora_parser::lexer::File;
use dora_parser::parser::NodeIdGenerator;

pub use self::class::{
    find_field_in_class, find_method_in_class, find_methods_in_class, Class, ClassDef, ClassDefId,
    ClassId, TypeParam,
};
pub use self::cnst::{ConstData, ConstId, ConstValue};
pub use self::enums::{EnumData, EnumId};
pub use self::exception::{exception_get_and_clear, exception_set};
pub use self::fct::{Fct, FctId, FctKind, FctParent, Intrinsic};
pub use self::field::{Field, FieldDef, FieldId};
pub use self::global::{GlobalData, GlobalId};
pub use self::impls::{ImplData, ImplId};
pub use self::src::{CallType, ConvInfo, FctSrc, ForTypeInfo, IdentType, NodeMap, Var, VarId};
pub use self::strct::{
    StructData, StructDef, StructDefId, StructFieldData, StructFieldDef, StructId,
};
pub use self::traits::{TraitData, TraitId};
pub use self::vip::{KnownClasses, KnownElements, KnownFunctions};

pub mod class;
mod cnst;
mod enums;
mod exception;
mod fct;
mod field;
mod global;
mod impls;
mod src;
mod strct;
mod traits;
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
    pub fcts: GrowableVec<RwLock<Fct<'ast>>>,  // stores all function definitions
    pub jit_fcts: GrowableVec<JitFct>,         // stores all function implementations
    pub enums: Vec<RwLock<EnumData>>,          // store all enum definitions
    pub traits: Vec<RwLock<TraitData>>,        // stores all trait definitions
    pub impls: Vec<RwLock<ImplData>>,          // stores all impl definitions
    pub code_map: Mutex<CodeMap>,              // stores all compiled functions
    pub globals: GrowableVec<Mutex<GlobalData>>, // stores all global variables
    pub gc: Gc,                                // garbage collector
    pub native_thunks: Mutex<NativeThunks>,
    pub polling_page: PollingPage,
    pub lists: Mutex<TypeLists>,
    pub lambda_types: Mutex<LambdaTypes>,
    pub compiler_thunk: Mutex<Address>,
    pub dora_entry: Mutex<Address>,
    pub trap_thunk: Mutex<Address>,
    pub throw_thunk: Mutex<Address>,
    pub threads: Threads,
    pub safepoint: Safepoint,
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
            consts: GrowableVec::new(),
            structs: GrowableVec::new(),
            struct_defs: GrowableVec::new(),
            classes: GrowableVec::new(),
            files: Vec::new(),
            class_defs: GrowableVec::new(),
            enums: Vec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
            globals: GrowableVec::new(),
            interner: Interner::new(),
            vips: KnownElements {
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
                throwable_class: empty_class_id,
                error_class: empty_class_id,
                exception_class: empty_class_id,
                stack_trace_element_class: empty_class_id,

                equals_trait: empty_trait_id,
                comparable_trait: empty_trait_id,
                stringable_trait: empty_trait_id,
                iterator_trait: Mutex::new(None),

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
            code_map: Mutex::new(CodeMap::new()),
            polling_page: PollingPage::new(),
            lists: Mutex::new(TypeLists::new()),
            lambda_types: Mutex::new(LambdaTypes::new()),
            native_thunks: Mutex::new(NativeThunks::new()),
            compiler_thunk: Mutex::new(Address::null()),
            dora_entry: Mutex::new(Address::null()),
            trap_thunk: Mutex::new(Address::null()),
            throw_thunk: Mutex::new(Address::null()),
            threads: Threads::new(),
            safepoint: Safepoint::new(),
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
        let dora_entry_thunk = self.dora_entry_thunk();
        let fct: extern "C" fn(Address, Address) -> i32 =
            unsafe { mem::transmute(dora_entry_thunk) };
        fct(tld, ptr)
    }

    pub fn run_test(&self, fct_id: FctId, testing: Ref<Testing>) {
        let tld = THREAD.with(|thread| {
            let thread = thread.borrow();
            let ptr = &thread.tld;

            Address::from_ptr(ptr as *const _)
        });
        let ptr = self.ensure_compiled(fct_id);
        let dora_entry_thunk = self.dora_entry_thunk();
        let fct: extern "C" fn(Address, Address, Ref<Testing>) -> i32 =
            unsafe { mem::transmute(dora_entry_thunk) };
        fct(tld, ptr, testing);
    }

    fn ensure_compiled(&self, fct_id: FctId) -> Address {
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

    pub fn add_fct_to_sym(&mut self, fct: Fct<'ast>) -> Result<FctId, Sym> {
        let name = fct.name;
        let fctid = self.add_fct(fct);

        let mut sym = self.sym.lock();

        match sym.get(name) {
            Some(sym) => Err(sym),
            None => {
                assert!(sym.insert(name, SymFct(fctid)).is_none());

                Ok(fctid)
            }
        }
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
    pub fn cls_def_by_name(&self, name: &'static str) -> ClassDefId {
        use crate::semck::specialize::specialize_class_id;

        let name = self.interner.intern(name);
        let cls_id = self.sym.lock().get_class(name).expect("class not found");

        specialize_class_id(self, cls_id)
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

    #[cfg(test)]
    pub fn fct_by_name(&self, name: &str) -> Option<FctId> {
        let name = self.interner.intern(name);
        self.sym.lock().get_fct(name)
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

    pub fn dora_entry_thunk(&self) -> Address {
        let mut dora_entry_thunk = self.dora_entry.lock();

        if dora_entry_thunk.is_null() {
            *dora_entry_thunk = dora_entry::generate(self);
        }

        *dora_entry_thunk
    }

    pub fn throw_thunk(&self) -> Address {
        let mut throw_thunk = self.throw_thunk.lock();

        if throw_thunk.is_null() {
            *throw_thunk = dora_throw::generate(self);
        }

        *throw_thunk
    }

    pub fn compiler_thunk(&self) -> Address {
        let mut compiler_thunk = self.compiler_thunk.lock();

        if compiler_thunk.is_null() {
            *compiler_thunk = dora_compile::generate(self);
        }

        *compiler_thunk
    }

    pub fn trap_thunk(&self) -> Address {
        let mut trap_thunk = self.trap_thunk.lock();

        if trap_thunk.is_null() {
            let ifct = InternalFct {
                ptr: Address::from_ptr(stdlib::trap as *const u8),
                args: &[BuiltinType::Int],
                return_type: BuiltinType::Unit,
                throws: false,
                desc: InternalFctDescriptor::TrapThunk,
            };
            let jit_fct_id = dora_native::generate(self, ifct, false);
            let jit_fct = self.jit_fcts.idx(jit_fct_id);
            let fct_ptr = jit_fct.fct_ptr();
            *trap_thunk = fct_ptr;
        }

        *trap_thunk
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
