use parking_lot::{Mutex, RwLock};
use std::collections::hash_map::Iter;
use std::collections::HashMap;
use std::collections::HashSet;
use std::mem;
use std::ops::{Index, IndexMut};
use std::ptr;
use std::sync::Arc;

use crate::driver::cmd::Args;
use dora_parser::error::diag::Diagnostic;

use crate::baseline;
use crate::baseline::dora_compile;
use crate::baseline::dora_entry;
use crate::baseline::dora_native::{self, InternalFct, InternalFctDescriptor, NativeThunks};
use crate::baseline::dora_throw;
use crate::baseline::fct::{JitFct, JitFctId};
use crate::baseline::map::{CodeDescriptor, CodeMap};
use crate::class::{Class, ClassDef, ClassDefId, ClassId, FieldId, TypeParams};
use crate::exception::DoraToNativeInfo;
use crate::gc::{Address, Gc};
use crate::object::{Ref, Testing};
use crate::os::perf::counters::PerfCounters;
use crate::safepoint::{PollingPage, Safepoint};
use crate::semck::specialize::{specialize_class_id, specialize_class_id_params};
use crate::stdlib;
use crate::sym::Sym::*;
use crate::sym::*;
use crate::threads::{Threads, THREAD};
use crate::ty::{BuiltinType, LambdaTypes, TypeLists};
use crate::utils::GrowableVec;
use dora_parser::ast;
use dora_parser::interner::*;
use dora_parser::lexer::position::Position;

pub static mut EXCEPTION_OBJECT: *const u8 = 0 as *const u8;

pub fn has_exception() -> bool {
    unsafe { !EXCEPTION_OBJECT.is_null() }
}

pub fn exception_get_and_clear() -> *const u8 {
    unsafe {
        let val = EXCEPTION_OBJECT;

        if !val.is_null() {
            EXCEPTION_OBJECT = ptr::null();
        }

        val
    }
}

pub fn exception_set(val: *const u8) {
    unsafe {
        EXCEPTION_OBJECT = val;
    }
}

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

pub type VM<'ast> = SemContext<'ast>;

pub struct SemContext<'ast> {
    pub args: Args,
    pub interner: Interner,
    pub ast: &'ast ast::Ast,
    pub diag: Mutex<Diagnostic>,
    pub sym: Mutex<SymTable>,
    pub vips: KnownElements,
    pub consts: GrowableVec<Mutex<ConstData<'ast>>>, // stores all const definitions
    pub structs: GrowableVec<Mutex<StructData>>,     // stores all struct source definitions
    pub struct_defs: GrowableVec<Mutex<StructDef>>,  // stores all struct definitions
    pub classes: GrowableVec<RwLock<Class>>,         // stores all class source definitions
    pub class_defs: GrowableVec<RwLock<ClassDef>>,   // stores all class definitions
    pub fcts: GrowableVec<RwLock<Fct<'ast>>>,        // stores all function definitions
    pub jit_fcts: GrowableVec<JitFct>,               // stores all function implementations
    pub traits: Vec<RwLock<TraitData>>,              // stores all trait definitions
    pub impls: Vec<RwLock<ImplData>>,                // stores all impl definitions
    pub code_map: Mutex<CodeMap>,                    // stores all compiled functions
    pub globals: GrowableVec<Mutex<GlobalData<'ast>>>, // stores all global variables
    pub gc: Gc,                                      // garbage collector
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
    pub perf_counters: PerfCounters,
}

impl<'ast> SemContext<'ast> {
    pub fn new(args: Args, ast: &'ast ast::Ast, interner: Interner) -> Box<SemContext<'ast>> {
        let empty_class_id: ClassId = 0.into();
        let empty_class_def_id: ClassDefId = 0.into();
        let empty_trait_id: TraitId = 0.into();
        let gc = Gc::new(&args);
        let perf_counters = PerfCounters::new(args.flag_gc_counters);

        let ctxt = Box::new(SemContext {
            args: args,
            consts: GrowableVec::new(),
            structs: GrowableVec::new(),
            struct_defs: GrowableVec::new(),
            classes: GrowableVec::new(),
            class_defs: GrowableVec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
            globals: GrowableVec::new(),
            interner: interner,
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

                testing_class: empty_class_id,
                exception_class: empty_class_id,
                stack_trace_element_class: empty_class_id,

                equals_trait: empty_trait_id,
                comparable_trait: empty_trait_id,
                iterator_trait: Mutex::new(None),

                int_array_def: Mutex::new(None),
                str_class_def: Mutex::new(None),
                obj_class_def: Mutex::new(None),
                ste_class_def: Mutex::new(None),
                ex_class_def: Mutex::new(None),

                free_object_class_def: empty_class_def_id,
                free_array_class_def: empty_class_def_id,
            },
            gc: gc,
            ast: ast,
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
            perf_counters: perf_counters,
        });

        set_vm(&ctxt);

        ctxt
    }

    pub fn run(&self, fct_id: FctId) -> i32 {
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
        let type_params = TypeParams::empty();

        THREAD.with(|thread| {
            thread.borrow().use_dtn(&mut dtn, || {
                baseline::generate(self, fct_id, &type_params, &type_params)
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
    pub fn cls_def_by_name(&self, name: &'static str) -> ClassDefId {
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
        let list_id = self.lists.lock().insert(TypeParams::empty());
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
}

unsafe impl<'ast> Sync for SemContext<'ast> {}

impl<'ast> GrowableVec<RwLock<Fct<'ast>>> {
    pub fn idx(&self, index: FctId) -> Arc<RwLock<Fct<'ast>>> {
        self.idx_usize(index.0)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructDefId(usize);

impl From<usize> for StructDefId {
    fn from(data: usize) -> StructDefId {
        StructDefId(data)
    }
}

impl GrowableVec<Mutex<StructDef>> {
    pub fn idx(&self, index: StructDefId) -> Arc<Mutex<StructDef>> {
        self.idx_usize(index.0)
    }
}

pub struct StructDef {
    pub fields: Vec<StructFieldDef>,
    pub size: i32,
    pub align: i32,
    pub ref_fields: Vec<i32>,
}

#[derive(Debug, Clone)]
pub struct StructFieldDef {
    pub offset: i32,
    pub ty: BuiltinType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GlobalId(u32);

impl From<u32> for GlobalId {
    fn from(data: u32) -> GlobalId {
        GlobalId(data)
    }
}

#[derive(Debug)]
pub struct GlobalData<'ast> {
    pub id: GlobalId,
    pub ast: &'ast ast::Global,
    pub pos: Position,
    pub ty: BuiltinType,
    pub reassignable: bool,
    pub name: Name,
    pub getter: Option<FctId>,
    pub address_init: Address,
    pub address_value: Address,
}

impl<'ast> GrowableVec<Mutex<GlobalData<'ast>>> {
    pub fn idx(&self, index: GlobalId) -> Arc<Mutex<GlobalData<'ast>>> {
        self.idx_usize(index.0 as usize)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplId(u32);

impl From<u32> for ImplId {
    fn from(data: u32) -> ImplId {
        ImplId(data)
    }
}

#[derive(Debug)]
pub struct ImplData {
    pub id: ImplId,
    pub pos: Position,
    pub trait_id: Option<TraitId>,
    pub class_id: Option<ClassId>,
    pub methods: Vec<FctId>,
}

impl ImplData {
    pub fn trait_id(&self) -> TraitId {
        self.trait_id.expect("trait_id not initialized yet.")
    }

    pub fn cls_id(&self) -> ClassId {
        self.class_id.expect("trait_id not initialized yet.")
    }

    pub fn find_implements(&self, ctxt: &SemContext, fct_id: FctId) -> Option<FctId> {
        for &mtd_id in &self.methods {
            let mtd = ctxt.fcts.idx(mtd_id);
            let mtd = mtd.read();

            if mtd.impl_for == Some(fct_id) {
                return Some(mtd_id);
            }
        }

        None
    }
}

impl Index<ImplId> for Vec<RwLock<ImplData>> {
    type Output = RwLock<ImplData>;

    fn index(&self, index: ImplId) -> &RwLock<ImplData> {
        &self[index.0 as usize]
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TraitId(u32);

impl From<u32> for TraitId {
    fn from(data: u32) -> TraitId {
        TraitId(data)
    }
}

#[derive(Debug)]
pub struct TraitData {
    pub id: TraitId,
    pub pos: Position,
    pub name: Name,
    pub methods: Vec<FctId>,
}

impl TraitData {
    pub fn find_method(
        &self,
        ctxt: &SemContext,
        is_static: bool,
        name: Name,
        replace: Option<BuiltinType>,
        args: &[BuiltinType],
    ) -> Option<FctId> {
        for &method in &self.methods {
            let method = ctxt.fcts.idx(method);
            let method = method.read();

            if method.name == name
                && method.is_static == is_static
                && params_match(replace, method.params_without_self(), args)
            {
                return Some(method.id);
            }
        }

        None
    }
}

fn params_match(
    replace: Option<BuiltinType>,
    trait_args: &[BuiltinType],
    args: &[BuiltinType],
) -> bool {
    if trait_args.len() != args.len() {
        return false;
    }

    for (ind, &ty) in trait_args.iter().enumerate() {
        let other = args[ind];

        let found = if ty == BuiltinType::This {
            replace.is_none() || replace.unwrap() == other
        } else {
            ty == other
        };

        if !found {
            return false;
        }
    }

    true
}

impl Index<TraitId> for Vec<RwLock<TraitData>> {
    type Output = RwLock<TraitData>;

    fn index(&self, index: TraitId) -> &RwLock<TraitData> {
        &self[index.0 as usize]
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructId(u32);

impl GrowableVec<Mutex<StructData>> {
    pub fn idx(&self, index: StructId) -> Arc<Mutex<StructData>> {
        self.idx_usize(index.0 as usize)
    }
}

impl From<u32> for StructId {
    fn from(data: u32) -> StructId {
        StructId(data)
    }
}

#[derive(Debug)]
pub struct StructData {
    pub id: StructId,
    pub pos: Position,
    pub name: Name,
    pub fields: Vec<StructFieldData>,
    pub specializations: RwLock<HashMap<TypeParams, StructDefId>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct StructFieldId(u32);

impl From<u32> for StructFieldId {
    fn from(data: u32) -> StructFieldId {
        StructFieldId(data)
    }
}

#[derive(Debug)]
pub struct StructFieldData {
    pub id: StructFieldId,
    pub pos: Position,
    pub name: Name,
    pub ty: BuiltinType,
    pub offset: i32,
}

#[derive(Debug)]
pub struct KnownElements {
    pub bool_class: ClassId,
    pub byte_class: ClassId,
    pub char_class: ClassId,
    pub int_class: ClassId,
    pub long_class: ClassId,
    pub float_class: ClassId,
    pub double_class: ClassId,
    pub object_class: ClassId,
    pub string_class: ClassId,
    pub array_class: ClassId,

    pub testing_class: ClassId,
    pub exception_class: ClassId,
    pub stack_trace_element_class: ClassId,

    pub equals_trait: TraitId,
    pub comparable_trait: TraitId,
    pub iterator_trait: Mutex<Option<TraitId>>,

    int_array_def: Mutex<Option<ClassDefId>>,
    str_class_def: Mutex<Option<ClassDefId>>,
    obj_class_def: Mutex<Option<ClassDefId>>,
    ste_class_def: Mutex<Option<ClassDefId>>,
    ex_class_def: Mutex<Option<ClassDefId>>,

    pub free_object_class_def: ClassDefId,
    pub free_array_class_def: ClassDefId,
}

impl KnownElements {
    pub fn iterator(&self) -> TraitId {
        self.iterator_trait.lock().expect("iterator trait not set")
    }

    pub fn int_array(&self, ctxt: &SemContext) -> ClassDefId {
        let mut int_array_def = self.int_array_def.lock();

        if let Some(cls_id) = *int_array_def {
            cls_id
        } else {
            let type_args: TypeParams = vec![BuiltinType::Int].into();
            let cls_id = specialize_class_id_params(ctxt, self.array_class, &type_args);
            *int_array_def = Some(cls_id);
            cls_id
        }
    }

    pub fn str(&self, ctxt: &SemContext) -> ClassDefId {
        let mut str_class_def = self.str_class_def.lock();

        if let Some(cls_id) = *str_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(ctxt, self.string_class);
            *str_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn obj(&self, ctxt: &SemContext) -> ClassDefId {
        let mut obj_class_def = self.obj_class_def.lock();

        if let Some(cls_id) = *obj_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(ctxt, self.object_class);
            *obj_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn stack_trace_element(&self, ctxt: &SemContext) -> ClassDefId {
        let mut ste_class_def = self.ste_class_def.lock();

        if let Some(cls_id) = *ste_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(ctxt, self.stack_trace_element_class);
            *ste_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn exception(&self, ctxt: &SemContext) -> ClassDefId {
        let mut ex_class_def = self.ex_class_def.lock();

        if let Some(cls_id) = *ex_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(ctxt, self.exception_class);
            *ex_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn find_class(&self, ty: BuiltinType) -> Option<ClassId> {
        match ty {
            BuiltinType::Bool => Some(self.bool_class),
            BuiltinType::Byte => Some(self.byte_class),
            BuiltinType::Char => Some(self.char_class),
            BuiltinType::Int => Some(self.int_class),
            BuiltinType::Long => Some(self.long_class),
            BuiltinType::Float => Some(self.float_class),
            BuiltinType::Double => Some(self.double_class),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct FctId(pub usize);

impl From<usize> for FctId {
    fn from(id: usize) -> FctId {
        FctId(id)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FctParent {
    Class(ClassId),
    Trait(TraitId),
    Impl(ImplId),
    None,
}

impl FctParent {
    pub fn is_none(&self) -> bool {
        match self {
            &FctParent::None => true,
            _ => false,
        }
    }

    pub fn cls_id(&self) -> ClassId {
        match self {
            &FctParent::Class(id) => id,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub name: Name,
    pub class_bound: Option<ClassId>,
    pub trait_bounds: HashSet<TraitId>,
}

impl TypeParam {
    pub fn new(name: Name) -> TypeParam {
        TypeParam {
            name: name,
            class_bound: None,
            trait_bounds: HashSet::new(),
        }
    }
}

#[derive(Debug)]
pub struct Fct<'ast> {
    pub id: FctId,
    pub ast: &'ast ast::Function,
    pub pos: Position,
    pub name: Name,
    pub parent: FctParent,
    pub has_open: bool,
    pub has_override: bool,
    pub has_final: bool,
    pub is_static: bool,
    pub is_pub: bool,
    pub is_abstract: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub overrides: Option<FctId>,
    pub param_types: Vec<BuiltinType>,
    pub return_type: BuiltinType,
    pub is_constructor: bool,

    pub vtable_index: Option<u32>,
    pub impl_for: Option<FctId>,
    pub initialized: bool,
    pub throws: bool,

    pub type_params: Vec<TypeParam>,
    pub kind: FctKind,
}

impl<'ast> Fct<'ast> {
    pub fn is_virtual(&self) -> bool {
        (self.has_open || self.has_override) && !self.has_final
    }

    pub fn in_class(&self) -> bool {
        match self.parent {
            FctParent::Class(_) => true,
            _ => false,
        }
    }

    pub fn in_trait(&self) -> bool {
        match self.parent {
            FctParent::Trait(_) => true,
            _ => false,
        }
    }

    pub fn cls_id(&self) -> ClassId {
        match self.parent {
            FctParent::Class(clsid) => clsid,
            _ => unreachable!(),
        }
    }

    pub fn trait_id(&self) -> TraitId {
        match self.parent {
            FctParent::Trait(traitid) => traitid,
            _ => unreachable!(),
        }
    }

    pub fn full_name(&self, ctxt: &SemContext) -> String {
        let mut repr = String::new();

        if let FctParent::Class(class_id) = self.parent {
            let cls = ctxt.classes.idx(class_id);
            let cls = cls.read();
            let name = cls.name;
            repr.push_str(&ctxt.interner.str(name));

            if self.is_static {
                repr.push_str("::");
            } else {
                repr.push_str(".");
            }
        }

        repr.push_str(&ctxt.interner.str(self.name));

        if self.type_params.len() > 0 {
            repr.push_str("<");

            repr.push_str(
                &self
                    .type_params
                    .iter()
                    .map(|n| ctxt.interner.str(n.name).to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            );
            repr.push_str(">");
        }

        repr.push_str("(");

        for (ind, ty) in self.params_without_self().iter().enumerate() {
            if ind > 0 {
                repr.push_str(", ");
            }

            let name = ty.name(ctxt);
            repr.push_str(&name);
        }

        repr.push_str(")");

        if self.return_type != BuiltinType::Unit {
            repr.push_str(" -> ");

            let name = self.return_type.name(ctxt);
            repr.push_str(&name);
        }

        repr
    }

    pub fn is_src(&self) -> bool {
        match self.kind {
            FctKind::Source(_) => true,
            _ => false,
        }
    }

    pub fn pos(&self) -> Position {
        self.ast.pos
    }

    pub fn src(&self) -> &RwLock<FctSrc> {
        match self.kind {
            FctKind::Source(ref src) => src,
            _ => panic!("source expected"),
        }
    }

    pub fn has_self(&self) -> bool {
        match self.parent {
            FctParent::Class(_) | FctParent::Trait(_) | FctParent::Impl(_) => !self.is_static,

            _ => false,
        }
    }

    pub fn params_with_self(&self) -> &[BuiltinType] {
        &self.param_types
    }

    pub fn params_without_self(&self) -> &[BuiltinType] {
        if self.has_self() {
            &self.param_types[1..]
        } else {
            &self.param_types
        }
    }
}

#[derive(Debug)]
pub enum FctKind {
    Source(RwLock<FctSrc>),
    Definition,
    Native(Address),
    Builtin(Intrinsic),
}

impl FctKind {
    pub fn is_src(&self) -> bool {
        match *self {
            FctKind::Source(_) => true,
            _ => false,
        }
    }

    pub fn is_intrinsic(&self) -> bool {
        match *self {
            FctKind::Builtin(_) => true,
            _ => false,
        }
    }

    pub fn is_definition(&self) -> bool {
        match *self {
            FctKind::Definition => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Intrinsic {
    GenericArrayCtorEmpty,
    GenericArrayCtorElem,
    GenericArrayLen,
    GenericArrayGet,
    GenericArraySet,

    DefaultValue,

    Assert,
    Debug,
    Shl,

    SetUint8,

    StrLen,
    StrGet,
    StrSet,

    BoolEq,
    BoolNot,
    BoolToInt,
    BoolToLong,

    ByteEq,
    ByteCmp,
    ByteNot,
    ByteToInt,
    ByteToLong,

    CharEq,
    CharCmp,
    CharToInt,
    CharToLong,

    IntToByte,
    IntToChar,
    IntToLong,
    IntToFloat,
    IntToDouble,
    IntAsFloat,

    IntEq,
    IntCmp,

    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntMod,

    IntOr,
    IntAnd,
    IntXor,

    IntShl,
    IntSar,
    IntShr,

    IntNot,
    IntNeg,
    IntPlus,

    LongToInt,
    LongToChar,
    LongToByte,
    LongToFloat,
    LongToDouble,
    LongAsDouble,

    LongEq,
    LongCmp,

    LongAdd,
    LongSub,
    LongMul,
    LongDiv,
    LongMod,

    LongOr,
    LongAnd,
    LongXor,

    LongShl,
    LongSar,
    LongShr,

    LongNot,
    LongNeg,
    LongPlus,

    FloatToInt,
    FloatToLong,
    FloatToDouble,
    FloatAsInt,

    FloatEq,
    FloatCmp,

    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,

    FloatPlus,
    FloatNeg,
    FloatIsNan,
    FloatSqrt,

    FloatArrayLen,
    FloatArrayGet,
    FloatArraySet,

    DoubleToInt,
    DoubleToLong,
    DoubleToFloat,
    DoubleAsLong,

    DoubleEq,
    DoubleCmp,

    DoubleAdd,
    DoubleSub,
    DoubleMul,
    DoubleDiv,

    DoublePlus,
    DoubleNeg,
    DoubleIsNan,
    DoubleSqrt,

    DoubleArrayLen,
    DoubleArrayGet,
    DoubleArraySet,
}

#[derive(Debug)]
pub struct FctSrc {
    pub map_calls: NodeMap<Arc<CallType>>, // maps function call to FctId
    pub map_idents: NodeMap<IdentType>,
    pub map_tys: NodeMap<BuiltinType>,
    pub map_vars: NodeMap<VarId>,
    pub map_convs: NodeMap<ConvInfo>,
    pub map_cls: NodeMap<ClassId>,
    pub map_fors: NodeMap<ForTypeInfo>,

    pub always_returns: bool, // true if function is always exited via return statement
    // false if function execution could reach the closing } of this function
    pub specializations: RwLock<HashMap<(TypeParams, TypeParams), JitFctId>>,
    pub vars: Vec<Var>, // variables in functions
}

impl Clone for FctSrc {
    fn clone(&self) -> FctSrc {
        FctSrc {
            map_calls: self.map_calls.clone(),
            map_idents: self.map_idents.clone(),
            map_tys: self.map_tys.clone(),
            map_vars: self.map_vars.clone(),
            map_convs: self.map_convs.clone(),
            map_cls: self.map_cls.clone(),
            map_fors: self.map_fors.clone(),

            vars: self.vars.clone(),
            always_returns: self.always_returns,
            specializations: RwLock::new(HashMap::new()),
        }
    }
}

impl FctSrc {
    pub fn new() -> FctSrc {
        FctSrc {
            map_calls: NodeMap::new(),
            map_idents: NodeMap::new(),
            map_tys: NodeMap::new(),
            map_vars: NodeMap::new(),
            map_convs: NodeMap::new(),
            map_cls: NodeMap::new(),
            map_fors: NodeMap::new(),

            vars: Vec::new(),
            always_returns: false,
            specializations: RwLock::new(HashMap::new()),
        }
    }

    pub fn set_ty(&mut self, id: ast::NodeId, ty: BuiltinType) {
        self.map_tys.insert_or_replace(id, ty);
    }

    pub fn ty(&self, id: ast::NodeId) -> BuiltinType {
        self.map_tys.get(id).unwrap().clone()
    }

    pub fn var_self(&self) -> &Var {
        &self.vars[0]
    }

    pub fn var_self_mut(&mut self) -> &mut Var {
        &mut self.vars[0]
    }
}

#[derive(Clone, Debug)]
pub struct NodeMap<V>
where
    V: Clone,
{
    map: HashMap<ast::NodeId, V>,
}

impl<V> NodeMap<V>
where
    V: Clone,
{
    pub fn new() -> NodeMap<V> {
        NodeMap {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, id: ast::NodeId) -> Option<&V> {
        self.map.get(&id)
    }

    pub fn get_mut(&mut self, id: ast::NodeId) -> Option<&mut V> {
        self.map.get_mut(&id)
    }

    pub fn insert(&mut self, id: ast::NodeId, data: V) {
        let old = self.map.insert(id, data);
        assert!(old.is_none());
    }

    pub fn replace(&mut self, id: ast::NodeId, data: V) {
        let old = self.map.insert(id, data);
        assert!(old.is_some());
    }

    pub fn insert_or_replace(&mut self, id: ast::NodeId, data: V) {
        self.map.insert(id, data);
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }

    pub fn iter(&self) -> Iter<ast::NodeId, V> {
        self.map.iter()
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ConvInfo {
    pub cls_id: ClassId,
    pub valid: bool,
}

#[derive(Debug, Copy, Clone)]
pub enum Store {
    Reg,
    Temp(i32, BuiltinType),
}

impl Store {
    pub fn offset(&self) -> i32 {
        match *self {
            Store::Temp(offset, _) => offset,
            Store::Reg => panic!(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum IdentType {
    Var(VarId),
    Global(GlobalId),
    Field(BuiltinType, FieldId),
    Struct(StructId),
    Const(ConstId),
    Fct(FctId),
}

impl IdentType {
    pub fn var_id(&self) -> VarId {
        match *self {
            IdentType::Var(varid) => varid,
            _ => panic!(),
        }
    }

    pub fn struct_id(&self) -> StructId {
        match self {
            &IdentType::Struct(sid) => sid,
            _ => panic!(),
        }
    }

    pub fn is_var(&self) -> bool {
        match *self {
            IdentType::Var(_) => true,
            _ => false,
        }
    }

    pub fn is_field(&self) -> bool {
        match *self {
            IdentType::Field(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ForTypeInfo {
    pub make_iterator: FctId,
    pub next: FctId,
    pub has_next: FctId,
    pub iterator_type: BuiltinType,
}

#[derive(Debug, Clone)]
pub enum CallType {
    Fct(FctId, TypeParams, TypeParams),
    Method(BuiltinType, FctId, TypeParams),
    CtorNew(ClassId, FctId, TypeParams),
    Ctor(ClassId, FctId, TypeParams),
}

impl CallType {
    pub fn is_ctor_new(&self) -> bool {
        match *self {
            CallType::CtorNew(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_ctor(&self) -> bool {
        match *self {
            CallType::Ctor(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_method(&self) -> bool {
        match *self {
            CallType::Method(_, _, _) => true,
            _ => false,
        }
    }

    pub fn fct_id(&self) -> FctId {
        match *self {
            CallType::Fct(fctid, _, _) => fctid,
            CallType::Method(_, fctid, _) => fctid,
            CallType::CtorNew(_, fctid, _) => fctid,
            CallType::Ctor(_, fctid, _) => fctid,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CallSite<'ast> {
    pub callee: FctId,
    pub cls_type_params: TypeParams,
    pub fct_type_params: TypeParams,
    pub args: Vec<Arg<'ast>>,
    pub super_call: bool,
    pub return_type: BuiltinType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConstId(usize);

impl From<usize> for ConstId {
    fn from(data: usize) -> ConstId {
        ConstId(data)
    }
}

impl<'ast> GrowableVec<Mutex<ConstData<'ast>>> {
    pub fn idx(&self, index: ConstId) -> Arc<Mutex<ConstData<'ast>>> {
        self.idx_usize(index.0 as usize)
    }
}

#[derive(Clone, Debug)]
pub struct ConstData<'ast> {
    pub id: ConstId,
    pub pos: Position,
    pub name: Name,
    pub ty: BuiltinType,
    pub expr: &'ast ast::Expr,
    pub value: ConstValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstValue {
    None,
    Bool(bool),
    Char(char),
    Int(i64),
    Float(f64),
}

impl ConstValue {
    pub fn to_bool(&self) -> bool {
        match self {
            &ConstValue::Bool(b) => b,
            _ => unreachable!(),
        }
    }

    pub fn to_char(&self) -> char {
        match self {
            &ConstValue::Char(c) => c,
            _ => unreachable!(),
        }
    }

    pub fn to_int(&self) -> i64 {
        match self {
            &ConstValue::Int(i) => i,
            _ => unreachable!(),
        }
    }

    pub fn to_float(&self) -> f64 {
        match self {
            &ConstValue::Float(f) => f,
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Arg<'ast> {
    Expr(&'ast ast::Expr, BuiltinType, i32),
    Stack(i32, BuiltinType, i32),
    SelfieNew(BuiltinType, i32),
    Selfie(BuiltinType, i32),
}

impl<'ast> Arg<'ast> {
    pub fn offset(&self) -> i32 {
        match *self {
            Arg::Expr(_, _, offset) => offset,
            Arg::Stack(_, _, offset) => offset,
            Arg::Selfie(_, offset) => offset,
            Arg::SelfieNew(_, offset) => offset,
        }
    }

    pub fn ty(&self) -> BuiltinType {
        match *self {
            Arg::Expr(_, ty, _) => ty,
            Arg::Stack(_, ty, _) => ty,
            Arg::Selfie(ty, _) => ty,
            Arg::SelfieNew(ty, _) => ty,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct VarId(pub usize);

#[derive(Clone, Debug)]
pub struct Var {
    pub id: VarId,
    pub name: Name,
    pub ty: BuiltinType,
    pub reassignable: bool,
    pub node_id: ast::NodeId,
}

impl Index<VarId> for Vec<Var> {
    type Output = Var;

    fn index(&self, index: VarId) -> &Var {
        &self[index.0]
    }
}

impl IndexMut<VarId> for Vec<Var> {
    fn index_mut(&mut self, index: VarId) -> &mut Var {
        &mut self[index.0]
    }
}
