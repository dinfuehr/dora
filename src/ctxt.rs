use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Index, IndexMut};
use std::ptr;
use std::sync::{Arc, Mutex, MutexGuard};

use driver::cmd::Args;
use error::diag::Diagnostic;

use ast;
use baseline::fct::JitFct;
use baseline::map::CodeMap;
use baseline::native::NativeFcts;
use baseline::stub::Stub;
use class::{Class, ClassId, FieldId};
use gc::Gc;
use interner::*;
use lexer::position::Position;
use mem;
use stacktrace::DoraToNativeInfo;
use sym::*;
use sym::Sym::*;
use ty::BuiltinType;

pub static mut CTXT: Option<*const u8> = None;

pub fn get_ctxt() -> &'static Context<'static> {
    unsafe { &*(CTXT.unwrap() as *const Context) }
}

pub struct Context<'ast> {
    pub args: Args,
    pub interner: Interner,
    pub ast: &'ast ast::Ast,
    pub diag: RefCell<Diagnostic>,
    pub sym: RefCell<SymTable>,
    pub primitive_classes: PrimitiveClasses,
    pub structs: Vec<RefCell<StructData>>,
    pub classes: Vec<RefCell<Box<Class>>>, // stores all class definitions
    pub fcts: Vec<RefCell<Fct<'ast>>>, // stores all function definitions
    pub code_map: Mutex<CodeMap>, // stores all compiled functions
    pub gc: Mutex<Gc>, // garbage collector
    pub sfi: RefCell<*const DoraToNativeInfo>,
    pub native_fcts: Mutex<NativeFcts>,
    pub compile_stub: RefCell<Option<Stub>>,
}

impl<'ast> Context<'ast> {
    pub fn new(args: Args, ast: &'ast ast::Ast, interner: Interner) -> Context<'ast> {
        let empty_class_id: ClassId = 0.into();
        let gc = Gc::new(&args);

        Context {
            args: args,
            structs: Vec::new(),
            classes: Vec::new(),
            interner: interner,
            primitive_classes: PrimitiveClasses {
                byte_class: empty_class_id,
                int_class: empty_class_id,
                long_class: empty_class_id,
                float_class: empty_class_id,
                double_class: empty_class_id,
                str_class: empty_class_id,
                bool_class: empty_class_id,
                int_array: empty_class_id,
                byte_array: empty_class_id,
                long_array: empty_class_id,
                float_array: empty_class_id,
                double_array: empty_class_id,
            },
            gc: Mutex::new(gc),
            ast: ast,
            diag: RefCell::new(Diagnostic::new()),
            sym: RefCell::new(SymTable::new()),
            fcts: Vec::new(),
            code_map: Mutex::new(CodeMap::new()),
            sfi: RefCell::new(ptr::null()),
            native_fcts: Mutex::new(NativeFcts::new()),
            compile_stub: RefCell::new(None),
        }
    }

    pub fn use_sfi<F, R>(&self, sfi: &mut DoraToNativeInfo, fct: F) -> R
        where F: FnOnce() -> R
    {
        sfi.last = *self.sfi.borrow();

        *self.sfi.borrow_mut() = sfi as *const DoraToNativeInfo;

        let ret = fct();

        *self.sfi.borrow_mut() = sfi.last;

        ret
    }

    pub fn push_sfi(&self, sfi: &mut DoraToNativeInfo) {
        let last = *self.sfi.borrow();

        sfi.last = last;

        *self.sfi.borrow_mut() = sfi as *const DoraToNativeInfo;
    }

    pub fn pop_sfi(&self) {
        let current_sfi = *self.sfi.borrow();
        assert!(!current_sfi.is_null());
        let sfi = unsafe { &*current_sfi };

        let last_sfi = sfi.last as *const DoraToNativeInfo;
        *self.sfi.borrow_mut() = last_sfi;
    }

    pub fn add_fct(&mut self, mut fct: Fct<'ast>) -> FctId {
        let fctid = FctId(self.fcts.len());

        fct.id = fctid;

        self.fcts.push(RefCell::new(fct));

        fctid
    }

    pub fn add_fct_to_sym(&mut self, fct: Fct<'ast>) -> Result<FctId, Sym> {
        let name = fct.name;
        let fctid = self.add_fct(fct);

        let mut sym = self.sym.borrow_mut();

        match sym.get(name) {
            Some(sym) => Err(sym),
            None => {
                assert!(sym.insert(name, SymFct(fctid)).is_none());

                Ok(fctid)
            }
        }
    }

    #[cfg(test)]
    pub fn fct_by_name(&self, name: &str) -> Option<FctId> {
        let name = self.interner.intern(name);
        self.sym.borrow().get_fct(name)
    }
}

impl<'ast> Index<FctId> for Vec<RefCell<Fct<'ast>>> {
    type Output = RefCell<Fct<'ast>>;

    fn index(&self, index: FctId) -> &RefCell<Fct<'ast>> {
        &self[index.0]
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructId(u32);

impl Index<StructId> for Vec<RefCell<StructData>> {
    type Output = RefCell<StructData>;

    fn index(&self, index: StructId) -> &RefCell<StructData> {
        &self[index.0 as usize]
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
    pub size: i32,
    pub align: i32,
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
pub struct PrimitiveClasses {
    pub byte_class: ClassId,
    pub int_class: ClassId,
    pub long_class: ClassId,
    pub float_class: ClassId,
    pub double_class: ClassId,
    pub str_class: ClassId,
    pub bool_class: ClassId,
    pub int_array: ClassId,
    pub byte_array: ClassId,
    pub long_array: ClassId,
    pub float_array: ClassId,
    pub double_array: ClassId,
}

impl PrimitiveClasses {
    pub fn find_class(&self, ty: BuiltinType) -> Option<ClassId> {
        match ty {
            BuiltinType::Byte => Some(self.byte_class),
            BuiltinType::Int => Some(self.int_class),
            BuiltinType::Long => Some(self.long_class),
            BuiltinType::Float => Some(self.float_class),
            BuiltinType::Double => Some(self.double_class),
            BuiltinType::Str => Some(self.str_class),
            BuiltinType::Bool => Some(self.bool_class),
            BuiltinType::IntArray => Some(self.int_array),
            BuiltinType::ByteArray => Some(self.byte_array),
            BuiltinType::LongArray => Some(self.long_array),
            BuiltinType::FloatArray => Some(self.float_array),
            BuiltinType::DoubleArray => Some(self.double_array),
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

#[derive(Debug)]
pub struct Fct<'ast> {
    pub id: FctId,
    pub ast: &'ast ast::Function,
    pub pos: Position,
    pub name: Name,
    pub owner_class: Option<ClassId>,
    pub has_open: bool,
    pub has_override: bool,
    pub has_final: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub overrides: Option<FctId>,
    pub params_types: Vec<BuiltinType>,
    pub return_type: BuiltinType,
    pub ctor: ast::CtorType,
    pub vtable_index: Option<u32>,
    pub initialized: bool,
    pub throws: bool,

    pub kind: FctKind<'ast>,
}

impl<'ast> Fct<'ast> {
    pub fn is_virtual(&self) -> bool {
        (self.has_open || self.has_override) && !self.has_final
    }

    pub fn in_class(&self) -> bool {
        self.owner_class.is_some()
    }

    pub fn full_name(&self, ctxt: &Context) -> String {
        let mut repr = String::new();

        if let Some(class_id) = self.owner_class {
            let name = ctxt.classes[class_id].borrow().name;
            repr.push_str(&ctxt.interner.str(name));
            repr.push_str(".");
        }

        repr.push_str(&ctxt.interner.str(self.name));
        repr.push_str("(");

        for (ind, ty) in self.params_types.iter().enumerate() {
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

    pub fn src(&self) -> Arc<Mutex<FctSrc<'ast>>> {
        match self.kind {
            FctKind::Source(ref src) => src.clone(),
            _ => panic!("source expected"),
        }
    }

    pub fn hidden_self(&self) -> bool {
        self.ctor.is()
    }

    pub fn has_self(&self) -> bool {
        self.owner_class.is_some()
    }

    pub fn real_args(&self) -> i32 {
        let params = self.params_types.len() as i32;

        if self.owner_class.is_some() {
            params + 1
        } else {
            params
        }
    }
}

#[derive(Debug)]
pub enum FctKind<'ast> {
    Source(Arc<Mutex<FctSrc<'ast>>>),
    Definition,
    Native(*const u8),
    Builtin(Intrinsic),
}

impl<'ast> FctKind<'ast> {
    pub fn is_src(&self) -> bool {
        match *self {
            FctKind::Source(_) => true,
            _ => false,
        }
    }

    pub fn src(&self) -> MutexGuard<FctSrc<'ast>> {
        match *self {
            FctKind::Source(ref src) => src.lock().unwrap(),
            _ => panic!(),
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
    LongArrayLen,
    LongArrayGet,
    LongArraySet,

    IntArrayLen,
    IntArrayGet,
    IntArraySet,

    ByteArrayLen,
    ByteArrayGet,
    ByteArraySet,

    Assert,
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

    IntToByte,
    IntToLong,
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
    LongToByte,
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

    FloatEq,
    FloatCmp,

    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,

    FloatPlus,
    FloatNeg,
    FloatIsNan,

    FloatArrayLen,
    FloatArrayGet,
    FloatArraySet,

    DoubleEq,
    DoubleCmp,

    DoubleAdd,
    DoubleSub,
    DoubleMul,
    DoubleDiv,

    DoublePlus,
    DoubleNeg,
    DoubleIsNan,

    DoubleArrayLen,
    DoubleArrayGet,
    DoubleArraySet,
}

#[derive(Debug)]
pub struct FctSrc<'ast> {
    pub map_calls: NodeMap<CallType>, // maps function call to FctId
    pub map_stores: NodeMap<Store>,
    pub map_csites: NodeMap<CallSite<'ast>>,
    pub map_idents: NodeMap<IdentType>,
    pub map_tys: NodeMap<BuiltinType>,
    pub map_vars: NodeMap<VarId>,
    pub map_offsets: NodeMap<i32>,
    pub map_convs: NodeMap<ConvInfo>,
    pub map_cls: NodeMap<ClassId>,

    pub tempsize: i32, // size of temporary variables on stack
    pub localsize: i32, // size of local variables on stack
    pub argsize: i32, // size of arguments on stack (need to be on bottom)
    pub leaf: bool, // false if fct calls other functions
    pub vars: Vec<Var>, // variables in functions
    pub always_returns: bool, // true if function is always exited via return statement
    // false if function execution could reach the closing } of this function
    pub jit_fct: Option<JitFct>, // compile function
    pub eh_return_value: Option<i32>, // stack slot for return value storage
}

impl<'ast> FctSrc<'ast> {
    pub fn new() -> FctSrc<'ast> {
        FctSrc {
            map_calls: NodeMap::new(),
            map_stores: NodeMap::new(),
            map_csites: NodeMap::new(),
            map_idents: NodeMap::new(),
            map_tys: NodeMap::new(),
            map_vars: NodeMap::new(),
            map_offsets: NodeMap::new(),
            map_convs: NodeMap::new(),
            map_cls: NodeMap::new(),

            tempsize: 0,
            localsize: 0,
            argsize: 0,
            leaf: false,
            vars: Vec::new(),
            always_returns: false,
            jit_fct: None,
            eh_return_value: None,
        }
    }

    pub fn get_store(&self, id: ast::NodeId) -> Store {
        match self.map_stores.get(id) {
            Some(store) => *store,
            None => Store::Reg,
        }
    }

    pub fn set_ty(&mut self, id: ast::NodeId, ty: BuiltinType) {
        self.map_tys.insert_or_replace(id, ty);
    }

    pub fn ty(&self, id: ast::NodeId) -> BuiltinType {
        self.map_tys.get(id).unwrap().clone()
    }

    pub fn stacksize(&self) -> i32 {
        mem::align_i32(self.tempsize + self.localsize + self.argsize, 16)
    }

    pub fn var_self(&self) -> &Var {
        &self.vars[0]
    }

    pub fn var_self_mut(&mut self) -> &mut Var {
        &mut self.vars[0]
    }
}

#[derive(Debug)]
pub struct NodeMap<V> {
    map: HashMap<ast::NodeId, V>,
}

impl<V> NodeMap<V> {
    pub fn new() -> NodeMap<V> {
        NodeMap { map: HashMap::new() }
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
    Field(ClassId, FieldId),
    Struct(StructId),
}

impl IdentType {
    pub fn var_id(&self) -> VarId {
        match *self {
            IdentType::Var(varid) => varid,
            _ => panic!(),
        }
    }

    pub fn cls_id(&self) -> ClassId {
        match *self {
            IdentType::Field(clsid, _) => clsid,
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

    pub fn field_id(&self) -> FieldId {
        match *self {
            IdentType::Field(_, fieldid) => fieldid,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum CallType {
    Fct(FctId),
    Method(ClassId, FctId),
    CtorNew(ClassId, FctId),
    Ctor(ClassId, FctId),
}

impl CallType {
    pub fn is_ctor_new(&self) -> bool {
        match *self {
            CallType::CtorNew(_, _) => true,
            _ => false,
        }
    }

    pub fn is_ctor(&self) -> bool {
        match *self {
            CallType::Ctor(_, _) => true,
            _ => false,
        }
    }

    pub fn is_method(&self) -> bool {
        match *self {
            CallType::Method(_, _) => true,
            _ => false,
        }
    }

    pub fn cls_id(&self) -> ClassId {
        match *self {
            CallType::Method(clsid, _) => clsid,
            CallType::CtorNew(clsid, _) => clsid,
            CallType::Ctor(clsid, _) => clsid,
            _ => panic!(),
        }
    }

    pub fn fct_id(&self) -> FctId {
        match *self {
            CallType::Fct(fctid) => fctid,
            CallType::Method(_, fctid) => fctid,
            CallType::CtorNew(_, fctid) => fctid,
            CallType::Ctor(_, fctid) => fctid,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CallSite<'ast> {
    pub callee: FctId,
    pub args: Vec<Arg<'ast>>,
    pub super_call: bool,
    pub return_type: BuiltinType,
}

#[derive(Copy, Clone, Debug)]
pub enum Arg<'ast> {
    Expr(&'ast ast::Expr, BuiltinType, i32),
    SelfieNew(ClassId, i32),
    Selfie(ClassId, i32),
}

impl<'ast> Arg<'ast> {
    pub fn offset(&self) -> i32 {
        match *self {
            Arg::Expr(_, _, offset) => offset,
            Arg::Selfie(_, offset) => offset,
            Arg::SelfieNew(_, offset) => offset,
        }
    }

    pub fn ty(&self) -> BuiltinType {
        match *self {
            Arg::Expr(_, ty, _) => ty,
            Arg::Selfie(cid, _) => BuiltinType::Class(cid),
            Arg::SelfieNew(cid, _) => BuiltinType::Class(cid),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct VarId(pub usize);

#[derive(Debug)]
pub struct Var {
    pub id: VarId,
    pub name: Name,
    pub ty: BuiltinType,
    pub reassignable: bool,
    pub node_id: ast::NodeId,
    pub offset: i32,
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
