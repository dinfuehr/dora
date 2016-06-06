use libc::c_void;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Index, IndexMut};
use std::sync::{Arc, Mutex, MutexGuard};

use driver::cmd::Args;
use error::diag::Diagnostic;

use ast;
use class::{Class, ClassId, Field, FieldId};
use cpu::Reg;
use gc::Gc;
use interner::*;
use jit::fct::JitFct;
use jit::map::CodeMap;
use jit::stub::Stub;
use mem::{self, Ptr};
use object::{Handle, Str};
use sym::*;
use sym::Sym::*;
use ty::BuiltinType;

pub static mut ctxt_ptr: Option<Ptr> = None;

pub fn get_ctxt() -> &'static Context<'static> {
    unsafe {
        &*(ctxt_ptr.unwrap().raw() as *const Context)
    }
}

pub struct Context<'ast> {
    pub args: Args,
    pub interner: Interner,
    pub ast: &'ast ast::Ast,
    pub diag: RefCell<Diagnostic>,
    pub sym: RefCell<SymTable>,
    pub primitive_classes: PrimitiveClasses,
    pub classes: Vec<Box<Class<'ast>>>, // stores all class definitions
    pub cls_defs: HashMap<ast::NodeId, ClassId>, // points from AST class to ClassId
    pub fct_defs: HashMap<ast::NodeId, FctId>, // points from AST function definition
                                                 // node id to FctId
    pub fcts: Vec<Arc<Mutex<Fct<'ast>>>>, // stores all function definitions
    pub code_map: Mutex<CodeMap>, // stores all compiled functions
    pub gc: Mutex<Gc>, // garbage collector
    pub literals: Mutex<Vec<Handle<Str>>>, // string literals
}

impl<'ast> Context<'ast> {
    pub fn new(args: Args, ast: &'ast ast::Ast, interner: Interner) -> Context<'ast> {
        Context {
            args: args,
            classes: Vec::new(),
            cls_defs: HashMap::new(),
            interner: interner,
            primitive_classes: PrimitiveClasses {
                int_class: ClassId(0),
                str_class: ClassId(0),
                str_classptr: 0,
                bool_class: ClassId(0),
                int_array: ClassId(0),
                int_array_classptr: 0
            },
            gc: Mutex::new(Gc::new()),
            literals: Mutex::new(Vec::new()),
            ast: ast,
            diag: RefCell::new(Diagnostic::new()),
            sym: RefCell::new(SymTable::new()),
            fct_defs: HashMap::new(),
            fcts: Vec::new(),
            code_map: Mutex::new(CodeMap::new()),
        }
    }

    pub fn add_fct(&mut self, mut fct: Fct<'ast>) -> FctId {
        let name = fct.name;
        let fctid = FctId(self.fcts.len());

        fct.id = fctid;

        if fct.kind.is_src() {
            assert!(self.fct_defs.insert(fct.kind.src().ast.id, fctid).is_none());
        }

        self.fcts.push(Arc::new(Mutex::new(fct)));

        fctid
    }

    pub fn add_fct_to_sym(&mut self, mut fct: Fct<'ast>) -> Result<FctId, Sym> {
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

    pub fn fct_by_id<F, R>(&self, id: FctId, f: F) -> R where F: FnOnce(&Fct<'ast>) -> R {
        let fct = self.fcts[id.0].clone();
        let fctxt = fct.lock().unwrap();

        f(&fctxt)
    }

    pub fn fct_by_id_mut<F, R>(&self, id: FctId, f: F) -> R where F: FnOnce(&mut Fct<'ast>) -> R {
        let fct = self.fcts[id.0].clone();
        let mut fctxt = fct.lock().unwrap();

        f(&mut fctxt)
    }

    pub fn field(&self, cid: ClassId, fid: FieldId) -> &Field {
        &self.classes[cid.0].fields[fid.0]
    }

    pub fn cls_by_id(&self, id: ClassId) -> &Class<'ast> {
        &self.classes[id.0]
    }

    pub fn cls_by_id_mut(&mut self, id: ClassId) -> &mut Class<'ast> {
        &mut self.classes[id.0]
    }

    pub fn fct_by_node_id_mut<F, R>(&self, id: ast::NodeId, f: F) -> R where F: FnOnce(&mut Fct<'ast>) -> R {
        let fct_id = *self.fct_defs.get(&id).unwrap();

        self.fct_by_id_mut(fct_id, f)
    }

    pub fn fct_by_node_id<F, R>(&self, id: ast::NodeId, f: F) -> R where
                     F: FnOnce(&Fct<'ast>) -> R {
        let fct_id = *self.fct_defs.get(&id).unwrap();

        self.fct_by_id(fct_id, f)
    }
}

#[derive(Debug)]
pub struct PrimitiveClasses {
    pub int_class: ClassId,
    pub str_class: ClassId,
    pub str_classptr: usize,
    pub bool_class: ClassId,
    pub int_array: ClassId,
    pub int_array_classptr: usize,
}

impl PrimitiveClasses {
    pub fn find_class(&self, ty: BuiltinType) -> Option<ClassId> {
        match ty {
            BuiltinType::Int => Some(self.int_class),
            BuiltinType::Str => Some(self.str_class),
            BuiltinType::Bool => Some(self.bool_class),
            BuiltinType::IntArray => Some(self.int_array),
            _ => None
        }
    }
}

#[derive(Debug, Clone)]
pub struct FctDecl {
    pub id: FctId,
    pub name: Name,
    pub owner_class: Option<ClassId>,
    pub params_types: Vec<BuiltinType>,
    pub ctor: Option<CtorType>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CtorType {
    Primary, Secondary
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct FctId(pub usize);

#[derive(Debug)]
pub struct Fct<'ast> {
    pub id: FctId,
    pub name: Name,
    pub owner_class: Option<ClassId>,
    pub params_types: Vec<BuiltinType>,
    pub return_type: BuiltinType,
    pub ctor: Option<CtorType>,
    pub initialized: bool,

    pub kind: FctKind<'ast>,
}

impl<'ast> Fct<'ast> {
    pub fn is_ctor(&self) -> bool {
        self.ctor.is_some()
    }

    pub fn is_primary_ctor(&self) -> bool {
        if let Some(ctor) = self.ctor {
            ctor == CtorType::Primary
        } else {
            false
        }
    }

    pub fn is_secondary_ctor(&self) -> bool {
        if let Some(ctor) = self.ctor {
            ctor == CtorType::Secondary
        } else {
            false
        }
    }

    pub fn full_name(&self, ctxt: &Context) -> String {
        let mut repr = String::new();

        if let Some(class_id) = self.owner_class {
            let name = ctxt.cls_by_id(class_id).name;
            repr.push_str(&ctxt.interner.str(name));
            repr.push_str(".");
        }

        repr.push_str(&ctxt.interner.str(self.name));
        repr.push_str("(");

        for (ind, ty) in self.params_types.iter().enumerate() {
            if ind > 0 { repr.push_str(", "); }

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
            _ => false
        }
    }

    pub fn src(&self) -> Arc<Mutex<FctSrc<'ast>>> {
        match self.kind {
            FctKind::Source(ref src) => src.clone(),
            _ => panic!("source expected")
        }
    }

    pub fn hidden_self(&self) -> bool {
        self.is_ctor()
    }

    pub fn has_self(&self) -> bool {
        self.owner_class.is_some()
    }
}

#[derive(Debug)]
pub enum FctKind<'ast> {
    Source(Arc<Mutex<FctSrc<'ast>>>), Builtin(Ptr), Intrinsic
}

impl<'ast> FctKind<'ast> {
    pub fn is_src(&self) -> bool {
        match *self {
            FctKind::Source(_) => true,
            _ => false
        }
    }

    pub fn src(&self) -> MutexGuard<FctSrc<'ast>> {
        match *self {
            FctKind::Source(ref src) => src.lock().unwrap(),
            _ => panic!()
        }
    }

    pub fn is_intrinsic(&self) -> bool {
        match *self {
            FctKind::Intrinsic => true,
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct FctSrc<'ast> {
    pub ast: &'ast ast::Function,
    pub calls: HashMap<ast::NodeId, CallType>, // maps function call to FctId
    pub storage: HashMap<ast::NodeId, Store>,
    pub call_sites: HashMap<ast::NodeId, CallSite<'ast>>,
    pub tempsize: i32, // size of temporary variables on stack
    pub localsize: i32, // size of local variables on stack
    pub argsize: i32, // size of arguments on stack (need to be on bottom)
    pub leaf: bool, // false if fct calls other functions
    pub vars: Vec<Var>, // variables in functions
    pub always_returns: bool, // true if function is always exited via return statement
                              // false if function execution could reach the closing } of this function
    pub jit_fct: Option<JitFct>, // compile function
    pub stub: Option<Stub>, // compiler stub
    pub eh_return_value: Option<i32>, // stack slot for return value storage
}

impl<'ast> FctSrc<'ast> {
    pub fn new(ast: &'ast ast::Function) -> FctSrc<'ast> {
        FctSrc {
            ast: ast,
            calls: HashMap::new(),
            storage: HashMap::new(),
            call_sites: HashMap::new(),
            tempsize: 0,
            localsize: 0,
            argsize: 0,
            leaf: false,
            vars: Vec::new(),
            always_returns: false,
            jit_fct: None,
            stub: None,
            eh_return_value: None,
        }
    }

    pub fn get_store(&self, id: ast::NodeId) -> Store {
        match self.storage.get(&id) {
            Some(store) => *store,
            None => Store::Reg,
        }
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

#[derive(Debug, Copy, Clone)]
pub enum Store {
    Reg, Temp(i32, BuiltinType)
}

impl Store {
    pub fn offset(&self) -> i32 {
        match *self {
            Store::Temp(offset, _) => offset,
            Store::Reg => panic!()
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum IdentType {
    Var(VarId), Field(ClassId, FieldId)
}

impl IdentType {
    pub fn var_id(&self) -> VarId {
        match *self {
            IdentType::Var(varid) => varid,
            _ => panic!()
        }
    }

    pub fn cls_id(&self) -> ClassId {
        match *self {
            IdentType::Field(clsid, _) => clsid,
            _ => panic!()
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
            _ => panic!()
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum CallType {
    Fct(FctId), Method(ClassId, FctId), Ctor(ClassId, FctId)
}

impl CallType {
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
            CallType::Ctor(clsid, _) => clsid,
            _ => panic!()
        }
    }

    pub fn fct_id(&self) -> FctId {
        match *self {
            CallType::Fct(fctid) => fctid,
            CallType::Method(_, fctid) => fctid,
            CallType::Ctor(_, fctid) => fctid,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CallSite<'ast> {
    pub callee: Callee,
    pub args: Vec<Arg<'ast>>,
    pub return_type: BuiltinType,
}

#[derive(Clone, Debug)]
pub enum Callee {
    Fct(FctId), Ptr(Ptr)
}

#[derive(Copy, Clone, Debug)]
pub enum Arg<'ast> {
    Expr(&'ast ast::Expr, BuiltinType, i32), Selfie(ClassId, i32)
}

impl<'ast> Arg<'ast> {
    pub fn offset(&self) -> i32 {
        match *self {
            Arg::Expr(_, _, offset) => offset,
            Arg::Selfie(_, offset) => offset,
        }
    }

    pub fn ty(&self) -> BuiltinType {
        match *self {
            Arg::Expr(_, ty, _) => ty,
            Arg::Selfie(cid, _) => BuiltinType::Class(cid)
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
