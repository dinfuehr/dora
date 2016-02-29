use libc::c_void;

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use driver::cmd::Args;
use error::diag::Diagnostic;

use ast;
use class::{Class, ClassId, PropId};
use gc::Gc;
use interner::*;
use jit::fct::JitFct;
use jit::map::CodeMap;
use jit::stub::Stub;
use mem::Ptr;
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
    pub classes: Vec<Box<Class<'ast>>>, // stores all class definitions
    pub cls_defs: HashMap<ast::NodeId, ClassId>, // points from AST class to ClassId
    pub fct_defs: HashMap<ast::NodeId, FctId>, // points from AST function definition
                                                 // node id to FctId
    pub fcts: Vec<Arc<Mutex<Fct<'ast>>>>, // stores all function definitions
    pub code_map: Mutex<CodeMap>, // stores all compiled functions
    pub gc: Mutex<Gc>, // garbage collector
}

impl<'ast> Context<'ast> {
    pub fn new(args: Args, ast: &'ast ast::Ast, interner: Interner) -> Context<'ast> {
        Context {
            args: args,
            classes: Vec::new(),
            cls_defs: HashMap::new(),
            interner: interner,
            gc: Mutex::new(Gc::new()),
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
            assert!(self.fct_defs.insert(fct.ast().id, fctid).is_none());
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct FctId(pub usize);

#[derive(Debug)]
pub struct Fct<'ast> {
    pub id: FctId,
    pub name: Name,
    pub owner_class: Option<ClassId>,
    pub params_types: Vec<BuiltinType>,
    pub return_type: BuiltinType,
    pub ctor: bool,
    pub initialized: bool,

    pub kind: FctKind<'ast>,
}

impl<'ast> Fct<'ast> {
    pub fn ast(&self) -> &'ast ast::Function {
        self.src().ast
    }

    pub fn is_src(&self) -> bool {
        match self.kind {
            FctKind::Source(_) => true,
            _ => false
        }
    }

    pub fn src(&self) -> &FctSrc<'ast> {
        self.kind.src()
    }

    pub fn src_mut(&mut self) -> &mut FctSrc<'ast> {
        self.kind.src_mut()
    }

    pub fn var_by_node_id(&self, id: ast::NodeId) -> &Var {
        let varid = *self.src().defs.get(&id).unwrap();

        &self.src().vars[varid.var_id().0]
    }

    pub fn var_by_node_id_mut(&mut self, id: ast::NodeId) -> &mut Var {
        let varid = *self.src().defs.get(&id).unwrap();

        &mut self.src_mut().vars[varid.var_id().0]
    }

    pub fn hidden_this(&self) -> bool {
        self.owner_class.is_some()
    }

    pub fn var_this(&mut self) -> &mut Var {
        assert!(self.hidden_this());

        &mut self.src_mut().vars[0]
    }
}

#[derive(Debug)]
pub enum FctKind<'ast> {
    Source(FctSrc<'ast>), Builtin(Ptr), Intrinsic
}

impl<'ast> FctKind<'ast> {
    pub fn is_src(&self) -> bool {
        match *self {
            FctKind::Source(_) => true,
            _ => false
        }
    }

    pub fn src(&self) -> &FctSrc<'ast> {
        match *self {
            FctKind::Source(ref ast_info) => ast_info,
            _ => unreachable!()
        }
    }

    pub fn src_mut(&mut self) -> &mut FctSrc<'ast> {
        match *self {
            FctKind::Source(ref mut ast_info) => ast_info,
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub struct FctSrc<'ast> {
    pub ast: &'ast ast::Function,
    pub types: HashMap<ast::NodeId, BuiltinType>, // maps expression to type
    pub calls: HashMap<ast::NodeId, CallType>, // maps function call to FctId
    pub defs: HashMap<ast::NodeId, IdentType>, // which definition does ident refer to
    pub tempsize: i32, // size of temporary variables on stack
    pub localsize: i32, // size of local variables on stack
    pub leaf: bool, // false if fct calls other functions
    pub vars: Vec<Var>, // variables in functions
    pub always_returns: bool, // true if function is always exited via return statement
                              // false if function execution could reach the closing } of this function
    pub jit_fct: Option<JitFct>, // compile function
    pub stub: Option<Stub> // compiler stub
}

impl<'ast> FctSrc<'ast> {
    pub fn new(ast: &'ast ast::Function) -> FctSrc<'ast> {
        FctSrc {
            ast: ast,
            types: HashMap::new(),
            calls: HashMap::new(),
            defs: HashMap::new(),
            tempsize: 0,
            localsize: 0,
            leaf: false,
            vars: Vec::new(),
            always_returns: false,
            jit_fct: None,
            stub: None
        }
    }

    pub fn jit_or_stub_ptr(&mut self) -> Ptr {
        if let Some(ref jit) = self.jit_fct { return jit.fct_ptr(); }
        if let Some(ref stub) = self.stub { return stub.ptr_start(); }

        let stub = Stub::new();
        let ptr = stub.ptr_start();
        self.stub = Some(stub);

        ptr
    }

    pub fn stacksize(&self) -> i32 {
        self.tempsize + self.localsize
    }
}

#[derive(Debug, Copy, Clone)]
pub enum IdentType {
    Var(VarId), Prop(ClassId, PropId)
}

impl IdentType {
    pub fn var_id(&self) -> VarId {
        match *self {
            IdentType::Var(varid) => varid,
            _ => unreachable!()
        }
    }

    pub fn cls_id(&self) -> ClassId {
        match *self {
            IdentType::Prop(clsid, _) => clsid,
            _ => unreachable!()
        }
    }

    pub fn prop_id(&self) -> PropId {
        match *self {
            IdentType::Prop(_, propid) => propid,
            _ => unreachable!()
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

    pub fn cls_id(&self) -> ClassId {
        match *self {
            CallType::Method(clsid, _) => clsid,
            CallType::Ctor(clsid, _) => clsid,
            _ => unreachable!()
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

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct VarId(pub usize);

#[derive(Debug)]
pub struct Var {
    pub id: VarId,
    pub name: Name,
    pub data_type: BuiltinType,
    pub node_id: ast::NodeId,
    pub offset: i32,
}
