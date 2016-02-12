use libc::c_void;

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use driver::cmd::Args;
use error::diag::Diagnostic;

use ast;
use ast::map::Map;
use class::{Class, ClassId};
use gc::Gc;
use interner::*;
use jit::fct::JitFct;
use jit::map::CodeMap;
use jit::stub::Stub;

use mem::Ptr;
use mir::Mir;

use sym::*;
use sym::Sym::*;
use ty::BuiltinType;

pub static mut ctxt_ptr: Option<Ptr> = None;

pub fn get_ctxt() -> &'static Context<'static, 'static> {
    unsafe {
        &*(ctxt_ptr.unwrap().raw() as *const Context)
    }
}

pub struct Context<'a, 'ast> where 'ast: 'a {
    pub args: &'a Args,
    pub interner: &'a Interner,
    pub map: &'a Map<'ast>,
    pub ast: &'ast ast::Ast,
    pub diag: RefCell<Diagnostic>,
    pub sym: RefCell<SymTable>,
    pub classes: Vec<Box<Class<'ast>>>, // stores all class definitions
    pub cls_defs: HashMap<ast::NodeId, ClassId>, // points from AST class to ClassId
    pub fct_defs: HashMap<ast::NodeId, FctContextId>, // points from AST function definition
                                                 // node id to FctContextId
    pub fcts: Vec<Arc<Mutex<FctContext<'ast>>>>, // stores all function definitions
    pub code_map: Mutex<CodeMap>, // stores all compiled functions
    pub gc: Mutex<Gc>, // garbage collector
}

impl<'a, 'ast> Context<'a, 'ast> {
    pub fn new(args: &'a Args, interner: &'a Interner,
           map: &'a Map<'ast>, ast: &'ast ast::Ast) -> Context<'a, 'ast> {
        Context {
            args: args,
            classes: Vec::new(),
            cls_defs: HashMap::new(),
            interner: interner,
            gc: Mutex::new(Gc::new()),
            map: map,
            ast: ast,
            diag: RefCell::new(Diagnostic::new()),
            sym: RefCell::new(SymTable::new()),
            fct_defs: HashMap::new(),
            fcts: Vec::new(),
            code_map: Mutex::new(CodeMap::new()),
        }
    }

    pub fn add_function(&mut self, mut fct: FctContext<'ast>) -> Result<FctContextId, Sym> {
        let name = fct.name;
        let fctid = FctContextId(self.fcts.len());

        fct.id = fctid;

        if let Some(ast) = fct.ast {
            assert!(self.fct_defs.insert(ast.id, fctid).is_none());
        }

        self.fcts.push(Arc::new(Mutex::new(fct)));

        let mut sym = self.sym.borrow_mut();

        match sym.get(name) {
            Some(sym) => Err(sym),
            None => {
                assert!(sym.insert(name, SymFunction(fctid)).is_none());

                Ok(fctid)
            }
        }
    }

    pub fn fct_by_id<F, R>(&self, id: FctContextId, f: F) -> R where F: FnOnce(&FctContext<'ast>) -> R {
        let fct = self.fcts[id.0].clone();
        let fctxt = fct.lock().unwrap();

        f(&fctxt)
    }

    pub fn fct_by_id_mut<F, R>(&self, id: FctContextId, f: F) -> R where F: FnOnce(&mut FctContext<'ast>) -> R {
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

    pub fn fct_by_node_id_mut<F, R>(&self, id: ast::NodeId, f: F) -> R where F: FnOnce(&mut FctContext<'ast>) -> R {
        let fct_id = *self.fct_defs.get(&id).unwrap();

        self.fct_by_id_mut(fct_id, f)
    }

    pub fn fct_by_node_id<F, R>(&self, id: ast::NodeId, f: F) -> R where
                     F: FnOnce(&FctContext<'ast>) -> R {
        let fct_id = *self.fct_defs.get(&id).unwrap();

        self.fct_by_id(fct_id, f)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct FctContextId(pub usize);

#[derive(Debug)]
pub struct FctContext<'ast> {
    pub id: FctContextId,
    pub name: Name,
    pub owner_class: Option<ClassId>,
    pub params_types: Vec<BuiltinType>,
    pub return_type: BuiltinType,
    pub is_ctor: bool,
    pub ast: Option<&'ast ast::Function>,
    pub types: HashMap<ast::NodeId, BuiltinType>, // maps expression to type
    pub calls: HashMap<ast::NodeId, FctContextId>, // maps function call to FctContextId
    pub defs: HashMap<ast::NodeId, VarContextId>, // points to the definition of variable from its usage
    pub ir: Option<Mir>,
    pub tempsize: i32, // size of temporary variables on stack
    pub localsize: i32, // size of local variables on stack
    pub leaf: bool,
    pub vars: Vec<VarContext>,
    pub always_returns: bool, // true if function is always exited via return statement
                              // false if function execution could reach the closing } of this function
    pub code: FctCode, // ptr to machine code if already compiled
    pub stub: Option<Stub> // compiler stub
}

impl<'ast> FctContext<'ast> {
    pub fn stacksize(&self) -> i32 {
        self.tempsize + self.localsize
    }

    pub fn var_by_node_id(&self, id: ast::NodeId) -> &VarContext {
        let varid = *self.defs.get(&id).unwrap();

        &self.vars[varid.0]
    }

    pub fn var_by_node_id_mut(&mut self, id: ast::NodeId) -> &mut VarContext {
        let varid = *self.defs.get(&id).unwrap();

        &mut self.vars[varid.0]
    }
}

#[derive(Debug)]
pub enum FctCode {
    Uncompiled, Builtin(Ptr), Fct(JitFct)
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct VarContextId(pub usize);

#[derive(Debug)]
pub struct VarContext {
    pub id: VarContextId,

    pub name: Name,

    pub data_type: BuiltinType,

    pub node_id: ast::NodeId,

    pub offset: i32,
}
