use libc::c_void;

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use driver::cmd::Args;
use error::diag::Diagnostic;

use ast::*;
use ast::map::Map;
use interner::*;
use jit::fct::JitFct;
use jit::map::CodeMap;
use jit::stub::Stub;

use mem::Ptr;
use mir::Mir;

use sym::*;
use sym::Sym::*;

pub struct Context<'a, 'ast> where 'ast: 'a {
    pub args: &'a Args,
    pub interner: &'a Interner,
    pub map: &'a Map<'ast>,
    pub ast: &'ast Ast,
    pub diag: RefCell<Diagnostic>,
    pub sym: RefCell<SymTable>,

    // points from AST function definition node id to FctContextId
    pub fct_defs: RefCell<HashMap<NodeId, FctContextId>>,

    // stores all function definitions
    pub fcts: RefCell<Vec<Arc<Mutex<FctContext<'ast>>>>>,

    // stores all compiled functions
    pub code_map: RefCell<CodeMap>,
}

impl<'a, 'ast> Context<'a, 'ast> {
    pub fn new(args: &'a Args, interner: &'a Interner,
           map: &'a Map<'ast>, ast: &'ast Ast) -> Context<'a, 'ast> {
        Context {
            args: args,
            interner: interner,
            map: map,
            ast: ast,
            diag: RefCell::new(Diagnostic::new()),
            sym: RefCell::new(SymTable::new()),
            fct_defs: RefCell::new(HashMap::new()),
            fcts: RefCell::new(Vec::new()),
            code_map: RefCell::new(CodeMap::new()),
        }
    }

    pub fn add_function(&self, fct: FctContext<'ast>) -> Result<FctContextId, Sym> {
        let name = fct.name;
        let fctid = FctContextId(self.fcts.borrow().len());

        if let Some(ast) = fct.ast {
            assert!(self.fct_defs.borrow_mut().insert(ast.id, fctid).is_none());
        }

        self.fcts.borrow_mut().push(Arc::new(Mutex::new(fct)));

        let mut sym = self.sym.borrow_mut();

        match sym.get(name) {
            Some(sym) => Err(sym),
            None => {
                assert!(sym.insert(name, SymFunction(fctid)).is_none());

                Ok(fctid)
            }
        }
    }

    pub fn add_var<F>(&self, fct: NodeId, var: VarContext, replacable: F) ->
            Result<VarContextId, Sym> where F: FnOnce(&Sym) -> bool {
        let name = var.name;
        let varid = self.fct(fct, |fct| fct.vars.len());
        let varid = VarContextId(varid);

        let result = match self.sym.borrow().get(name) {
            Some(sym) => if replacable(&sym) { Ok(varid) } else { Err(sym) },
            None => Ok(varid)
        };

        if result.is_ok() {
            self.sym.borrow_mut().insert(name, SymVar(varid));
            self.fct_mut(fct, |fct| {
                assert!(fct.defs.insert(var.node_id, varid).is_none());
            });
        }

        self.fct_mut(fct, |fct| { fct.vars.push(var); });

        result
    }

    pub fn fct_by_id<F, R>(&self, id: FctContextId, f: F) -> R where F: FnOnce(&FctContext<'ast>) -> R {
        let fct = {
            let fcts = self.fcts.borrow();
            fcts[id.0].clone()
        };

        let fctxt = fct.lock().unwrap();

        f(&fctxt)
    }

    pub fn fct_by_id_mut<F, R>(&self, id: FctContextId, f: F) -> R where F: FnOnce(&mut FctContext<'ast>) -> R {
        let fct = {
            let fcts = self.fcts.borrow();
            fcts[id.0].clone()
        };

        let mut fctxt = fct.lock().unwrap();

        f(&mut fctxt)
    }

    pub fn fct_mut<F, R>(&self, id: NodeId, f: F) -> R where F: FnOnce(&mut FctContext<'ast>) -> R {
        let map = self.fct_defs.borrow();
        let fct_id = *map.get(&id).unwrap();

        self.fct_by_id_mut(fct_id, f)
    }

    pub fn fct<F, R>(&self, id: NodeId, f: F) -> R where
                     F: FnOnce(&FctContext<'ast>) -> R {
        let map = self.fct_defs.borrow();
        let fct_id = *map.get(&id).unwrap();

        self.fct_by_id(fct_id, f)
    }

    pub fn var_mut<F, R>(&self, fctid: NodeId, id: NodeId, f: F) -> R where
                         F: FnOnce(&mut VarContext, VarContextId) -> R {
        self.fct_mut(fctid, |fct| {
            let varid = *fct.defs.get(&id).unwrap();
            f(&mut fct.vars[varid.0], varid)
        })
    }

    pub fn var<F, R>(&self, fctid: NodeId, id: NodeId, f: F) -> R where
                     F: FnOnce(&VarContext, VarContextId) -> R {
         self.fct(fctid, |fct| {
             let varid = *fct.defs.get(&id).unwrap();
             f(&fct.vars[varid.0], varid)
         })
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct FctContextId(pub usize);

#[derive(Debug)]
pub struct FctContext<'ast> {
    pub name: Name,

    pub params_types: Vec<BuiltinType>,

    pub return_type: BuiltinType,

    pub ast: Option<&'ast Function>,

    // maps function call to FctContextId
    pub calls: HashMap<NodeId, FctContextId>,

    // points to the definition of variable from its usage
    pub defs: HashMap<NodeId, VarContextId>,

    pub ir: Option<Mir>,

    pub vars: Vec<VarContext>,

    // true if function is always exited via return statement
    // false if function execution could reach the closing } of this function
    pub always_returns: bool,

    // ptr to machine code if already compiled
    pub code: FctCode,

    // compiler stub
    pub stub: Option<Stub>
}

#[derive(Debug)]
pub enum FctCode {
    Uncompiled, Builtin(Ptr), Fct(JitFct)
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct VarContextId(pub usize);

#[derive(Debug)]
pub struct VarContext {
    pub name: Name,

    pub data_type: BuiltinType,

    pub node_id: NodeId,

    pub offset: i32,
}
