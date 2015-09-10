use std::cell::RefCell;
use std::collections::HashMap;

use driver::cmd::Args;
use error::diag::Diagnostic;

use parser::ast::*;
use parser::ast::map::Map;
use parser::interner::*;

use sym::*;
use sym::Sym::*;

pub struct Context<'a, 'ast> where 'ast: 'a {
    pub args: &'a Args,
    pub interner: &'a Interner,
    pub map: &'a Map<'ast>,
    pub ast: &'a Ast,
    pub diag: RefCell<Diagnostic>,
    pub sym: RefCell<SymTable>,

    // saves type of element
    // used for functions, params and variables
    pub types: RefCell<HashMap<NodeId, BuiltinType>>,

    // points to the definition of variable from its usage
    pub defs: RefCell<HashMap<NodeId, NodeId>>,

    // maps Function-NodeId to FctInfoId
    pub nodeid_to_fctinfoid: RefCell<HashMap<NodeId, FctInfoId>>,

    // maps function call to FctInfoId
    pub calls: RefCell<HashMap<NodeId, FctInfoId>>,

    // stores all function definitions
    pub fct_infos: RefCell<Vec<FctInfo<'ast>>>,
}

impl<'a, 'ast> Context<'a, 'ast> {
    pub fn new(args: &'a Args, interner: &'a Interner,
           map: &'a Map<'ast>, ast: &'a Ast) -> Context<'a, 'ast> {
        Context {
            args: args,
            interner: interner,
            map: map,
            ast: ast,
            diag: RefCell::new(Diagnostic::new()),
            sym: RefCell::new(SymTable::new()),
            types: RefCell::new(HashMap::new()),
            defs: RefCell::new(HashMap::new()),
            calls: RefCell::new(HashMap::new()),
            nodeid_to_fctinfoid: RefCell::new(HashMap::new()),
            fct_infos: RefCell::new(Vec::new()),
        }
    }

    pub fn add_function(&self, fct_info: FctInfo<'ast>) -> Result<FctInfoId, Sym> {
        let name = fct_info.name;
        let fctid = FctInfoId(self.fct_infos.borrow().len());

        if let Some(ast) = fct_info.ast {
            assert!(self.nodeid_to_fctinfoid.borrow_mut().insert(ast.id, fctid).is_none());
        }

        self.fct_infos.borrow_mut().push(fct_info);

        match self.sym.borrow_mut().insert(name, SymFunction(fctid)) {
            Some(sym) => Err(sym),
            None => Ok(fctid),
        }
    }

    pub fn function<F, R>(&self, id: NodeId, f: F) -> R where F: FnOnce(&mut FctInfo<'ast>) -> R {
        let map = self.nodeid_to_fctinfoid.borrow();
        let fctid = *map.get(&id).unwrap();

        let mut fcts = self.fct_infos.borrow_mut();
        f(&mut fcts[fctid.0])
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct FctInfoId(pub usize);

#[derive(Debug)]
pub struct FctInfo<'ast> {
    pub name: Name,
    pub params_types: Vec<BuiltinType>,
    pub return_type: BuiltinType,
    pub ast: Option<&'ast Function>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct VarInfoId(pub usize);

#[derive(Debug)]
pub struct VarInfo<'ast> {
    pub name: Name,
    pub data_type: BuiltinType,
    pub ast: &'ast StmtVarType,
}
