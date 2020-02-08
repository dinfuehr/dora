use parking_lot::RwLock;
use std::collections::hash_map::{HashMap, Iter};
use std::ops::{Index, IndexMut};
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;

use crate::compiler::fct::JitFctId;
use crate::ty::{BuiltinType, TypeList};
use crate::vm::module::ModuleId;
use crate::vm::{
    ClassId, ConstId, EnumId, FctId, FieldId, GlobalId, Intrinsic, StructId, TraitId, TypeParamId,
};

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
    pub specializations: RwLock<HashMap<(TypeList, TypeList), JitFctId>>,
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
        self.map_tys.get(id).expect("no type found").clone()
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
    pub check_type: BuiltinType,
    pub valid: bool,
}

#[derive(Debug, Clone)]
pub enum IdentType {
    /// name of local variable
    Var(VarId),
    /// name of a global variable
    Global(GlobalId),

    /// field expression: <expr>.<field_name>
    Field(BuiltinType, FieldId),

    /// name of structure
    Struct(StructId),

    // name of constant
    Const(ConstId),

    // name of function
    Fct(FctId),

    // name of function with type params: some_fct[T1, T2, ...]
    FctType(FctId, TypeList),

    // name of class
    Class(ClassId),

    // name of class with type params: SomeClass[T1, T2, ...]
    ClassType(ClassId, TypeList),

    // name of method
    Module(ModuleId),

    // method expression: <expr>.<method_name>
    Method(BuiltinType, Name),

    // method expression with type params: <expr>.<method_name>[T1, T2, ...]
    MethodType(BuiltinType, Name, TypeList),

    // static method expression: SomeClass[T1, T2, ...]::<name>
    StaticMethod(BuiltinType, Name),

    // static method expression: SomeClass[T1, T2, ...]::<name>[T1, T2, ...]
    StaticMethodType(BuiltinType, Name, TypeList),

    // function or class type param: e.g. T
    TypeParam(BuiltinType),

    // static method call on type param: <T>::<name>
    TypeParamStaticMethod(BuiltinType, Name),

    // name of enum
    Enum(EnumId),

    // specific value in enum
    EnumValue(EnumId, u32),
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

    pub fn is_class(&self) -> bool {
        match *self {
            IdentType::Class(_) => true,
            IdentType::ClassType(_, _) => true,
            _ => false,
        }
    }

    pub fn is_fct(&self) -> bool {
        match *self {
            IdentType::Fct(_) => true,
            IdentType::FctType(_, _) => true,
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
    // Function calls, e.g. fct(<args>) or Class::static_fct(<args>)
    Fct(FctId, TypeList, TypeList),

    // Direct or virtual method calls, e.g. obj.method(<args>)
    Method(BuiltinType, FctId, TypeList),

    // Constructor call Class(<args>)
    CtorNew(BuiltinType, FctId),
    Ctor(BuiltinType, FctId),

    // Invoke on expression, e.g. <expr>(<args>)
    Expr(BuiltinType, FctId),

    // Invoke method on trait object
    Trait(TraitId, FctId),

    // Invoke static trait method on type param, e.g. T::method()
    TraitStatic(TypeParamId, TraitId, FctId),

    Intrinsic(Intrinsic),
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
            CallType::Method(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_expr(&self) -> bool {
        match *self {
            CallType::Expr(_, _) => true,
            _ => false,
        }
    }

    pub fn to_intrinsic(&self) -> Option<Intrinsic> {
        match *self {
            CallType::Intrinsic(intrinsic) => Some(intrinsic),
            _ => None,
        }
    }

    pub fn fct_id(&self) -> Option<FctId> {
        match *self {
            CallType::Fct(fctid, _, _) => Some(fctid),
            CallType::Method(_, fctid, _) => Some(fctid),
            CallType::CtorNew(_, fctid) => Some(fctid),
            CallType::Ctor(_, fctid) => Some(fctid),
            CallType::Expr(_, fctid) => Some(fctid),
            CallType::Trait(_, fctid) => Some(fctid),
            CallType::TraitStatic(_, _, fctid) => Some(fctid),
            CallType::Intrinsic(_) => None,
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
