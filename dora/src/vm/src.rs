use std::collections::hash_map::{HashMap, Iter};
use std::ops::{Index, IndexMut};
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;

use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    ClassId, ConstId, EnumId, FctId, FieldId, GlobalId, Intrinsic, ModuleId, StructFieldId,
    StructId, TraitId, TypeParamId,
};

#[derive(Debug)]
pub struct AnalysisData {
    pub map_calls: NodeMap<Arc<CallType>>, // maps function call to FctId
    pub map_idents: NodeMap<IdentType>,
    pub map_tys: NodeMap<SourceType>,
    pub map_vars: NodeMap<VarId>,
    pub map_convs: NodeMap<ConvInfo>,
    pub map_cls: NodeMap<ClassId>,
    pub map_fors: NodeMap<ForTypeInfo>,

    // false if function execution could reach the closing } of this function
    pub vars: Vec<Var>, // variables in functions
}

impl AnalysisData {
    pub fn new() -> AnalysisData {
        AnalysisData {
            map_calls: NodeMap::new(),
            map_idents: NodeMap::new(),
            map_tys: NodeMap::new(),
            map_vars: NodeMap::new(),
            map_convs: NodeMap::new(),
            map_cls: NodeMap::new(),
            map_fors: NodeMap::new(),

            vars: Vec::new(),
        }
    }

    pub fn set_ty(&mut self, id: ast::NodeId, ty: SourceType) {
        self.map_tys.insert_or_replace(id, ty);
    }

    pub fn ty(&self, id: ast::NodeId) -> SourceType {
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

#[derive(Debug, Clone)]
pub struct ConvInfo {
    pub check_type: SourceType,
    pub valid: bool,
}

#[derive(Debug, Clone)]
pub enum IdentType {
    /// name of local variable
    Var(VarId),

    /// name of a global variable
    Global(GlobalId),

    /// field expression: <expr>.<field_name>
    Field(SourceType, FieldId),

    /// field expression: <expr>.<field_name>
    StructField(SourceType, StructFieldId),

    /// name of structure
    Struct(StructId),

    // name of constant
    Const(ConstId),

    // name of module
    Module(ModuleId),

    // name of function with type params: some_fct[T1, T2, ...]
    Fct(FctId, SourceTypeArray),

    // name of class with type params: SomeClass[T1, T2, ...]
    Class(ClassId, SourceTypeArray),

    // specific value in enum
    EnumValue(EnumId, SourceTypeArray, usize),
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
            IdentType::Class(_, _) => true,
            _ => false,
        }
    }

    pub fn is_fct(&self) -> bool {
        match *self {
            IdentType::Fct(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ForTypeInfo {
    pub make_iterator: Option<FctId>,
    pub next: FctId,
    pub has_next: FctId,
    pub iterator_type: SourceType,
    pub next_type: SourceType,
}

#[derive(Debug, Clone)]
pub enum CallType {
    // Function calls, e.g. fct(<args>) or Class::static_fct(<args>)
    Fct(FctId, SourceTypeArray),

    // Direct or virtual method calls, e.g. obj.method(<args>)
    Method(SourceType, FctId, SourceTypeArray),

    // Module method call, e.g. Module::method(<args>)
    ModuleMethod(SourceType, FctId, SourceTypeArray),

    // Constructor call Class(<args>)
    Ctor(SourceType, FctId),
    // Call to parent constructor, i.e. class Foo() : Bar()
    CtorParent(SourceType, FctId),

    // Invoke on expression, e.g. <expr>(<args>)
    Expr(SourceType, FctId),

    // Invoke method on trait object
    TraitObjectMethod(TraitId, FctId),

    // Invoke trait method on type param, e.g. (T: SomeTrait).method()
    GenericMethod(TypeParamId, TraitId, FctId),

    // Invoke static trait method on type param, e.g. T::method()
    GenericStaticMethod(TypeParamId, TraitId, FctId),

    // Construct enum value
    Enum(SourceType, usize),

    // Struct constructor call Struct(<args>)
    Struct(StructId, SourceTypeArray),

    // Used for *internal* functions (those are not exposed to Dora as Fct)
    Intrinsic(Intrinsic),
}

impl CallType {
    pub fn is_ctor_new(&self) -> bool {
        match *self {
            CallType::Ctor(_, _) => true,
            _ => false,
        }
    }

    pub fn is_ctor(&self) -> bool {
        match *self {
            CallType::CtorParent(_, _) => true,
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

    pub fn is_enum(&self) -> bool {
        match *self {
            CallType::Enum(_, _) => true,
            _ => false,
        }
    }

    pub fn fct_id(&self) -> Option<FctId> {
        match *self {
            CallType::Fct(fctid, _) => Some(fctid),
            CallType::Method(_, fctid, _) => Some(fctid),
            CallType::ModuleMethod(_, fctid, _) => Some(fctid),
            CallType::Ctor(_, fctid) => Some(fctid),
            CallType::CtorParent(_, fctid) => Some(fctid),
            CallType::Expr(_, fctid) => Some(fctid),
            CallType::TraitObjectMethod(_, fctid) => Some(fctid),
            CallType::GenericMethod(_, _, fctid) => Some(fctid),
            CallType::GenericStaticMethod(_, _, fctid) => Some(fctid),
            CallType::Intrinsic(_) => None,
            CallType::Enum(_, _) => None,
            CallType::Struct(_, _) => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct VarId(pub usize);

#[derive(Clone, Debug)]
pub struct Var {
    pub id: VarId,
    pub name: Name,
    pub ty: SourceType,
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
