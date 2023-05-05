use std::collections::hash_map::{HashMap, Iter};
use std::ops::{Index, IndexMut};
use std::sync::Arc;

use dora_bytecode::Intrinsic;
use dora_parser::ast;

use crate::language::sem_analysis::{
    ClassDefinitionId, ConstDefinitionId, EnumDefinitionId, FctDefinitionId, FieldId,
    GlobalDefinitionId, StructDefinitionFieldId, StructDefinitionId, TraitDefinitionId,
    TypeParamId,
};
use crate::language::ty::{SourceType, SourceTypeArray};

#[derive(Debug)]
pub struct AnalysisData {
    pub map_calls: NodeMap<Arc<CallType>>, // maps function call to FctId
    pub map_idents: NodeMap<IdentType>,
    pub map_tys: NodeMap<SourceType>,
    pub map_vars: NodeMap<VarId>,
    pub map_literals: NodeMap<(i64, f64)>,
    pub map_char_literals: NodeMap<char>,
    pub map_string_literals: NodeMap<String>,
    pub map_cls: NodeMap<ClassDefinitionId>,
    pub map_fors: NodeMap<ForTypeInfo>,
    pub map_lambdas: NodeMap<FctDefinitionId>,
    pub vars: VarAccess, // variables in functions
    pub context_cls_id: Option<ClassDefinitionId>,
    pub context_has_outer_context_slot: Option<bool>,
    pub outer_context_access: Option<bool>,
}

impl AnalysisData {
    pub fn new() -> AnalysisData {
        AnalysisData {
            map_calls: NodeMap::new(),
            map_idents: NodeMap::new(),
            map_tys: NodeMap::new(),
            map_vars: NodeMap::new(),
            map_cls: NodeMap::new(),
            map_fors: NodeMap::new(),
            map_lambdas: NodeMap::new(),
            map_literals: NodeMap::new(),
            map_char_literals: NodeMap::new(),
            map_string_literals: NodeMap::new(),

            vars: VarAccess::empty(),
            context_cls_id: None,
            context_has_outer_context_slot: None,
            outer_context_access: None,
        }
    }

    pub fn set_ty(&mut self, id: ast::NodeId, ty: SourceType) {
        self.map_tys.insert_or_replace(id, ty);
    }

    pub fn set_literal_value(&mut self, id: ast::NodeId, value_i64: i64, value_f64: f64) {
        self.map_literals.insert(id, (value_i64, value_f64));
    }

    pub fn literal_value(&self, id: ast::NodeId) -> (i64, f64) {
        self.map_literals.get(id).expect("no literal found").clone()
    }

    pub fn set_literal_char(&mut self, id: ast::NodeId, value: char) {
        self.map_char_literals.insert(id, value)
    }

    pub fn literal_char(&self, id: ast::NodeId) -> char {
        self.map_char_literals
            .get(id)
            .expect("no literal found")
            .clone()
    }

    pub fn set_literal_string(&mut self, id: ast::NodeId, value: String) {
        self.map_string_literals.insert(id, value);
    }

    pub fn literal_string(&self, id: ast::NodeId) -> String {
        self.map_string_literals
            .get(id)
            .expect("no literal found")
            .clone()
    }

    pub fn ty(&self, id: ast::NodeId) -> SourceType {
        self.map_tys.get(id).expect("no type found").clone()
    }

    pub fn has_context_class(&self) -> bool {
        self.context_cls_id.is_some()
    }

    pub fn context_has_outer_context_slot(&self) -> bool {
        self.context_has_outer_context_slot.expect("missing")
    }

    pub fn outer_context_access(&self) -> bool {
        self.outer_context_access.expect("missing")
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
pub enum IdentType {
    /// name of local variable
    Var(VarId),

    // context variable
    Context(usize, ContextIdx),

    /// name of a global variable
    Global(GlobalDefinitionId),

    /// field expression: <expr>.<field_name>
    Field(SourceType, FieldId),

    /// field expression: <expr>.<field_name>
    StructField(SourceType, StructDefinitionFieldId),

    /// name of structure
    Struct(StructDefinitionId),

    // name of constant
    Const(ConstDefinitionId),

    // name of function with type params: some_fct[T1, T2, ...]
    Fct(FctDefinitionId, SourceTypeArray),

    // name of class with type params: SomeClass[T1, T2, ...]
    Class(ClassDefinitionId, SourceTypeArray),

    // specific value in enum
    EnumValue(EnumDefinitionId, SourceTypeArray, u32),
}

impl IdentType {
    pub fn var_id(&self) -> VarId {
        match *self {
            IdentType::Var(var_id) => var_id,
            _ => panic!(),
        }
    }

    pub fn struct_id(&self) -> StructDefinitionId {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ContextIdx(pub usize);

#[derive(Debug, Clone)]
pub struct ForTypeInfo {
    pub make_iterator: Option<FctDefinitionId>,
    pub next: FctDefinitionId,
    pub iterator_type: SourceType,
    pub next_type: SourceType,
    pub value_type: SourceType,
}

#[derive(Debug, Clone)]
pub enum CallType {
    // Function calls, e.g. fct(<args>) or Class::static_fct(<args>)
    Fct(FctDefinitionId, SourceTypeArray),

    // Direct or virtual method calls, e.g. obj.method(<args>)
    Method(SourceType, FctDefinitionId, SourceTypeArray),

    // Module method call, e.g. Module::method(<args>)
    ModuleMethod(SourceType, FctDefinitionId, SourceTypeArray),

    // Constructor call Class(<args>)
    Ctor(SourceType, FctDefinitionId),
    // Call to parent constructor, i.e. class Foo() : Bar()
    CtorParent(SourceType, FctDefinitionId),

    // Class constructor of new class syntax, i.e. ClassName(<args>).
    Class2Ctor(ClassDefinitionId, SourceTypeArray),

    // Invoke on expression, e.g. <expr>(<args>)
    Expr(SourceType, FctDefinitionId, SourceTypeArray),

    // Invoke method on trait object
    TraitObjectMethod(SourceType, FctDefinitionId),

    // Invoke trait method on type param, e.g. (T: SomeTrait).method()
    GenericMethod(TypeParamId, TraitDefinitionId, FctDefinitionId),

    // Invoke static trait method on type param, e.g. T::method()
    GenericStaticMethod(TypeParamId, TraitDefinitionId, FctDefinitionId),

    // Construct enum value
    Enum(SourceType, u32),

    // Struct constructor call Struct(<args>)
    Struct(StructDefinitionId, SourceTypeArray),

    // Used for *internal* functions (those are not exposed to Dora as Fct)
    Intrinsic(Intrinsic),

    // Call to lambda,
    Lambda(SourceTypeArray, SourceType),
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
            CallType::Expr(_, _, _) => true,
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

    pub fn fct_id(&self) -> Option<FctDefinitionId> {
        match *self {
            CallType::Fct(fctid, _) => Some(fctid),
            CallType::Method(_, fctid, _) => Some(fctid),
            CallType::ModuleMethod(_, fctid, _) => Some(fctid),
            CallType::Ctor(_, fctid) => Some(fctid),
            CallType::CtorParent(_, fctid) => Some(fctid),
            CallType::Expr(_, fctid, _) => Some(fctid),
            CallType::TraitObjectMethod(_, fctid) => Some(fctid),
            CallType::GenericMethod(_, _, fctid) => Some(fctid),
            CallType::GenericStaticMethod(_, _, fctid) => Some(fctid),
            CallType::Intrinsic(_) => None,
            CallType::Enum(_, _) => None,
            CallType::Struct(_, _) => None,
            CallType::Lambda(_, _) => None,
            CallType::Class2Ctor(_, _) => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct NestedVarId(pub usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct VarId(pub usize);

#[derive(Clone, Debug)]
pub struct Var {
    pub ty: SourceType,
    pub location: VarLocation,
}

impl Index<NestedVarId> for Vec<Var> {
    type Output = Var;

    fn index(&self, index: NestedVarId) -> &Var {
        &self[index.0]
    }
}

impl IndexMut<NestedVarId> for Vec<Var> {
    fn index_mut(&mut self, index: NestedVarId) -> &mut Var {
        &mut self[index.0]
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VarLocation {
    Stack,
    Context(ContextIdx),
}

impl VarLocation {
    pub fn is_stack(&self) -> bool {
        match self {
            VarLocation::Stack => true,
            VarLocation::Context(_) => false,
        }
    }

    pub fn is_context(&self) -> bool {
        match self {
            VarLocation::Context(_) => true,
            VarLocation::Stack => false,
        }
    }
}

#[derive(Debug)]
pub struct VarAccess {
    vars: Vec<Var>,
}

impl VarAccess {
    pub fn new(vars: Vec<Var>) -> VarAccess {
        VarAccess { vars }
    }

    fn empty() -> VarAccess {
        VarAccess { vars: Vec::new() }
    }

    pub fn get_var(&self, idx: VarId) -> &Var {
        &self.vars[idx.0]
    }

    pub fn get_self(&self) -> &Var {
        &self.vars[0]
    }
}
