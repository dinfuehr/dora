use std::cell::{Cell, OnceCell};
use std::collections::hash_map::{HashMap, Iter};
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use std::sync::Arc;

use dora_parser::ast;

use crate::sema::{
    ClassDefinition, ClassDefinitionId, ConstDefinitionId, ConstValue, EnumDefinitionId,
    FctDefinition, FctDefinitionId, FieldId, GlobalDefinitionId, Intrinsic,
    StructDefinitionFieldId, StructDefinitionId, TraitDefinitionId, TypeParamId,
};
use crate::ty::{SourceType, SourceTypeArray};

#[derive(Debug)]
pub struct AnalysisData {
    pub has_self: Option<bool>,
    pub map_templates: NodeMap<(FctDefinitionId, SourceTypeArray)>,
    pub map_calls: NodeMap<Arc<CallType>>, // maps function call to FctId
    pub map_idents: NodeMap<IdentType>,
    pub map_tys: NodeMap<SourceType>,
    pub map_vars: NodeMap<VarId>,
    pub map_consts: NodeMap<ConstValue>,
    pub map_cls: NodeMap<ClassDefinitionId>,
    pub map_fors: NodeMap<ForTypeInfo>,
    pub map_lambdas: NodeMap<LazyLambdaId>,
    pub map_block_contexts: NodeMap<LazyContextData>,
    pub map_argument: NodeMap<usize>,
    pub map_field_ids: NodeMap<usize>,
    pub map_array_assignments: NodeMap<ArrayAssignment>,

    // All variables defined in this function (including
    // context allocated ones).
    pub vars: VarAccess,

    pub function_context_data: OnceCell<LazyContextData>,
    pub needs_context_slot_in_lambda_object: OnceCell<bool>,
    pub outer_contexts: Vec<LazyContextData>,
}

impl AnalysisData {
    pub fn new() -> AnalysisData {
        AnalysisData {
            has_self: None,
            map_templates: NodeMap::new(),
            map_calls: NodeMap::new(),
            map_idents: NodeMap::new(),
            map_tys: NodeMap::new(),
            map_vars: NodeMap::new(),
            map_cls: NodeMap::new(),
            map_fors: NodeMap::new(),
            map_lambdas: NodeMap::new(),
            map_consts: NodeMap::new(),
            map_block_contexts: NodeMap::new(),
            map_argument: NodeMap::new(),
            map_field_ids: NodeMap::new(),
            map_array_assignments: NodeMap::new(),

            vars: VarAccess::empty(),
            function_context_data: OnceCell::new(),
            needs_context_slot_in_lambda_object: OnceCell::new(),
            outer_contexts: Vec::new(),
        }
    }

    pub fn set_has_self(&mut self, value: bool) {
        self.has_self = Some(value);
    }

    pub fn has_self(&self) -> bool {
        self.has_self.expect("has_self uninitialized")
    }

    pub fn set_ty(&mut self, id: ast::NodeId, ty: SourceType) {
        self.map_tys.insert_or_replace(id, ty);
    }

    pub fn set_const_value(&mut self, id: ast::NodeId, value: ConstValue) {
        self.map_consts.insert(id, value);
    }

    pub fn const_value(&self, id: ast::NodeId) -> &ConstValue {
        self.map_consts.get(id).expect("no literal found")
    }

    pub fn ty(&self, id: ast::NodeId) -> SourceType {
        self.map_tys.get(id).expect("no type found").clone()
    }

    pub fn function_context_data(&self) -> LazyContextData {
        self.function_context_data
            .get()
            .cloned()
            .expect("missing context")
    }

    pub fn needs_context_slot_in_lambda_object(&self) -> bool {
        self.needs_context_slot_in_lambda_object
            .get()
            .cloned()
            .expect("missing value")
    }
}

#[derive(Clone, Debug)]
pub struct LazyLambdaId(Rc<OnceCell<FctDefinitionId>>);

impl LazyLambdaId {
    pub fn new() -> LazyLambdaId {
        LazyLambdaId(Rc::new(OnceCell::new()))
    }

    pub fn fct_id(&self) -> FctDefinitionId {
        self.0.get().cloned().expect("uninitialized")
    }

    pub fn set_fct_id(&self, fct_id: FctDefinitionId) {
        assert!(self.0.set(fct_id).is_ok());
    }
}

pub struct LazyContextClassCreationData {
    pub context: LazyContextData,
    pub class_definition: ClassDefinition,
}

pub struct LazyLambdaCreationData {
    pub id: LazyLambdaId,
    pub fct_definition: FctDefinition,
}

#[derive(Clone, Debug)]
pub struct LazyContextData(Rc<ContextData>);

impl LazyContextData {
    pub fn new() -> LazyContextData {
        LazyContextData(Rc::new(ContextData {
            has_parent_slot: Cell::new(false),
            class_id: OnceCell::new(),
        }))
    }

    pub fn require_parent_slot(&self) {
        assert!(!self.has_class_id());

        if !self.has_parent_slot() {
            self.0.has_parent_slot.set(true);
        }
    }

    pub fn has_parent_slot(&self) -> bool {
        self.0.has_parent_slot.get()
    }

    pub fn set_class_id(&self, id: ClassDefinitionId) {
        assert!(self.0.class_id.set(id).is_ok());
    }

    pub fn has_class_id(&self) -> bool {
        self.0.class_id.get().is_some()
    }

    pub fn class_id(&self) -> ClassDefinitionId {
        self.0.class_id.get().cloned().expect("missing class id")
    }
}

#[derive(Clone, Debug)]
pub struct ContextData {
    pub has_parent_slot: Cell<bool>,
    pub class_id: OnceCell<ClassDefinitionId>,
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
pub struct OuterContextIdx(pub usize);

#[derive(Debug, Clone)]
pub struct ArrayAssignment {
    pub index_get: Option<Arc<CallType>>,
    pub index_set: Option<Arc<CallType>>,
    pub item_ty: Option<SourceType>,
}

impl ArrayAssignment {
    pub fn new() -> ArrayAssignment {
        ArrayAssignment {
            index_get: None,
            index_set: None,
            item_ty: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum IdentType {
    /// Name of local variable.
    Var(VarId),

    // Context variable.
    Context(OuterContextIdx, ContextFieldId),

    // Name of a global variable.
    Global(GlobalDefinitionId),

    // Field expression: <expr>.<field_name>
    Field(SourceType, FieldId),

    // Field expression: <expr>.<field_name>
    StructField(SourceType, StructDefinitionFieldId),

    // Name of structure.
    Struct(StructDefinitionId, SourceTypeArray),

    // Name of constant.
    Const(ConstDefinitionId),

    // Name of function with type params: some_fct[T1, T2, ...].
    Fct(FctDefinitionId, SourceTypeArray),

    // Name of class with type params: SomeClass[T1, T2, ...].
    Class(ClassDefinitionId, SourceTypeArray),

    // Specific enum variant.
    EnumVariant(EnumDefinitionId, SourceTypeArray, u32),
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
            &IdentType::Struct(sid, ..) => sid,
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
pub struct InnerContextId(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ContextFieldId(pub usize);

#[derive(Debug, Clone)]
pub struct ForTypeInfo {
    pub iter: Option<(FctDefinitionId, SourceTypeArray)>,
    pub next: Option<FctDefinitionId>,
    pub iterator_type: SourceType,
    pub next_type: SourceType,
    pub value_type: SourceType,
}

#[derive(Debug, Clone)]
pub enum CallType {
    // Function calls, e.g. fct(<args>) or Class::static_fct(<args>).
    Fct(FctDefinitionId, SourceTypeArray),

    // Direct or virtual method calls, e.g. obj.method(<args>).
    Method(SourceType, FctDefinitionId, SourceTypeArray),

    // Invoke on expression, e.g. <expr>(<args>). Used for array loads/stores.
    Expr(SourceType, FctDefinitionId, SourceTypeArray),

    // Invoke method on trait object
    TraitObjectMethod(SourceType, FctDefinitionId),

    // Invoke trait method on type param, e.g. (T: SomeTrait).method()
    GenericMethod(
        TypeParamId,
        TraitDefinitionId,
        FctDefinitionId,
        SourceTypeArray,
        SourceTypeArray,
    ),

    // Invoke trait method from a default trait method, e.g. self.method().
    GenericMethodSelf(
        TraitDefinitionId,
        FctDefinitionId,
        SourceTypeArray,
        SourceTypeArray,
    ),

    // Invoke static trait method on type param, e.g. T::method()
    GenericStaticMethod(
        TypeParamId,
        TraitDefinitionId,
        FctDefinitionId,
        SourceTypeArray,
        SourceTypeArray,
    ),

    // Invoke static trait method from a default trait method, e.g. Self::method().
    GenericStaticMethodSelf(
        TraitDefinitionId,
        FctDefinitionId,
        SourceTypeArray,
        SourceTypeArray,
    ),

    // Class constructor of new class syntax, i.e. ClassName(<args>).
    NewClass(ClassDefinitionId, SourceTypeArray),

    // Construct enum value.
    NewEnum(SourceType, u32),

    // Struct constructor call Struct(<args>).
    NewStruct(StructDefinitionId, SourceTypeArray),

    // Used for internal functions (those are not exposed to Dora as Fct). Used for enum comparisons.
    Intrinsic(Intrinsic),

    // Invoke lambda function.
    Lambda(SourceTypeArray, SourceType),
}

impl CallType {
    pub fn is_method(&self) -> bool {
        match *self {
            CallType::Method(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_generic_method(&self) -> bool {
        match *self {
            CallType::GenericMethod(..) => true,
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
            CallType::NewEnum(_, _) => true,
            _ => false,
        }
    }

    pub fn fct_id(&self) -> Option<FctDefinitionId> {
        match *self {
            CallType::Fct(fct_id, _)
            | CallType::Method(_, fct_id, _)
            | CallType::Expr(_, fct_id, _)
            | CallType::TraitObjectMethod(_, fct_id)
            | CallType::GenericMethod(_, _, fct_id, ..)
            | CallType::GenericStaticMethod(_, _, fct_id, ..)
            | CallType::GenericMethodSelf(_, fct_id, ..)
            | CallType::GenericStaticMethodSelf(_, fct_id, ..) => Some(fct_id),

            CallType::NewClass(..)
            | CallType::NewStruct(..)
            | CallType::NewEnum(..)
            | CallType::Lambda(..)
            | CallType::Intrinsic(..) => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct NestedScopeId(pub usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct ScopeId(pub usize);

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
    Context(ScopeId, ContextFieldId),
}

impl VarLocation {
    pub fn is_stack(&self) -> bool {
        match self {
            VarLocation::Stack => true,
            VarLocation::Context(..) => false,
        }
    }

    pub fn is_context(&self) -> bool {
        match self {
            VarLocation::Context(..) => true,
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
