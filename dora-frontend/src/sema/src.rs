use std::collections::hash_map::{HashMap, Iter};
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use crate::sema::{
    ClassDefinition, ClassDefinitionId, ConstDefinitionId, EnumDefinitionId, FctDefinitionId,
    FieldDefinition, FieldIndex, GlobalDefinitionId, Intrinsic, StructDefinitionId, UniversalId,
};
use crate::ty::{SourceType, SourceTypeArray, TraitType, TypeArgs};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LambdaId(pub usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ContextId(pub usize);

#[derive(Debug)]
pub struct ContextData {
    parent: Option<ContextId>,
    has_parent_slot: bool,
    class_id: Option<ClassDefinitionId>,
    class_definition: Option<ClassDefinition>,
    fields: Vec<FieldDefinition>,
}

impl ContextData {
    pub fn new(parent: Option<ContextId>) -> ContextData {
        ContextData {
            parent,
            has_parent_slot: false,
            class_id: None,
            class_definition: None,
            fields: Vec::new(),
        }
    }

    pub fn require_parent_slot(&mut self) {
        assert!(!self.has_class_id());
        self.has_parent_slot = true;
    }

    pub fn has_parent_slot(&self) -> bool {
        self.has_parent_slot
    }

    pub fn parent(&self) -> Option<ContextId> {
        self.parent
    }

    pub fn set_class_data(
        &mut self,
        class_definition: ClassDefinition,
        fields: Vec<FieldDefinition>,
    ) {
        assert!(self.class_definition.is_none());
        assert!(self.fields.is_empty());
        self.class_definition = Some(class_definition);
        self.fields = fields;
    }

    pub fn class_definition(&mut self) -> Option<ClassDefinition> {
        self.class_definition.take()
    }

    pub fn fields(&mut self) -> Vec<FieldDefinition> {
        std::mem::take(&mut self.fields)
    }

    pub fn set_class_id(&mut self, id: ClassDefinitionId) {
        assert!(self.class_id.replace(id).is_none());
    }

    pub fn has_class_id(&self) -> bool {
        self.class_id.is_some()
    }

    pub fn class_id(&self) -> ClassDefinitionId {
        self.class_id.expect("missing class id")
    }
}

#[derive(Debug)]
pub struct NodeMap<V> {
    map: HashMap<UniversalId, V>,
}

impl<V> NodeMap<V> {
    pub fn new() -> NodeMap<V> {
        NodeMap {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, id: UniversalId) -> Option<&V> {
        self.map.get(&id)
    }

    pub fn get_mut(&mut self, id: UniversalId) -> Option<&mut V> {
        self.map.get_mut(&id)
    }

    pub fn insert(&mut self, id: UniversalId, data: V) {
        let old = self.map.insert(id, data);
        assert!(old.is_none());
    }

    pub fn replace(&mut self, id: UniversalId, data: V) {
        let old = self.map.insert(id, data);
        assert!(old.is_some());
    }

    pub fn insert_or_replace(&mut self, id: UniversalId, data: V) {
        self.map.insert(id, data);
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }

    pub fn iter(&self) -> Iter<'_, UniversalId, V> {
        self.map.iter()
    }
}

#[derive(Debug, Clone)]
pub struct ArrayAssignment {
    pub index_get: Option<Rc<CallType>>,
    pub index_set: Option<Rc<CallType>>,
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
    Context(ContextId, ContextFieldId),

    // Name of a global variable.
    Global(GlobalDefinitionId),

    // Class field expression: <expr>.<field_name>
    ClassField(SourceType, FieldIndex),

    // Field expression: <expr>.<field_name>
    StructField(SourceType, FieldIndex),

    // Tuple field expression: <expr>.<index>
    TupleField(SourceType, u32),

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
            IdentType::ClassField(_, _) => true,
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
    pub iter: Option<(FctDefinitionId, TypeArgs)>,
    pub next: Option<FctDefinitionId>,
    pub iterator_type: SourceType,
    pub next_type: SourceType,
    pub value_type: SourceType,
}

#[derive(Debug, Clone)]
pub enum CallType {
    // Function calls, e.g. fct(<args>) or Class::static_fct(<args>).
    Fct(FctDefinitionId, TypeArgs),

    // Direct or virtual method calls, e.g. obj.method(<args>).
    Method(SourceType, FctDefinitionId, TypeArgs),

    // Invoke on expression, e.g. <expr>(<args>). Used for array loads/stores.
    Expr(SourceType, FctDefinitionId, TypeArgs),

    // Invoke method on trait object
    TraitObjectMethod(SourceType, FctDefinitionId),

    // Invoke trait method on type param, Self, or associated type, e.g. (T: SomeTrait).method(), self.method() in trait defaults
    GenericMethod {
        object_type: SourceType,
        trait_ty: TraitType,
        fct_id: FctDefinitionId,
        fct_type_params: SourceTypeArray,
    },

    // Invoke static trait method on type param, Self, or associated type, e.g. T::method(), Self::method(), Self::T::method()
    GenericStaticMethod {
        object_type: SourceType,
        trait_ty: TraitType,
        fct_id: FctDefinitionId,
        fct_type_params: SourceTypeArray,
    },

    // Class constructor of new class syntax, i.e. ClassName(<args>).
    NewClass(ClassDefinitionId, SourceTypeArray),

    // Construct enum value.
    NewEnum(SourceType, u32),

    // Struct constructor call Struct(<args>).
    NewStruct(StructDefinitionId, SourceTypeArray),

    // Used for internal functions (those are not exposed to Dora as Fct). Used for enum comparisons.
    Intrinsic(Intrinsic),

    // Invoke lambda function.
    Lambda(SourceTypeArray, SourceType, bool),
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
            CallType::GenericMethod { .. } => true,
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
            | CallType::GenericMethod { fct_id, .. }
            | CallType::GenericStaticMethod { fct_id, .. } => Some(fct_id),

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

    pub fn get_var(&self, idx: VarId) -> &Var {
        &self.vars[idx.0]
    }

    pub fn get_self(&self) -> &Var {
        &self.vars[0]
    }
}
