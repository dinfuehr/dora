use std::cell::OnceCell;

use crate::ParsedType;
use crate::element_collector::Annotations;
use crate::interner::Name;
use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase, SyntaxNodePtr};
use id_arena::Id;

use crate::sema::{
    Body, ClassDefinitionId, Element, ElementId, EnumDefinitionId, ExprId, ExtensionDefinitionId,
    FieldDefinitionId, ImplDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, StructDefinitionId, TraitDefinitionId, TypeParamDefinitionId, TypeRefArena,
    TypeRefArenaBuilder, Visibility, lower_type, module_path,
};
use crate::ty::SourceType;
use dora_bytecode::BytecodeBody;
pub use dora_bytecode::Intrinsic;

pub type FctDefinitionId = Id<FctDefinition>;

#[derive(Clone, Copy)]
pub enum DerivedTarget {
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
    Enum(EnumDefinitionId),
}

impl DerivedTarget {
    pub(crate) fn element(self, sa: &Sema) -> &dyn Element {
        match self {
            DerivedTarget::Class(id) => sa.class(id),
            DerivedTarget::Struct(id) => sa.struct_(id),
            DerivedTarget::Enum(id) => sa.enum_(id),
        }
    }

    pub(crate) fn ty(self, sa: &Sema) -> SourceType {
        let type_params = self
            .element(sa)
            .type_param_definition(sa)
            .identity_type_params(sa);

        match self {
            DerivedTarget::Class(id) => SourceType::Class(id, type_params),
            DerivedTarget::Struct(id) => SourceType::Struct(id, type_params),
            DerivedTarget::Enum(id) => SourceType::Enum(id, type_params),
        }
    }

    pub(crate) fn annotation_span(self, sa: &Sema, annotation: &str) -> Span {
        let modifier_list = match self {
            DerivedTarget::Class(id) => sa.class(id).ast(sa).modifier_list(),
            DerivedTarget::Struct(id) => sa.struct_(id).ast(sa).modifier_list(),
            DerivedTarget::Enum(id) => sa.enum_(id).ast(sa).modifier_list(),
        };

        modifier_list
            .and_then(|list| list.find_modifier(annotation))
            .expect("missing annotation")
            .span()
    }

    pub(crate) fn is_simple_enum(self, sa: &Sema) -> bool {
        match self {
            DerivedTarget::Enum(id) => sa.enum_(id).is_simple_enum(),
            DerivedTarget::Class(_) | DerivedTarget::Struct(_) => false,
        }
    }

    pub(crate) fn field_ids(self, sa: &Sema) -> Vec<FieldDefinitionId> {
        match self {
            DerivedTarget::Class(id) => sa.class(id).field_ids().to_vec(),
            DerivedTarget::Struct(id) => sa.struct_(id).field_ids().to_vec(),
            DerivedTarget::Enum(id) => sa
                .enum_(id)
                .variant_ids()
                .iter()
                .flat_map(|&variant_id| sa.variant(variant_id).field_ids())
                .copied()
                .collect(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum DerivedMethod {
    Comparable(DerivedTarget),
    Equals(DerivedTarget),
    Hash(DerivedTarget),
    Stringable(DerivedTarget),
}

pub struct FctDefinition {
    pub id: Option<FctDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub syntax_node_ptr: Option<SyntaxNodePtr>,
    pub declaration_span: Span,
    pub span: Span,
    pub name: Name,
    pub parent: FctParent,
    pub is_static: bool,
    pub is_mutating: bool,
    pub visibility: Visibility,
    pub is_test: bool,
    pub is_internal: bool,
    pub is_native: bool,
    pub is_force_inline: bool,
    pub is_never_inline: bool,
    pub is_trait_object_ignore: bool,
    pub is_in_trait: bool,
    pub is_lambda: bool,
    pub params: Params,
    pub return_type: ParsedType,
    pub type_refs: OnceCell<TypeRefArena>,

    pub body: OnceCell<Body>,

    pub type_param_definition_id: TypeParamDefinitionId,
    pub container_type_params: OnceCell<usize>,
    pub bytecode: OnceCell<BytecodeBody>,
    pub intrinsic: OnceCell<Intrinsic>,
    pub trait_method_impl: OnceCell<FctDefinitionId>,
    pub derived_method: Option<DerivedMethod>,
    is_default_trait_method_adapter: bool,
}

impl FctDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: ast::AstCallable,
        modifiers: Annotations,
        name: Name,
        type_param_definition_id: TypeParamDefinitionId,
        params: Params,
        return_type: ParsedType,
        parent: FctParent,
    ) -> FctDefinition {
        let is_in_trait = matches!(parent, FctParent::Trait(_));

        FctDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            declaration_span: ast.declaration_span(),
            span: ast.span(),
            syntax_node_ptr: Some(ast.as_ptr()),
            name,
            params,
            return_type,
            parent,
            visibility: modifiers.visibility(),
            is_static: modifiers.is_static,
            is_mutating: modifiers.is_mutating,
            is_test: modifiers.is_test,
            is_internal: modifiers.is_internal,
            is_native: modifiers.is_native,
            is_force_inline: modifiers.is_force_inline,
            is_never_inline: modifiers.is_never_inline,
            is_trait_object_ignore: modifiers.is_trait_object_ignore,
            is_in_trait,
            is_lambda: false,
            body: OnceCell::new(),
            type_refs: OnceCell::new(),
            type_param_definition_id,
            container_type_params: OnceCell::new(),
            bytecode: OnceCell::new(),
            intrinsic: OnceCell::new(),
            trait_method_impl: OnceCell::new(),
            derived_method: None,
            is_default_trait_method_adapter: false,
        }
    }

    pub(crate) fn new_no_source(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        declaration_span: Span,
        span: Span,
        syntax_node_ptr: Option<SyntaxNodePtr>,
        modifiers: Annotations,
        name: Name,
        type_param_definition_id: TypeParamDefinitionId,
        params: Params,
        return_type: SourceType,
        parent: FctParent,
        is_in_trait: bool,
    ) -> FctDefinition {
        FctDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            declaration_span: declaration_span,
            span: span,
            syntax_node_ptr,
            name,
            params,
            return_type: ParsedType::new_ty(return_type),
            parent,
            visibility: modifiers.visibility(),
            is_static: modifiers.is_static,
            is_mutating: modifiers.is_mutating,
            is_test: modifiers.is_test,
            is_internal: modifiers.is_internal,
            is_native: modifiers.is_native,
            is_force_inline: modifiers.is_force_inline,
            is_never_inline: modifiers.is_never_inline,
            is_trait_object_ignore: modifiers.is_trait_object_ignore,
            is_in_trait,
            is_lambda: false,
            body: OnceCell::new(),
            type_refs: OnceCell::new(),
            type_param_definition_id,
            container_type_params: OnceCell::new(),
            bytecode: OnceCell::new(),
            intrinsic: OnceCell::new(),
            trait_method_impl: OnceCell::new(),
            derived_method: None,
            is_default_trait_method_adapter: false,
        }
    }

    pub(crate) fn new_default_trait_method_adapter(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        declaration_span: Span,
        span: Span,
        modifiers: Annotations,
        name: Name,
        type_param_definition_id: TypeParamDefinitionId,
        params: Params,
        return_type: SourceType,
        impl_id: ImplDefinitionId,
        trait_method_id: FctDefinitionId,
    ) -> FctDefinition {
        let mut fct = FctDefinition::new_no_source(
            package_id,
            module_id,
            file_id,
            declaration_span,
            span,
            None,
            modifiers,
            name,
            type_param_definition_id,
            params,
            return_type,
            FctParent::Impl(impl_id),
            false,
        );
        fct.is_default_trait_method_adapter = true;
        assert!(fct.trait_method_impl.set(trait_method_id).is_ok());
        fct
    }

    pub fn id(&self) -> FctDefinitionId {
        self.id.expect("id missing")
    }

    pub fn ast<'a>(&self, sa: &'a Sema) -> ast::AstCallable {
        let node_ptr = self.syntax_node_ptr.expect("missing ptr");
        sa.syntax(self.file_id, node_ptr)
    }

    pub fn container_type_params(&self) -> usize {
        self.container_type_params
            .get()
            .cloned()
            .expect("missing type params")
    }

    pub fn body(&self) -> &Body {
        self.body.get().expect("missing body")
    }

    pub fn body_expr_id(&self) -> ExprId {
        self.body().root_expr_id()
    }

    pub fn set_body(&self, body: Body) {
        assert!(self.body.set(body).is_ok());
    }

    pub fn has_parent(&self) -> bool {
        match self.parent {
            FctParent::None => false,
            _ => true,
        }
    }

    pub fn in_trait(&self) -> bool {
        match self.parent {
            FctParent::Trait(_) => true,
            _ => false,
        }
    }

    pub fn is_default_trait_method_adapter(&self) -> bool {
        self.is_default_trait_method_adapter
    }

    pub fn is_self_allowed(&self) -> bool {
        match self.parent {
            FctParent::Impl(..) | FctParent::Trait(..) | FctParent::Extension(..) => true,
            FctParent::None => false,
            FctParent::Function => unreachable!(),
        }
    }

    pub fn trait_id(&self) -> TraitDefinitionId {
        match self.parent {
            FctParent::Trait(traitid) => traitid,
            _ => unreachable!(),
        }
    }

    pub fn display_name(&self, sa: &Sema) -> String {
        let mut repr = match self.parent {
            FctParent::Trait(trait_id) => {
                let trait_ = sa.trait_(trait_id);
                trait_.name(sa)
            }

            FctParent::Extension(extension_id) => {
                let extension = sa.extension(extension_id);
                path_for_type(sa, extension.ty().clone())
            }

            FctParent::Impl(impl_id) => {
                let impl_ = sa.impl_(impl_id);
                let module = sa.module(impl_.module_id());
                module.name(sa)
            }

            FctParent::None => {
                return module_path(sa, self.module_id, self.name);
            }

            FctParent::Function => "lamba".into(),
        };

        if !self.has_parent() || self.is_static {
            repr.push_str("::");
        } else {
            repr.push_str("#");
        }

        repr.push_str(&sa.interner.str(self.name));
        repr
    }

    pub fn has_body(&self, sa: &Sema) -> bool {
        self.syntax_node_ptr
            .map(|ptr| {
                let node = sa.syntax::<ast::AstCallable>(self.file_id, ptr);
                node.block().is_some()
            })
            .unwrap_or(false)
    }

    pub fn is_lambda(&self) -> bool {
        self.is_lambda
    }

    pub fn needs_self_type_param(&self, sa: &Sema) -> bool {
        self.is_in_trait && self.has_body(sa)
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn analysis(&self) -> &Body {
        self.body()
    }

    pub fn has_hidden_self_argument(&self) -> bool {
        match self.parent {
            FctParent::Trait(_) | FctParent::Impl(_) | FctParent::Extension(_) => !self.is_static,
            FctParent::Function => true,
            FctParent::None => false,
        }
    }

    pub fn params_with_self(&self) -> &[Param] {
        &self.params.params
    }

    pub fn params_without_self(&self) -> &[Param] {
        if self.has_hidden_self_argument() {
            &self.params_with_self()[1..]
        } else {
            self.params_with_self()
        }
    }

    pub fn self_param(&self) -> Option<&Param> {
        if self.has_hidden_self_argument() {
            Some(&self.params.params[0])
        } else {
            None
        }
    }

    pub fn return_type(&self) -> SourceType {
        self.parsed_return_type().ty()
    }

    pub fn parsed_return_type(&self) -> &ParsedType {
        &self.return_type
    }

    pub fn set_type_refs(&self, type_refs: TypeRefArena) {
        assert!(self.type_refs.set(type_refs).is_ok());
    }
}

impl Element for FctDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Fct(self.id())
    }

    fn file_id(&self) -> SourceFileId {
        self.file_id
    }

    fn span(&self) -> Span {
        self.span
    }

    fn module_id(&self) -> ModuleDefinitionId {
        self.module_id
    }

    fn package_id(&self) -> PackageDefinitionId {
        self.package_id
    }

    fn type_param_definition_id(&self) -> TypeParamDefinitionId {
        self.type_param_definition_id
    }

    fn to_fct(&self) -> Option<&FctDefinition> {
        Some(self)
    }

    fn self_ty(&self, sa: &Sema) -> Option<SourceType> {
        match self.parent {
            FctParent::Extension(id) => Some(sa.extension(id).ty()),
            FctParent::Impl(id) => Some(sa.impl_(id).extended_ty()),
            FctParent::Function => unreachable!(),
            FctParent::Trait(..) => unimplemented!(),
            FctParent::None => None,
        }
    }

    fn visibility(&self) -> Visibility {
        self.visibility
    }

    fn type_ref_arena(&self) -> &TypeRefArena {
        self.type_refs.get().expect("missing type refs")
    }

    fn children(&self) -> &[ElementId] {
        &[]
    }
}

fn path_for_type(sa: &Sema, ty: SourceType) -> String {
    if let Some(enum_id) = ty.enum_id() {
        let enum_ = sa.enum_(enum_id);
        enum_.name(sa)
    } else if let Some(cls_id) = ty.cls_id() {
        let cls = sa.class(cls_id);
        cls.name(sa)
    } else if let Some(struct_id) = ty.struct_id() {
        let struct_ = sa.struct_(struct_id);
        struct_.name(sa)
    } else if let Some(struct_id) = ty.primitive_struct_id(sa) {
        let struct_ = sa.struct_(struct_id);
        struct_.name(sa)
    } else if ty.is_tuple_or_unit() {
        unimplemented!()
    } else {
        unreachable!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FctParent {
    Trait(TraitDefinitionId),
    Impl(ImplDefinitionId),
    Extension(ExtensionDefinitionId),
    Function,
    None,
}

impl FctParent {
    pub fn is_none(&self) -> bool {
        match self {
            &FctParent::None => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            &FctParent::Function => true,
            _ => false,
        }
    }

    pub fn is_impl(&self) -> bool {
        match self {
            &FctParent::Impl(..) => true,
            _ => false,
        }
    }

    pub fn is_trait(&self) -> bool {
        match self {
            &FctParent::Trait(_) => true,
            _ => false,
        }
    }

    pub fn trait_id(&self) -> Option<TraitDefinitionId> {
        match self {
            &FctParent::Trait(id) => Some(id),
            _ => None,
        }
    }

    pub fn extension_id(&self) -> Option<ExtensionDefinitionId> {
        match self {
            &FctParent::Extension(id) => Some(id),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Params {
    pub params: Vec<Param>,
    pub has_self: bool,
    pub is_variadic: bool,
}

impl Params {
    pub fn new(params: Vec<Param>, has_self: bool, is_variadic: bool) -> Params {
        Params {
            params,
            has_self,
            is_variadic,
        }
    }

    pub fn regular_params(&self) -> &[Param] {
        let start = self.has_self as usize;
        let end = self.params.len() - self.is_variadic as usize;

        &self.params[start..end]
    }

    pub fn variadic_param(&self) -> Option<&Param> {
        if self.is_variadic() {
            Some(self.params.last().expect("missing param"))
        } else {
            None
        }
    }

    pub fn is_variadic(&self) -> bool {
        self.is_variadic
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub parsed_ty: ParsedType,
}

impl Param {
    pub fn new(
        sa: &mut Sema,
        type_ref_arena: &mut TypeRefArenaBuilder,
        file_id: SourceFileId,
        ast: &ast::AstParam,
    ) -> Param {
        Param {
            parsed_ty: ParsedType::new_opt(
                ast.data_type()
                    .map(|ty| lower_type(sa, type_ref_arena, file_id, ty)),
            ),
        }
    }

    pub fn new_ty(ty: SourceType) -> Param {
        Param {
            parsed_ty: ParsedType::new_ty(ty),
        }
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }

    pub fn set_ty(&self, ty: SourceType) {
        self.parsed_ty().set_ty(ty);
    }
}

pub fn emit_as_bytecode_operation(intrinsic: Intrinsic) -> bool {
    match intrinsic {
        Intrinsic::ArrayNewOfSize
        | Intrinsic::ArrayWithValues
        | Intrinsic::ArrayLen
        | Intrinsic::ArrayGet
        | Intrinsic::ArraySet
        | Intrinsic::Assert
        | Intrinsic::StrLen
        | Intrinsic::StrGet
        | Intrinsic::BoolEq
        | Intrinsic::BoolNot
        | Intrinsic::UInt8Eq
        | Intrinsic::CharEq
        | Intrinsic::EnumEq
        | Intrinsic::EnumNe
        | Intrinsic::Int32Eq
        | Intrinsic::Int32CheckedAdd
        | Intrinsic::Int32CheckedSub
        | Intrinsic::Int32CheckedMul
        | Intrinsic::Int32CheckedDiv
        | Intrinsic::Int32CheckedMod
        | Intrinsic::Int32Or
        | Intrinsic::Int32And
        | Intrinsic::Int32Xor
        | Intrinsic::Int32Shl
        | Intrinsic::Int32Sar
        | Intrinsic::Int32Shr
        | Intrinsic::Int32Not
        | Intrinsic::Int32CheckedNeg
        | Intrinsic::Int64Eq
        | Intrinsic::Int64CheckedAdd
        | Intrinsic::Int64CheckedSub
        | Intrinsic::Int64CheckedMul
        | Intrinsic::Int64CheckedDiv
        | Intrinsic::Int64CheckedMod
        | Intrinsic::Int64Or
        | Intrinsic::Int64And
        | Intrinsic::Int64Xor
        | Intrinsic::Int64Shl
        | Intrinsic::Int64Sar
        | Intrinsic::Int64Shr
        | Intrinsic::Int64Not
        | Intrinsic::Int64CheckedNeg
        | Intrinsic::Float32Eq
        | Intrinsic::Float32Add
        | Intrinsic::Float32Sub
        | Intrinsic::Float32Mul
        | Intrinsic::Float32Div
        | Intrinsic::Float32Neg
        | Intrinsic::Float32IsNan
        | Intrinsic::Float64Eq
        | Intrinsic::Float64Add
        | Intrinsic::Float64Sub
        | Intrinsic::Float64Mul
        | Intrinsic::Float64Div
        | Intrinsic::Float64Neg
        | Intrinsic::Float64IsNan => true,
        _ => false,
    }
}
