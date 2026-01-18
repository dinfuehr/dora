use std::cell::OnceCell;
use std::rc::Rc;

use crate::ParsedType;
use crate::element_collector::Annotations;
use crate::interner::Name;
use dora_parser::GreenId;
use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase, SyntaxNodePtr};
use id_arena::Id;

use crate::sema::{
    Body, Element, ElementId, ExprId, ExtensionDefinitionId, ImplDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, Sema, SourceFileId, TraitDefinitionId, TypeParamDefinition, TypeRefArena,
    TypeRefArenaBuilder, Visibility, module_path,
};
use crate::ty::SourceType;
use dora_bytecode::BytecodeFunction;

pub type FctDefinitionId = Id<FctDefinition>;

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
    pub is_optimize_immediately: bool,
    pub is_static: bool,
    pub visibility: Visibility,
    pub is_test: bool,
    pub is_internal: bool,
    pub is_force_inline: bool,
    pub is_never_inline: bool,
    pub is_trait_object_ignore: bool,
    pub params: Params,
    pub return_type: ParsedType,
    pub type_refs: OnceCell<TypeRefArena>,

    pub body: OnceCell<Body>,

    pub type_param_definition: Rc<TypeParamDefinition>,
    pub container_type_params: OnceCell<usize>,
    pub bytecode: OnceCell<BytecodeFunction>,
    pub intrinsic: OnceCell<Intrinsic>,
    pub trait_method_impl: OnceCell<FctDefinitionId>,
}

impl FctDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: ast::AstCallable,
        modifiers: Annotations,
        name: Name,
        type_params: Rc<TypeParamDefinition>,
        params: Params,
        return_type: ParsedType,
        parent: FctParent,
    ) -> FctDefinition {
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
            is_optimize_immediately: modifiers.is_optimize_immediately,
            visibility: modifiers.visibility(),
            is_static: modifiers.is_static,
            is_test: modifiers.is_test,
            is_internal: modifiers.is_internal,
            is_force_inline: modifiers.is_force_inline,
            is_never_inline: modifiers.is_never_inline,
            is_trait_object_ignore: modifiers.is_trait_object_ignore,
            body: OnceCell::new(),
            type_refs: OnceCell::new(),
            type_param_definition: type_params,
            container_type_params: OnceCell::new(),
            bytecode: OnceCell::new(),
            intrinsic: OnceCell::new(),
            trait_method_impl: OnceCell::new(),
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
        type_params: Rc<TypeParamDefinition>,
        params: Params,
        return_type: SourceType,
        parent: FctParent,
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
            is_optimize_immediately: modifiers.is_optimize_immediately,
            visibility: modifiers.visibility(),
            is_static: modifiers.is_static,
            is_test: modifiers.is_test,
            is_internal: modifiers.is_internal,
            is_force_inline: modifiers.is_force_inline,
            is_never_inline: modifiers.is_never_inline,
            is_trait_object_ignore: modifiers.is_trait_object_ignore,
            body: OnceCell::new(),
            type_refs: OnceCell::new(),
            type_param_definition: type_params,
            container_type_params: OnceCell::new(),
            bytecode: OnceCell::new(),
            intrinsic: OnceCell::new(),
            trait_method_impl: OnceCell::new(),
        }
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

    pub fn use_trait_default_impl(&self, sa: &Sema) -> bool {
        if self.parent.is_impl() {
            let trait_method_id = self
                .trait_method_impl
                .get()
                .cloned()
                .expect("missing trait method");

            let trait_method = sa.fct(trait_method_id);
            trait_method.has_body(sa)
        } else {
            false
        }
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
        self.parent.is_function()
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

    fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        &self.type_param_definition
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
    pub ast: Option<GreenId>,
    pub parsed_ty: ParsedType,
}

impl Param {
    pub fn new(
        sa: &mut Sema,
        type_ref_arena: &mut TypeRefArenaBuilder,
        file_id: SourceFileId,
        ast_id: GreenId,
        ast: &ast::AstParam,
    ) -> Param {
        Param {
            parsed_ty: ParsedType::new_ast_opt(sa, type_ref_arena, file_id, ast.data_type()),
            ast: Some(ast_id),
        }
    }

    pub fn new_ty(ty: SourceType) -> Param {
        Param {
            ast: None,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Intrinsic {
    ArrayNewOfSize,
    ArrayWithValues,
    ArrayLen,
    ArrayGet,
    ArraySet,

    Unreachable,
    UnsafeKillRefs,

    Assert,
    Debug,

    StrLen,
    StrGet,

    BoolEq,
    BoolNot,
    BoolToInt32,
    BoolToInt64,

    UInt8Eq,
    UInt8Cmp,
    UInt8ToChar,
    UInt8ToInt32,
    UInt8ToInt64,

    CharEq,
    CharCmp,
    CharToInt32,
    CharToInt64,

    Int32ToUInt8,
    Int32ToChar,
    Int32ToInt64,
    Int32ToFloat32,
    Int32ToFloat64,
    ReinterpretInt32AsFloat32,

    EnumEq,
    EnumNe,

    Int32Eq,
    Int32Cmp,

    Int32Add,
    Int32AddUnchecked,
    Int32Sub,
    Int32SubUnchecked,
    Int32Mul,
    Int32MulUnchecked,
    Int32Div,
    Int32Mod,

    Int32Or,
    Int32And,
    Int32Xor,

    Int32Shl,
    Int32Sar,
    Int32Shr,

    Int32RotateLeft,
    Int32RotateRight,

    Int32Not,
    Int32Neg,
    Int32NegUnchecked,

    Int32CountZeroBits,
    Int32CountOneBits,
    Int32CountZeroBitsLeading,
    Int32CountOneBitsLeading,
    Int32CountZeroBitsTrailing,
    Int32CountOneBitsTrailing,

    Int64ToInt32,
    Int64ToChar,
    Int64ToUInt8,
    Int64ToFloat32,
    Int64ToFloat64,
    ReinterpretInt64AsFloat64,

    Int64Eq,
    Int64Cmp,

    Int64Add,
    Int64AddUnchecked,
    Int64Sub,
    Int64SubUnchecked,
    Int64Mul,
    Int64MulUnchecked,
    Int64Div,
    Int64Mod,

    Int64Or,
    Int64And,
    Int64Xor,

    Int64Shl,
    Int64Sar,
    Int64Shr,

    Int64RotateLeft,
    Int64RotateRight,

    Int64Not,
    Int64Neg,
    Int64NegUnchecked,

    Int64CountZeroBits,
    Int64CountOneBits,
    Int64CountZeroBitsLeading,
    Int64CountOneBitsLeading,
    Int64CountZeroBitsTrailing,
    Int64CountOneBitsTrailing,

    Float32ToInt32,
    Float32ToInt64,
    PromoteFloat32ToFloat64,
    ReinterpretFloat32AsInt32,

    Float32Eq,
    Float32Cmp,

    Float32Add,
    Float32Sub,
    Float32Mul,
    Float32Div,

    Float32Neg,
    Float32Abs,
    Float32IsNan,

    Float32RoundToZero,
    Float32RoundUp,
    Float32RoundDown,
    Float32RoundHalfEven,

    Float32Sqrt,

    Float64ToInt32,
    Float64ToInt64,
    DemoteFloat64ToFloat32,
    ReinterpretFloat64AsInt64,

    Float64Eq,
    Float64Cmp,

    Float64Add,
    Float64Sub,
    Float64Mul,
    Float64Div,

    Float64Neg,
    Float64Abs,
    Float64IsNan,

    Float64RoundToZero,
    Float64RoundUp,
    Float64RoundDown,
    Float64RoundHalfEven,

    Float64Sqrt,

    OptionGetOrPanic,
    OptionIsNone,
    OptionIsSome,

    AtomicInt32Get,
    AtomicInt32Set,
    AtomicInt32Exchange,
    AtomicInt32CompareExchange,
    AtomicInt32FetchAdd,

    AtomicInt64Get,
    AtomicInt64Set,
    AtomicInt64Exchange,
    AtomicInt64CompareExchange,
    AtomicInt64FetchAdd,

    ThreadCurrent,
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
        | Intrinsic::Int32Add
        | Intrinsic::Int32Sub
        | Intrinsic::Int32Mul
        | Intrinsic::Int32Div
        | Intrinsic::Int32Mod
        | Intrinsic::Int32Or
        | Intrinsic::Int32And
        | Intrinsic::Int32Xor
        | Intrinsic::Int32Shl
        | Intrinsic::Int32Sar
        | Intrinsic::Int32Shr
        | Intrinsic::Int32Not
        | Intrinsic::Int32Neg
        | Intrinsic::Int64Eq
        | Intrinsic::Int64Add
        | Intrinsic::Int64Sub
        | Intrinsic::Int64Mul
        | Intrinsic::Int64Div
        | Intrinsic::Int64Mod
        | Intrinsic::Int64Or
        | Intrinsic::Int64And
        | Intrinsic::Int64Xor
        | Intrinsic::Int64Shl
        | Intrinsic::Int64Sar
        | Intrinsic::Int64Shr
        | Intrinsic::Int64Not
        | Intrinsic::Int64Neg
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
