use std::cell::{Cell, OnceCell};
use std::sync::Arc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;
use id_arena::Id;

use crate::generator::bty_from_ty;
use crate::sema::{
    module_path, AnalysisData, ExtensionDefinitionId, ImplDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, Sema, SourceFileId, TraitDefinitionId, TypeParamDefinition, Visibility,
};
use crate::ty::SourceType;
use dora_bytecode::{BytecodeFunction, BytecodeType, BytecodeTypeArray, Intrinsic, NativeFunction};

pub type FctDefinitionId = Id<FctDefinition>;

pub struct FctDefinition {
    pub id: Option<FctDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Function>,
    pub declaration_span: Span,
    pub span: Span,
    pub name: Name,
    pub parent: FctParent,
    pub is_optimize_immediately: bool,
    pub is_static: bool,
    pub visibility: Visibility,
    pub is_test: bool,
    pub is_internal: bool,
    pub param_types: OnceCell<Vec<SourceType>>,
    pub return_type: OnceCell<SourceType>,
    pub is_variadic: Cell<bool>,

    pub vtable_index: OnceCell<u32>,
    pub initialized: Cell<bool>,
    pub analysis: OnceCell<AnalysisData>,

    pub type_params: OnceCell<TypeParamDefinition>,
    pub container_type_params: OnceCell<usize>,
    pub bytecode: OnceCell<BytecodeFunction>,
    pub intrinsic: OnceCell<Intrinsic>,
    pub native_function: OnceCell<NativeFunction>,
}

impl FctDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: &Arc<ast::Function>,
        modifiers: ParsedModifierList,
        name: Name,
        parent: FctParent,
    ) -> FctDefinition {
        FctDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            declaration_span: ast.declaration_span,
            span: ast.span,
            ast: ast.clone(),
            name,
            param_types: OnceCell::new(),
            return_type: OnceCell::new(),
            parent,
            is_optimize_immediately: modifiers.is_optimize_immediately,
            visibility: modifiers.visibility(),
            is_static: modifiers.is_static,
            is_test: modifiers.is_test,
            is_internal: modifiers.is_internal,
            vtable_index: OnceCell::new(),
            initialized: Cell::new(false),
            is_variadic: Cell::new(false),
            analysis: OnceCell::new(),
            type_params: OnceCell::new(),
            container_type_params: OnceCell::new(),
            bytecode: OnceCell::new(),
            intrinsic: OnceCell::new(),
            native_function: OnceCell::new(),
        }
    }

    pub fn id(&self) -> FctDefinitionId {
        self.id.expect("id missing")
    }

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.get().expect("uninitialized type params")
    }

    pub fn container_type_params(&self) -> usize {
        self.container_type_params
            .get()
            .cloned()
            .expect("missing type params")
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
                path_for_type(sa, impl_.extended_ty())
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

    pub fn has_body(&self) -> bool {
        self.ast.block.is_some()
    }

    pub fn is_lambda(&self) -> bool {
        self.ast.kind.is_lambda()
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn analysis(&self) -> &AnalysisData {
        self.analysis.get().expect("uninitialized")
    }

    pub fn has_hidden_self_argument(&self) -> bool {
        match self.parent {
            FctParent::Trait(_) | FctParent::Impl(_) | FctParent::Extension(_) => !self.is_static,
            FctParent::Function => true,
            FctParent::None => false,
        }
    }

    pub fn params_with_self(&self) -> &[SourceType] {
        self.param_types.get().expect("missing params")
    }

    pub fn params_without_self(&self) -> &[SourceType] {
        if self.has_hidden_self_argument() {
            &self.params_with_self()[1..]
        } else {
            self.params_with_self()
        }
    }

    pub fn params_with_self_bty(&self) -> BytecodeTypeArray {
        let params = self
            .params_with_self()
            .iter()
            .map(|ty| bty_from_ty(ty.clone()))
            .collect();
        BytecodeTypeArray::new(params)
    }

    pub fn return_type(&self) -> SourceType {
        self.return_type
            .get()
            .cloned()
            .expect("missing return type")
    }

    pub fn return_type_bty(&self) -> BytecodeType {
        bty_from_ty(self.return_type())
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
        | Intrinsic::BoolToInt64
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
