use parking_lot::RwLock;

use std::sync::Arc;

use crate::Id;
use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::bytecode::{BytecodeFunction, BytecodeType, BytecodeTypeArray};
use crate::language::generator::bty_from_ty;
use crate::language::sem_analysis::{
    module_path, AnalysisData, ExtensionDefinitionId, ImplDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, SemAnalysis, SourceFileId, TraitDefinitionId, TypeParamDefinition,
    Visibility,
};
use crate::language::ty::SourceType;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct FctDefinitionId(pub usize);

impl FctDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl Id for FctDefinition {
    type IdType = FctDefinitionId;

    fn id_to_usize(id: FctDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> FctDefinitionId {
        FctDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut FctDefinition, id: FctDefinitionId) {
        value.id = Some(id);
    }
}

impl From<usize> for FctDefinitionId {
    fn from(id: usize) -> FctDefinitionId {
        FctDefinitionId(id)
    }
}

pub struct FctDefinition {
    pub id: Option<FctDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Function>,
    pub pos: Position,
    pub name: Name,
    pub parent: FctParent,
    pub is_optimize_immediately: bool,
    pub is_static: bool,
    pub visibility: Visibility,
    pub is_test: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub param_types: Vec<SourceType>,
    pub return_type: SourceType,
    pub is_constructor: bool,
    pub is_variadic: bool,

    pub vtable_index: Option<u32>,
    pub initialized: bool,
    pub analysis: Option<AnalysisData>,

    pub type_params: TypeParamDefinition,
    pub container_type_params: usize,
    pub bytecode: Option<BytecodeFunction>,
    pub intrinsic: Option<Intrinsic>,
    pub thunk_id: RwLock<Option<FctDefinitionId>>,
}

impl FctDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: &Arc<ast::Function>,
        parent: FctParent,
    ) -> FctDefinition {
        FctDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            pos: ast.pos,
            ast: ast.clone(),
            name: ast.name,
            param_types: Vec::new(),
            return_type: SourceType::Error,
            parent,
            is_optimize_immediately: ast.is_optimize_immediately,
            visibility: Visibility::from_ast(ast.visibility),
            is_static: ast.is_static,
            is_test: ast.is_test,
            internal: ast.internal,
            internal_resolved: false,
            is_constructor: ast.is_constructor,
            vtable_index: None,
            initialized: false,
            is_variadic: false,
            analysis: None,
            type_params: TypeParamDefinition::new(),
            container_type_params: 0,
            bytecode: None,
            intrinsic: None,
            thunk_id: RwLock::new(None),
        }
    }

    pub fn id(&self) -> FctDefinitionId {
        self.id.expect("id missing")
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

    pub fn trait_id(&self) -> TraitDefinitionId {
        match self.parent {
            FctParent::Trait(traitid) => traitid,
            _ => unreachable!(),
        }
    }

    pub fn display_name(&self, sa: &SemAnalysis) -> String {
        let mut repr = match self.parent {
            FctParent::Trait(trait_id) => {
                let trait_ = sa.traits[trait_id].read();
                trait_.name(sa)
            }

            FctParent::Extension(extension_id) => {
                let extension = &sa.extensions[extension_id];
                let extension = extension.read();
                path_for_type(sa, extension.ty.clone())
            }

            FctParent::Impl(impl_id) => {
                let impl_ = &sa.impls[impl_id];
                let impl_ = impl_.read();
                path_for_type(sa, impl_.extended_ty.clone())
            }

            FctParent::None => {
                return module_path(sa, self.module_id, self.name);
            }

            FctParent::Function(_) => "lamba".into(),
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

    pub fn pos(&self) -> Position {
        self.pos
    }

    pub fn analysis(&self) -> &AnalysisData {
        self.analysis.as_ref().unwrap()
    }

    pub fn has_self(&self) -> bool {
        match self.parent {
            FctParent::Trait(_) | FctParent::Impl(_) | FctParent::Extension(_) => !self.is_static,

            FctParent::Function(_) => true,

            _ => false,
        }
    }

    pub fn params_with_self(&self) -> &[SourceType] {
        &self.param_types
    }

    pub fn params_without_self(&self) -> &[SourceType] {
        if self.has_self() {
            &self.param_types[1..]
        } else {
            &self.param_types
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

    pub fn return_type_bty(&self) -> BytecodeType {
        bty_from_ty(self.return_type.clone())
    }
}

fn path_for_type(sa: &SemAnalysis, ty: SourceType) -> String {
    if let Some(enum_id) = ty.enum_id() {
        let enum_ = &sa.enums[enum_id];
        let enum_ = enum_.read();
        enum_.name(sa)
    } else if let Some(cls_id) = ty.cls_id() {
        let cls = sa.classes.idx(cls_id);
        let cls = cls.read();
        cls.name(sa)
    } else if let Some(struct_id) = ty.struct_id() {
        let struct_ = sa.structs.idx(struct_id);
        let struct_ = struct_.read();
        struct_.name(sa)
    } else if let Some(struct_id) = ty.primitive_struct_id(sa) {
        let struct_ = sa.structs.idx(struct_id);
        let struct_ = struct_.read();
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
    Function(FctDefinitionId),
    None,
}

impl FctParent {
    pub fn is_none(&self) -> bool {
        match self {
            &FctParent::None => true,
            _ => false,
        }
    }

    pub fn is_trait(&self) -> bool {
        match self {
            &FctParent::Trait(_) => true,
            _ => false,
        }
    }

    pub fn fct_id(&self) -> FctDefinitionId {
        match self {
            &FctParent::Function(id) => id,
            _ => unreachable!(),
        }
    }

    pub fn trait_id(&self) -> Option<TraitDefinitionId> {
        match self {
            &FctParent::Trait(id) => Some(id),
            _ => None,
        }
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
    StrSet,

    BoolEq,
    BoolNot,
    BoolToInt32,
    BoolToInt64,

    ByteEq,
    ByteCmp,
    ByteToChar,
    ByteToInt32,
    ByteToInt64,

    CharEq,
    CharCmp,
    CharToInt32,
    CharToInt64,

    Int32ToInt32,

    Int32ToByte,
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
    Int32Plus,

    Int32CountZeroBits,
    Int32CountOneBits,
    Int32CountZeroBitsLeading,
    Int32CountOneBitsLeading,
    Int32CountZeroBitsTrailing,
    Int32CountOneBitsTrailing,

    Int64ToInt32,
    Int64ToChar,
    Int64ToByte,
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
    Int64Plus,

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

    Float32Plus,
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

    Float64Plus,
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

impl Intrinsic {
    pub fn emit_as_bytecode_operation(&self) -> bool {
        match self {
            Intrinsic::ArrayNewOfSize
            | Intrinsic::ArrayWithValues
            | Intrinsic::ArrayLen
            | Intrinsic::ArrayGet
            | Intrinsic::ArraySet
            | Intrinsic::Assert
            | Intrinsic::StrLen
            | Intrinsic::StrGet
            | Intrinsic::StrSet
            | Intrinsic::BoolEq
            | Intrinsic::BoolNot
            | Intrinsic::BoolToInt64
            | Intrinsic::ByteEq
            | Intrinsic::ByteCmp
            | Intrinsic::CharEq
            | Intrinsic::CharCmp
            | Intrinsic::Int32ToInt32
            | Intrinsic::EnumEq
            | Intrinsic::EnumNe
            | Intrinsic::Int32Eq
            | Intrinsic::Int32Cmp
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
            | Intrinsic::Int32Plus
            | Intrinsic::Int64Eq
            | Intrinsic::Int64Cmp
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
            | Intrinsic::Int64Plus
            | Intrinsic::Float32Eq
            | Intrinsic::Float32Cmp
            | Intrinsic::Float32Add
            | Intrinsic::Float32Sub
            | Intrinsic::Float32Mul
            | Intrinsic::Float32Div
            | Intrinsic::Float32Plus
            | Intrinsic::Float32Neg
            | Intrinsic::Float32IsNan
            | Intrinsic::Float64Eq
            | Intrinsic::Float64Cmp
            | Intrinsic::Float64Add
            | Intrinsic::Float64Sub
            | Intrinsic::Float64Mul
            | Intrinsic::Float64Div
            | Intrinsic::Float64Plus
            | Intrinsic::Float64Neg
            | Intrinsic::Float64IsNan => true,
            _ => false,
        }
    }
}
