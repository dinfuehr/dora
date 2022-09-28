use parking_lot::RwLock;

use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::bytecode::{BytecodeFunction, BytecodeType};
use crate::gc::Address;
use crate::language::sem_analysis::{
    module_path, AnalysisData, ExtensionDefinitionId, ImplDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, SemAnalysis, SourceFileId, TraitDefinitionId, TypeParamDefinition,
    Visibility,
};
use crate::language::ty::SourceType;
use crate::utils::GrowableVec;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct FctDefinitionId(pub usize);

impl FctDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for FctDefinitionId {
    fn from(id: usize) -> FctDefinitionId {
        FctDefinitionId(id)
    }
}

impl GrowableVec<RwLock<FctDefinition>> {
    pub fn idx(&self, index: FctDefinitionId) -> Arc<RwLock<FctDefinition>> {
        self.idx_usize(index.0)
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
    pub param_names: Vec<Name>,
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
    pub native_pointer: Option<Address>,
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
            param_names: ast.params.iter().map(|param| param.name).collect(),
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
            native_pointer: None,
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
    Float32Srt,

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
    Float64Srt,

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
    pub fn emit_as_function_in_bytecode(&self) -> bool {
        match self {
            Intrinsic::UnsafeKillRefs
            | Intrinsic::Unreachable
            | Intrinsic::Int32AddUnchecked
            | Intrinsic::Int64AddUnchecked
            | Intrinsic::Int32SubUnchecked
            | Intrinsic::Int64SubUnchecked
            | Intrinsic::Int32MulUnchecked
            | Intrinsic::Int64MulUnchecked
            | Intrinsic::Int64CountZeroBits
            | Intrinsic::Int64CountZeroBitsLeading
            | Intrinsic::Int64CountZeroBitsTrailing
            | Intrinsic::Int64CountOneBits
            | Intrinsic::Int64CountOneBitsLeading
            | Intrinsic::Int64CountOneBitsTrailing
            | Intrinsic::Int32CountZeroBits
            | Intrinsic::Int32CountZeroBitsLeading
            | Intrinsic::Int32CountZeroBitsTrailing
            | Intrinsic::Int32CountOneBits
            | Intrinsic::Int32CountOneBitsLeading
            | Intrinsic::Int32CountOneBitsTrailing
            | Intrinsic::Float32Abs
            | Intrinsic::Float64Abs
            | Intrinsic::Float32Srt
            | Intrinsic::Float64Srt
            | Intrinsic::Float32RoundToZero
            | Intrinsic::Float32RoundUp
            | Intrinsic::Float32RoundDown
            | Intrinsic::Float32RoundHalfEven
            | Intrinsic::Float64RoundToZero
            | Intrinsic::Float64RoundUp
            | Intrinsic::Float64RoundDown
            | Intrinsic::Float64RoundHalfEven
            | Intrinsic::Float32Sqrt
            | Intrinsic::Float64Sqrt
            | Intrinsic::PromoteFloat32ToFloat64
            | Intrinsic::DemoteFloat64ToFloat32
            | Intrinsic::BoolToInt32
            | Intrinsic::Float32ToInt32
            | Intrinsic::Float32ToInt64
            | Intrinsic::Float64ToInt32
            | Intrinsic::Float64ToInt64
            | Intrinsic::Int32ToFloat32
            | Intrinsic::Int32ToFloat64
            | Intrinsic::Int64ToFloat32
            | Intrinsic::Int64ToFloat64
            | Intrinsic::ReinterpretFloat32AsInt32
            | Intrinsic::ReinterpretInt32AsFloat32
            | Intrinsic::ReinterpretFloat64AsInt64
            | Intrinsic::ReinterpretInt64AsFloat64
            | Intrinsic::OptionGetOrPanic
            | Intrinsic::OptionIsNone
            | Intrinsic::OptionIsSome
            | Intrinsic::Debug
            | Intrinsic::AtomicInt32Get
            | Intrinsic::AtomicInt32Set
            | Intrinsic::AtomicInt32Exchange
            | Intrinsic::AtomicInt32CompareExchange
            | Intrinsic::AtomicInt32FetchAdd
            | Intrinsic::AtomicInt64Get
            | Intrinsic::AtomicInt64Set
            | Intrinsic::AtomicInt64Exchange
            | Intrinsic::AtomicInt64CompareExchange
            | Intrinsic::AtomicInt64FetchAdd
            | Intrinsic::ByteToChar
            | Intrinsic::ByteToInt32
            | Intrinsic::ByteToInt64
            | Intrinsic::Int32ToInt64
            | Intrinsic::CharToInt64
            | Intrinsic::Int64ToByte
            | Intrinsic::Int64ToChar
            | Intrinsic::Int64ToInt32
            | Intrinsic::CharToInt32
            | Intrinsic::Int32ToByte
            | Intrinsic::Int32ToChar
            | Intrinsic::Int32RotateLeft
            | Intrinsic::Int32RotateRight
            | Intrinsic::Int64RotateLeft
            | Intrinsic::Int64RotateRight
            | Intrinsic::ThreadCurrent => true,
            _ => false,
        }
    }

    pub fn result_type(self) -> BytecodeType {
        match self {
            Intrinsic::Int32Add
            | Intrinsic::Int32AddUnchecked
            | Intrinsic::Int32Sub
            | Intrinsic::Int32SubUnchecked
            | Intrinsic::Int32Mul
            | Intrinsic::Int32MulUnchecked
            | Intrinsic::Int32Div
            | Intrinsic::Int32Mod
            | Intrinsic::Int32Or
            | Intrinsic::Int32And
            | Intrinsic::Int32Xor
            | Intrinsic::Int32Shl
            | Intrinsic::Int32Shr
            | Intrinsic::Int32Sar
            | Intrinsic::Int32RotateLeft
            | Intrinsic::Int32RotateRight
            | Intrinsic::Int32Not
            | Intrinsic::Int32Plus
            | Intrinsic::Int32Neg
            | Intrinsic::ReinterpretFloat32AsInt32
            | Intrinsic::CharToInt32
            | Intrinsic::Int64ToInt32
            | Intrinsic::ByteToInt32
            | Intrinsic::Float32ToInt32
            | Intrinsic::Float64ToInt32
            | Intrinsic::BoolToInt32
            | Intrinsic::ByteCmp
            | Intrinsic::CharCmp
            | Intrinsic::Int32Cmp
            | Intrinsic::Int64Cmp
            | Intrinsic::Float32Cmp
            | Intrinsic::Float64Cmp
            | Intrinsic::Float32Srt
            | Intrinsic::Float64Srt
            | Intrinsic::Int32CountZeroBits
            | Intrinsic::Int32CountZeroBitsLeading
            | Intrinsic::Int32CountZeroBitsTrailing
            | Intrinsic::Int32CountOneBits
            | Intrinsic::Int32CountOneBitsLeading
            | Intrinsic::Int32CountOneBitsTrailing
            | Intrinsic::Int64CountZeroBits
            | Intrinsic::Int64CountZeroBitsLeading
            | Intrinsic::Int64CountZeroBitsTrailing
            | Intrinsic::Int64CountOneBits
            | Intrinsic::Int64CountOneBitsLeading
            | Intrinsic::Int64CountOneBitsTrailing => BytecodeType::Int32,
            Intrinsic::Int32ToInt32 => BytecodeType::Int32,
            Intrinsic::Int64Add
            | Intrinsic::Int64AddUnchecked
            | Intrinsic::Int64Sub
            | Intrinsic::Int64SubUnchecked
            | Intrinsic::Int64Mul
            | Intrinsic::Int64MulUnchecked
            | Intrinsic::Int64Div
            | Intrinsic::Int64Mod
            | Intrinsic::Int64Or
            | Intrinsic::Int64And
            | Intrinsic::Int64Xor
            | Intrinsic::Int64Shl
            | Intrinsic::Int64Shr
            | Intrinsic::Int64Sar
            | Intrinsic::Int64RotateLeft
            | Intrinsic::Int64RotateRight
            | Intrinsic::Int64Not
            | Intrinsic::Int64Plus
            | Intrinsic::Int64Neg
            | Intrinsic::CharToInt64
            | Intrinsic::ByteToInt64
            | Intrinsic::Int32ToInt64
            | Intrinsic::Float32ToInt64
            | Intrinsic::Float64ToInt64
            | Intrinsic::ArrayLen
            | Intrinsic::StrLen
            | Intrinsic::ReinterpretFloat64AsInt64 => BytecodeType::Int64,
            Intrinsic::Float32Add
            | Intrinsic::Float32Sub
            | Intrinsic::Float32Div
            | Intrinsic::Float32Mul
            | Intrinsic::Float32Neg
            | Intrinsic::Float32Plus
            | Intrinsic::ReinterpretInt32AsFloat32
            | Intrinsic::Int32ToFloat32
            | Intrinsic::Int64ToFloat32
            | Intrinsic::Float32RoundToZero
            | Intrinsic::Float32RoundUp
            | Intrinsic::Float32RoundDown
            | Intrinsic::Float32RoundHalfEven
            | Intrinsic::Float32Sqrt
            | Intrinsic::DemoteFloat64ToFloat32 => BytecodeType::Float32,
            Intrinsic::Float64Add
            | Intrinsic::Float64Sub
            | Intrinsic::Float64Div
            | Intrinsic::Float64Mul
            | Intrinsic::Float64Neg
            | Intrinsic::Float64Plus
            | Intrinsic::Int32ToFloat64
            | Intrinsic::Int64ToFloat64
            | Intrinsic::ReinterpretInt64AsFloat64
            | Intrinsic::Float64RoundToZero
            | Intrinsic::Float64RoundUp
            | Intrinsic::Float64RoundDown
            | Intrinsic::Float64RoundHalfEven
            | Intrinsic::Float64Sqrt
            | Intrinsic::PromoteFloat32ToFloat64 => BytecodeType::Float64,
            Intrinsic::BoolEq
            | Intrinsic::ByteEq
            | Intrinsic::CharEq
            | Intrinsic::EnumEq
            | Intrinsic::EnumNe
            | Intrinsic::Int32Eq
            | Intrinsic::Int64Eq
            | Intrinsic::Float32Eq
            | Intrinsic::Float64Eq
            | Intrinsic::BoolNot
            | Intrinsic::Float64IsNan
            | Intrinsic::Float32IsNan => BytecodeType::Bool,
            Intrinsic::Int32ToByte | Intrinsic::Int64ToByte => BytecodeType::UInt8,
            Intrinsic::Int32ToChar | Intrinsic::Int64ToChar | Intrinsic::ByteToChar => {
                BytecodeType::Char
            }
            _ => panic!("no return type for {:?}", self),
        }
    }
}
