use parking_lot::RwLock;

use std::collections::HashMap;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::bytecode::{BytecodeFunction, BytecodeType};
use crate::compiler::fct::JitFctId;
use crate::gc::Address;
use crate::ty::{SourceType, SourceTypeArray};
use crate::utils::GrowableVec;
use crate::vm::{
    accessible_from, namespace_path, AnalysisData, ClassId, ExtensionId, FileId, ImplId, ModuleId,
    NamespaceId, TraitId, TypeParam, TypeParamId, VM,
};

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct FctId(pub usize);

impl FctId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for FctId {
    fn from(id: usize) -> FctId {
        FctId(id)
    }
}

impl GrowableVec<RwLock<Fct>> {
    pub fn idx(&self, index: FctId) -> Arc<RwLock<Fct>> {
        self.idx_usize(index.0)
    }
}

pub struct Fct {
    pub id: FctId,
    pub ast: Arc<ast::Function>,
    pub pos: Position,
    pub name: Name,
    pub namespace_id: NamespaceId,
    pub parent: FctParent,
    pub has_open: bool,
    pub has_override: bool,
    pub has_final: bool,
    pub has_optimize_immediately: bool,
    pub is_static: bool,
    pub is_pub: bool,
    pub is_abstract: bool,
    pub is_test: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub overrides: Option<FctId>,
    pub param_types: Vec<SourceType>,
    pub return_type: SourceType,
    pub is_constructor: bool,
    pub file_id: FileId,
    pub variadic_arguments: bool,

    pub vtable_index: Option<u32>,
    pub initialized: bool,
    pub specializations: RwLock<HashMap<SourceTypeArray, JitFctId>>,
    pub analysis: Option<AnalysisData>,

    pub type_params: Vec<TypeParam>,
    pub container_type_params: usize,
    pub bytecode: Option<BytecodeFunction>,
    pub intrinsic: Option<Intrinsic>,
    pub native_pointer: Option<Address>,
    pub thunk_id: RwLock<Option<FctId>>,
}

impl Fct {
    pub fn new(
        file_id: FileId,
        namespace_id: NamespaceId,
        ast: &Arc<ast::Function>,
        parent: FctParent,
    ) -> Fct {
        Fct {
            id: FctId(0),
            file_id,
            pos: ast.pos,
            ast: ast.clone(),
            name: ast.name,
            namespace_id,
            param_types: Vec::new(),
            return_type: SourceType::Error,
            parent,
            has_override: ast.has_override,
            has_open: ast.has_open || ast.is_abstract,
            has_final: ast.has_final,
            has_optimize_immediately: ast.has_optimize_immediately,
            is_pub: ast.is_pub,
            is_static: ast.is_static,
            is_abstract: ast.is_abstract,
            is_test: ast.is_test,
            internal: ast.internal,
            internal_resolved: false,
            overrides: None,
            is_constructor: ast.is_constructor,
            vtable_index: None,
            initialized: false,
            variadic_arguments: false,
            specializations: RwLock::new(HashMap::new()),
            analysis: None,
            type_params: Vec::new(),
            container_type_params: 0,
            bytecode: None,
            intrinsic: None,
            native_pointer: None,
            thunk_id: RwLock::new(None),
        }
    }

    pub fn container_type_params(&self) -> &[TypeParam] {
        &self.type_params[0..self.container_type_params]
    }

    pub fn fct_type_params(&self) -> &[TypeParam] {
        &self.type_params[self.container_type_params..]
    }

    pub fn type_param(&self, id: TypeParamId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }

    pub fn is_virtual(&self) -> bool {
        (self.has_open || self.has_override) && !self.has_final
    }

    pub fn in_class(&self) -> bool {
        match self.parent {
            FctParent::Class(_) => true,
            _ => false,
        }
    }

    pub fn in_trait(&self) -> bool {
        match self.parent {
            FctParent::Trait(_) => true,
            _ => false,
        }
    }

    pub fn parent_cls_id(&self) -> Option<ClassId> {
        match self.parent {
            FctParent::Class(cls_id) => Some(cls_id),
            _ => None,
        }
    }

    pub fn cls_id(&self) -> ClassId {
        match self.parent {
            FctParent::Class(clsid) => clsid,
            _ => unreachable!(),
        }
    }

    pub fn trait_id(&self) -> TraitId {
        match self.parent {
            FctParent::Trait(traitid) => traitid,
            _ => unreachable!(),
        }
    }

    pub fn name(&self, vm: &VM) -> String {
        namespace_path(vm, self.namespace_id, self.name)
    }

    pub fn name_with_params(&self, vm: &VM) -> String {
        let mut repr = String::new();

        match self.parent {
            FctParent::Class(class_id) => {
                let cls = vm.classes.idx(class_id);
                let cls = cls.read();
                let name = cls.name;
                repr.push_str(&vm.interner.str(name));

                if cls.type_params.len() > 0 {
                    repr.push('[');

                    repr.push_str(
                        &cls.type_params
                            .iter()
                            .map(|n| vm.interner.str(n.name).to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                    );
                    repr.push(']');
                }

                if self.is_constructor {
                    // do nothing
                } else if self.is_static {
                    repr.push_str("::");
                } else {
                    repr.push_str("#");
                }
            }

            FctParent::Trait(trait_id) => {
                let xtrait = vm.traits[trait_id].read();
                repr.push_str(&vm.interner.str(xtrait.name));
                if self.is_static {
                    repr.push_str("::");
                } else {
                    repr.push_str("#");
                }
            }

            FctParent::Extension(extension_id) => {
                let extension = &vm.extensions[extension_id];
                let extension = extension.read();

                if let Some(enum_id) = extension.ty.enum_id() {
                    let xenum = &vm.enums[enum_id];
                    let xenum = xenum.read();
                    repr.push_str(&vm.interner.str(xenum.name));
                } else if let Some(cls_id) = extension.ty.cls_id() {
                    let cls = vm.classes.idx(cls_id);
                    let cls = cls.read();
                    let name = cls.name;
                    repr.push_str(&vm.interner.str(name));
                } else if let Some(struct_id) = extension.ty.struct_id() {
                    let xstruct = vm.structs.idx(struct_id);
                    let xstruct = xstruct.read();
                    let name = xstruct.name;
                    repr.push_str(&vm.interner.str(name));
                } else if let Some(struct_id) = extension.ty.primitive_struct_id(vm) {
                    let xstruct = vm.structs.idx(struct_id);
                    let xstruct = xstruct.read();
                    let name = xstruct.name;
                    repr.push_str(&vm.interner.str(name));
                } else {
                    unreachable!()
                }

                if self.is_static {
                    repr.push_str("::");
                } else {
                    repr.push_str("#");
                }
            }

            _ => {}
        }

        if !self.is_constructor {
            repr.push_str(&vm.interner.str(self.name));
        }

        if !self.fct_type_params().is_empty() {
            repr.push('[');

            repr.push_str(
                &self
                    .fct_type_params()
                    .iter()
                    .map(|n| vm.interner.str(n.name).to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            );
            repr.push(']');
        }

        repr.push_str("(");

        for (ind, ty) in self.params_without_self().iter().enumerate() {
            if ind > 0 {
                repr.push_str(", ");
            }

            let name = ty.name_fct(vm, self);
            repr.push_str(&name);
        }

        repr.push_str(")");

        if self.return_type != SourceType::Unit {
            repr.push_str(": ");

            let name = self.return_type.name_fct(vm, self);
            repr.push_str(&name);
        }

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
            FctParent::Class(_)
            | FctParent::Trait(_)
            | FctParent::Impl(_)
            | FctParent::Extension(_) => !self.is_static,

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FctParent {
    Class(ClassId),
    Trait(TraitId),
    Module(ModuleId),
    Impl(ImplId),
    Extension(ExtensionId),
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

    pub fn cls_id(&self) -> ClassId {
        match self {
            &FctParent::Class(id) => id,
            _ => unreachable!(),
        }
    }

    pub fn trait_id(&self) -> Option<TraitId> {
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
    Float32IsNan,
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
    Float64IsNan,
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
}

impl Intrinsic {
    pub fn emit_as_function_in_bytecode(&self) -> bool {
        match self {
            Intrinsic::UnsafeKillRefs
            | Intrinsic::Unreachable
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
            | Intrinsic::AtomicInt64FetchAdd => true,
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

pub fn fct_accessible_from(vm: &VM, fct_id: FctId, namespace_id: NamespaceId) -> bool {
    let fct = vm.fcts.idx(fct_id);
    let fct = fct.read();

    accessible_from(vm, fct.namespace_id, fct.is_pub, namespace_id)
}
