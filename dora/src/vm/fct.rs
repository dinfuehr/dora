use parking_lot::RwLock;

use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::bytecode::{BytecodeFunction, BytecodeType};
use crate::gc::Address;
use crate::ty::{BuiltinType, TypeListId};
use crate::utils::GrowableVec;
use crate::vm::module::ModuleId;
use crate::vm::{ClassId, ExtensionId, FctSrc, FileId, ImplId, TraitId, TypeParam, VM};

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

impl<'ast> GrowableVec<RwLock<Fct<'ast>>> {
    pub fn idx(&self, index: FctId) -> Arc<RwLock<Fct<'ast>>> {
        self.idx_usize(index.0)
    }
}

pub struct Fct<'ast> {
    pub id: FctId,
    pub ast: &'ast ast::Function,
    pub pos: Position,
    pub name: Name,
    pub parent: FctParent,
    pub has_open: bool,
    pub has_override: bool,
    pub has_final: bool,
    pub has_optimize_immediately: bool,
    pub is_static: bool,
    pub is_pub: bool,
    pub is_abstract: bool,
    pub is_test: bool,
    pub use_cannon: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub overrides: Option<FctId>,
    pub param_types: Vec<BuiltinType>,
    pub return_type: BuiltinType,
    pub is_constructor: bool,
    pub file: FileId,
    pub variadic_arguments: bool,

    pub vtable_index: Option<u32>,
    pub impl_for: Option<FctId>,
    pub initialized: bool,

    pub type_params: Vec<TypeParam>,
    pub kind: FctKind,
    pub bytecode: Option<BytecodeFunction>,
}

impl<'ast> Fct<'ast> {
    pub fn type_param(&self, id: TypeListId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }

    pub fn type_param_id<F: FnOnce(&TypeParam, TypeListId) -> R, R>(
        &self,
        vm: &VM,
        id: TypeListId,
        callback: F,
    ) -> R {
        self.type_param_ty(vm, BuiltinType::TypeParam(id), callback)
    }

    pub fn type_param_ty<F: FnOnce(&TypeParam, TypeListId) -> R, R>(
        &self,
        vm: &VM,
        ty: BuiltinType,
        callback: F,
    ) -> R {
        match ty {
            BuiltinType::TypeParam(id) => {
                let fct_id = if let Some(cls_id) = self.parent_cls_id() {
                    let cls = vm.classes.idx(cls_id);
                    let cls = cls.read();
                    let len = cls.type_params.len();

                    if id.to_usize() < len {
                        return callback(cls.type_param(id), id);
                    }

                    (id.to_usize() - len).into()
                } else {
                    id
                };

                callback(self.type_param(fct_id), id)
            }

            _ => unreachable!(),
        }
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

    pub fn full_name(&self, vm: &VM<'ast>) -> String {
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

            _ => {}
        }

        if !self.is_constructor {
            repr.push_str(&vm.interner.str(self.name));
        }

        if self.type_params.len() > 0 {
            repr.push('[');

            repr.push_str(
                &self
                    .type_params
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

        if self.return_type != BuiltinType::Unit {
            repr.push_str(": ");

            let name = self.return_type.name_fct(vm, self);
            repr.push_str(&name);
        }

        repr
    }

    pub fn is_src(&self) -> bool {
        match self.kind {
            FctKind::Source(_) => true,
            _ => false,
        }
    }

    pub fn pos(&self) -> Position {
        self.pos
    }

    pub fn src(&self) -> &RwLock<FctSrc> {
        match self.kind {
            FctKind::Source(ref src) => src,
            _ => panic!("source expected"),
        }
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

    pub fn params_with_self(&self) -> &[BuiltinType] {
        &self.param_types
    }

    pub fn params_without_self(&self) -> &[BuiltinType] {
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
}

#[derive(Debug)]
pub enum FctKind {
    Source(RwLock<FctSrc>),
    Definition,
    Native(Address),
    Builtin(Intrinsic),
}

impl FctKind {
    pub fn is_src(&self) -> bool {
        match *self {
            FctKind::Source(_) => true,
            _ => false,
        }
    }

    pub fn is_intrinsic(&self) -> bool {
        match *self {
            FctKind::Builtin(_) => true,
            _ => false,
        }
    }

    pub fn is_definition(&self) -> bool {
        match *self {
            FctKind::Definition => true,
            _ => false,
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

    DefaultValue,

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
    ByteNot,
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
    Int32Sub,
    Int32Mul,
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
    Int64Sub,
    Int64Mul,
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
}

impl Intrinsic {
    pub fn result_type(self) -> BytecodeType {
        match self {
            Intrinsic::Int32Add
            | Intrinsic::Int32Sub
            | Intrinsic::Int32Mul
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
            | Intrinsic::Int64Sub
            | Intrinsic::Int64Mul
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
