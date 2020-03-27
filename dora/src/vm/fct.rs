use crate::ty::TypeList;
use parking_lot::RwLock;
use std::collections::HashMap;

use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::bytecode::BytecodeType;
use crate::gc::Address;
use crate::ty::BuiltinType;
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

#[derive(Debug)]
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

    pub vtable_index: Option<u32>,
    pub impl_for: Option<FctId>,
    pub initialized: bool,

    pub type_params: Vec<TypeParam>,
    pub kind: FctKind,

    pub specializations: RwLock<HashMap<(TypeList, TypeList), FctDefId>>,
}

impl<'ast> Fct<'ast> {
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

    pub fn full_name(&self, vm: &VM) -> String {
        let mut repr = String::new();

        match self.parent {
            FctParent::Class(class_id) => {
                let cls = vm.classes.idx(class_id);
                let cls = cls.read();
                let name = cls.name;
                repr.push_str(&vm.interner.str(name));
                if self.is_static {
                    repr.push_str("::");
                } else {
                    repr.push_str(".");
                }
            }

            FctParent::Trait(trait_id) => {
                let xtrait = vm.traits[trait_id].read();
                repr.push_str(&vm.interner.str(xtrait.name));
                if self.is_static {
                    repr.push_str("::");
                } else {
                    repr.push_str(".");
                }
            }

            _ => {}
        }

        repr.push_str(&vm.interner.str(self.name));

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

            let name = ty.name(vm);
            repr.push_str(&name);
        }

        repr.push_str(")");

        if self.return_type != BuiltinType::Unit {
            repr.push_str(" -> ");

            let name = self.return_type.name(vm);
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
    GenericArrayCtorEmpty,
    GenericArrayCtorElem,
    GenericArrayLen,
    GenericArrayGet,
    GenericArraySet,

    DefaultValue,

    Assert,
    Debug,
    Shl,

    StrLen,
    StrGet,
    StrSet,

    BoolEq,
    BoolNot,
    BoolToInt,
    BoolToLong,

    ByteEq,
    ByteCmp,
    ByteNot,
    ByteToInt,
    ByteToLong,

    CharEq,
    CharCmp,
    CharToInt,
    CharToLong,

    IntToByte,
    IntToChar,
    IntToLong,
    IntToFloat,
    IntToDouble,
    ReinterpretIntAsFloat,

    EnumEq,
    EnumNe,

    IntEq,
    IntCmp,

    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntMod,

    IntOr,
    IntAnd,
    IntXor,

    IntShl,
    IntSar,
    IntShr,

    IntRotateLeft,
    IntRotateRight,

    IntNot,
    IntNeg,
    IntPlus,

    IntCountZeroBits,
    IntCountOneBits,
    IntCountZeroBitsLeading,
    IntCountOneBitsLeading,
    IntCountZeroBitsTrailing,
    IntCountOneBitsTrailing,

    LongToInt,
    LongToChar,
    LongToByte,
    LongToFloat,
    LongToDouble,
    ReinterpretLongAsDouble,

    LongEq,
    LongCmp,

    LongAdd,
    LongSub,
    LongMul,
    LongDiv,
    LongMod,

    LongOr,
    LongAnd,
    LongXor,

    LongShl,
    LongSar,
    LongShr,

    LongRotateLeft,
    LongRotateRight,

    LongNot,
    LongNeg,
    LongPlus,

    LongCountZeroBits,
    LongCountOneBits,
    LongCountZeroBitsLeading,
    LongCountOneBitsLeading,
    LongCountZeroBitsTrailing,
    LongCountOneBitsTrailing,

    FloatToInt,
    FloatToLong,
    PromoteFloatToDouble,
    ReinterpretFloatAsInt,

    FloatEq,
    FloatCmp,

    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,

    FloatPlus,
    FloatNeg,
    FloatIsNan,
    FloatSqrt,

    DoubleToInt,
    DoubleToLong,
    DemoteDoubleToFloat,
    ReinterpretDoubleAsLong,

    DoubleEq,
    DoubleCmp,

    DoubleAdd,
    DoubleSub,
    DoubleMul,
    DoubleDiv,

    DoublePlus,
    DoubleNeg,
    DoubleIsNan,
    DoubleSqrt,
}

impl Intrinsic {
    pub fn result_type(self) -> BytecodeType {
        match self {
            Intrinsic::IntAdd
            | Intrinsic::IntSub
            | Intrinsic::IntMul
            | Intrinsic::IntDiv
            | Intrinsic::IntMod
            | Intrinsic::IntOr
            | Intrinsic::IntAnd
            | Intrinsic::IntXor
            | Intrinsic::IntShl
            | Intrinsic::IntShr
            | Intrinsic::IntSar
            | Intrinsic::IntRotateLeft
            | Intrinsic::IntRotateRight
            | Intrinsic::IntNot
            | Intrinsic::IntPlus
            | Intrinsic::IntNeg
            | Intrinsic::ReinterpretFloatAsInt
            | Intrinsic::GenericArrayLen
            | Intrinsic::StrLen
            | Intrinsic::CharToInt
            | Intrinsic::LongToInt
            | Intrinsic::ByteToInt
            | Intrinsic::FloatToInt
            | Intrinsic::DoubleToInt
            | Intrinsic::BoolToInt
            | Intrinsic::ByteCmp
            | Intrinsic::CharCmp
            | Intrinsic::IntCmp
            | Intrinsic::LongCmp
            | Intrinsic::FloatCmp
            | Intrinsic::DoubleCmp
            | Intrinsic::IntCountZeroBits
            | Intrinsic::IntCountZeroBitsLeading
            | Intrinsic::IntCountZeroBitsTrailing
            | Intrinsic::IntCountOneBits
            | Intrinsic::IntCountOneBitsLeading
            | Intrinsic::IntCountOneBitsTrailing
            | Intrinsic::LongCountZeroBits
            | Intrinsic::LongCountZeroBitsLeading
            | Intrinsic::LongCountZeroBitsTrailing
            | Intrinsic::LongCountOneBits
            | Intrinsic::LongCountOneBitsLeading
            | Intrinsic::LongCountOneBitsTrailing => BytecodeType::Int,
            Intrinsic::LongAdd
            | Intrinsic::LongSub
            | Intrinsic::LongMul
            | Intrinsic::LongDiv
            | Intrinsic::LongMod
            | Intrinsic::LongOr
            | Intrinsic::LongAnd
            | Intrinsic::LongXor
            | Intrinsic::LongShl
            | Intrinsic::LongShr
            | Intrinsic::LongSar
            | Intrinsic::LongRotateLeft
            | Intrinsic::LongRotateRight
            | Intrinsic::LongNot
            | Intrinsic::LongPlus
            | Intrinsic::LongNeg
            | Intrinsic::CharToLong
            | Intrinsic::ByteToLong
            | Intrinsic::IntToLong
            | Intrinsic::FloatToLong
            | Intrinsic::DoubleToLong
            | Intrinsic::ReinterpretDoubleAsLong => BytecodeType::Long,
            Intrinsic::FloatAdd
            | Intrinsic::FloatSub
            | Intrinsic::FloatDiv
            | Intrinsic::FloatMul
            | Intrinsic::FloatNeg
            | Intrinsic::FloatPlus
            | Intrinsic::ReinterpretIntAsFloat
            | Intrinsic::IntToFloat
            | Intrinsic::LongToFloat
            | Intrinsic::FloatSqrt => BytecodeType::Float,
            Intrinsic::DoubleAdd
            | Intrinsic::DoubleSub
            | Intrinsic::DoubleDiv
            | Intrinsic::DoubleMul
            | Intrinsic::DoubleNeg
            | Intrinsic::DoublePlus
            | Intrinsic::IntToDouble
            | Intrinsic::LongToDouble
            | Intrinsic::ReinterpretLongAsDouble
            | Intrinsic::DoubleSqrt => BytecodeType::Double,
            Intrinsic::BoolEq
            | Intrinsic::ByteEq
            | Intrinsic::CharEq
            | Intrinsic::EnumEq
            | Intrinsic::EnumNe
            | Intrinsic::IntEq
            | Intrinsic::LongEq
            | Intrinsic::FloatEq
            | Intrinsic::DoubleEq
            | Intrinsic::BoolNot
            | Intrinsic::DoubleIsNan
            | Intrinsic::FloatIsNan => BytecodeType::Bool,
            Intrinsic::IntToByte | Intrinsic::LongToByte => BytecodeType::Byte,
            Intrinsic::IntToChar | Intrinsic::LongToChar => BytecodeType::Char,
            _ => panic!("no return type for {:?}", self),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct FctDefId(pub usize);

impl FctDefId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for FctDefId {
    fn from(data: usize) -> FctDefId {
        FctDefId(data)
    }
}

impl<'ast> GrowableVec<RwLock<FctDef>> {
    pub fn idx(&self, index: FctDefId) -> Arc<RwLock<FctDef>> {
        self.idx_usize(index.0)
    }
}

#[derive(Debug)]
pub struct FctDef {
    pub id: FctDefId,
    pub fct_id: FctId,
    pub cls_type_params: TypeList,
    pub fct_type_params: TypeList,
}

impl FctDef {
    pub fn fct(vm: &VM, fct: &Fct) -> FctDefId {
        FctDef::with(vm, fct, TypeList::empty(), TypeList::empty())
    }

    pub fn fct_types(
        vm: &VM,
        fct: &Fct,
        cls_type_params: TypeList,
        fct_type_params: TypeList,
    ) -> FctDefId {
        FctDef::with(vm, fct, cls_type_params, fct_type_params)
    }

    pub fn fct_id(vm: &VM, fct_id: FctId) -> FctDefId {
        let fct = vm.fcts.idx(fct_id);
        let fct = fct.read();

        FctDef::fct(vm, &*fct)
    }

    pub fn fct_id_types(
        vm: &VM,
        fct_id: FctId,
        cls_type_params: TypeList,
        fct_type_params: TypeList,
    ) -> FctDefId {
        let fct = vm.fcts.idx(fct_id);
        let fct = fct.read();

        FctDef::fct_types(vm, &*fct, cls_type_params, fct_type_params)
    }

    pub fn with(
        vm: &VM,
        fct: &Fct,
        cls_type_params: TypeList,
        fct_type_params: TypeList,
    ) -> FctDefId {
        debug_assert!(cls_type_params.iter().all(|ty| ty.is_concrete_type(vm)));
        debug_assert!(fct_type_params.iter().all(|ty| ty.is_concrete_type(vm)));

        let mut specializations = fct.specializations.write();
        let type_params = (cls_type_params.clone(), fct_type_params.clone());

        if let Some(&id) = specializations.get(&type_params) {
            return id;
        }

        let fct_def_id = vm.add_fct_def(FctDef {
            id: FctDefId(0),
            fct_id: fct.id,
            cls_type_params: cls_type_params.clone(),
            fct_type_params: fct_type_params.clone(),
        });

        let old = specializations.insert(
            (cls_type_params.clone(), fct_type_params.clone()),
            fct_def_id,
        );
        assert!(old.is_none());

        fct_def_id
    }
}
