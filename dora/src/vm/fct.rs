use crate::compiler::fct::JitFctId;
use crate::ty::TypeList;
use parking_lot::RwLock;
use std::collections::HashMap;

use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

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
    pub throws: bool,

    pub type_params: Vec<TypeParam>,
    pub kind: FctKind,

    pub specializations_fct_def: RwLock<HashMap<(TypeList, TypeList), FctDefId>>,
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

        if let FctParent::Class(class_id) = self.parent {
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
        self.ast.pos
    }

    pub fn src(&self) -> &RwLock<FctSrc> {
        match self.kind {
            FctKind::Source(ref src) => src,
            _ => panic!("source expected"),
        }
    }

    pub fn has_self(&self) -> bool {
        match self.parent {
            FctParent::Class(_) | FctParent::Trait(_) | FctParent::Impl(_) => !self.is_static,

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
    IntAsFloat,

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
    LongAsDouble,

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
    FloatToDouble,
    FloatAsInt,

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

    FloatArrayLen,
    FloatArrayGet,
    FloatArraySet,

    DoubleToInt,
    DoubleToLong,
    DoubleToFloat,
    DoubleAsLong,

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

    DoubleArrayLen,
    DoubleArrayGet,
    DoubleArraySet,
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
    pub jit_fct_id: Option<JitFctId>,
}
