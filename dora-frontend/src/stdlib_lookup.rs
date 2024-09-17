use std::cell::OnceCell;
use std::rc::Rc;

use crate::sema::{
    ClassDefinition, ClassDefinitionId, EnumDefinitionId, ExtensionDefinitionId, FctDefinitionId,
    Field, FieldId, ImplDefinitionId, Intrinsic, ModuleDefinition, ModuleDefinitionId, Sema,
    StructDefinitionId, TraitDefinitionId, TypeParamDefinition, Visibility,
};
use crate::sym::{SymTable, SymbolKind};
use crate::ty::{SourceType, SourceTypeArray};

use crate::interner::Name;

pub fn lookup_known_fundamental_types(sa: &mut Sema) {
    let stdlib_id = sa.stdlib_module_id();

    sa.known.structs.bool = Some(internal_struct(
        sa,
        stdlib_id,
        "primitives::Bool",
        Some(SourceType::Bool),
    ));

    sa.known.structs.uint8 = Some(internal_struct(
        sa,
        stdlib_id,
        "primitives::UInt8",
        Some(SourceType::UInt8),
    ));
    sa.known.structs.char = Some(internal_struct(
        sa,
        stdlib_id,
        "primitives::Char",
        Some(SourceType::Char),
    ));
    sa.known.structs.int32 = Some(internal_struct(
        sa,
        stdlib_id,
        "primitives::Int32",
        Some(SourceType::Int32),
    ));
    sa.known.structs.int64 = Some(internal_struct(
        sa,
        stdlib_id,
        "primitives::Int64",
        Some(SourceType::Int64),
    ));

    sa.known.structs.float32 = Some(internal_struct(
        sa,
        stdlib_id,
        "primitives::Float32",
        Some(SourceType::Float32),
    ));
    sa.known.structs.float64 = Some(internal_struct(
        sa,
        stdlib_id,
        "primitives::Float64",
        Some(SourceType::Float64),
    ));

    sa.known.classes.string = Some(internal_class(sa, stdlib_id, "string::String"));

    sa.known.classes.string_buffer = Some(find_class(sa, stdlib_id, "string::StringBuffer"));

    sa.known.classes.atomic_int32 = Some(find_class(sa, stdlib_id, "thread::AtomicInt32"));
    sa.known.classes.atomic_int64 = Some(find_class(sa, stdlib_id, "thread::AtomicInt64"));

    let cls = &mut sa.classes[sa.known.classes.string()];
    cls.is_str = true;

    sa.known.classes.array = Some(internal_class(sa, stdlib_id, "collections::Array"));

    let cls = &mut sa.classes[sa.known.classes.array()];
    cls.is_array = true;

    sa.known.classes.stacktrace = Some(find_class(sa, stdlib_id, "Stacktrace"));
    sa.known.classes.stacktrace_element = Some(find_class(sa, stdlib_id, "StacktraceElement"));
    sa.known.classes.thread = Some(find_class(sa, stdlib_id, "thread::Thread"));

    sa.known.traits.add = Some(find_trait(sa, stdlib_id, "traits::Add"));
    sa.known.traits.bit_and = Some(find_trait(sa, stdlib_id, "traits::BitAnd"));
    sa.known.traits.bit_or = Some(find_trait(sa, stdlib_id, "traits::BitOr"));
    sa.known.traits.bit_xor = Some(find_trait(sa, stdlib_id, "traits::BitXor"));
    sa.known.traits.comparable = Some(find_trait(sa, stdlib_id, "traits::Comparable"));
    sa.known.traits.div = Some(find_trait(sa, stdlib_id, "traits::Div"));
    sa.known.traits.equals = Some(find_trait(sa, stdlib_id, "traits::Equals"));
    sa.known.traits.into_iterator = Some(find_trait(sa, stdlib_id, "traits::IntoIterator"));
    sa.known.traits.iterator = Some(find_trait(sa, stdlib_id, "traits::Iterator"));
    sa.known.traits.mul = Some(find_trait(sa, stdlib_id, "traits::Mul"));
    sa.known.traits.mod_ = Some(find_trait(sa, stdlib_id, "traits::Mod"));
    sa.known.traits.neg = Some(find_trait(sa, stdlib_id, "traits::Neg"));
    sa.known.traits.not = Some(find_trait(sa, stdlib_id, "traits::Not"));
    sa.known.traits.sar = Some(find_trait(sa, stdlib_id, "traits::Sar"));
    sa.known.traits.shl = Some(find_trait(sa, stdlib_id, "traits::Shl"));
    sa.known.traits.shr = Some(find_trait(sa, stdlib_id, "traits::Shr"));
    sa.known.traits.stringable = Some(find_trait(sa, stdlib_id, "string::Stringable"));
    sa.known.traits.sub = Some(find_trait(sa, stdlib_id, "traits::Sub"));
    sa.known.traits.zero = Some(find_trait(sa, stdlib_id, "traits::Zero"));

    sa.known.enums.option = Some(find_enum(sa, stdlib_id, "primitives::Option"));
    sa.known.enums.ordering = Some(find_enum(sa, stdlib_id, "traits::Ordering"));
}

pub fn setup_prelude(sa: &mut Sema) {
    let stdlib_id = sa.stdlib_module_id();

    let symbols = [
        "primitives::Bool",
        "primitives::UInt8",
        "primitives::Char",
        "primitives::Int32",
        "primitives::Int64",
        "primitives::Int",
        "primitives::Float32",
        "primitives::Float64",
        "string::String",
        "collections::Array",
        "collections::Vec",
        "print",
        "println",
        "primitives::Option",
        "unimplemented",
        "unreachable",
        "assert",
        "primitives::Result",
    ];

    let mut prelude_table = SymTable::new();

    for name in &symbols {
        let sym = resolve_name(sa, name, stdlib_id);
        let name = final_path_name(sa, name);
        let old_sym = prelude_table.insert(name, sym);
        assert!(old_sym.is_none());
    }

    {
        // include None and Some from Option
        let enum_id = resolve_name(sa, "primitives::Option", stdlib_id)
            .to_enum()
            .expect("enum expected");

        let enum_ = sa.enum_(enum_id);

        for variant in enum_.variants() {
            let old_sym =
                prelude_table.insert(variant.name, SymbolKind::EnumVariant(enum_id, variant.id));
            assert!(old_sym.is_none());
        }
    }

    {
        // include Ok and Err from Result
        let enum_id = resolve_name(sa, "primitives::Result", stdlib_id)
            .to_enum()
            .expect("enum expected");

        let enum_ = sa.enum_(enum_id);

        for variant in enum_.variants() {
            let old_sym =
                prelude_table.insert(variant.name, SymbolKind::EnumVariant(enum_id, variant.id));
            assert!(old_sym.is_none());
        }
    }

    let stdlib_name = sa.interner.intern("std");
    prelude_table.insert(stdlib_name, SymbolKind::Module(stdlib_id));

    let module = ModuleDefinition::new_top_level(None);
    assert!(module.table.set(Rc::new(prelude_table)).is_ok());
    let module_id = sa.modules.alloc(module);
    sa.set_prelude_module_id(module_id);
}

fn final_path_name(sa: &mut Sema, path: &str) -> Name {
    let name = path.split("::").last().expect("name missing");
    sa.interner.intern(name)
}

pub fn lookup_known_methods(sa: &mut Sema) {
    let stdlib_id = sa.stdlib_module_id();

    sa.known.functions.string_equals =
        Some(lookup_fct(sa, "traits::Equals for string::String#equals"));

    sa.known.functions.string_buffer_empty = Some(find_static_method(
        sa,
        stdlib_id,
        "string::StringBuffer",
        "empty",
    ));
    sa.known.functions.string_buffer_append = Some(find_instance_method(
        sa,
        stdlib_id,
        "string::StringBuffer",
        "append",
    ));
    sa.known.functions.string_buffer_to_string = Some(find_instance_method(
        sa,
        stdlib_id,
        "string::StringBuffer",
        "toString",
    ));
}

pub fn create_lambda_class(sa: &mut Sema) {
    let class_name = sa.interner.intern("$Lambda");
    let context_name = sa.interner.intern("context");

    let field = Field {
        id: FieldId(0),
        name: context_name,
        ty: OnceCell::new(),
        mutable: false,
        visibility: Visibility::Public,
    };
    assert!(field.ty.set(SourceType::Ptr).is_ok());

    let fields = vec![field];

    let class = ClassDefinition::new_without_source(
        sa.stdlib_package_id(),
        sa.stdlib_module_id(),
        None,
        None,
        class_name,
        Visibility::Public,
        fields,
    );
    class
        .type_params
        .set(TypeParamDefinition::new())
        .expect("already initialized");
    let class_id = sa.classes.alloc(class);
    sa.classes[class_id].id = Some(class_id);
    sa.known.classes.lambda = Some(class_id);
}

fn find_class(sa: &Sema, module_id: ModuleDefinitionId, name: &str) -> ClassDefinitionId {
    resolve_name(sa, name, module_id)
        .to_class()
        .expect("class expected")
}

fn internal_class(sa: &mut Sema, module_id: ModuleDefinitionId, name: &str) -> ClassDefinitionId {
    let cls_id = find_class(sa, module_id, name);

    let cls = &mut sa.classes[cls_id];
    assert!(cls.is_internal);
    cls.internal_resolved = true;

    cls_id
}

fn internal_struct(
    sa: &mut Sema,
    module_id: ModuleDefinitionId,
    name: &str,
    ty: Option<SourceType>,
) -> StructDefinitionId {
    let struct_id = resolve_name(sa, name, module_id)
        .to_struct()
        .expect("struct expected");

    let struct_ = &mut sa.structs[struct_id];
    assert!(struct_.is_internal);
    struct_.primitive_ty = ty;
    struct_.internal_resolved = true;

    struct_id
}

fn resolve_name(sa: &Sema, name: &str, module_id: ModuleDefinitionId) -> SymbolKind {
    let path = name.split("::");
    let mut sym = SymbolKind::Module(module_id);

    for name in path {
        let module_id = sym.to_module().expect("module expected");
        let table = sa.module(module_id).table();

        let interned_name = sa.interner.intern(name);

        if let Some(current_sym) = table.get(interned_name) {
            sym = current_sym;
        } else {
            let module = sa.module(module_id);
            panic!("{} not found in module {}.", name, module.name(sa));
        }
    }

    sym
}

fn find_trait(sa: &Sema, module_id: ModuleDefinitionId, name: &str) -> TraitDefinitionId {
    resolve_name(sa, name, module_id)
        .to_trait()
        .expect("trait expected")
}

fn find_enum(sa: &Sema, module_id: ModuleDefinitionId, name: &str) -> EnumDefinitionId {
    resolve_name(sa, name, module_id)
        .to_enum()
        .expect("enum not found")
}

pub fn resolve_internal_functions(sa: &mut Sema) {
    let stdlib_id = sa.stdlib_module_id();

    resolve_string(sa, stdlib_id);
    resolve_uint8(sa, stdlib_id);
    resolve_bool(sa, stdlib_id);
    resolve_char(sa, stdlib_id);
    resolve_int32(sa, stdlib_id);
    resolve_int64(sa, stdlib_id);
    resolve_float32(sa, stdlib_id);
    resolve_float64(sa, stdlib_id);

    resolve_array(sa, stdlib_id);
    resolve_option(sa, stdlib_id);

    resolve_thread(sa, stdlib_id);

    resolve_atomic_int32(sa, stdlib_id);
    resolve_atomic_int64(sa, stdlib_id);

    lookup_ordering(sa, stdlib_id);

    resolve_functions(sa, stdlib_id);
}

fn resolve_functions(sa: &mut Sema, stdlib_id: ModuleDefinitionId) {
    sa.known.functions.unreachable = Some(find_fct(sa, stdlib_id, "unreachable"));
    sa.known.functions.fatal_error = Some(find_fct(sa, stdlib_id, "fatalError"));
}

fn lookup_ordering(sa: &mut Sema, stdlib_id: ModuleDefinitionId) {
    sa.known.functions.ordering_is_ge =
        Some(find_instance_method(sa, stdlib_id, "Ordering", "is_ge"));
    sa.known.functions.ordering_is_gt =
        Some(find_instance_method(sa, stdlib_id, "Ordering", "is_gt"));
    sa.known.functions.ordering_is_le =
        Some(find_instance_method(sa, stdlib_id, "Ordering", "is_le"));
    sa.known.functions.ordering_is_lt =
        Some(find_instance_method(sa, stdlib_id, "Ordering", "is_lt"));
}

fn resolve_atomic_int32(sa: &mut Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt32",
        "get",
        Intrinsic::AtomicInt32Get,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt32",
        "set",
        Intrinsic::AtomicInt32Set,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt32",
        "exchange",
        Intrinsic::AtomicInt32Exchange,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt32",
        "compareExchange",
        Intrinsic::AtomicInt32CompareExchange,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt32",
        "fetchAdd",
        Intrinsic::AtomicInt32FetchAdd,
    );
}

fn resolve_atomic_int64(sa: &mut Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt64",
        "get",
        Intrinsic::AtomicInt64Get,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt64",
        "set",
        Intrinsic::AtomicInt64Set,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt64",
        "exchange",
        Intrinsic::AtomicInt64Exchange,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt64",
        "compareExchange",
        Intrinsic::AtomicInt64CompareExchange,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "thread::AtomicInt64",
        "fetchAdd",
        Intrinsic::AtomicInt64FetchAdd,
    );
}

fn resolve_thread(sa: &mut Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_static(
        sa,
        stdlib_id,
        "thread::Thread",
        "current",
        Intrinsic::ThreadCurrent,
    );
}

fn resolve_string(sa: &Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(sa, stdlib_id, "string::String", "size", Intrinsic::StrLen);
    intrinsic_method(
        sa,
        stdlib_id,
        "string::String",
        "getByte",
        Intrinsic::StrGet,
    );
}

fn resolve_uint8(sa: &mut Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        "toInt64",
        Intrinsic::UInt8ToInt64,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        "toInt32",
        Intrinsic::UInt8ToInt32,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        "toChar",
        Intrinsic::UInt8ToChar,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        sa.known.traits.equals(),
        "equals",
        Intrinsic::UInt8Eq,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        sa.known.traits.comparable(),
        "cmp",
        Intrinsic::UInt8Cmp,
    );
}

fn resolve_bool(sa: &Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Bool",
        "toInt32",
        Intrinsic::BoolToInt32,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Bool",
        "toInt64",
        Intrinsic::BoolToInt64,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Bool",
        sa.known.traits.equals(),
        "equals",
        Intrinsic::BoolEq,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Bool",
        sa.known.traits.not(),
        "not",
        Intrinsic::BoolNot,
    );
}

fn resolve_char(sa: &Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Char",
        "toInt64",
        Intrinsic::CharToInt64,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Char",
        "toInt32",
        Intrinsic::CharToInt32,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Char",
        sa.known.traits.equals(),
        "equals",
        Intrinsic::CharEq,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Char",
        sa.known.traits.comparable(),
        "cmp",
        Intrinsic::CharCmp,
    );
}

fn resolve_int32(sa: &Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "toUInt8",
        Intrinsic::Int32ToUInt8,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "toCharUnchecked",
        Intrinsic::Int32ToChar,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "toInt64",
        Intrinsic::Int32ToInt64,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "toFloat32",
        Intrinsic::Int32ToFloat32,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "Int32",
        "toFloat64",
        Intrinsic::Int32ToFloat64,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "Int32",
        "asFloat32",
        Intrinsic::ReinterpretInt32AsFloat32,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.equals(),
        "equals",
        Intrinsic::Int32Eq,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.comparable(),
        "cmp",
        Intrinsic::Int32Cmp,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.add(),
        "add",
        Intrinsic::Int32Add,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.sub(),
        "sub",
        Intrinsic::Int32Sub,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.mul(),
        "mul",
        Intrinsic::Int32Mul,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.div(),
        "div",
        Intrinsic::Int32Div,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.mod_(),
        "modulo",
        Intrinsic::Int32Mod,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "wrappingAdd",
        Intrinsic::Int32AddUnchecked,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "wrappingSub",
        Intrinsic::Int32SubUnchecked,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "wrappingMul",
        Intrinsic::Int32MulUnchecked,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.bit_or(),
        "bitor",
        Intrinsic::Int32Or,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.bit_and(),
        "bitand",
        Intrinsic::Int32And,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.bit_xor(),
        "bitxor",
        Intrinsic::Int32Xor,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.shl(),
        "shl",
        Intrinsic::Int32Shl,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.shr(),
        "shr",
        Intrinsic::Int32Shr,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.sar(),
        "sar",
        Intrinsic::Int32Sar,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "rotateLeft",
        Intrinsic::Int32RotateLeft,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "rotateRight",
        Intrinsic::Int32RotateRight,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.neg(),
        "neg",
        Intrinsic::Int32Neg,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "wrappingNeg",
        Intrinsic::Int32NegUnchecked,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        sa.known.traits.not(),
        "not",
        Intrinsic::Int32Not,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "countZeroBits",
        Intrinsic::Int32CountZeroBits,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "countOneBits",
        Intrinsic::Int32CountOneBits,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "countZeroBitsLeading",
        Intrinsic::Int32CountZeroBitsLeading,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "countOneBitsLeading",
        Intrinsic::Int32CountOneBitsLeading,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "countZeroBitsTrailing",
        Intrinsic::Int32CountZeroBitsTrailing,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "countOneBitsTrailing",
        Intrinsic::Int32CountOneBitsTrailing,
    );
}

fn resolve_float32(sa: &Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "toInt32",
        Intrinsic::Float32ToInt32,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "toInt64",
        Intrinsic::Float32ToInt64,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "toFloat64",
        Intrinsic::PromoteFloat32ToFloat64,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "asInt32",
        Intrinsic::ReinterpretFloat32AsInt32,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        sa.known.traits.equals(),
        "equals",
        Intrinsic::Float32Eq,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        sa.known.traits.comparable(),
        "cmp",
        Intrinsic::Float32Cmp,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        sa.known.traits.add(),
        "add",
        Intrinsic::Float32Add,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        sa.known.traits.sub(),
        "sub",
        Intrinsic::Float32Sub,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        sa.known.traits.mul(),
        "mul",
        Intrinsic::Float32Mul,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        sa.known.traits.div(),
        "div",
        Intrinsic::Float32Div,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        sa.known.traits.neg(),
        "neg",
        Intrinsic::Float32Neg,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "isNan",
        Intrinsic::Float32IsNan,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "abs",
        Intrinsic::Float32Abs,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "roundToZero",
        Intrinsic::Float32RoundToZero,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "roundUp",
        Intrinsic::Float32RoundUp,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "roundDown",
        Intrinsic::Float32RoundDown,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "roundHalfEven",
        Intrinsic::Float32RoundHalfEven,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "sqrt",
        Intrinsic::Float32Sqrt,
    );
}

fn resolve_float64(sa: &Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "toInt32",
        Intrinsic::Float64ToInt32,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "toInt64",
        Intrinsic::Float64ToInt64,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "toFloat32",
        Intrinsic::DemoteFloat64ToFloat32,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "asInt64",
        Intrinsic::ReinterpretFloat64AsInt64,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        sa.known.traits.equals(),
        "equals",
        Intrinsic::Float64Eq,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        sa.known.traits.comparable(),
        "cmp",
        Intrinsic::Float64Cmp,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        sa.known.traits.add(),
        "add",
        Intrinsic::Float64Add,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        sa.known.traits.sub(),
        "sub",
        Intrinsic::Float64Sub,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        sa.known.traits.mul(),
        "mul",
        Intrinsic::Float64Mul,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        sa.known.traits.div(),
        "div",
        Intrinsic::Float64Div,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        sa.known.traits.neg(),
        "neg",
        Intrinsic::Float64Neg,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "isNan",
        Intrinsic::Float64IsNan,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "abs",
        Intrinsic::Float64Abs,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "roundToZero",
        Intrinsic::Float64RoundToZero,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "roundUp",
        Intrinsic::Float64RoundUp,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "roundDown",
        Intrinsic::Float64RoundDown,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "roundHalfEven",
        Intrinsic::Float64RoundHalfEven,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "sqrt",
        Intrinsic::Float64Sqrt,
    );
}

fn resolve_int64(sa: &Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "toCharUnchecked",
        Intrinsic::Int64ToChar,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "toInt32",
        Intrinsic::Int64ToInt32,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "toUInt8",
        Intrinsic::Int64ToUInt8,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "toFloat32",
        Intrinsic::Int64ToFloat32,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "toFloat64",
        Intrinsic::Int64ToFloat64,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "asFloat64",
        Intrinsic::ReinterpretInt64AsFloat64,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.equals(),
        "equals",
        Intrinsic::Int64Eq,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.comparable(),
        "cmp",
        Intrinsic::Int64Cmp,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.add(),
        "add",
        Intrinsic::Int64Add,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.sub(),
        "sub",
        Intrinsic::Int64Sub,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.mul(),
        "mul",
        Intrinsic::Int64Mul,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.div(),
        "div",
        Intrinsic::Int64Div,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.mod_(),
        "modulo",
        Intrinsic::Int64Mod,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "wrappingAdd",
        Intrinsic::Int64AddUnchecked,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "wrappingSub",
        Intrinsic::Int64SubUnchecked,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "wrappingMul",
        Intrinsic::Int64MulUnchecked,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.bit_or(),
        "bitor",
        Intrinsic::Int64Or,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.bit_and(),
        "bitand",
        Intrinsic::Int64And,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.bit_xor(),
        "bitxor",
        Intrinsic::Int64Xor,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.shl(),
        "shl",
        Intrinsic::Int64Shl,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.shr(),
        "shr",
        Intrinsic::Int64Shr,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.sar(),
        "sar",
        Intrinsic::Int64Sar,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "rotateLeft",
        Intrinsic::Int64RotateLeft,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "rotateRight",
        Intrinsic::Int64RotateRight,
    );

    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.neg(),
        "neg",
        Intrinsic::Int64Neg,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "wrappingNeg",
        Intrinsic::Int64NegUnchecked,
    );
    intrinsic_impl_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        sa.known.traits.not(),
        "not",
        Intrinsic::Int64Not,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "countZeroBits",
        Intrinsic::Int64CountZeroBits,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "countOneBits",
        Intrinsic::Int64CountOneBits,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "countZeroBitsLeading",
        Intrinsic::Int64CountZeroBitsLeading,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "countOneBitsLeading",
        Intrinsic::Int64CountOneBitsLeading,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "countZeroBitsTrailing",
        Intrinsic::Int64CountZeroBitsTrailing,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "countOneBitsTrailing",
        Intrinsic::Int64CountOneBitsTrailing,
    );
}

fn resolve_array(sa: &Sema, stdlib_id: ModuleDefinitionId) {
    intrinsic_method(
        sa,
        stdlib_id,
        "collections::Array",
        "size",
        Intrinsic::ArrayLen,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "collections::Array",
        "get",
        Intrinsic::ArrayGet,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "collections::Array",
        "set",
        Intrinsic::ArraySet,
    );

    intrinsic_static(
        sa,
        stdlib_id,
        "collections::Array",
        "unsafeNew",
        Intrinsic::ArrayNewOfSize,
    );
    intrinsic_static(
        sa,
        stdlib_id,
        "collections::Array",
        "new",
        Intrinsic::ArrayWithValues,
    );
}

fn resolve_option(sa: &mut Sema, stdlib_id: ModuleDefinitionId) {
    let fct_id = intrinsic_method(sa, stdlib_id, "Option", "isNone", Intrinsic::OptionIsNone);
    sa.known.functions.option_is_none = Some(fct_id);
    let fct_id = intrinsic_method(sa, stdlib_id, "Option", "isSome", Intrinsic::OptionIsSome);
    sa.known.functions.option_is_some = Some(fct_id);
    let fct_id = intrinsic_method(
        sa,
        stdlib_id,
        "Option",
        "getOrPanic",
        Intrinsic::OptionGetOrPanic,
    );
    sa.known.functions.option_unwrap = Some(fct_id);
}

fn find_instance_method(
    sa: &Sema,
    module_id: ModuleDefinitionId,
    container_name: &str,
    name: &str,
) -> FctDefinitionId {
    find_method(sa, module_id, container_name, name, false)
}

fn find_static_method(
    sa: &Sema,
    module_id: ModuleDefinitionId,
    container_name: &str,
    name: &str,
) -> FctDefinitionId {
    find_method(sa, module_id, container_name, name, true)
}

fn find_method(
    sa: &Sema,
    module_id: ModuleDefinitionId,
    container_name: &str,
    name: &str,
    is_static: bool,
) -> FctDefinitionId {
    let sym = resolve_name(sa, container_name, module_id);

    match sym {
        SymbolKind::Enum(enum_id) => {
            let enum_ = &sa.enum_(enum_id);
            let extensions = enum_.extensions.borrow();
            find_method_in_extensions(sa, &*extensions, sa.interner.intern(name), is_static)
        }

        SymbolKind::Class(cls_id) => {
            let class = sa.class(cls_id);
            let extensions = class.extensions.borrow();
            find_method_in_extensions(sa, &*extensions, sa.interner.intern(name), is_static)
        }

        SymbolKind::Struct(struct_id) => {
            let struct_ = sa.struct_(struct_id);
            let extensions = struct_.extensions.borrow();
            find_method_in_extensions(sa, &*extensions, sa.interner.intern(name), is_static)
        }

        _ => panic!("unknown symbol {}::{}", container_name, name),
    }
}

fn find_fct(sa: &Sema, module_id: ModuleDefinitionId, name: &str) -> FctDefinitionId {
    let iname = sa.interner.intern(name);
    let sym = sa.module(module_id).table().get(iname);

    match sym {
        Some(SymbolKind::Fct(fct_id)) => fct_id,
        _ => panic!("unknown fct {}", name),
    }
}

fn find_method_in_extensions(
    sa: &Sema,
    extensions: &[ExtensionDefinitionId],
    name: Name,
    is_static: bool,
) -> FctDefinitionId {
    for &extension_id in extensions.iter() {
        let extension = sa.extension(extension_id);

        for &mid in extension.methods() {
            let mtd = sa.fct(mid);

            if mtd.name == name && mtd.is_static == is_static {
                return mid;
            }
        }
    }

    panic!("cannot find method `{}`", sa.interner.str(name))
}

fn intrinsic_method(
    sa: &Sema,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    intrinsic: Intrinsic,
) -> FctDefinitionId {
    common_method(sa, module_id, container_name, method_name, false, intrinsic)
}

fn intrinsic_static(
    sa: &Sema,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    intrinsic: Intrinsic,
) {
    common_method(sa, module_id, container_name, method_name, true, intrinsic);
}

fn common_method(
    sa: &Sema,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    is_static: bool,
    marker: Intrinsic,
) -> FctDefinitionId {
    let sym = resolve_name(sa, container_name, module_id);

    match sym {
        SymbolKind::Class(cls_id) => {
            let cls = sa.class(cls_id);
            let extensions = cls.extensions.borrow();
            internal_extension_method(sa, &extensions, method_name, is_static, marker)
        }

        SymbolKind::Struct(struct_id) => {
            let struct_ = sa.struct_(struct_id);
            let extensions = struct_.extensions.borrow();
            internal_extension_method(sa, &extensions, method_name, is_static, marker)
        }
        SymbolKind::Enum(enum_id) => {
            let enum_ = sa.enum_(enum_id);
            let extensions = enum_.extensions.borrow();
            internal_extension_method(sa, &extensions, method_name, is_static, marker)
        }

        _ => panic!("unexpected type"),
    }
}

fn internal_extension_method(
    sa: &Sema,
    extensions: &[ExtensionDefinitionId],
    name_as_string: &str,
    is_static: bool,
    intrinsic: Intrinsic,
) -> FctDefinitionId {
    let name = sa.interner.intern(name_as_string);

    for &extension_id in extensions {
        let extension = sa.extension(extension_id);

        let table = if is_static {
            &extension.static_names
        } else {
            &extension.instance_names
        };

        if let Some(&method_id) = table.borrow().get(&name) {
            let fct = sa.fct(method_id);
            assert!(fct.intrinsic.set(intrinsic).is_ok());
            return method_id;
        }
    }

    panic!("method {} not found!", name_as_string)
}

fn intrinsic_impl_method(
    sa: &Sema,
    module_id: ModuleDefinitionId,
    container_name: &str,
    trait_id: TraitDefinitionId,
    method_name: &str,
    intrinsic: Intrinsic,
) -> FctDefinitionId {
    internal_impl_method(
        sa,
        module_id,
        container_name,
        trait_id,
        method_name,
        intrinsic,
    )
}

fn internal_impl_method(
    sa: &Sema,
    module_id: ModuleDefinitionId,
    container_name: &str,
    trait_id: TraitDefinitionId,
    method_name: &str,
    intrinsic: Intrinsic,
) -> FctDefinitionId {
    let sym = resolve_name(sa, container_name, module_id);

    let ty = match sym {
        SymbolKind::Struct(struct_id) => sa.struct_(struct_id).ty(),

        SymbolKind::Class(cls_id) => SourceType::Class(cls_id, SourceTypeArray::empty()),

        _ => panic!("unexpected type"),
    };

    let trait_ty = SourceType::new_trait(trait_id);

    for (_id, impl_) in sa.impls.iter() {
        if impl_.trait_ty() == trait_ty && impl_.extended_ty() == ty {
            let method_name = sa.interner.intern(method_name);
            let trait_ = &sa.trait_(impl_.trait_id());

            let trait_method_id = trait_
                .get_method(method_name, false)
                .expect("method not found");

            let method_id = impl_
                .get_method_for_trait_method_id(trait_method_id)
                .expect("missing method");

            let fct = sa.fct(method_id);
            assert!(fct.intrinsic.set(intrinsic).is_ok());
            return method_id;
        }
    }

    panic!("method {} not found!", method_name)
}

#[allow(unused)]
fn lookup_fct(sa: &Sema, path: &str) -> FctDefinitionId {
    let module_id = sa.stdlib_module_id();

    if path.contains("#") {
        let parts = path.split("#").collect::<Vec<_>>();
        assert_eq!(parts.len(), 2);
        let path = parts[0];
        let method_name = parts[1];

        if path.contains(" for ") {
            let parts = path.split(" for ").collect::<Vec<_>>();
            assert_eq!(parts.len(), 2);

            let trait_path = parts[0];
            let extended_ty_path = parts[1];

            let trait_id = resolve_name(sa, trait_path, module_id)
                .to_trait()
                .expect("trait expected");
            let extended_ty = resolve_name(sa, extended_ty_path, module_id);
            let impl_id = lookup_impl_for_item(sa, trait_id, extended_ty).expect("impl not found");

            lookup_fct_by_impl_id_and_name(sa, impl_id, method_name)
                .expect("method in impl not found")
        } else {
            unimplemented!()
        }
    } else {
        resolve_name(sa, path, module_id)
            .to_fct()
            .expect("function expected")
    }
}

fn lookup_impl_for_item(
    sa: &Sema,
    trait_id: TraitDefinitionId,
    extended_ty: SymbolKind,
) -> Option<ImplDefinitionId> {
    for (id, impl_) in sa.impls.iter() {
        match impl_.trait_ty() {
            SourceType::Trait(impl_trait_id, ..) if impl_trait_id == trait_id => {}
            _ => continue,
        }

        match impl_.extended_ty() {
            SourceType::Class(class_id, ..) if extended_ty.to_class() == Some(class_id) => {
                return Some(id);
            }

            _ => {}
        }
    }

    None
}

fn lookup_fct_by_impl_id_and_name(
    sa: &Sema,
    impl_id: ImplDefinitionId,
    name: &str,
) -> Option<FctDefinitionId> {
    let impl_ = sa.impl_(impl_id);
    let name = sa.interner.intern(name);

    for &method_id in impl_.methods() {
        let method = sa.fct(method_id);
        if method.name == name {
            return Some(method_id);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn builtin_functions() {
        ok("fn f() { assert(true); }");
        ok("fn f() { print(\"test\"); }");
        ok("fn f() { println(\"test\"); }");
    }
}
