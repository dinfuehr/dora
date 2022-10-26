use dora_parser::ast::Modifier;
use dora_parser::interner::Name;

use crate::gc::Address;
use crate::language::sem_analysis::{
    AnnotationDefinitionId, ClassDefinition, ClassDefinitionId, EnumDefinitionId,
    ExtensionDefinitionId, FctDefinitionId, Field, FieldId, Intrinsic, ModuleDefinition,
    ModuleDefinitionId, SemAnalysis, StructDefinitionId, TraitDefinitionId, Visibility,
};
use crate::language::sym::Sym;
use crate::language::ty::SourceType;
use crate::stack;
use crate::stdlib;

pub fn resolve_internal_annotations(sa: &mut SemAnalysis) {
    let stdlib_id = sa.stdlib_module_id();

    sa.known.annotations.internal = Some(internal_annotation(
        sa,
        stdlib_id,
        "annotations::internal",
        Modifier::Internal,
    ));
    sa.known.annotations.pub_ = Some(internal_annotation(
        sa,
        stdlib_id,
        "annotations::pub",
        Modifier::Pub,
    ));
    sa.known.annotations.static_ = Some(internal_annotation(
        sa,
        stdlib_id,
        "annotations::static",
        Modifier::Static,
    ));

    sa.known.annotations.test = Some(internal_annotation(
        sa,
        stdlib_id,
        "annotations::Test",
        Modifier::Test,
    ));

    sa.known.annotations.optimize_immediately = Some(internal_annotation(
        sa,
        stdlib_id,
        "annotations::optimizeImmediately",
        Modifier::OptimizeImmediately,
    ));
}

pub fn resolve_internal_classes(sa: &mut SemAnalysis) {
    let stdlib_id = sa.stdlib_module_id();

    sa.known.structs.unit = Some(internal_struct(
        sa,
        stdlib_id,
        "primitives::Unit",
        Some(SourceType::Unit),
    ));

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

    let cls = sa.classes.idx(sa.known.classes.string());
    let mut cls = cls.write();
    cls.is_str = true;

    sa.known.classes.array = Some(internal_class(sa, stdlib_id, "collections::Array"));

    let cls = sa.classes.idx(sa.known.classes.array());
    let mut cls = cls.write();
    cls.is_array = true;

    sa.known.classes.stacktrace = Some(find_class(sa, stdlib_id, "Stacktrace"));
    sa.known.classes.stacktrace_element = Some(find_class(sa, stdlib_id, "StacktraceElement"));
    sa.known.classes.thread = Some(find_class(sa, stdlib_id, "thread::Thread"));

    sa.known.traits.stringable = Some(find_trait(sa, stdlib_id, "string::Stringable"));
    sa.known.traits.zero = Some(find_trait(sa, stdlib_id, "traits::Zero"));
    sa.known.traits.iterator = Some(find_trait(sa, stdlib_id, "traits::Iterator"));

    sa.known.enums.option = Some(find_enum(sa, stdlib_id, "primitives::Option"));
}

pub fn fill_prelude(sa: &mut SemAnalysis) {
    let stdlib_id = sa.stdlib_module_id();

    let symbols = [
        "primitives::Unit",
        "primitives::Bool",
        "primitives::UInt8",
        "primitives::Char",
        "primitives::Int32",
        "primitives::Int64",
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

    let module = ModuleDefinition::new_top_level(None);
    let module_id = sa.modules.push(module);
    sa.set_prelude_module_id(module_id);

    let prelude = sa.prelude_module();
    let mut prelude = prelude.write();

    for name in &symbols {
        let sym = resolve_name(sa, name, stdlib_id);
        let name = final_path_name(sa, name);
        let old_sym = prelude.insert(name, sym);
        assert!(old_sym.is_none());
    }

    {
        // include None and Some from Option
        let enum_id = resolve_name(sa, "primitives::Option", stdlib_id)
            .to_enum()
            .expect("enum expected");

        let enum_ = &sa.enums[enum_id];
        let enum_ = enum_.read();

        for variant in &enum_.variants {
            let old_sym = prelude.insert(variant.name, Sym::EnumVariant(enum_id, variant.id));
            assert!(old_sym.is_none());
        }
    }

    {
        // include Ok and Err from Result
        let enum_id = resolve_name(sa, "primitives::Result", stdlib_id)
            .to_enum()
            .expect("enum expected");

        let enum_ = &sa.enums[enum_id];
        let enum_ = enum_.read();

        for variant in &enum_.variants {
            let old_sym = prelude.insert(variant.name, Sym::EnumVariant(enum_id, variant.id));
            assert!(old_sym.is_none());
        }
    }

    let stdlib_name = sa.interner.intern("std");
    prelude.insert(stdlib_name, Sym::Module(stdlib_id));

    for package in sa.packages.iter() {
        let package = package.read();

        if let Some(name) = package.iname() {
            prelude.insert(name, Sym::Module(package.top_level_module_id()));
        }
    }
}

fn final_path_name(sa: &mut SemAnalysis, path: &str) -> Name {
    let name = path.split("::").last().expect("name missing");
    sa.interner.intern(name)
}

pub fn discover_known_methods(sa: &mut SemAnalysis) {
    let stdlib_id = sa.stdlib_module_id();

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
    sa.known.functions.stacktrace_retrieve = Some(find_instance_method(
        sa,
        stdlib_id,
        "Stacktrace",
        "retrieveStacktrace",
    ));
}

pub fn create_lambda_class(sa: &mut SemAnalysis) {
    let class_name = sa.interner.intern("$Lambda");
    let context_name = sa.interner.intern("context");

    let fields = vec![Field {
        id: FieldId(0),
        name: context_name,
        ty: SourceType::Ptr,
        mutable: false,
        visibility: Visibility::Public,
    }];

    let class = ClassDefinition::new_without_source(
        sa.stdlib_package_id(),
        sa.stdlib_module_id(),
        None,
        None,
        class_name,
        Visibility::Public,
        fields,
    );
    let class_id = sa.classes.push(class);
    sa.known.classes.lambda = Some(class_id);
}

fn find_class(sa: &SemAnalysis, module_id: ModuleDefinitionId, name: &str) -> ClassDefinitionId {
    resolve_name(sa, name, module_id)
        .to_class()
        .expect("class expected")
}

fn internal_class(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    name: &str,
) -> ClassDefinitionId {
    let cls_id = find_class(sa, module_id, name);

    let cls = sa.classes.idx(cls_id);
    let mut cls = cls.write();
    assert!(cls.internal);
    cls.internal_resolved = true;

    cls_id
}

fn internal_struct(
    sa: &mut SemAnalysis,
    module_id: ModuleDefinitionId,
    name: &str,
    ty: Option<SourceType>,
) -> StructDefinitionId {
    let struct_id = resolve_name(sa, name, module_id)
        .to_struct()
        .expect("struct expected");

    let struct_ = sa.structs.idx(struct_id);
    let mut struct_ = struct_.write();

    assert!(struct_.internal);
    struct_.primitive_ty = ty;
    struct_.internal_resolved = true;

    struct_id
}

fn resolve_name(sa: &SemAnalysis, name: &str, module_id: ModuleDefinitionId) -> Sym {
    let path = name.split("::");
    let mut sym = Sym::Module(module_id);

    for name in path {
        let module_id = sym.to_module().expect("module expected");
        let table = sa.modules.idx(module_id).read().table.clone();
        let table = table.read();

        let interned_name = sa.interner.intern(name);

        if let Some(current_sym) = table.get(interned_name) {
            sym = current_sym;
        } else {
            let module = sa.modules.idx(module_id);
            let module = module.read();
            panic!("{} not found in module {}.", name, module.name(sa));
        }
    }

    sym
}

fn internal_annotation(
    sa: &mut SemAnalysis,
    module_id: ModuleDefinitionId,
    name: &str,
    internal_annotation: Modifier,
) -> AnnotationDefinitionId {
    let annotation_id = resolve_name(sa, name, module_id)
        .to_annotation()
        .expect("annotation expected");

    let annotation = sa.annotations.idx(annotation_id);
    let mut annotation = annotation.write();
    annotation.internal_annotation = Some(internal_annotation);

    annotation_id
}

fn find_trait(
    sa: &mut SemAnalysis,
    module_id: ModuleDefinitionId,
    name: &str,
) -> TraitDefinitionId {
    resolve_name(sa, name, module_id)
        .to_trait()
        .expect("trait expected")
}

fn find_enum(sa: &mut SemAnalysis, module_id: ModuleDefinitionId, name: &str) -> EnumDefinitionId {
    resolve_name(sa, name, module_id)
        .to_enum()
        .expect("enum not found")
}

pub fn resolve_internal_functions(sa: &mut SemAnalysis) {
    let stdlib_id = sa.stdlib_module_id();

    native_fct(
        sa,
        stdlib_id,
        "fatalError",
        stdlib::fatal_error as *const u8,
    );
    native_fct(sa, stdlib_id, "abort", stdlib::abort as *const u8);
    native_fct(sa, stdlib_id, "exit", stdlib::exit as *const u8);
    intrinsic_fct(sa, stdlib_id, "unreachable", Intrinsic::Unreachable);

    native_fct(sa, stdlib_id, "print", stdlib::print as *const u8);
    native_fct(sa, stdlib_id, "println", stdlib::println as *const u8);
    let fid = intrinsic_fct(sa, stdlib_id, "assert", Intrinsic::Assert);
    sa.known.functions.assert = Some(fid);
    intrinsic_fct(sa, stdlib_id, "debug", Intrinsic::Debug);
    native_fct(sa, stdlib_id, "argc", stdlib::argc as *const u8);
    native_fct(sa, stdlib_id, "argv", stdlib::argv as *const u8);
    native_fct(
        sa,
        stdlib_id,
        "forceCollect",
        stdlib::gc_collect as *const u8,
    );
    native_fct(sa, stdlib_id, "timestamp", stdlib::timestamp as *const u8);
    native_fct(
        sa,
        stdlib_id,
        "forceMinorCollect",
        stdlib::gc_minor_collect as *const u8,
    );
    native_fct(sa, stdlib_id, "sleep", stdlib::sleep as *const u8);

    intrinsic_fct(sa, stdlib_id, "unsafeKillRefs", Intrinsic::UnsafeKillRefs);

    native_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        "toString",
        stdlib::uint8_to_string as *const u8,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        "toInt64",
        Intrinsic::ByteToInt64,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        "toInt32",
        Intrinsic::ByteToInt32,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        "toChar",
        Intrinsic::ByteToChar,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        "equals",
        Intrinsic::ByteEq,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::UInt8",
        "compareTo",
        Intrinsic::ByteCmp,
    );

    native_method(
        sa,
        stdlib_id,
        "primitives::Char",
        "toString",
        stdlib::char_to_string as *const u8,
    );
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
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Char",
        "equals",
        Intrinsic::CharEq,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Char",
        "compareTo",
        Intrinsic::CharCmp,
    );

    native_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "toString",
        stdlib::int32_to_string as *const u8,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "toUInt8",
        Intrinsic::Int32ToByte,
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

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "equals",
        Intrinsic::Int32Eq,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "compareTo",
        Intrinsic::Int32Cmp,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "plus",
        Intrinsic::Int32Add,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "minus",
        Intrinsic::Int32Sub,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "times",
        Intrinsic::Int32Mul,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "div",
        Intrinsic::Int32Div,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "remainder",
        Intrinsic::Int32Rem,
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

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "bitwiseOr",
        Intrinsic::Int32Or,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "bitwiseAnd",
        Intrinsic::Int32And,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "bitwiseXor",
        Intrinsic::Int32Xor,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "shiftLeft",
        Intrinsic::Int32Shl,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "shiftRight",
        Intrinsic::Int32Shr,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "shiftRightSigned",
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

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "unaryPlus",
        Intrinsic::Int32Plus,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
        "unaryMinus",
        Intrinsic::Int32Neg,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int32",
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

    native_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "toString",
        stdlib::int64_to_string as *const u8,
    );

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
        Intrinsic::Int64ToByte,
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

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "equals",
        Intrinsic::Int64Eq,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "compareTo",
        Intrinsic::Int64Cmp,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "plus",
        Intrinsic::Int64Add,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "minus",
        Intrinsic::Int64Sub,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "times",
        Intrinsic::Int64Mul,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "div",
        Intrinsic::Int64Div,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "remainder",
        Intrinsic::Int64Rem,
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

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "bitwiseOr",
        Intrinsic::Int64Or,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "bitwiseAnd",
        Intrinsic::Int64And,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "bitwiseXor",
        Intrinsic::Int64Xor,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "shiftLeft",
        Intrinsic::Int64Shl,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "shiftRight",
        Intrinsic::Int64Shr,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "shiftRightSigned",
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

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "unaryPlus",
        Intrinsic::Int64Plus,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
        "unaryMinus",
        Intrinsic::Int64Neg,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Int64",
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
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Bool",
        "equals",
        Intrinsic::BoolEq,
    );
    intrinsic_method(sa, stdlib_id, "primitives::Bool", "not", Intrinsic::BoolNot);

    native_method(
        sa,
        stdlib_id,
        "string::String",
        "hash",
        stdlib::strhash as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "compareTo",
        stdlib::strcmp as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "toInt32Success",
        stdlib::str_to_int32_success as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "toInt64Success",
        stdlib::str_to_int64_success as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "toFloat32Success",
        stdlib::str_to_float32_success as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "toFloat64Success",
        stdlib::str_to_float64_success as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "toInt32OrZero",
        stdlib::str_to_int32 as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "toInt64OrZero",
        stdlib::str_to_int64 as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "toFloat32OrZero",
        stdlib::str_to_float32 as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "toFloat64OrZero",
        stdlib::str_to_float64 as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "plus",
        stdlib::strcat as *const u8,
    );

    intrinsic_method(sa, stdlib_id, "string::String", "size", Intrinsic::StrLen);
    intrinsic_method(
        sa,
        stdlib_id,
        "string::String",
        "getByte",
        Intrinsic::StrGet,
    );
    native_method(
        sa,
        stdlib_id,
        "string::String",
        "clone",
        stdlib::str_clone as *const u8,
    );

    native_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "toString",
        stdlib::float32_to_string as *const u8,
    );
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

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "equals",
        Intrinsic::Float32Eq,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "compareTo",
        Intrinsic::Float32Cmp,
    );
    #[cfg(target_arch = "x86_64")]
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "sortsAs",
        Intrinsic::Float32Srt,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "plus",
        Intrinsic::Float32Add,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "minus",
        Intrinsic::Float32Sub,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "times",
        Intrinsic::Float32Mul,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "div",
        Intrinsic::Float32Div,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "unaryPlus",
        Intrinsic::Float32Plus,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float32",
        "unaryMinus",
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

    native_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "toString",
        stdlib::float64_to_string as *const u8,
    );
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

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "equals",
        Intrinsic::Float64Eq,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "compareTo",
        Intrinsic::Float64Cmp,
    );
    #[cfg(target_arch = "x86_64")]
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "sortsAs",
        Intrinsic::Float64Srt,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "plus",
        Intrinsic::Float64Add,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "minus",
        Intrinsic::Float64Sub,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "times",
        Intrinsic::Float64Mul,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "div",
        Intrinsic::Float64Div,
    );

    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "unaryPlus",
        Intrinsic::Float64Plus,
    );
    intrinsic_method(
        sa,
        stdlib_id,
        "primitives::Float64",
        "unaryMinus",
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

    native_static(
        sa,
        stdlib_id,
        "string::String",
        "fromBytesPart",
        stdlib::str_from_bytes as *const u8,
    );
    native_static(
        sa,
        stdlib_id,
        "string::String",
        "fromStringPart",
        stdlib::str_from_bytes as *const u8,
    );

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

    native_method(
        sa,
        stdlib_id,
        "Stacktrace",
        "retrieveStacktrace",
        stack::retrieve_stack_trace as *const u8,
    );
    native_method(
        sa,
        stdlib_id,
        "Stacktrace",
        "getStacktraceElement",
        stack::stack_element as *const u8,
    );

    native_fct(
        sa,
        stdlib_id,
        "thread::spawn",
        stdlib::spawn_thread as *const u8,
    );

    native_method(
        sa,
        stdlib_id,
        "thread::Thread",
        "join",
        stdlib::join_thread as *const u8,
    );

    intrinsic_static(
        sa,
        stdlib_id,
        "thread::Thread",
        "current",
        Intrinsic::ThreadCurrent,
    );

    native_method(
        sa,
        stdlib_id,
        "thread::Mutex",
        "wait",
        stdlib::mutex_wait as *const u8,
    );

    native_method(
        sa,
        stdlib_id,
        "thread::Mutex",
        "notify",
        stdlib::mutex_notify as *const u8,
    );

    native_method(
        sa,
        stdlib_id,
        "thread::Condition",
        "enqueue",
        stdlib::condition_enqueue as *const u8,
    );

    native_method(
        sa,
        stdlib_id,
        "thread::Condition",
        "block",
        stdlib::condition_block_after_enqueue as *const u8,
    );

    native_method(
        sa,
        stdlib_id,
        "thread::Condition",
        "wakeupOne",
        stdlib::condition_wakeup_one as *const u8,
    );

    native_method(
        sa,
        stdlib_id,
        "thread::Condition",
        "wakeupAll",
        stdlib::condition_wakeup_all as *const u8,
    );

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

fn find_instance_method(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    container_name: &str,
    name: &str,
) -> FctDefinitionId {
    find_class_method(sa, module_id, container_name, name, false)
}

fn find_function(sa: &SemAnalysis, module_id: ModuleDefinitionId, name: &str) -> FctDefinitionId {
    let fct_id = resolve_name(sa, name, module_id)
        .to_fct()
        .expect("function expected");

    fct_id
}

fn find_static_method(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    container_name: &str,
    name: &str,
) -> FctDefinitionId {
    find_class_method(sa, module_id, container_name, name, true)
}

fn find_class_method(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    container_name: &str,
    name: &str,
    is_static: bool,
) -> FctDefinitionId {
    let cls_id = resolve_name(sa, container_name, module_id)
        .to_class()
        .expect("class not found");

    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();
    let intern_name = sa.interner.intern(name);

    for &extension_id in &cls.extensions {
        let extension = sa.extensions.idx(extension_id);
        let extension = extension.read();

        for &mid in &extension.methods {
            let mtd = sa.fcts.idx(mid);
            let mtd = mtd.read();

            if mtd.name == intern_name && mtd.is_static == is_static {
                return mid;
            }
        }
    }

    panic!("cannot find class method `{}`", name)
}

fn native_fct(sa: &mut SemAnalysis, module_id: ModuleDefinitionId, name: &str, fctptr: *const u8) {
    common_fct(
        sa,
        module_id,
        name,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn intrinsic_fct(
    sa: &mut SemAnalysis,
    module_id: ModuleDefinitionId,
    name: &str,
    intrinsic: Intrinsic,
) -> FctDefinitionId {
    common_fct(sa, module_id, name, FctImplementation::Intrinsic(intrinsic))
}

fn common_fct(
    sa: &mut SemAnalysis,
    module_id: ModuleDefinitionId,
    name: &str,
    kind: FctImplementation,
) -> FctDefinitionId {
    let fct_id = resolve_name(sa, name, module_id)
        .to_fct()
        .expect("function expected");

    let fct = sa.fcts.idx(fct_id);
    let mut fct = fct.write();

    match kind {
        FctImplementation::Intrinsic(intrinsic) => fct.intrinsic = Some(intrinsic),
        FctImplementation::Native(address) => {
            fct.native_pointer = Some(address);
        }
    }
    fct.internal_resolved = true;
    fct_id
}

enum FctImplementation {
    Intrinsic(Intrinsic),
    Native(Address),
}

fn native_method(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    fctptr: *const u8,
) {
    common_method(
        sa,
        module_id,
        container_name,
        method_name,
        false,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn intrinsic_method(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    intrinsic: Intrinsic,
) -> FctDefinitionId {
    common_method(
        sa,
        module_id,
        container_name,
        method_name,
        false,
        FctImplementation::Intrinsic(intrinsic),
    )
}

fn native_static(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    fctptr: *const u8,
) {
    common_method(
        sa,
        module_id,
        container_name,
        method_name,
        true,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn intrinsic_static(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    intrinsic: Intrinsic,
) {
    common_method(
        sa,
        module_id,
        container_name,
        method_name,
        true,
        FctImplementation::Intrinsic(intrinsic),
    );
}

fn common_method(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    is_static: bool,
    implementation: FctImplementation,
) -> FctDefinitionId {
    let sym = resolve_name(sa, container_name, module_id);

    match sym {
        Sym::Class(cls_id) => {
            internal_class_method(sa, cls_id, method_name, is_static, implementation)
        }

        Sym::Struct(struct_id) => {
            let struct_ = sa.structs.idx(struct_id);
            let struct_ = struct_.read();
            internal_extension_method(
                sa,
                &struct_.extensions,
                method_name,
                is_static,
                implementation,
            )
        }
        Sym::Enum(enum_id) => {
            let enum_ = &sa.enums[enum_id].read();
            internal_extension_method(
                sa,
                &enum_.extensions,
                method_name,
                is_static,
                implementation,
            )
        }

        _ => panic!("unexpected type"),
    }
}

fn internal_class_method(
    sa: &SemAnalysis,
    cls_id: ClassDefinitionId,
    name: &str,
    is_static: bool,
    kind: FctImplementation,
) -> FctDefinitionId {
    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();

    internal_extension_method(sa, &cls.extensions, name, is_static, kind)
}

fn internal_extension_method(
    sa: &SemAnalysis,
    extensions: &[ExtensionDefinitionId],
    name_as_string: &str,
    is_static: bool,
    kind: FctImplementation,
) -> FctDefinitionId {
    let name = sa.interner.intern(name_as_string);

    for &extension_id in extensions {
        let extension = sa.extensions[extension_id].read();

        let table = if is_static {
            &extension.static_names
        } else {
            &extension.instance_names
        };

        if let Some(&method_id) = table.get(&name) {
            let fct = sa.fcts.idx(method_id);
            let mut fct = fct.write();

            match kind {
                FctImplementation::Intrinsic(intrinsic) => fct.intrinsic = Some(intrinsic),
                FctImplementation::Native(address) => {
                    fct.native_pointer = Some(address);
                }
            }
            fct.internal_resolved = true;
            return method_id;
        }
    }

    panic!("method {} not found!", name_as_string)
}

#[cfg(test)]
mod tests {
    use crate::language::tests::*;

    #[test]
    fn builtin_functions() {
        ok("fun f(): Unit { assert(true); }");
        ok("fun f(): Unit { print(\"test\"); }");
        ok("fun f(): Unit { println(\"test\"); }");
    }
}
