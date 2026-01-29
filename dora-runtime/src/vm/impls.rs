use crate::vm::{BytecodeTypeExt, VM, block_matches_ty, specialize_bty_array};
use dora_bytecode::{
    BytecodeTraitType, BytecodeType, BytecodeTypeArray, FunctionId, ImplId, TypeParamBound,
    TypeParamData,
};

pub fn find_trait_impl(
    vm: &VM,
    fct_id: FunctionId,
    trait_ty: BytecodeTraitType,
    object_type: BytecodeType,
) -> (FunctionId, BytecodeTypeArray) {
    debug_assert!(object_type.is_concrete_type());

    let type_param_data = TypeParamData {
        names: Vec::new(),
        container_count: 0,
        bounds: Vec::new(),
    };

    let (impl_id, bindings) = find_impl(vm, object_type, &type_param_data, trait_ty.clone())
        .expect("no impl found for generic trait method call");

    let impl_ = vm.impl_(impl_id);
    let impl_trait_id = impl_.trait_ty.trait_id;

    assert_eq!(impl_trait_id, trait_ty.trait_id);

    let trait_ = vm.trait_(trait_ty.trait_id);
    let trait_method_idx = trait_
        .methods
        .iter()
        .position(|mid| *mid == fct_id)
        .expect("trait method id not found");

    let impl_fct_id = impl_.methods[trait_method_idx];

    (impl_fct_id, bindings)
}

pub fn find_trait_ty_impl(
    vm: &VM,
    trait_ty: BytecodeTraitType,
    object_type: BytecodeType,
) -> Option<(ImplId, BytecodeTypeArray)> {
    let type_param_data = TypeParamData {
        names: Vec::new(),
        container_count: 0,
        bounds: Vec::new(),
    };

    find_impl(vm, object_type, &type_param_data, trait_ty)
}
pub fn find_impl(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    trait_ty: BytecodeTraitType,
) -> Option<(ImplId, BytecodeTypeArray)> {
    let trait_id = trait_ty.trait_id;

    for (impl_id, impl_) in vm.program.impls.iter().enumerate() {
        let impl_id: ImplId = impl_id.into();

        if impl_.trait_ty.trait_id != trait_id {
            continue;
        }

        if let Some(binding) =
            impl_block_matches_ty(vm, check_ty.clone(), check_type_param_defs, impl_id)
        {
            let impl_trait_ty_params = specialize_bty_array(&impl_.trait_ty.type_params, &binding);

            if impl_trait_ty_params != trait_ty.type_params {
                continue;
            }

            return Some((impl_id, binding));
        }
    }

    None
}

fn impl_block_matches_ty(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    impl_id: ImplId,
) -> Option<BytecodeTypeArray> {
    let impl_ = vm.impl_(impl_id);
    block_matches_ty(
        vm,
        check_ty,
        check_type_param_defs,
        impl_.extended_ty.clone(),
        &impl_.type_params,
    )
}

pub fn ty_implements_trait(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    trait_ty: BytecodeTraitType,
) -> bool {
    let trait_id = trait_ty.trait_id;

    if check_ty.is_zeroable_primitive() && vm.known.zero_trait_id() == trait_id {
        assert!(trait_ty.type_params.is_empty());
        return true;
    }

    match check_ty.clone() {
        BytecodeType::Bool
        | BytecodeType::UInt8
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Struct(_, _)
        | BytecodeType::Enum(_, _)
        | BytecodeType::Class(_, _)
        | BytecodeType::Tuple(_)
        | BytecodeType::Unit
        | BytecodeType::TraitObject(..)
        | BytecodeType::Lambda(..) => {
            find_impl(vm, check_ty, check_type_param_defs, trait_ty).is_some()
        }

        BytecodeType::TypeParam(tp_id) => {
            tp_implements_trait(&check_type_param_defs, tp_id, trait_ty)
        }

        BytecodeType::TypeAlias(..)
        | BytecodeType::Assoc { .. }
        | BytecodeType::GenericAssoc { .. }
        | BytecodeType::Ptr
        | BytecodeType::This => unreachable!(),
    }
}

pub fn tp_implements_trait(
    type_param_defs: &TypeParamData,
    tp_id: u32,
    trait_ty: BytecodeTraitType,
) -> bool {
    let ty = BytecodeType::TypeParam(tp_id);

    for bound in &type_param_defs.bounds {
        if bound.ty == ty && bound.trait_ty == trait_ty {
            return true;
        }
    }

    false
}

pub fn bounds_for_tp(type_param_defs: &TypeParamData, id: u32) -> TypeParamBoundsIter<'_> {
    TypeParamBoundsIter {
        bounds: &type_param_defs.bounds,
        current: 0,
        id,
    }
}

pub struct TypeParamBoundsIter<'a> {
    bounds: &'a [TypeParamBound],
    current: usize,
    id: u32,
}

impl<'a> Iterator for TypeParamBoundsIter<'a> {
    type Item = BytecodeTraitType;

    fn next(&mut self) -> Option<BytecodeTraitType> {
        while self.current < self.bounds.len() {
            let bound = &self.bounds[self.current];
            if bound.ty == BytecodeType::TypeParam(self.id) {
                self.current += 1;
                return Some(bound.trait_ty.clone());
            }

            self.current += 1;
        }

        None
    }
}
