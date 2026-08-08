use fixedbitset::FixedBitSet;

use crate::{
    BytecodeBody, BytecodeInstruction, BytecodeOffset, BytecodeReader, BytecodeTraitType,
    BytecodeType, BytecodeTypeArray, ClassId, ConstPoolEntry, ConstPoolIdx, FunctionData,
    FunctionId, FunctionKind, Program, Register, TraitId, TypeParamData, resolve_path,
};

pub fn verify(program: &Program) {
    verify_program_types(program);
    verify_function_intrinsics(program);
    let array_class_id = resolve_stdlib_class(program, "collections::Array");
    let string_class_id = resolve_stdlib_class(program, "string::String");

    for (function_idx, function) in program.functions.iter().enumerate() {
        if let Some(bytecode) = &function.bytecode {
            Verifier::new(
                program,
                function_idx.into(),
                bytecode,
                array_class_id,
                string_class_id,
            )
            .verify();
        }
    }
}

fn verify_function_intrinsics(program: &Program) {
    let mut functions_with_intrinsics = FixedBitSet::with_capacity(program.functions.len());

    for entry in &program.function_intrinsics {
        let function_id = entry.function_id.index();
        assert!(
            function_id < program.functions.len(),
            "intrinsic references invalid function {:?}",
            entry.function_id
        );
        assert!(
            !functions_with_intrinsics.contains(function_id),
            "duplicate intrinsic for function {:?}",
            entry.function_id
        );
        functions_with_intrinsics.insert(function_id);
    }
}

fn resolve_stdlib_class(program: &Program, path: &str) -> ClassId {
    let package_name = &program.package(program.stdlib_package_id).name;
    resolve_path(program, &format!("{}::{}", package_name, path))
        .unwrap()
        .class_id()
        .unwrap()
}

struct Verifier<'a> {
    program: &'a Program,
    function_id: FunctionId,
    bytecode: &'a BytecodeBody,
    array_class_id: ClassId,
    string_class_id: ClassId,
    offset: BytecodeOffset,
    instruction_offsets: FixedBitSet,
    jump_targets: Vec<(BytecodeOffset, u32, JumpDirection)>,
}

#[derive(Copy, Clone)]
enum JumpDirection {
    Forward,
    Backward,
}

impl<'a> Verifier<'a> {
    fn new(
        program: &'a Program,
        function_id: FunctionId,
        bytecode: &'a BytecodeBody,
        array_class_id: ClassId,
        string_class_id: ClassId,
    ) -> Verifier<'a> {
        Verifier {
            program,
            function_id,
            bytecode,
            array_class_id,
            string_class_id,
            offset: BytecodeOffset(0),
            instruction_offsets: FixedBitSet::with_capacity(bytecode.code().len()),
            jump_targets: Vec::new(),
        }
    }

    fn verify(mut self) {
        self.verify_signature();
        self.verify_types();

        for (offset, _, instruction) in BytecodeReader::new(self.bytecode.code()) {
            self.offset = BytecodeOffset(offset.try_into().expect("bytecode offset overflow"));
            let offset = self.offset.to_usize();
            assert!(!self.instruction_offsets.contains(offset));
            self.instruction_offsets.insert(offset);
            self.verify_instruction(instruction);
        }

        self.verify_jump_targets();
    }

    fn verify_types(&self) {
        let type_param_count = self
            .program
            .fct(self.function_id)
            .type_params
            .type_param_count();
        for ty in self.bytecode.registers() {
            verify_transient_type(ty, type_param_count);
            // `This` is only valid in trait-level metadata; executable bodies use `$Self`.
            assert!(!type_contains_this(ty));
        }
        for entry in self.bytecode.const_pool_entries() {
            verify_const_pool_entry(entry, type_param_count);
            // Constant-pool types belong to the body and therefore also use `$Self`.
            assert!(!const_pool_entry_contains_this(entry));
        }
    }

    fn verify_signature(&self) {
        let function = self.program.fct(self.function_id);
        let type_params = BytecodeTypeArray::new(
            (0..function.type_params.type_param_count())
                .map(|idx| {
                    BytecodeType::TypeParam(idx.try_into().expect("type parameter overflow"))
                })
                .collect(),
        );
        let self_type = match function.kind {
            FunctionKind::Impl(id) => Some(&self.program.impl_(id).extended_ty),
            FunctionKind::Extension(id) => Some(&self.program.extension(id).extended_ty),
            FunctionKind::Trait(_) | FunctionKind::Function => None,
        };
        let mut params = specialize_function_params(function, &type_params, self_type);

        if function.is_variadic {
            let element_type = params
                .last_mut()
                .expect("variadic function without parameter");
            *element_type = BytecodeType::Class(
                self.array_class_id,
                BytecodeTypeArray::new(vec![element_type.clone()]),
            );
        }

        assert!(params.len() <= self.bytecode.registers().len());
        for (idx, expected) in params.iter().enumerate() {
            self.assert_type(Register(idx), expected);
        }
    }

    fn verify_instruction(&mut self, instruction: BytecodeInstruction) {
        match instruction {
            BytecodeInstruction::Add { dest, lhs, rhs }
            | BytecodeInstruction::Mul { dest, lhs, rhs }
            | BytecodeInstruction::Div { dest, lhs, rhs } => {
                self.assert_same_types(dest, lhs, rhs);
                assert!(self.ty(dest).is_any_float());
            }

            BytecodeInstruction::Sub { dest, lhs, rhs }
            | BytecodeInstruction::Mod { dest, lhs, rhs } => {
                self.assert_same_types(dest, lhs, rhs);
                assert!(matches!(
                    self.ty(dest),
                    BytecodeType::Int32
                        | BytecodeType::Int64
                        | BytecodeType::Float32
                        | BytecodeType::Float64
                ));
            }

            BytecodeInstruction::Neg { dest, src } => {
                self.assert_same_type(dest, src);
                assert!(self.ty(dest).is_any_float());
            }

            BytecodeInstruction::CheckedAdd { dest, lhs, rhs }
            | BytecodeInstruction::CheckedSub { dest, lhs, rhs }
            | BytecodeInstruction::CheckedMul { dest, lhs, rhs }
            | BytecodeInstruction::CheckedDiv { dest, lhs, rhs }
            | BytecodeInstruction::CheckedMod { dest, lhs, rhs } => {
                self.assert_same_types(dest, lhs, rhs);
                self.assert_integer(dest);
            }

            BytecodeInstruction::CheckedNeg { dest, src } => {
                self.assert_same_type(dest, src);
                self.assert_integer(dest);
            }

            BytecodeInstruction::And { dest, lhs, rhs }
            | BytecodeInstruction::Or { dest, lhs, rhs }
            | BytecodeInstruction::Xor { dest, lhs, rhs } => {
                self.assert_same_types(dest, lhs, rhs);
                self.assert_integer(dest);
            }

            BytecodeInstruction::Not { dest, src } => {
                self.assert_same_type(dest, src);
                assert!(matches!(
                    self.ty(dest),
                    BytecodeType::Bool | BytecodeType::Int32 | BytecodeType::Int64
                ));
            }

            BytecodeInstruction::Shl { dest, lhs, rhs }
            | BytecodeInstruction::Shr { dest, lhs, rhs }
            | BytecodeInstruction::Sar { dest, lhs, rhs } => {
                self.assert_same_type(dest, lhs);
                self.assert_integer(dest);
                self.assert_type(rhs, &BytecodeType::Int32);
            }

            BytecodeInstruction::Mov { dest, src } => {
                self.assert_assignable_type(self.ty(dest), self.ty(src));
            }

            BytecodeInstruction::LoadEnumElement { dest, src, idx } => {
                let ConstPoolEntry::EnumElement(enum_id, type_params, variant_idx, element_idx) =
                    self.const_pool(idx)
                else {
                    panic!("expected EnumElement constant pool entry");
                };
                let variant = &self.program.enum_(*enum_id).variants[*variant_idx as usize];
                let field = &variant.fields[*element_idx as usize];
                self.assert_type(src, &BytecodeType::Enum(*enum_id, type_params.clone()));
                self.assert_assignable_type(
                    self.ty(dest),
                    &specialize_type(&field.ty, type_params),
                );
            }

            BytecodeInstruction::LoadEnumVariant { dest, src, idx } => {
                let ConstPoolEntry::Enum(enum_id, type_params) = self.const_pool(idx) else {
                    panic!("expected Enum constant pool entry");
                };
                self.program.enum_(*enum_id);
                self.assert_type(src, &BytecodeType::Enum(*enum_id, type_params.clone()));
                self.assert_type(dest, &BytecodeType::Int32);
            }

            BytecodeInstruction::LoadField { dest, obj, field } => {
                let (object_ty, field_ty) = self.field_types(field);
                self.assert_type(obj, &object_ty);
                self.assert_assignable_type(self.ty(dest), &field_ty);
            }

            BytecodeInstruction::StoreField { src, obj, field } => {
                let (object_ty, field_ty) = self.field_types(field);
                self.assert_type(obj, &object_ty);
                self.assert_field_type(src, &field_ty);
            }

            BytecodeInstruction::LoadGlobal { dest, global_id } => {
                self.assert_assignable_type(self.ty(dest), &self.program.global(global_id).ty);
            }

            BytecodeInstruction::StoreGlobal { src, global_id } => {
                self.assert_type(src, &self.program.global(global_id).ty);
            }

            BytecodeInstruction::GetGlobalRef { dest, global_id } => {
                let global_ty = self.program.global(global_id).ty.clone();
                self.assert_type(dest, &BytecodeType::Ref(Box::new(global_ty)));
            }

            BytecodeInstruction::LoadConst { dest, const_id } => {
                self.assert_assignable_type(self.ty(dest), &self.program.const_(const_id).ty);
            }

            BytecodeInstruction::ConstTrue { dest } | BytecodeInstruction::ConstFalse { dest } => {
                self.assert_type(dest, &BytecodeType::Bool);
            }

            BytecodeInstruction::ConstUInt8 { dest, .. } => {
                self.assert_type(dest, &BytecodeType::UInt8);
            }

            BytecodeInstruction::ConstChar { dest, idx } => {
                assert!(matches!(self.const_pool(idx), ConstPoolEntry::Char(_)));
                self.assert_type(dest, &BytecodeType::Char);
            }

            BytecodeInstruction::ConstInt32 { dest, idx } => {
                assert!(matches!(self.const_pool(idx), ConstPoolEntry::Int32(_)));
                self.assert_type(dest, &BytecodeType::Int32);
            }

            BytecodeInstruction::ConstInt64 { dest, idx } => {
                assert!(matches!(self.const_pool(idx), ConstPoolEntry::Int64(_)));
                self.assert_type(dest, &BytecodeType::Int64);
            }

            BytecodeInstruction::ConstFloat32 { dest, idx } => {
                assert!(matches!(self.const_pool(idx), ConstPoolEntry::Float32(_)));
                self.assert_type(dest, &BytecodeType::Float32);
            }

            BytecodeInstruction::ConstFloat64 { dest, idx } => {
                assert!(matches!(self.const_pool(idx), ConstPoolEntry::Float64(_)));
                self.assert_type(dest, &BytecodeType::Float64);
            }

            BytecodeInstruction::ConstString { dest, idx } => {
                assert!(matches!(self.const_pool(idx), ConstPoolEntry::String(_)));
                assert!(matches!(self.ty(dest), BytecodeType::Class(..)));
            }

            BytecodeInstruction::TestIdentity { dest, lhs, rhs } => {
                self.assert_type(dest, &BytecodeType::Bool);
                self.assert_same_type(lhs, rhs);
                assert!(self.ty(lhs).is_reference_type() || *self.ty(lhs) == BytecodeType::Address);
            }

            BytecodeInstruction::TestEq { dest, lhs, rhs }
            | BytecodeInstruction::TestNe { dest, lhs, rhs }
            | BytecodeInstruction::TestGt { dest, lhs, rhs }
            | BytecodeInstruction::TestGe { dest, lhs, rhs }
            | BytecodeInstruction::TestLt { dest, lhs, rhs }
            | BytecodeInstruction::TestLe { dest, lhs, rhs } => {
                self.assert_type(dest, &BytecodeType::Bool);
                self.assert_same_type(lhs, rhs);
                assert!(matches!(
                    self.ty(lhs),
                    BytecodeType::Bool
                        | BytecodeType::UInt8
                        | BytecodeType::Char
                        | BytecodeType::Int32
                        | BytecodeType::Int64
                        | BytecodeType::Float32
                        | BytecodeType::Float64
                        | BytecodeType::Enum(..)
                ));
            }

            BytecodeInstruction::JumpLoop { offset } => {
                self.add_jump_target(offset, JumpDirection::Backward);
            }

            BytecodeInstruction::LoopStart => {}

            BytecodeInstruction::Jump { offset } => {
                self.add_jump_target(offset, JumpDirection::Forward);
            }

            BytecodeInstruction::JumpIfFalse { opnd, offset }
            | BytecodeInstruction::JumpIfTrue { opnd, offset } => {
                self.assert_type(opnd, &BytecodeType::Bool);
                self.add_jump_target(offset, JumpDirection::Forward);
            }

            BytecodeInstruction::Switch { opnd, idx } => {
                self.assert_type(opnd, &BytecodeType::Int32);
                let ConstPoolEntry::JumpTable {
                    targets,
                    default_target,
                } = self.const_pool(idx)
                else {
                    panic!("expected JumpTable constant pool entry");
                };
                let targets = targets.clone();
                let default_target = *default_target;
                for target in targets {
                    self.jump_targets
                        .push((self.offset, target, JumpDirection::Forward));
                }
                self.jump_targets
                    .push((self.offset, default_target, JumpDirection::Forward));
            }

            BytecodeInstruction::InvokeDirect {
                dest,
                fct,
                arguments,
            }
            | BytecodeInstruction::InvokeStatic {
                dest,
                fct,
                arguments,
            } => self.verify_invoke(dest, fct, &arguments),

            BytecodeInstruction::InvokeVirtual {
                dest,
                fct,
                arguments,
            } => self.verify_invoke_virtual(dest, fct, &arguments),

            BytecodeInstruction::InvokeGenericStatic {
                dest,
                fct,
                arguments,
            }
            | BytecodeInstruction::InvokeGenericDirect {
                dest,
                fct,
                arguments,
            } => self.verify_invoke_generic(dest, fct, &arguments),

            BytecodeInstruction::NewObject {
                dest,
                cls,
                arguments,
            } => {
                let (class_id, type_params) = self.class_entry(cls);
                self.assert_type(dest, &BytecodeType::Class(class_id, type_params.clone()));
                let class = self.program.class(class_id);

                if class.is_context {
                    assert!(arguments.is_empty());
                } else {
                    let fields = class
                        .fields
                        .iter()
                        .map(|field| specialize_type(&field.ty, &type_params))
                        .collect::<Vec<_>>();
                    self.assert_argument_types(&arguments, &fields);
                }
            }

            BytecodeInstruction::NewArray { dest, length, idx } => {
                let (class_id, type_params) = self.class_entry(idx);
                assert_eq!(class_id, self.array_class_id);
                assert_eq!(type_params.len(), 1);
                self.assert_type(dest, &BytecodeType::Class(class_id, type_params));
                self.assert_type(length, &BytecodeType::Int64);
            }

            BytecodeInstruction::NewTuple {
                dest,
                idx,
                arguments,
            } => {
                let ConstPoolEntry::Tuple(subtypes) = self.const_pool(idx) else {
                    panic!("expected Tuple constant pool entry");
                };
                self.assert_type(dest, &BytecodeType::Tuple(subtypes.clone()));
                let fields = subtypes
                    .iter()
                    .filter(|ty| !ty.is_unit())
                    .collect::<Vec<_>>();
                self.assert_argument_types(&arguments, &fields);
            }

            BytecodeInstruction::NewEnum {
                dest,
                idx,
                arguments,
            } => {
                let ConstPoolEntry::EnumVariant(enum_id, type_params, variant_idx) =
                    self.const_pool(idx)
                else {
                    panic!("expected EnumVariant constant pool entry");
                };
                let fields = self.program.enum_(*enum_id).variants[*variant_idx as usize]
                    .fields
                    .iter()
                    .map(|field| specialize_type(&field.ty, type_params))
                    .collect::<Vec<_>>();
                self.assert_type(dest, &BytecodeType::Enum(*enum_id, type_params.clone()));
                self.assert_argument_types(&arguments, &fields);
            }

            BytecodeInstruction::NewStruct {
                dest,
                idx,
                arguments,
            } => {
                let ConstPoolEntry::Struct(struct_id, type_params) = self.const_pool(idx) else {
                    panic!("expected Struct constant pool entry");
                };
                let fields = self
                    .program
                    .struct_(*struct_id)
                    .fields
                    .iter()
                    .map(|field| specialize_type(&field.ty, type_params))
                    .collect::<Vec<_>>();
                self.assert_type(dest, &BytecodeType::Struct(*struct_id, type_params.clone()));
                self.assert_argument_types(&arguments, &fields);
            }

            BytecodeInstruction::NewTraitObject { dest, src, idx } => {
                let ConstPoolEntry::TraitObject {
                    trait_ty,
                    actual_object_ty,
                } = self.const_pool(idx)
                else {
                    panic!("expected TraitObject constant pool entry");
                };
                assert!(trait_ty.is_trait_object());
                self.assert_type(dest, trait_ty);
                self.assert_type(src, actual_object_ty);
            }

            BytecodeInstruction::ArrayLength { dest, arr } => {
                self.assert_type(dest, &BytecodeType::Int64);
                self.indexed_element_type(arr);
            }

            BytecodeInstruction::LoadArray { dest, arr, idx } => {
                let element_type = self.indexed_element_type(arr);
                self.assert_assignable_type(self.ty(dest), &element_type);
                self.assert_type(idx, &BytecodeType::Int64);
            }

            BytecodeInstruction::StoreArray { src, arr, idx } => {
                self.assert_type(src, self.array_element_type(arr));
                self.assert_type(idx, &BytecodeType::Int64);
            }

            BytecodeInstruction::GetArrayRef { dest, arr, idx } => {
                let element_type = self.array_element_type(arr).clone();
                self.assert_type(dest, &BytecodeType::Ref(Box::new(element_type)));
                self.assert_type(idx, &BytecodeType::Int64);
            }

            BytecodeInstruction::GetFieldRef { dest, obj, field } => {
                let (object_ty, field_ty) = self.field_types(field);
                assert!(
                    self.ty(obj) == &object_ty
                        || self.ty(obj) == &BytecodeType::Ref(Box::new(object_ty))
                );
                self.assert_type(dest, &BytecodeType::Ref(Box::new(field_ty)));
            }

            BytecodeInstruction::StoreRef { src, reference } => {
                let referenced_ty = self.referenced_type(reference);
                self.assert_type(src, &referenced_ty);
            }

            BytecodeInstruction::LoadRef { dest, reference } => {
                let referenced_ty = self.referenced_type(reference);
                self.assert_assignable_type(self.ty(dest), &referenced_ty);
            }

            BytecodeInstruction::GetRegisterRef { dest, src } => {
                self.assert_type(dest, &BytecodeType::Ref(Box::new(self.ty(src).clone())));
            }

            BytecodeInstruction::Ret { opnd } => {
                let return_type = &self.program.fct(self.function_id).return_type;
                self.assert_assignable_type(return_type, self.ty(opnd));
            }
        }
    }

    fn verify_invoke(&self, dest: Register, fct: ConstPoolIdx, arguments: &[Register]) {
        let ConstPoolEntry::Fct(function_id, type_params) = self.const_pool(fct) else {
            panic!("expected Fct constant pool entry");
        };
        let function = self.program.fct(*function_id);
        assert_eq!(function.type_params.type_param_count(), type_params.len());
        self.assert_invoke_return_type(dest, &specialize_type(&function.return_type, type_params));
        let self_type = match function.kind {
            FunctionKind::Impl(id) => Some(specialize_type(
                &self.program.impl_(id).extended_ty,
                type_params,
            )),
            FunctionKind::Extension(id) => Some(specialize_type(
                &self.program.extension(id).extended_ty,
                type_params,
            )),
            FunctionKind::Trait(_) | FunctionKind::Function => None,
        };
        let params = specialize_function_params(function, type_params, self_type.as_ref());
        self.assert_call_argument_types(arguments, &params, function.is_variadic);
    }

    fn verify_invoke_virtual(&self, dest: Register, fct: ConstPoolIdx, arguments: &[Register]) {
        let ConstPoolEntry::TraitObjectMethod(trait_object_ty, function_id) = self.const_pool(fct)
        else {
            panic!("expected TraitObjectMethod constant pool entry");
        };
        let BytecodeType::TraitObject(trait_id, type_params, bindings) = trait_object_ty else {
            panic!("InvokeVirtual receiver type is not a trait object");
        };
        let function = self.program.fct(*function_id);
        assert!(matches!(function.kind, FunctionKind::Trait(id) if id == *trait_id));
        let type_params = if function.has_bytecode_self_type_param() {
            type_params.append(trait_object_ty.clone())
        } else {
            type_params.clone()
        };
        assert_eq!(function.type_params.type_param_count(), type_params.len());
        self.assert_invoke_return_type(
            dest,
            &specialize_type_for_trait_object(
                self.program,
                &function.return_type,
                &type_params,
                bindings,
            ),
        );
        assert!(!function.is_static);
        assert!(!function.params.is_empty());
        let (&receiver, arguments) = arguments
            .split_first()
            .expect("InvokeVirtual is missing its receiver");
        self.assert_type(receiver, trait_object_ty);
        let params = function
            .params
            .iter()
            .skip(1)
            .map(|param| {
                specialize_type_for_trait_object(self.program, param, &type_params, bindings)
            })
            .collect::<Vec<_>>();
        self.assert_call_argument_types(arguments, &params, function.is_variadic);
    }

    fn verify_invoke_generic(&self, dest: Register, fct: ConstPoolIdx, arguments: &[Register]) {
        let ConstPoolEntry::Generic {
            object_type,
            trait_ty,
            fct_id,
            fct_type_params,
        } = self.const_pool(fct)
        else {
            panic!("expected Generic constant pool entry");
        };
        let function = self.program.fct(*fct_id);
        assert!(matches!(function.kind, FunctionKind::Trait(id) if id == trait_ty.trait_id));
        let type_params = trait_ty.type_params.connect(fct_type_params);
        let type_params = if function.has_bytecode_self_type_param() {
            type_params.append(object_type.clone())
        } else {
            type_params
        };
        assert_eq!(function.type_params.type_param_count(), type_params.len());
        self.assert_invoke_return_type(
            dest,
            &specialize_type_with_self(&function.return_type, &type_params, Some(object_type)),
        );
        let params = specialize_function_params(function, &type_params, Some(object_type));
        self.assert_call_argument_types(arguments, &params, function.is_variadic);
    }

    fn class_entry(&self, idx: ConstPoolIdx) -> (ClassId, BytecodeTypeArray) {
        let ConstPoolEntry::Class(class_id, type_params) = self.const_pool(idx) else {
            panic!("expected Class constant pool entry");
        };
        assert_eq!(
            self.program.class(*class_id).type_params.type_param_count(),
            type_params.len()
        );
        (*class_id, type_params.clone())
    }

    fn field_types(&self, idx: ConstPoolIdx) -> (BytecodeType, BytecodeType) {
        match self.const_pool(idx) {
            ConstPoolEntry::ClassField(class_id, type_params, field_id) => {
                let class = self.program.class(*class_id);
                let field = &class.fields[*field_id as usize];
                (
                    BytecodeType::Class(*class_id, type_params.clone()),
                    specialize_type(&field.ty, type_params),
                )
            }
            ConstPoolEntry::StructField(struct_id, type_params, field_id) => {
                let struct_ = self.program.struct_(*struct_id);
                let field = &struct_.fields[*field_id as usize];
                (
                    BytecodeType::Struct(*struct_id, type_params.clone()),
                    specialize_type(&field.ty, type_params),
                )
            }
            ConstPoolEntry::TupleElement(tuple_ty, element_idx) => {
                let BytecodeType::Tuple(subtypes) = tuple_ty else {
                    panic!("TupleElement has non-tuple type");
                };
                (tuple_ty.clone(), subtypes[*element_idx as usize].clone())
            }
            _ => panic!("expected field constant pool entry"),
        }
    }

    fn referenced_type(&self, reference: Register) -> BytecodeType {
        let BytecodeType::Ref(inner) = self.ty(reference) else {
            panic!("reference register does not have Ref type");
        };
        inner.as_ref().clone()
    }

    fn array_element_type(&self, array: Register) -> &BytecodeType {
        let BytecodeType::Class(class_id, type_params) = self.ty(array) else {
            panic!("array register does not have class type");
        };
        assert_eq!(*class_id, self.array_class_id);
        assert_eq!(type_params.len(), 1);
        &type_params[0]
    }

    fn indexed_element_type(&self, indexed: Register) -> BytecodeType {
        let BytecodeType::Class(class_id, type_params) = self.ty(indexed) else {
            panic!("indexed register does not have class type");
        };

        if *class_id == self.array_class_id {
            assert_eq!(type_params.len(), 1);
            type_params[0].clone()
        } else {
            assert_eq!(*class_id, self.string_class_id);
            assert!(type_params.is_empty());
            BytecodeType::UInt8
        }
    }

    fn assert_call_argument_types(
        &self,
        arguments: &[Register],
        expected_types: &[BytecodeType],
        variadic: bool,
    ) {
        assert!(!variadic || !expected_types.is_empty());
        assert_eq!(arguments.len(), expected_types.len());
        for (idx, (&argument, expected)) in arguments.iter().zip(expected_types).enumerate() {
            if variadic && idx + 1 == expected_types.len() {
                assert!(types_match(self.array_element_type(argument), expected));
            } else {
                self.assert_type(argument, expected);
            }
        }
    }

    fn assert_argument_types(&self, arguments: &[Register], expected: &[BytecodeType]) {
        assert_eq!(arguments.len(), expected.len());
        for (&argument, expected) in arguments.iter().zip(expected) {
            self.assert_type(argument, expected);
        }
    }

    fn assert_same_types(&self, first: Register, second: Register, third: Register) {
        self.assert_same_type(first, second);
        self.assert_same_type(first, third);
    }

    fn assert_same_type(&self, first: Register, second: Register) {
        assert_eq!(self.ty(first), self.ty(second));
    }

    fn assert_type(&self, register: Register, expected: &BytecodeType) {
        assert!(
            types_match(self.ty(register), expected),
            "register type {:?} does not match expected type {:?} in function {} at {:?}",
            self.ty(register),
            expected,
            self.program.fct(self.function_id).name,
            self.offset,
        );
    }

    fn assert_assignable_type(&self, dest: &BytecodeType, source: &BytecodeType) {
        // Never is the bottom type and can be assigned to any destination type.
        assert!(
            source.is_never() || types_match(dest, source),
            "source type {:?} is not assignable to destination type {:?} in function {} at {:?}",
            source,
            dest,
            self.program.fct(self.function_id).name,
            self.offset,
        );
    }

    fn assert_invoke_return_type(&self, dest: Register, expected: &BytecodeType) {
        // Unit is used as the destination when the call result is discarded.
        if !self.ty(dest).is_unit() {
            self.assert_assignable_type(self.ty(dest), expected);
        }
    }

    fn assert_field_type(&self, register: Register, expected: &BytecodeType) {
        let actual = self.ty(register);
        assert!(
            types_match(actual, expected),
            "register type {:?} does not match expected field type {:?} in function {} at {:?}",
            actual,
            expected,
            self.program.fct(self.function_id).name,
            self.offset,
        );
    }

    fn assert_integer(&self, register: Register) {
        assert!(matches!(
            self.ty(register),
            BytecodeType::Int32 | BytecodeType::Int64
        ));
    }

    fn ty(&self, register: Register) -> &BytecodeType {
        &self.bytecode.registers()[register.to_usize()]
    }

    fn const_pool(&self, idx: ConstPoolIdx) -> &ConstPoolEntry {
        &self.bytecode.const_pool_entries()[idx.0 as usize]
    }

    fn add_jump_target(&mut self, distance: u32, direction: JumpDirection) {
        let target = match direction {
            JumpDirection::Forward => self.offset.to_u32().checked_add(distance),
            JumpDirection::Backward => self.offset.to_u32().checked_sub(distance),
        }
        .expect("jump target overflow");
        self.jump_targets.push((self.offset, target, direction));
    }

    fn verify_jump_targets(&self) {
        for &(origin, target, direction) in &self.jump_targets {
            match direction {
                JumpDirection::Forward => assert!(target > origin.to_u32()),
                JumpDirection::Backward => assert!(target < origin.to_u32()),
            }
            assert!(self.instruction_offsets.contains(target as usize));
            if matches!(direction, JumpDirection::Backward) {
                assert!(
                    self.bytecode
                        .read_opcode(BytecodeOffset(target))
                        .is_loop_start()
                );
            }
        }
    }
}

fn types_match(actual: &BytecodeType, expected: &BytecodeType) -> bool {
    if actual == expected {
        return true;
    }

    match (actual, expected) {
        (_, BytecodeType::Assoc { .. }) => true,
        (BytecodeType::Tuple(actual), BytecodeType::Tuple(expected)) => {
            type_arrays_match(actual, expected)
        }
        (BytecodeType::Enum(actual_id, actual), BytecodeType::Enum(expected_id, expected)) => {
            actual_id == expected_id && type_arrays_match(actual, expected)
        }
        (BytecodeType::Struct(actual_id, actual), BytecodeType::Struct(expected_id, expected)) => {
            actual_id == expected_id && type_arrays_match(actual, expected)
        }
        (BytecodeType::Class(actual_id, actual), BytecodeType::Class(expected_id, expected)) => {
            actual_id == expected_id && type_arrays_match(actual, expected)
        }
        (
            BytecodeType::TraitObject(actual_id, actual_params, actual_bindings),
            BytecodeType::TraitObject(expected_id, expected_params, expected_bindings),
        ) => {
            actual_id == expected_id
                && type_arrays_match(actual_params, expected_params)
                && type_arrays_match(actual_bindings, expected_bindings)
        }
        (BytecodeType::Ref(actual), BytecodeType::Ref(expected)) => types_match(actual, expected),
        _ => false,
    }
}

fn type_arrays_match(actual: &BytecodeTypeArray, expected: &BytecodeTypeArray) -> bool {
    actual.len() == expected.len()
        && actual
            .iter()
            .zip(expected.iter())
            .all(|(actual, expected)| types_match(&actual, &expected))
}

fn specialize_type(ty: &BytecodeType, type_params: &BytecodeTypeArray) -> BytecodeType {
    specialize_type_with_self(ty, type_params, None)
}

fn specialize_type_with_self(
    ty: &BytecodeType,
    type_params: &BytecodeTypeArray,
    self_type: Option<&BytecodeType>,
) -> BytecodeType {
    match ty {
        BytecodeType::TypeParam(id) => type_params[*id as usize].clone(),
        BytecodeType::This => self_type.cloned().unwrap_or(BytecodeType::This),
        BytecodeType::Tuple(types) => {
            BytecodeType::Tuple(specialize_types_with_self(types, type_params, self_type))
        }
        BytecodeType::Enum(id, types) => BytecodeType::Enum(
            *id,
            specialize_types_with_self(types, type_params, self_type),
        ),
        BytecodeType::Struct(id, types) => BytecodeType::Struct(
            *id,
            specialize_types_with_self(types, type_params, self_type),
        ),
        BytecodeType::Class(id, types) => BytecodeType::Class(
            *id,
            specialize_types_with_self(types, type_params, self_type),
        ),
        BytecodeType::TraitObject(id, types, bindings) => BytecodeType::TraitObject(
            *id,
            specialize_types_with_self(types, type_params, self_type),
            specialize_types_with_self(bindings, type_params, self_type),
        ),
        BytecodeType::Assoc {
            ty,
            trait_ty,
            assoc_id,
        } => BytecodeType::Assoc {
            ty: Box::new(specialize_type_with_self(ty, type_params, self_type)),
            trait_ty: specialize_trait_type_with_self(trait_ty, type_params, self_type),
            assoc_id: *assoc_id,
        },
        BytecodeType::Ref(inner) => BytecodeType::Ref(Box::new(specialize_type_with_self(
            inner,
            type_params,
            self_type,
        ))),
        _ => ty.clone(),
    }
}

fn specialize_types_with_self(
    types: &BytecodeTypeArray,
    type_params: &BytecodeTypeArray,
    self_type: Option<&BytecodeType>,
) -> BytecodeTypeArray {
    BytecodeTypeArray::new(
        types
            .iter()
            .map(|ty| specialize_type_with_self(&ty, type_params, self_type))
            .collect(),
    )
}

fn specialize_trait_type_with_self(
    trait_ty: &BytecodeTraitType,
    type_params: &BytecodeTypeArray,
    self_type: Option<&BytecodeType>,
) -> BytecodeTraitType {
    BytecodeTraitType {
        trait_id: trait_ty.trait_id,
        type_params: specialize_types_with_self(&trait_ty.type_params, type_params, self_type),
        bindings: trait_ty
            .bindings
            .iter()
            .map(|(id, ty)| (*id, specialize_type_with_self(ty, type_params, self_type)))
            .collect(),
    }
}

fn uses_ref_self(function: &FunctionData, self_type: Option<&BytecodeType>) -> bool {
    if function.is_static || !function.is_mutating {
        return false;
    }

    // A mutating trait method always receives self by reference. This lets the
    // implementation replace the value stored in a trait object for any type,
    // including primitive and reference types.
    if function.trait_method_impl.is_some() || matches!(function.kind, FunctionKind::Trait(_)) {
        return true;
    }

    // Inherent mutating methods only need a reference for value types whose
    // fields are stored inline. Classes already have reference semantics.
    matches!(
        self_type,
        Some(BytecodeType::Struct(..) | BytecodeType::Tuple(..))
    )
}

fn specialize_function_params(
    function: &FunctionData,
    type_params: &BytecodeTypeArray,
    self_type: Option<&BytecodeType>,
) -> Vec<BytecodeType> {
    let mut params = function
        .params
        .iter()
        .map(|param| specialize_type_with_self(param, type_params, self_type))
        .collect::<Vec<_>>();

    if uses_ref_self(function, self_type) {
        assert!(!params.is_empty());
        let receiver_ty = self_type.cloned().unwrap_or_else(|| params[0].clone());
        params[0] = BytecodeType::Ref(Box::new(receiver_ty));
    }

    params
}

fn specialize_type_for_trait_object(
    program: &Program,
    ty: &BytecodeType,
    type_params: &BytecodeTypeArray,
    bindings: &BytecodeTypeArray,
) -> BytecodeType {
    match ty {
        BytecodeType::TypeParam(id) => type_params[*id as usize].clone(),
        BytecodeType::Tuple(types) => BytecodeType::Tuple(specialize_types_for_trait_object(
            program,
            types,
            type_params,
            bindings,
        )),
        BytecodeType::Enum(id, types) => BytecodeType::Enum(
            *id,
            specialize_types_for_trait_object(program, types, type_params, bindings),
        ),
        BytecodeType::Struct(id, types) => BytecodeType::Struct(
            *id,
            specialize_types_for_trait_object(program, types, type_params, bindings),
        ),
        BytecodeType::Class(id, types) => BytecodeType::Class(
            *id,
            specialize_types_for_trait_object(program, types, type_params, bindings),
        ),
        BytecodeType::TraitObject(id, types, inner_bindings) => BytecodeType::TraitObject(
            *id,
            specialize_types_for_trait_object(program, types, type_params, bindings),
            specialize_types_for_trait_object(program, inner_bindings, type_params, bindings),
        ),
        BytecodeType::Assoc { assoc_id, .. } => {
            bindings[program.alias(*assoc_id).idx_in_trait()].clone()
        }
        BytecodeType::Ref(inner) => BytecodeType::Ref(Box::new(specialize_type_for_trait_object(
            program,
            inner,
            type_params,
            bindings,
        ))),
        BytecodeType::This | BytecodeType::TypeAlias(_) => {
            panic!("unexpected type in trait-object function signature")
        }
        _ => ty.clone(),
    }
}

fn specialize_types_for_trait_object(
    program: &Program,
    types: &BytecodeTypeArray,
    type_params: &BytecodeTypeArray,
    bindings: &BytecodeTypeArray,
) -> BytecodeTypeArray {
    BytecodeTypeArray::new(
        types
            .iter()
            .map(|ty| specialize_type_for_trait_object(program, &ty, type_params, bindings))
            .collect(),
    )
}

fn verify_program_types(program: &Program) {
    for function in &program.functions {
        let type_param_count = verify_type_params(&function.type_params);
        for (idx, ty) in function.params.iter().enumerate() {
            let is_variadic_param = function.is_variadic && idx + 1 == function.params.len();
            if is_variadic_param {
                verify_type(ty, type_param_count);
            } else {
                verify_transient_type(ty, type_param_count);
            }
        }
        verify_transient_type(&function.return_type, type_param_count);

        if function.bytecode.is_some() {
            verify_executable_function_signature_has_no_this(function);
        }
    }

    verify_trait_functions(program);

    for global in &program.globals {
        verify_type(&global.ty, 0);
    }

    for const_ in &program.consts {
        verify_type(&const_.ty, 0);
    }

    for class in &program.classes {
        let type_param_count = verify_type_params(&class.type_params);
        for field in &class.fields {
            verify_type(&field.ty, type_param_count);
        }
    }

    for struct_ in &program.structs {
        let type_param_count = verify_type_params(&struct_.type_params);
        for field in &struct_.fields {
            verify_type(&field.ty, type_param_count);
        }
    }

    for enum_ in &program.enums {
        let type_param_count = verify_type_params(&enum_.type_params);
        for variant in &enum_.variants {
            for field in &variant.fields {
                verify_type(&field.ty, type_param_count);
            }
        }
    }

    for trait_ in &program.traits {
        verify_type_params(&trait_.type_params);
    }

    for impl_ in &program.impls {
        let type_param_count = verify_type_params(&impl_.type_params);
        verify_trait_type(&impl_.trait_ty, type_param_count);
        verify_type(&impl_.extended_ty, type_param_count);
    }

    for extension in &program.extensions {
        let type_param_count = verify_type_params(&extension.type_params);
        verify_type(&extension.extended_ty, type_param_count);
    }

    for alias in &program.aliases {
        let type_param_count = verify_type_params(&alias.type_params);
        if let Some(ty) = &alias.ty {
            verify_type(ty, type_param_count);
        }
    }
}

fn verify_executable_function_signature_has_no_this(function: &FunctionData) {
    assert!(function.params.iter().all(|ty| !type_contains_this(ty)));
    assert!(!type_contains_this(&function.return_type));
    for bound in &function.type_params.bounds {
        assert!(!type_contains_this(&bound.ty));
        assert!(!trait_type_contains_this(&bound.trait_ty));
    }
}

fn verify_trait_functions(program: &Program) {
    let mut trait_method_owners = vec![None; program.functions.len()];
    let mut default_body_owners = vec![None; program.functions.len()];

    for (trait_idx, trait_) in program.traits.iter().enumerate() {
        let trait_id: TraitId = trait_idx.into();
        let mut virtual_methods = FixedBitSet::with_capacity(program.functions.len());

        for &method_id in &trait_.methods {
            assert!(
                method_id.index() < program.functions.len(),
                "trait references invalid method {method_id:?}"
            );
            let method = program.fct(method_id);
            assert!(matches!(method.kind, FunctionKind::Trait(id) if id == trait_id));
            assert!(
                method.bytecode.is_none(),
                "default method body must not appear in trait method metadata"
            );
            assert!(
                trait_method_owners[method_id.index()]
                    .replace(trait_id)
                    .is_none(),
                "trait method is listed more than once"
            );
        }

        for &method_id in &trait_.virtual_methods {
            assert!(
                method_id.index() < program.functions.len(),
                "trait references invalid virtual method {method_id:?}"
            );
            assert_eq!(trait_method_owners[method_id.index()], Some(trait_id));
            assert!(
                !virtual_methods.contains(method_id.index()),
                "virtual method is listed more than once"
            );
            virtual_methods.insert(method_id.index());
        }

        for &method_id in &trait_.methods {
            assert_eq!(
                virtual_methods.contains(method_id.index()),
                !program.fct(method_id).is_trait_object_ignore
            );
        }
    }

    for (declaration_idx, declaration) in program.functions.iter().enumerate() {
        let Some(body_id) = declaration.default_method_body else {
            continue;
        };
        assert!(
            body_id.index() < program.functions.len(),
            "default method references invalid body {body_id:?}"
        );
        let declaration_id: FunctionId = declaration_idx.into();
        assert!(
            default_body_owners[body_id.index()]
                .replace(declaration_id)
                .is_none(),
            "default method body is shared by multiple declarations"
        );
        verify_default_method_body(program, declaration, body_id);
    }

    for (function_idx, function) in program.functions.iter().enumerate() {
        let function_id: FunctionId = function_idx.into();

        if let FunctionKind::Trait(trait_id) = function.kind {
            assert!(
                trait_id.index() < program.traits.len(),
                "function references invalid trait {trait_id:?}"
            );
            if function.bytecode.is_some() {
                assert!(
                    default_body_owners[function_idx].is_some(),
                    "orphaned default method body {function_id:?}"
                );
                assert!(trait_method_owners[function_idx].is_none());
            } else {
                assert_eq!(trait_method_owners[function_idx], Some(trait_id));
                assert!(default_body_owners[function_idx].is_none());
            }
        } else {
            assert!(trait_method_owners[function_idx].is_none());
            assert!(default_body_owners[function_idx].is_none());
        }
    }
}

fn verify_default_method_body(program: &Program, declaration: &FunctionData, body_id: FunctionId) {
    let trait_id = match declaration.kind {
        FunctionKind::Trait(trait_id) => trait_id,
        _ => panic!("default method body on non-trait function"),
    };
    assert!(declaration.bytecode.is_none());
    assert!(declaration.trait_method_impl.is_none());

    let body = program.fct(body_id);
    assert!(matches!(body.kind, FunctionKind::Trait(id) if id == trait_id));
    assert!(body.bytecode.is_some());
    assert!(body.default_method_body.is_none());
    assert!(body.trait_method_impl.is_none());
    assert_eq!(body.name, format!("{}$body", declaration.name));
    assert_eq!(body.file_id, declaration.file_id);
    assert_eq!(body.loc, declaration.loc);
    assert_eq!(body.package_id, declaration.package_id);
    assert_eq!(body.module_id, declaration.module_id);
    assert_eq!(body.source_file_id, declaration.source_file_id);
    assert!(!body.is_public);
    assert_eq!(body.is_internal, declaration.is_internal);
    assert!(!body.is_native);
    assert!(!body.is_test);
    assert_eq!(body.is_force_inline, declaration.is_force_inline);
    assert_eq!(body.is_never_inline, declaration.is_never_inline);
    assert_eq!(
        body.is_trait_object_ignore,
        declaration.is_trait_object_ignore
    );

    let declaration_type_param_count = declaration.type_params.type_param_count();
    assert_eq!(
        body.type_params.type_param_count(),
        declaration_type_param_count + 1
    );
    assert_eq!(
        &body.type_params.names[..declaration_type_param_count],
        declaration.type_params.names.as_slice()
    );
    assert_eq!(
        body.type_params.names[declaration_type_param_count],
        "$Self"
    );
    assert_eq!(
        body.type_params.container_count,
        declaration.type_params.container_count
    );
    assert_eq!(
        body.type_params.container_bound_count,
        declaration.type_params.container_bound_count
    );

    let identity_type_params = BytecodeTypeArray::new(
        (0..declaration_type_param_count)
            .map(|idx| BytecodeType::TypeParam(idx.try_into().expect("type parameter overflow")))
            .collect(),
    );
    let self_ty = BytecodeType::TypeParam(
        declaration_type_param_count
            .try_into()
            .expect("type parameter overflow"),
    );

    assert_eq!(
        body.type_params.bounds.len(),
        declaration.type_params.bounds.len() + 1
    );
    for (declaration_bound, body_bound) in declaration
        .type_params
        .bounds
        .iter()
        .zip(body.type_params.bounds.iter())
    {
        assert_eq!(
            body_bound.ty,
            specialize_type_with_self(&declaration_bound.ty, &identity_type_params, Some(&self_ty))
        );
        assert_eq!(
            body_bound.trait_ty,
            specialize_trait_type_with_self(
                &declaration_bound.trait_ty,
                &identity_type_params,
                Some(&self_ty)
            )
        );
    }

    let self_bound = body.type_params.bounds.last().expect("Self bound missing");
    assert_eq!(self_bound.ty, self_ty);
    assert_eq!(self_bound.trait_ty.trait_id, trait_id);
    assert_eq!(
        self_bound.trait_ty.type_params,
        BytecodeTypeArray::new(
            (0..declaration.type_params.container_count)
                .map(|idx| {
                    BytecodeType::TypeParam(idx.try_into().expect("type parameter overflow"))
                })
                .collect()
        )
    );
    assert!(self_bound.trait_ty.bindings.is_empty());

    assert_eq!(body.params.len(), declaration.params.len());
    for (declaration_param, body_param) in declaration.params.iter().zip(&body.params) {
        assert_eq!(
            *body_param,
            specialize_type_with_self(declaration_param, &identity_type_params, Some(&self_ty))
        );
    }
    assert_eq!(
        body.return_type,
        specialize_type_with_self(
            &declaration.return_type,
            &identity_type_params,
            Some(&self_ty)
        )
    );
    assert_eq!(body.is_static, declaration.is_static);
    assert_eq!(body.is_mutating, declaration.is_mutating);
    assert_eq!(body.is_variadic, declaration.is_variadic);
}

fn verify_type_params(type_params: &TypeParamData) -> usize {
    let type_param_count = type_params.type_param_count();
    for bound in &type_params.bounds {
        verify_type(&bound.ty, type_param_count);
        verify_trait_type(&bound.trait_ty, type_param_count);
    }
    type_param_count
}

fn verify_type(ty: &BytecodeType, type_param_count: usize) {
    match ty {
        BytecodeType::TypeParam(id) => assert!((*id as usize) < type_param_count),
        BytecodeType::Tuple(types)
        | BytecodeType::Enum(_, types)
        | BytecodeType::Struct(_, types)
        | BytecodeType::Class(_, types) => verify_types(types, type_param_count),
        BytecodeType::TraitObject(_, type_params, bindings) => {
            // Lambdas are emitted as trait objects. Since their argument and return types can be
            // references, the corresponding trait object type parameters need transient validation.
            verify_transient_types(type_params, type_param_count);
            verify_types(bindings, type_param_count);
        }
        BytecodeType::Assoc { ty, trait_ty, .. } => {
            verify_type(ty, type_param_count);
            verify_trait_type(trait_ty, type_param_count);
        }
        BytecodeType::Ref(_) => {
            panic!("reference type is only allowed at the top level of transient values")
        }
        BytecodeType::Unit
        | BytecodeType::Never
        | BytecodeType::Bool
        | BytecodeType::UInt8
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Address
        | BytecodeType::This
        | BytecodeType::TypeAlias(_) => {}
    }
}

/// Verifies a type used only while executing a function. References can be passed, returned, and
/// held in registers, but they cannot contain another reference or be embedded in another type.
fn verify_transient_type(ty: &BytecodeType, type_param_count: usize) {
    match ty {
        BytecodeType::Ref(inner) => verify_type(inner, type_param_count),
        _ => verify_type(ty, type_param_count),
    }
}

fn verify_types(types: &BytecodeTypeArray, type_param_count: usize) {
    for ty in types.iter() {
        verify_type(&ty, type_param_count);
    }
}

fn verify_transient_types(types: &BytecodeTypeArray, type_param_count: usize) {
    for ty in types.iter() {
        verify_transient_type(&ty, type_param_count);
    }
}

fn verify_trait_type(trait_ty: &BytecodeTraitType, type_param_count: usize) {
    verify_transient_types(&trait_ty.type_params, type_param_count);
    for (_, ty) in &trait_ty.bindings {
        verify_type(ty, type_param_count);
    }
}

fn verify_const_pool_entry(entry: &ConstPoolEntry, type_param_count: usize) {
    match entry {
        ConstPoolEntry::Class(_, types)
        | ConstPoolEntry::ClassField(_, types, _)
        | ConstPoolEntry::Fct(_, types)
        | ConstPoolEntry::Enum(_, types)
        | ConstPoolEntry::EnumVariant(_, types, _)
        | ConstPoolEntry::EnumElement(_, types, _, _)
        | ConstPoolEntry::Struct(_, types)
        | ConstPoolEntry::StructField(_, types, _)
        | ConstPoolEntry::Tuple(types) => verify_types(types, type_param_count),
        ConstPoolEntry::TraitObjectMethod(ty, _) | ConstPoolEntry::TupleElement(ty, _) => {
            verify_type(ty, type_param_count);
        }
        ConstPoolEntry::Generic {
            object_type,
            trait_ty,
            fct_type_params,
            ..
        } => {
            verify_type(object_type, type_param_count);
            verify_trait_type(trait_ty, type_param_count);
            verify_types(fct_type_params, type_param_count);
        }
        ConstPoolEntry::TraitObject {
            trait_ty,
            actual_object_ty,
        } => {
            verify_type(trait_ty, type_param_count);
            verify_type(actual_object_ty, type_param_count);
        }
        ConstPoolEntry::String(_)
        | ConstPoolEntry::Float32(_)
        | ConstPoolEntry::Float64(_)
        | ConstPoolEntry::Int32(_)
        | ConstPoolEntry::Int64(_)
        | ConstPoolEntry::Char(_)
        | ConstPoolEntry::JumpTable { .. } => {}
    }
}

fn type_contains_this(ty: &BytecodeType) -> bool {
    match ty {
        BytecodeType::This => true,
        BytecodeType::Tuple(types)
        | BytecodeType::Enum(_, types)
        | BytecodeType::Struct(_, types)
        | BytecodeType::Class(_, types) => types.iter().any(|ty| type_contains_this(&ty)),
        BytecodeType::TraitObject(_, type_params, bindings) => type_params
            .iter()
            .chain(bindings.iter())
            .any(|ty| type_contains_this(&ty)),
        BytecodeType::Assoc { ty, trait_ty, .. } => {
            type_contains_this(ty) || trait_type_contains_this(trait_ty)
        }
        BytecodeType::Ref(inner) => type_contains_this(inner),
        BytecodeType::Unit
        | BytecodeType::Never
        | BytecodeType::Bool
        | BytecodeType::UInt8
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Address
        | BytecodeType::TypeParam(_)
        | BytecodeType::TypeAlias(_) => false,
    }
}

fn trait_type_contains_this(trait_ty: &BytecodeTraitType) -> bool {
    trait_ty
        .type_params
        .iter()
        .chain(trait_ty.bindings.iter().map(|(_, ty)| ty.clone()))
        .any(|ty| type_contains_this(&ty))
}

fn const_pool_entry_contains_this(entry: &ConstPoolEntry) -> bool {
    match entry {
        ConstPoolEntry::Class(_, types)
        | ConstPoolEntry::ClassField(_, types, _)
        | ConstPoolEntry::Fct(_, types)
        | ConstPoolEntry::Enum(_, types)
        | ConstPoolEntry::EnumVariant(_, types, _)
        | ConstPoolEntry::EnumElement(_, types, _, _)
        | ConstPoolEntry::Struct(_, types)
        | ConstPoolEntry::StructField(_, types, _)
        | ConstPoolEntry::Tuple(types) => types.iter().any(|ty| type_contains_this(&ty)),
        ConstPoolEntry::TraitObjectMethod(ty, _) | ConstPoolEntry::TupleElement(ty, _) => {
            type_contains_this(ty)
        }
        ConstPoolEntry::Generic {
            object_type,
            trait_ty,
            fct_type_params,
            ..
        } => {
            type_contains_this(object_type)
                || trait_type_contains_this(trait_ty)
                || fct_type_params.iter().any(|ty| type_contains_this(&ty))
        }
        ConstPoolEntry::TraitObject {
            trait_ty,
            actual_object_ty,
        } => type_contains_this(trait_ty) || type_contains_this(actual_object_ty),
        ConstPoolEntry::String(_)
        | ConstPoolEntry::Float32(_)
        | ConstPoolEntry::Float64(_)
        | ConstPoolEntry::Int32(_)
        | ConstPoolEntry::Int64(_)
        | ConstPoolEntry::Char(_)
        | ConstPoolEntry::JumpTable { .. } => false,
    }
}
