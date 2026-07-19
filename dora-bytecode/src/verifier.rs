use fixedbitset::FixedBitSet;

use crate::{
    BytecodeFunction, BytecodeInstruction, BytecodeOffset, BytecodeReader, BytecodeTraitType,
    BytecodeType, BytecodeTypeArray, ClassId, ConstPoolEntry, ConstPoolIdx, FunctionData,
    FunctionId, FunctionKind, Program, Register, TypeParamData,
};

pub fn verify(program: &Program) {
    verify_program_types(program);

    for (function_idx, function) in program.functions.iter().enumerate() {
        if let Some(bytecode) = &function.bytecode {
            Verifier::new(program, function_idx.into(), bytecode).verify();
        }
    }
}

struct Verifier<'a> {
    program: &'a Program,
    function_id: FunctionId,
    bytecode: &'a BytecodeFunction,
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
        bytecode: &'a BytecodeFunction,
    ) -> Verifier<'a> {
        Verifier {
            program,
            function_id,
            bytecode,
            offset: BytecodeOffset(0),
            instruction_offsets: FixedBitSet::with_capacity(bytecode.code().len()),
            jump_targets: Vec::new(),
        }
    }

    fn verify(mut self) {
        assert!(self.bytecode.arguments() as usize <= self.bytecode.registers().len());
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
            verify_type(ty, type_param_count);
        }
        verify_type(self.bytecode.return_type(), type_param_count);
        for entry in self.bytecode.const_pool_entries() {
            verify_const_pool_entry(entry, type_param_count);
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

            BytecodeInstruction::Mov { dest, src } => self.assert_same_type(dest, src),

            BytecodeInstruction::LoadEnumElement { dest, src, idx } => {
                let ConstPoolEntry::EnumElement(enum_id, type_params, variant_idx, element_idx) =
                    self.const_pool(idx)
                else {
                    panic!("expected EnumElement constant pool entry");
                };
                let variant = &self.program.enum_(*enum_id).variants[*variant_idx as usize];
                let field = &variant.fields[*element_idx as usize];
                self.assert_type(src, &BytecodeType::Enum(*enum_id, type_params.clone()));
                self.assert_type(dest, &specialize_type(&field.ty, type_params));
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
                self.assert_field_type(dest, &field_ty);
            }

            BytecodeInstruction::StoreField { src, obj, field } => {
                let (object_ty, field_ty) = self.field_types(field);
                self.assert_type(obj, &object_ty);
                self.assert_field_type(src, &field_ty);
            }

            BytecodeInstruction::LoadGlobal { dest, global_id } => {
                self.assert_type(dest, &self.program.global(global_id).ty);
            }

            BytecodeInstruction::StoreGlobal { src, global_id } => {
                self.assert_type(src, &self.program.global(global_id).ty);
            }

            BytecodeInstruction::LoadConst { dest, const_id } => {
                self.assert_type(dest, &self.program.const_(const_id).ty);
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
                assert!(matches!(
                    self.ty(dest),
                    BytecodeType::Class(..) | BytecodeType::Ptr
                ));
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

            BytecodeInstruction::InvokeLambda {
                dest,
                idx,
                arguments,
            } => {
                let ConstPoolEntry::Lambda(params, return_type, variadic) = self.const_pool(idx)
                else {
                    panic!("expected Lambda constant pool entry");
                };
                let BytecodeType::Lambda(receiver_params, receiver_return, receiver_variadic) =
                    self.ty(arguments[0])
                else {
                    panic!("InvokeLambda receiver is not a lambda");
                };
                assert_eq!(receiver_params, params);
                assert_eq!(receiver_return.as_ref(), return_type);
                assert_eq!(receiver_variadic, variadic);
                self.assert_invoke_return_type(dest, return_type);
                let call_arguments = &arguments[1..];
                assert_eq!(call_arguments.len(), params.len());
                for (argument_idx, (&argument, expected)) in
                    call_arguments.iter().zip(params.iter()).enumerate()
                {
                    if *variadic && argument_idx + 1 == params.len() {
                        let BytecodeType::Class(_, type_params) = self.ty(argument) else {
                            panic!("variadic lambda argument is not an array");
                        };
                        assert_eq!(type_params.len(), 1);
                        assert_eq!(&type_params[0], &expected);
                    } else {
                        self.assert_type(argument, &expected);
                    }
                }
            }

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

            BytecodeInstruction::NewObjectUninitialized { dest, cls } => {
                let (class_id, type_params) = self.class_entry(cls);
                self.assert_type(dest, &BytecodeType::Class(class_id, type_params));
            }

            BytecodeInstruction::NewObject {
                dest,
                cls,
                arguments,
            } => {
                let (class_id, type_params) = self.class_entry(cls);
                self.assert_type(dest, &BytecodeType::Class(class_id, type_params.clone()));
                let fields = self
                    .program
                    .class(class_id)
                    .fields
                    .iter()
                    .map(|field| specialize_type(&field.ty, &type_params))
                    .collect::<Vec<_>>();
                self.assert_argument_types(&arguments, &fields);
            }

            BytecodeInstruction::NewArray { dest, length, idx } => {
                let (class_id, type_params) = self.class_entry(idx);
                assert!(!type_params.is_empty());
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

            BytecodeInstruction::NewLambda {
                dest,
                idx,
                arguments,
            } => {
                let ConstPoolEntry::Fct(function_id, _) = self.const_pool(idx) else {
                    panic!("expected Fct constant pool entry");
                };
                let function = self.program.fct(*function_id);
                assert!(matches!(function.kind, FunctionKind::Lambda));
                let lambda_ty = BytecodeType::Lambda(
                    BytecodeTypeArray::new(function.params.iter().skip(1).cloned().collect()),
                    Box::new(function.return_type.clone()),
                    function.is_variadic,
                );
                self.assert_type(dest, &lambda_ty);
                assert!(arguments.len() <= 1);
                for argument in arguments {
                    assert!(self.ty(argument).is_reference_type());
                }
            }

            BytecodeInstruction::ArrayLength { dest, arr } => {
                self.assert_type(dest, &BytecodeType::Int64);
                assert!(self.ty(arr).is_class());
            }

            BytecodeInstruction::LoadArray { dest, arr, idx } => {
                self.ty(dest);
                assert!(self.ty(arr).is_class());
                self.assert_type(idx, &BytecodeType::Int64);
            }

            BytecodeInstruction::StoreArray { src, arr, idx } => {
                self.ty(src);
                assert!(self.ty(arr).is_class());
                self.assert_type(idx, &BytecodeType::Int64);
            }

            BytecodeInstruction::LoadTraitObjectValue { dest, object } => {
                assert!(self.ty(object).is_trait_object());
                self.ty(dest);
            }

            BytecodeInstruction::GetFieldAddress { dest, obj, field } => {
                self.assert_type(dest, &BytecodeType::Address);
                let (object_ty, _) = self.field_types(field);
                assert!(self.ty(obj) == &object_ty || *self.ty(obj) == BytecodeType::Address);
            }

            BytecodeInstruction::StoreAddress { src, address } => {
                self.ty(src);
                self.assert_type(address, &BytecodeType::Address);
            }

            BytecodeInstruction::LoadAddress { dest, address } => {
                self.ty(dest);
                self.assert_type(address, &BytecodeType::Address);
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
                self.assert_type(dest, &referenced_ty);
            }

            BytecodeInstruction::GetRef { dest, src } => {
                self.assert_type(dest, &BytecodeType::Ref(Box::new(self.ty(src).clone())));
            }

            BytecodeInstruction::Ret { opnd } => {
                self.assert_type(opnd, &self.program.fct(self.function_id).return_type);
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
            FunctionKind::Lambda | FunctionKind::Trait(_) | FunctionKind::Function => None,
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
        assert_eq!(function.type_params.type_param_count(), type_params.len());
        self.assert_invoke_return_type(dest, &specialize_type(&function.return_type, type_params));
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
                specialize_type_for_trait_object(self.program, param, type_params, bindings)
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
                let BytecodeType::Class(_, type_params) = self.ty(argument) else {
                    panic!("variadic argument is not an array");
                };
                assert_eq!(type_params.len(), 1);
                assert!(types_match(&type_params[0], expected));
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

    fn assert_invoke_return_type(&self, dest: Register, expected: &BytecodeType) {
        // Unit is used as the destination when the call result is discarded.
        if !self.ty(dest).is_unit() {
            self.assert_type(dest, expected);
        }
    }

    fn assert_field_type(&self, register: Register, expected: &BytecodeType) {
        let actual = self.ty(register);
        // Lambda context and parent-context fields are explicitly erased to Ptr.
        assert!(
            types_match(actual, expected) || (*expected == BytecodeType::Ptr && actual.is_class()),
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
        (
            BytecodeType::Lambda(actual_params, actual_return, actual_variadic),
            BytecodeType::Lambda(expected_params, expected_return, expected_variadic),
        ) => {
            actual_variadic == expected_variadic
                && type_arrays_match(actual_params, expected_params)
                && types_match(actual_return, expected_return)
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
        BytecodeType::Lambda(params, return_type, variadic) => BytecodeType::Lambda(
            specialize_types_with_self(params, type_params, self_type),
            Box::new(specialize_type_with_self(
                return_type,
                type_params,
                self_type,
            )),
            *variadic,
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

    if !function.is_static
        && function.is_mutating
        && matches!(
            self_type,
            Some(BytecodeType::Struct(..) | BytecodeType::Tuple(..))
        )
    {
        assert!(!params.is_empty());
        params[0] = BytecodeType::Ref(Box::new(self_type.unwrap().clone()));
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
        BytecodeType::Lambda(params, return_type, variadic) => BytecodeType::Lambda(
            specialize_types_for_trait_object(program, params, type_params, bindings),
            Box::new(specialize_type_for_trait_object(
                program,
                return_type,
                type_params,
                bindings,
            )),
            *variadic,
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
        for ty in &function.params {
            verify_type(ty, type_param_count);
        }
        verify_type(&function.return_type, type_param_count);
    }

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
            verify_types(type_params, type_param_count);
            verify_types(bindings, type_param_count);
        }
        BytecodeType::Lambda(params, return_type, _) => {
            verify_types(params, type_param_count);
            verify_type(return_type, type_param_count);
        }
        BytecodeType::Assoc { ty, trait_ty, .. } => {
            verify_type(ty, type_param_count);
            verify_trait_type(trait_ty, type_param_count);
        }
        BytecodeType::Ref(inner) => verify_type(inner, type_param_count),
        BytecodeType::Unit
        | BytecodeType::Bool
        | BytecodeType::UInt8
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Ptr
        | BytecodeType::Address
        | BytecodeType::This
        | BytecodeType::TypeAlias(_) => {}
    }
}

fn verify_types(types: &BytecodeTypeArray, type_param_count: usize) {
    for ty in types.iter() {
        verify_type(&ty, type_param_count);
    }
}

fn verify_trait_type(trait_ty: &BytecodeTraitType, type_param_count: usize) {
    verify_types(&trait_ty.type_params, type_param_count);
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
        ConstPoolEntry::Lambda(params, return_type, _) => {
            verify_types(params, type_param_count);
            verify_type(return_type, type_param_count);
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
