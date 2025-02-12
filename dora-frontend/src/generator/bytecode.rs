use std::collections::{HashMap, HashSet};

use dora_bytecode::{
    BytecodeFunction, BytecodeType, BytecodeTypeArray, BytecodeWriter, ClassId, ConstPoolEntry,
    ConstPoolIdx, EnumId, FunctionId, GlobalId, Label, Location, Register, StructId,
};

pub struct BytecodeBuilder {
    writer: BytecodeWriter,
    registers: Registers,
}

impl BytecodeBuilder {
    pub fn new() -> BytecodeBuilder {
        BytecodeBuilder {
            writer: BytecodeWriter::new(),
            registers: Registers::new(),
        }
    }

    pub fn create_label(&mut self) -> Label {
        self.writer.create_label()
    }

    pub fn define_label(&mut self) -> Label {
        self.writer.define_label()
    }

    pub fn bind_label(&mut self, lbl: Label) {
        self.writer.bind_label(lbl)
    }

    pub fn set_params(&mut self, params: Vec<BytecodeType>) {
        self.writer.set_arguments(params.len() as u32);
        self.writer.set_params(params);
    }

    pub fn set_return_type(&mut self, return_type: BytecodeType) {
        self.writer.set_return_type(return_type);
    }

    pub fn add_const(&mut self, entry: ConstPoolEntry) -> ConstPoolIdx {
        self.writer.add_const(entry)
    }

    pub fn add_const_lambda(
        &mut self,
        params: BytecodeTypeArray,
        return_type: BytecodeType,
    ) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Lambda(params, return_type))
    }

    pub fn add_const_fct(&mut self, id: FunctionId) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Fct(id, BytecodeTypeArray::empty()))
    }

    pub fn add_const_enum(&mut self, id: EnumId, type_params: BytecodeTypeArray) -> ConstPoolIdx {
        self.writer.add_const(ConstPoolEntry::Enum(id, type_params))
    }

    pub fn add_const_enum_variant(
        &mut self,
        id: EnumId,
        type_params: BytecodeTypeArray,
        variant_idx: u32,
    ) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::EnumVariant(id, type_params, variant_idx))
    }

    pub fn add_const_enum_element(
        &mut self,
        id: EnumId,
        type_params: BytecodeTypeArray,
        variant_idx: u32,
        element_idx: u32,
    ) -> ConstPoolIdx {
        self.writer.add_const(ConstPoolEntry::EnumElement(
            id,
            type_params,
            variant_idx,
            element_idx,
        ))
    }

    pub fn add_const_struct(
        &mut self,
        id: StructId,
        type_params: BytecodeTypeArray,
    ) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Struct(id, type_params))
    }

    pub fn add_const_struct_field(
        &mut self,
        id: StructId,
        type_params: BytecodeTypeArray,
        field_idx: u32,
    ) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::StructField(id, type_params, field_idx))
    }

    pub fn add_const_fct_types(
        &mut self,
        id: FunctionId,
        type_params: BytecodeTypeArray,
    ) -> ConstPoolIdx {
        self.writer.add_const(ConstPoolEntry::Fct(id, type_params))
    }

    pub fn add_const_field_types(
        &mut self,
        cls_id: ClassId,
        type_params: BytecodeTypeArray,
        field_id: u32,
    ) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Field(cls_id, type_params, field_id))
    }

    pub fn add_const_cls_types(
        &mut self,
        id: ClassId,
        type_params: BytecodeTypeArray,
    ) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Class(id, type_params))
    }

    pub fn add_const_trait(
        &mut self,
        trait_ty: BytecodeType,
        actual_object_ty: BytecodeType,
    ) -> ConstPoolIdx {
        self.writer.add_const(ConstPoolEntry::TraitObject {
            trait_ty,
            actual_object_ty,
        })
    }

    pub fn add_const_tuple_element(
        &mut self,
        tuple_ty: BytecodeType,
        subtype_idx: u32,
    ) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::TupleElement(tuple_ty, subtype_idx))
    }

    pub fn add_const_tuple(&mut self, subtypes: BytecodeTypeArray) -> ConstPoolIdx {
        self.writer.add_const(ConstPoolEntry::Tuple(subtypes))
    }

    pub fn emit_add(&mut self, dest: Register, lhs: Register, rhs: Register, location: Location) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.set_location(location);
        self.writer.emit_add(dest, lhs, rhs);
    }

    pub fn emit_and(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_and(dest, lhs, rhs);
    }

    pub fn emit_or(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_or(dest, lhs, rhs);
    }

    pub fn emit_xor(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_xor(dest, lhs, rhs);
    }

    pub fn emit_div(&mut self, dest: Register, lhs: Register, rhs: Register, location: Location) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.set_location(location);
        self.writer.emit_div(dest, lhs, rhs);
    }

    pub fn emit_load_struct_field(
        &mut self,
        dest: Register,
        obj: Register,
        field_idx: ConstPoolIdx,
    ) {
        assert!(self.def(dest) && self.used(obj));
        self.writer.emit_load_struct_field(dest, obj, field_idx);
    }

    pub fn emit_load_field(
        &mut self,
        dest: Register,
        obj: Register,
        field_idx: ConstPoolIdx,
        location: Location,
    ) {
        assert!(self.def(dest) && self.used(obj));
        self.writer.set_location(location);
        self.writer.emit_load_field(dest, obj, field_idx);
    }

    pub fn emit_store_field(
        &mut self,
        src: Register,
        obj: Register,
        field_idx: ConstPoolIdx,
        location: Location,
    ) {
        assert!(self.used(src) && self.used(obj));
        self.writer.set_location(location);
        self.writer.emit_store_field(src, obj, field_idx);
    }

    pub fn emit_const_char(&mut self, dest: Register, value: char) {
        assert!(self.def(dest));
        self.writer.emit_const_char(dest, value);
    }

    pub fn emit_const_uint8(&mut self, dest: Register, value: u8) {
        assert!(self.def(dest));
        self.writer.emit_const_uint8(dest, value);
    }

    pub fn emit_const_int32(&mut self, dest: Register, value: i32) {
        assert!(self.def(dest));
        self.writer.emit_const_int32(dest, value);
    }

    pub fn emit_const_int64(&mut self, dest: Register, value: i64) {
        assert!(self.def(dest));
        self.writer.emit_const_int64(dest, value);
    }

    pub fn emit_const_float32(&mut self, dest: Register, value: f32) {
        assert!(self.def(dest));
        self.writer.emit_const_float32(dest, value);
    }

    pub fn emit_const_float64(&mut self, dest: Register, value: f64) {
        assert!(self.def(dest));
        self.writer.emit_const_float64(dest, value);
    }

    pub fn emit_const_string(&mut self, dest: Register, value: String) {
        assert!(self.def(dest));
        self.writer.emit_const_string(dest, value);
    }

    pub fn emit_const_true(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_true(dest);
    }

    pub fn emit_const_false(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_false(dest);
    }

    pub fn emit_not(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_not(dest, src);
    }

    pub fn emit_jump_if_false(&mut self, opnd: Register, lbl: Label) {
        assert!(self.used(opnd));
        self.writer.emit_jump_if_false(opnd, lbl);
    }

    pub fn emit_jump_if_true(&mut self, opnd: Register, lbl: Label) {
        assert!(self.used(opnd));
        self.writer.emit_jump_if_true(opnd, lbl);
    }

    pub fn emit_jump_loop(&mut self, lbl: Label) {
        self.writer.emit_jump_loop(lbl);
    }

    pub fn emit_loop_start(&mut self) {
        self.writer.emit_loop_start();
    }

    pub fn emit_jump(&mut self, lbl: Label) {
        self.writer.emit_jump(lbl);
    }

    pub fn emit_mod(&mut self, dest: Register, lhs: Register, rhs: Register, location: Location) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.set_location(location);
        self.writer.emit_mod(dest, lhs, rhs);
    }

    pub fn emit_mul(&mut self, dest: Register, lhs: Register, rhs: Register, location: Location) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.set_location(location);
        self.writer.emit_mul(dest, lhs, rhs);
    }

    pub fn emit_neg(&mut self, dest: Register, src: Register, location: Location) {
        assert!(self.def(dest) && self.used(src));
        self.writer.set_location(location);
        self.writer.emit_neg(dest, src);
    }

    pub fn emit_shl(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_shl(dest, lhs, rhs);
    }

    pub fn emit_shr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_shr(dest, lhs, rhs);
    }

    pub fn emit_sar(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_sar(dest, lhs, rhs);
    }

    pub fn emit_sub(&mut self, dest: Register, lhs: Register, rhs: Register, location: Location) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.set_location(location);
        self.writer.emit_sub(dest, lhs, rhs);
    }

    pub fn emit_mov(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov(dest, src);
    }

    pub fn emit_load_tuple_element(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_load_tuple_element(dest, src, idx);
    }

    pub fn emit_load_enum_element(
        &mut self,
        dest: Register,
        src: Register,
        idx: ConstPoolIdx,
        location: Location,
    ) {
        assert!(self.def(dest) && self.used(src));
        self.writer.set_location(location);
        self.writer.emit_load_enum_element(dest, src, idx);
    }

    pub fn emit_load_enum_variant(
        &mut self,
        dest: Register,
        src: Register,
        idx: ConstPoolIdx,
        location: Location,
    ) {
        assert!(self.def(dest) && self.used(src));
        self.writer.set_location(location);
        self.writer.emit_load_enum_variant(dest, src, idx);
    }

    pub fn emit_ret(&mut self, src: Register) {
        assert!(self.used(src));
        self.writer.emit_ret(src);
    }

    pub fn emit_test_identity(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_identity(dest, lhs, rhs);
    }

    pub fn emit_test_eq(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq(dest, lhs, rhs);
    }

    pub fn emit_test_ne(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne(dest, lhs, rhs);
    }

    pub fn emit_test_gt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_gt(dest, lhs, rhs);
    }

    pub fn emit_test_ge(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ge(dest, lhs, rhs);
    }

    pub fn emit_test_lt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_lt(dest, lhs, rhs);
    }

    pub fn emit_test_le(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_le(dest, lhs, rhs);
    }

    pub fn emit_load_global(&mut self, dest: Register, gid: GlobalId, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_load_global(dest, gid);
    }

    pub fn emit_store_global(&mut self, src: Register, gid: GlobalId) {
        assert!(self.used(src));
        self.writer.emit_store_global(src, gid);
    }

    pub fn emit_push_register(&mut self, src: Register) {
        assert!(self.used(src));
        self.writer.emit_push_register(src);
    }

    pub fn emit_invoke_direct(&mut self, dest: Register, fid: ConstPoolIdx, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_invoke_direct(dest, fid);
    }

    pub fn emit_invoke_virtual(&mut self, dest: Register, idx: ConstPoolIdx, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_invoke_virtual(dest, idx);
    }

    pub fn emit_invoke_static(&mut self, dest: Register, idx: ConstPoolIdx, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_invoke_static(dest, idx);
    }

    pub fn emit_invoke_lambda(&mut self, dest: Register, idx: ConstPoolIdx, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_invoke_lambda(dest, idx);
    }

    pub fn emit_invoke_generic_static(
        &mut self,
        dest: Register,
        idx: ConstPoolIdx,
        location: Location,
    ) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_invoke_generic_static(dest, idx);
    }

    pub fn emit_invoke_generic_direct(
        &mut self,
        dest: Register,
        idx: ConstPoolIdx,
        location: Location,
    ) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_invoke_generic_direct(dest, idx);
    }

    pub fn emit_new_object(&mut self, dest: Register, idx: ConstPoolIdx, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_new_object(dest, idx);
    }
    pub fn emit_new_object_initialized(
        &mut self,
        dest: Register,
        idx: ConstPoolIdx,
        location: Location,
    ) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_new_object_initialized(dest, idx);
    }
    pub fn emit_new_array(
        &mut self,
        dest: Register,
        length: Register,
        cls_idx: ConstPoolIdx,
        location: Location,
    ) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_new_array(dest, length, cls_idx);
    }
    pub fn emit_new_tuple(&mut self, dest: Register, idx: ConstPoolIdx, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_new_tuple(dest, idx);
    }
    pub fn emit_new_enum(&mut self, dest: Register, idx: ConstPoolIdx, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_new_enum(dest, idx);
    }
    pub fn emit_new_struct(&mut self, dest: Register, idx: ConstPoolIdx, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_new_struct(dest, idx);
    }
    pub fn emit_new_trait_object(
        &mut self,
        dest: Register,
        idx: ConstPoolIdx,
        src: Register,
        location: Location,
    ) {
        assert!(self.def(dest) && self.used(src));
        self.writer.set_location(location);
        self.writer.emit_new_trait_object(dest, idx, src);
    }
    pub fn emit_new_lambda(&mut self, dest: Register, idx: ConstPoolIdx, location: Location) {
        assert!(self.def(dest));
        self.writer.set_location(location);
        self.writer.emit_new_lambda(dest, idx);
    }

    pub fn emit_array_length(&mut self, dest: Register, array: Register, location: Location) {
        assert!(self.def(dest) && self.used(array));
        self.writer.set_location(location);
        self.writer.emit_array_length(dest, array);
    }

    pub fn emit_store_array(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        location: Location,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_location(location);
        self.writer.emit_store_array(src, array, index);
    }

    pub fn emit_load_array(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        location: Location,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_location(location);
        self.writer.emit_load_array(dest, array, index);
    }

    pub fn generate(self) -> BytecodeFunction {
        for reg in &self.registers.used {
            println!("used reg {}", reg);
        }

        assert!(!self.registers.used());

        self.writer.generate_with_registers(self.registers.all())
    }

    pub fn push_scope(&mut self) {
        self.registers.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.registers.pop_scope();
    }

    pub fn alloc_var(&mut self, ty: BytecodeType) -> Register {
        assert!(!ty.is_class());
        self.registers.alloc_var(ty)
    }

    pub fn alloc_temp(&mut self, ty: BytecodeType) -> Register {
        assert!(!ty.is_class());
        self.registers.alloc_temp(ty)
    }

    pub fn alloc_global(&mut self, ty: BytecodeType) -> Register {
        assert!(!ty.is_class());
        self.registers.alloc_global(ty)
    }

    pub fn free_if_temp(&mut self, reg: Register) {
        self.registers.free_if_temp(reg);
    }

    pub fn free_temp(&mut self, reg: Register) {
        self.registers.free_temp(reg);
    }

    fn used(&self, reg: Register) -> bool {
        self.registers.used.contains(&reg) || Some(reg) == self.registers.unit
    }

    fn def(&self, reg: Register) -> bool {
        self.registers.used.contains(&reg) || Some(reg) == self.registers.unit
    }
}

struct Registers {
    all: Vec<BytecodeType>,
    scopes: Vec<RegisterScope>,
    used: HashSet<Register>,
    temps: HashSet<Register>,
    unused: HashMap<BytecodeType, Vec<Register>>,
    unit: Option<Register>,
}

impl Registers {
    fn new() -> Registers {
        Registers {
            all: Vec::new(),
            scopes: Vec::new(),
            used: HashSet::new(),
            temps: HashSet::new(),
            unused: HashMap::new(),
            unit: None,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(RegisterScope::new());
    }

    fn pop_scope(&mut self) {
        let scope = self.scopes.pop().expect("missing scope");

        for reg in scope.0 {
            let ty = self.all[reg.0].clone();
            self.unused.entry(ty).or_insert(Vec::new()).push(reg);
            assert!(self.used.remove(&reg));
        }
    }

    fn alloc_var(&mut self, ty: BytecodeType) -> Register {
        if ty.is_unit() {
            return self.ensure_unit_register();
        }

        let reg = self.alloc_internal(ty);
        assert!(self.scopes.last_mut().expect("missing scope").0.insert(reg));
        assert!(self.used.insert(reg));
        reg
    }

    fn alloc_temp(&mut self, ty: BytecodeType) -> Register {
        if ty.is_unit() {
            return self.ensure_unit_register();
        }

        let reg = self.alloc_internal(ty);
        assert!(self.temps.insert(reg));
        assert!(self.used.insert(reg));
        reg
    }

    fn alloc_global(&mut self, ty: BytecodeType) -> Register {
        if ty.is_unit() {
            return self.ensure_unit_register();
        }

        let reg = self.alloc_internal(ty);
        assert!(self
            .scopes
            .first_mut()
            .expect("missing scope")
            .0
            .insert(reg));
        assert!(self.used.insert(reg));
        reg
    }

    fn free_temp(&mut self, reg: Register) {
        if Some(reg) != self.unit {
            assert!(self.temps.remove(&reg));
            let ty = self.all[reg.0].clone();
            self.unused.entry(ty).or_insert(Vec::new()).push(reg);
            assert!(self.used.remove(&reg));
        }
    }

    fn free_if_temp(&mut self, reg: Register) -> bool {
        if self.temps.contains(&reg) {
            self.free_temp(reg);
            true
        } else {
            false
        }
    }

    fn alloc_internal(&mut self, ty: BytecodeType) -> Register {
        if let Some(regs) = self.unused.get_mut(&ty) {
            if let Some(reg) = regs.pop() {
                return reg;
            }
        }

        self.new_register(ty)
    }

    fn new_register(&mut self, ty: BytecodeType) -> Register {
        assert!(!ty.is_unit());
        self.all.push(ty);
        Register(self.all.len() - 1)
    }

    fn ensure_unit_register(&mut self) -> Register {
        if self.unit.is_none() {
            let idx = self.all.len();
            self.all.push(BytecodeType::Unit);
            self.unit = Some(Register(idx));
        }

        self.unit.expect("missing unit register")
    }

    fn used(&self) -> bool {
        !self.scopes.is_empty() || !self.used.is_empty()
    }

    fn all(self) -> Vec<BytecodeType> {
        self.all
    }
}

struct RegisterScope(HashSet<Register>);

impl RegisterScope {
    fn new() -> RegisterScope {
        RegisterScope(HashSet::new())
    }
}
