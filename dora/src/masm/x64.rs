use crate::asm::Register as AsmRegister;
use crate::compiler::codegen::ExprStore;
use crate::compiler::fct::LazyCompilationSite;
use crate::cpu::*;
use crate::gc::swiper::CARD_SIZE_BITS;
use crate::gc::Address;
use crate::masm::{CondCode, Label, MacroAssembler};
use crate::mem::{fits_i32, ptr_width};
use crate::object::{offset_of_array_data, offset_of_array_length, Header};
use crate::threads::ThreadLocalData;
use crate::ty::{MachineMode, TypeList};
use crate::vm::get_vm;
use crate::vm::FctId;
use crate::vm::Trap;
use crate::vtable::VTable;
use byteorder::{LittleEndian, WriteBytesExt};
use dora_parser::lexer::position::Position;

impl MacroAssembler {
    pub fn prolog_size(&mut self, stacksize: i32) {
        self.asm.pushq_r(RBP.into());
        self.asm.movq_rr(RBP.into(), RSP.into());
        debug_assert!(stacksize as usize % STACK_FRAME_ALIGNMENT == 0);

        if stacksize > 0 {
            asm::emit_subq_imm_reg(self, stacksize, RSP);
        }
    }

    pub fn prolog(&mut self) -> usize {
        self.asm.pushq_r(RBP.into());
        self.asm.movq_rr(RBP.into(), RSP.into());

        asm::emit_subq_immd_reg(self, 0, RSP);
        let patch_offset = self.pos() - 4;

        patch_offset
    }

    pub fn patch_stacksize(&mut self, patch_offset: usize, stacksize: i32) {
        self.emit_u32_at(patch_offset as i32, stacksize as u32);
    }

    pub fn check_stack_pointer(&mut self, lbl_overflow: Label) {
        asm::emit_cmp_mem_reg(
            self,
            MachineMode::Ptr,
            REG_THREAD,
            ThreadLocalData::stack_limit_offset(),
            RSP,
        );

        asm::emit_jcc(self, CondCode::UnsignedGreater, lbl_overflow);
    }

    pub fn epilog(&mut self) {
        self.asm.movq_rr(RSP.into(), RBP.into());
        self.asm.popq_r(RBP.into());
        self.asm.retq();
    }

    pub fn epilog_without_return(&mut self) {
        self.asm.movq_rr(RSP.into(), RBP.into());
        self.asm.popq_r(RBP.into());
    }

    pub fn increase_stack_frame(&mut self, size: i32) {
        debug_assert!(size as usize % STACK_FRAME_ALIGNMENT == 0);

        if size > 0 {
            asm::emit_subq_imm_reg(self, size, RSP);
        }
    }

    pub fn decrease_stack_frame(&mut self, size: i32) {
        if size > 0 {
            asm::emit_addq_imm_reg(self, size, RSP);
        }
    }

    pub fn direct_call(
        &mut self,
        fct_id: FctId,
        ptr: *const u8,
        cls_tps: TypeList,
        fct_tps: TypeList,
    ) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        self.load_constpool(REG_RESULT, disp + pos);
        self.call_reg(REG_RESULT);

        let pos = self.pos() as i32;
        self.emit_lazy_compilation_site(LazyCompilationSite::Compile(
            fct_id,
            disp + pos,
            cls_tps,
            fct_tps,
        ));
    }

    pub fn raw_call(&mut self, ptr: *const u8) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        self.load_constpool(REG_RESULT, disp + pos);
        self.call_reg(REG_RESULT);
    }

    pub fn indirect_call(&mut self, pos: Position, index: u32, cls_type_params: TypeList) {
        let obj = REG_PARAMS[0];

        self.test_if_nil_bailout(pos, obj, Trap::NIL);

        // REG_RESULT = [obj] (load vtable)
        self.load_mem(MachineMode::Ptr, REG_RESULT.into(), Mem::Base(obj, 0));

        // calculate offset of VTable entry
        let disp = VTable::offset_of_method_table() + (index as i32) * ptr_width();

        // load vtable entry
        self.load_mem(
            MachineMode::Ptr,
            REG_RESULT.into(),
            Mem::Base(REG_RESULT, disp),
        );

        // call *REG_RESULT
        self.call_reg(REG_RESULT);
        self.emit_lazy_compilation_site(LazyCompilationSite::VirtCompile(
            index,
            cls_type_params,
            TypeList::empty(),
        ));
    }

    pub fn load_array_elem(&mut self, mode: MachineMode, dest: ExprStore, array: Reg, index: Reg) {
        self.load_mem(
            mode,
            dest,
            Mem::Index(array, index, mode.size(), offset_of_array_data()),
        );
    }

    pub fn store_array_elem(
        &mut self,
        mode: MachineMode,
        array: Reg,
        index: Reg,
        value: ExprStore,
        write_barrier: bool,
        card_table_offset: usize,
    ) {
        self.store_mem(
            mode,
            Mem::Index(array, index, mode.size(), offset_of_array_data()),
            value,
        );

        if write_barrier {
            let scratch = self.get_scratch();
            asm::lea(
                self,
                *scratch,
                Mem::Index(array, index, mode.size(), offset_of_array_data()),
            );
            self.emit_barrier(*scratch, card_table_offset);
        }
    }

    pub fn set(&mut self, dest: Reg, op: CondCode) {
        asm::emit_setb_reg(self, op, dest);
        asm::emit_movzbl_reg_reg(self, dest, dest);
    }

    pub fn cmp_mem(&mut self, mode: MachineMode, mem: Mem, rhs: Reg) {
        match mem {
            Mem::Local(offset) => asm::emit_cmp_mem_reg(self, mode, REG_FP, offset, rhs),
            Mem::Base(base, disp) => asm::emit_cmp_mem_reg(self, mode, base, disp, rhs),
            Mem::Index(base, index, scale, disp) => {
                asm::emit_cmp_memindex_reg(self, mode, base, index, scale, disp, rhs)
            }
            Mem::Offset(_, _, _) => unimplemented!(),
        }
    }

    pub fn cmp_mem_imm(&mut self, mode: MachineMode, mem: Mem, imm: i32) {
        match mem {
            Mem::Local(_) => unimplemented!(),
            Mem::Base(base, disp) => asm::emit_cmp_mem_imm(self, mode, base, disp, imm),
            Mem::Index(_, _, _, _) => unimplemented!(),
            Mem::Offset(_, _, _) => unimplemented!(),
        }
    }

    pub fn cmp_reg(&mut self, mode: MachineMode, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int8 | MachineMode::Int32 => 0,
            MachineMode::Int64 | MachineMode::Ptr => 1,
            MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
        };

        asm::emit_cmp_reg_reg(self, x64, rhs, lhs);
    }

    pub fn cmp_reg_imm(&mut self, mode: MachineMode, lhs: Reg, imm: i32) {
        asm::emit_cmp_imm_reg(self, mode, imm, lhs);
    }

    pub fn float_cmp(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: FReg,
        rhs: FReg,
        cond: CondCode,
    ) {
        let scratch = self.get_scratch();

        match cond {
            CondCode::Equal | CondCode::NotEqual => {
                let init = if cond == CondCode::Equal { 0 } else { 1 };

                self.load_int_const(MachineMode::Int32, *scratch, init);
                self.load_int_const(MachineMode::Int32, dest, 0);

                match mode {
                    MachineMode::Float32 => asm::ucomiss(self, lhs, rhs),
                    MachineMode::Float64 => asm::ucomisd(self, lhs, rhs),
                    _ => unreachable!(),
                }

                let parity = if cond == CondCode::Equal { false } else { true };

                asm::emit_setb_reg_parity(self, dest, parity);
                asm::cmov(self, 0, dest, *scratch, CondCode::NotEqual);
            }

            CondCode::Greater | CondCode::GreaterEq => {
                self.load_int_const(MachineMode::Int32, dest, 0);

                match mode {
                    MachineMode::Float32 => asm::ucomiss(self, lhs, rhs),
                    MachineMode::Float64 => asm::ucomisd(self, lhs, rhs),
                    _ => unreachable!(),
                }

                let cond = match cond {
                    CondCode::Greater => CondCode::UnsignedGreater,
                    CondCode::GreaterEq => CondCode::UnsignedGreaterEq,
                    _ => unreachable!(),
                };

                asm::emit_setb_reg(self, cond, dest);
            }

            CondCode::Less | CondCode::LessEq => {
                self.load_int_const(MachineMode::Int32, dest, 0);

                match mode {
                    MachineMode::Float32 => asm::ucomiss(self, rhs, lhs),
                    MachineMode::Float64 => asm::ucomisd(self, rhs, lhs),
                    _ => unreachable!(),
                }

                let cond = match cond {
                    CondCode::Less => CondCode::UnsignedGreater,
                    CondCode::LessEq => CondCode::UnsignedGreaterEq,
                    _ => unreachable!(),
                };

                asm::emit_setb_reg(self, cond, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn float_cmp_nan(&mut self, mode: MachineMode, dest: Reg, src: FReg) {
        self.load_int_const(MachineMode::Int32, dest, 0);

        match mode {
            MachineMode::Float32 => asm::ucomiss(self, src, src),
            MachineMode::Float64 => asm::ucomisd(self, src, src),
            _ => unreachable!(),
        }

        asm::emit_setb_reg_parity(self, dest, true);
    }

    pub fn cmp_zero(&mut self, mode: MachineMode, lhs: Reg) {
        asm::emit_cmp_imm_reg(self, mode, 0, lhs);
    }

    pub fn test_and_jump_if(&mut self, cond: CondCode, reg: Reg, lbl: Label) {
        assert!(cond == CondCode::Zero || cond == CondCode::NonZero);

        asm::emit_testl_reg_reg(self, reg, reg);
        self.jump_if(cond, lbl);
    }

    pub fn jump_if(&mut self, cond: CondCode, lbl: Label) {
        asm::emit_jcc(self, cond, lbl);
    }

    pub fn jump(&mut self, lbl: Label) {
        asm::emit_jmp(self, lbl);
    }

    pub fn jump_reg(&mut self, reg: Reg) {
        asm::emit_jmp_reg(self, reg);
    }

    pub fn int_div(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg, pos: Position) {
        self.div_common(mode, dest, lhs, rhs, RAX, pos);
    }

    pub fn int_mod(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg, pos: Position) {
        self.div_common(mode, dest, lhs, rhs, RDX, pos);
    }

    fn div_common(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        result: Reg,
        pos: Position,
    ) {
        asm::emit_cmp_imm_reg(self, mode, 0, rhs);
        let lbl = self.create_label();
        self.jump_if(CondCode::Zero, lbl);
        self.emit_bailout(lbl, Trap::DIV0, pos);

        if lhs != RAX {
            assert!(rhs != RAX);
            self.mov_rr(mode.is64(), RAX.into(), lhs.into());
        }

        if mode.w() != 0 {
            self.asm.cqo();
        } else {
            self.asm.cdq();
        }

        asm::emit_idiv_reg_reg(self, mode.w(), rhs);

        if dest != result {
            self.mov_rr(mode.is64(), dest.into(), result.into());
        }
    }

    pub fn int_mul(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_imul_reg_reg(self, mode.w(), rhs, lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_add(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if mode.is64() {
            self.asm.addq_rr(lhs.into(), rhs.into());
        } else {
            self.asm.addl_rr(lhs.into(), rhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_add_imm(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, value: i64) {
        if !fits_i32(value) {
            assert!(mode == MachineMode::Int64 || mode == MachineMode::Ptr);
            let reg_size = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *reg_size, value);
            self.int_add(mode, dest, lhs, *reg_size);
            return;
        }

        assert!(mode.is64());
        asm::emit_addq_imm_reg(self, value as i32, lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_sub(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_sub_reg_reg(self, mode.w(), rhs, lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_shl(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        asm::emit_shl_reg_cl(self, mode.w(), lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_shr(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        asm::emit_shr_reg_cl(self, mode.w(), lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_sar(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        asm::emit_sar_reg_cl(self, mode.w(), lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_rol(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        asm::emit_rol_reg_cl(self, mode.w(), lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_ror(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        asm::emit_ror_reg_cl(self, mode.w(), lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_or(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_or_reg_reg(self, mode.w(), rhs, lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_and(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_and_reg_reg(self, mode.w(), rhs, lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_xor(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_xor_reg_reg(self, mode.w(), rhs, lhs);

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn count_bits(&mut self, mode: MachineMode, dest: Reg, src: Reg, count_one_bits: bool) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => unimplemented!(),
        };

        if count_one_bits {
            asm::popcnt(self, x64, dest, src);
        } else {
            asm::emit_not_reg(self, x64, src);
            asm::popcnt(self, x64, dest, src);
        }
    }

    pub fn count_bits_leading(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        src: Reg,
        count_one_bits: bool,
    ) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => unimplemented!(),
        };

        if count_one_bits {
            asm::emit_not_reg(self, x64, src);
            asm::lzcnt(self, x64, dest, src);
        } else {
            asm::lzcnt(self, x64, dest, src);
        }
    }

    pub fn count_bits_trailing(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        src: Reg,
        count_one_bits: bool,
    ) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => unimplemented!(),
        };

        if count_one_bits {
            asm::emit_not_reg(self, x64, src);
            asm::tzcnt(self, x64, dest, src);
        } else {
            asm::tzcnt(self, x64, dest, src);
        }
    }

    pub fn int_to_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        asm::pxor(self, dest, dest);

        match dest_mode {
            MachineMode::Float32 => asm::cvtsi2ss(self, dest, src_mode.w(), src),
            MachineMode::Float64 => asm::cvtsi2sd(self, dest, src_mode.w(), src),
            _ => unreachable!(),
        }
    }

    pub fn float_to_int(
        &mut self,
        dest_mode: MachineMode,
        dest: Reg,
        src_mode: MachineMode,
        src: FReg,
    ) {
        match src_mode {
            MachineMode::Float32 => asm::cvttss2si(self, dest_mode.w(), dest, src),
            MachineMode::Float64 => asm::cvttsd2si(self, dest_mode.w(), dest, src),
            _ => unreachable!(),
        }
    }

    pub fn float_to_double(&mut self, dest: FReg, src: FReg) {
        asm::cvtss2sd(self, dest, src);
    }

    pub fn double_to_float(&mut self, dest: FReg, src: FReg) {
        asm::cvtsd2ss(self, dest, src);
    }

    pub fn int_as_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        assert!(src_mode.size() == dest_mode.size());

        match dest_mode {
            MachineMode::Float32 | MachineMode::Float64 => {
                asm::movd_i2f(self, src_mode.w(), dest, src)
            }
            _ => unreachable!(),
        }
    }

    pub fn float_as_int(
        &mut self,
        dest_mode: MachineMode,
        dest: Reg,
        src_mode: MachineMode,
        src: FReg,
    ) {
        assert!(src_mode.size() == dest_mode.size());

        match src_mode {
            MachineMode::Float32 | MachineMode::Float64 => {
                asm::movd_f2i(self, dest_mode.w(), dest, src)
            }
            _ => unreachable!(),
        }
    }

    pub fn determine_array_size(
        &mut self,
        dest: Reg,
        length: Reg,
        element_size: i32,
        with_header: bool,
    ) {
        assert!(element_size == 1 || element_size == 2 || element_size == 4 || element_size == 8);

        let header_size = if with_header {
            Header::size() + ptr_width()
        } else {
            0
        };

        let size = header_size
            + if element_size != ptr_width() {
                ptr_width() - 1
            } else {
                0
            };

        asm::lea(self, dest, Mem::Offset(length, element_size, size));

        if element_size != ptr_width() {
            asm::emit_andq_imm_reg(self, -ptr_width(), dest);
        }
    }

    pub fn check_index_out_of_bounds(&mut self, pos: Position, array: Reg, index: Reg) {
        let scratch = self.get_scratch();
        self.load_mem(
            MachineMode::Int32,
            (*scratch).into(),
            Mem::Base(array, offset_of_array_length()),
        );
        self.cmp_reg(MachineMode::Int32, index, *scratch);

        let lbl = self.create_label();
        self.jump_if(CondCode::UnsignedGreaterEq, lbl);
        self.emit_bailout(lbl, Trap::INDEX_OUT_OF_BOUNDS, pos);
    }

    pub fn load_nil(&mut self, dest: Reg) {
        asm::emit_movl_imm_reg(self, 0, dest);
    }

    pub fn load_field(
        &mut self,
        mode: MachineMode,
        dest: ExprStore,
        base: Reg,
        offset: i32,
        pos: Position,
    ) {
        self.test_if_nil_bailout(pos, base, Trap::NIL);
        self.load_mem(mode, dest, Mem::Base(base, offset));
    }

    pub fn load_mem(&mut self, mode: MachineMode, dest: ExprStore, mem: Mem) {
        match mem {
            Mem::Local(offset) => match mode {
                MachineMode::Int8 => asm::emit_movzbl_memq_reg(self, RBP, offset, dest.reg()),
                MachineMode::Int32 => asm::emit_movl_memq_reg(self, RBP, offset, dest.reg()),
                MachineMode::Int64 | MachineMode::Ptr => {
                    asm::emit_movq_memq_reg(self, RBP, offset, dest.reg())
                }
                MachineMode::Float32 => asm::movss_load(self, dest.freg(), mem),
                MachineMode::Float64 => asm::movsd_load(self, dest.freg(), mem),
            },

            Mem::Base(base, disp) => match mode {
                MachineMode::Int8 => asm::emit_movzbl_memq_reg(self, base, disp, dest.reg()),
                MachineMode::Int32 => asm::emit_movl_memq_reg(self, base, disp, dest.reg()),
                MachineMode::Int64 | MachineMode::Ptr => {
                    asm::emit_movq_memq_reg(self, base, disp, dest.reg())
                }
                MachineMode::Float32 => asm::movss_load(self, dest.freg(), mem),
                MachineMode::Float64 => asm::movsd_load(self, dest.freg(), mem),
            },

            Mem::Index(base, index, scale, disp) => match mode {
                MachineMode::Int8 => {
                    assert!(scale == 1);
                    asm::emit_movzx_memindex_byte_reg(self, 0, base, index, disp, dest.reg())
                }

                MachineMode::Int32 | MachineMode::Int64 | MachineMode::Ptr => {
                    asm::emit_mov_memindex_reg(self, mode, base, index, scale, disp, dest.reg())
                }

                MachineMode::Float32 => asm::movss_load(self, dest.freg(), mem),
                MachineMode::Float64 => asm::movsd_load(self, dest.freg(), mem),
            },

            Mem::Offset(_, _, _) => unimplemented!(),
        }
    }

    pub fn lea(&mut self, dest: Reg, mem: Mem) {
        asm::lea(self, dest, mem);
    }

    pub fn store_field(
        &mut self,
        mode: MachineMode,
        base: Reg,
        offset: i32,
        src: ExprStore,
        pos: Position,
        write_barrier: bool,
        card_table_offset: usize,
    ) {
        self.test_if_nil_bailout(pos, base, Trap::NIL);
        self.store_mem(mode, Mem::Base(base, offset), src);

        if write_barrier {
            self.emit_barrier(base, card_table_offset);
        }
    }

    fn emit_barrier(&mut self, src: Reg, card_table_offset: usize) {
        asm::emit_shr_reg_imm(self, 1, src, CARD_SIZE_BITS as u8);

        // test if card table offset fits into displacement of memory store
        if card_table_offset <= 0x7FFF_FFFF {
            // emit mov [card_table_offset + base], 0
            asm::emit_movb_imm_memq(self, 0, src, card_table_offset as i32);
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, card_table_offset as i64);
            asm::emit_movb_imm_memscaleq(self, 0, src, *scratch, 0);
        }
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: ExprStore) {
        match mem {
            Mem::Local(offset) => match mode {
                MachineMode::Int8 => asm::emit_movb_reg_memq(self, src.reg(), RBP, offset),
                MachineMode::Int32 => asm::emit_movl_reg_memq(self, src.reg(), RBP, offset),
                MachineMode::Int64 | MachineMode::Ptr => {
                    asm::emit_movq_reg_memq(self, src.reg(), RBP, offset)
                }
                MachineMode::Float32 => asm::movss_store(self, mem, src.freg()),
                MachineMode::Float64 => asm::movsd_store(self, mem, src.freg()),
            },

            Mem::Base(base, disp) => match mode {
                MachineMode::Int8 => asm::emit_movb_reg_memq(self, src.reg(), base, disp),
                MachineMode::Int32 => asm::emit_movl_reg_memq(self, src.reg(), base, disp),
                MachineMode::Int64 | MachineMode::Ptr => {
                    asm::emit_movq_reg_memq(self, src.reg(), base, disp)
                }
                MachineMode::Float32 => asm::movss_store(self, mem, src.freg()),
                MachineMode::Float64 => asm::movsd_store(self, mem, src.freg()),
            },

            Mem::Index(base, index, scale, disp) => match mode {
                MachineMode::Int8 | MachineMode::Int32 | MachineMode::Int64 | MachineMode::Ptr => {
                    asm::emit_mov_reg_memindex(self, mode, src.reg(), base, index, scale, disp)
                }

                MachineMode::Float32 => asm::movss_store(self, mem, src.freg()),
                MachineMode::Float64 => asm::movsd_store(self, mem, src.freg()),
            },

            Mem::Offset(_, _, _) => unimplemented!(),
        }
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.mov_rr(mode.is64(), dest.into(), src.into());
    }

    pub fn copy_pc(&mut self, dest: Reg) {
        asm::lea(self, dest, Mem::Base(RIP, 0));
    }

    pub fn copy_ra(&mut self, dest: Reg) {
        self.load_mem(MachineMode::Ptr, dest.into(), Mem::Base(REG_SP, 0));
    }

    pub fn copy_sp(&mut self, dest: Reg) {
        self.copy_reg(MachineMode::Ptr, dest, REG_SP);
    }

    pub fn set_sp(&mut self, src: Reg) {
        self.copy_reg(MachineMode::Ptr, REG_SP, src);
    }

    pub fn copy_freg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => asm::movss(self, dest, src),
            MachineMode::Float64 => asm::movsd(self, dest, src),
            _ => unreachable!(),
        }
    }

    pub fn extend_int_long(&mut self, dest: Reg, src: Reg) {
        asm::emit_movsx(self, src, dest);
    }

    pub fn extend_byte(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        asm::emit_movzx_byte(self, mode.w(), src, dest);
    }

    pub fn load_constpool(&mut self, dest: Reg, disp: i32) {
        // next instruction has 7 bytes
        let disp = -(disp + 7);

        asm::emit_movq_memq_reg(self, RIP, disp, dest); // 7 bytes
    }

    pub fn call_reg(&mut self, reg: Reg) {
        asm::emit_callq_reg(self, reg);
    }

    // emit debug instruction
    pub fn debug(&mut self) {
        // emit int3 = 0xCC
        asm::emit_op(self, 0xCC);
    }

    pub fn load_int_const(&mut self, mode: MachineMode, dest: Reg, imm: i64) {
        match mode {
            MachineMode::Int8 | MachineMode::Int32 => {
                asm::emit_movl_imm_reg(self, imm as i32, dest)
            }
            MachineMode::Int64 | MachineMode::Ptr => {
                if fits_i32(imm) {
                    asm::emit_movq_imm_reg(self, imm as i32, dest);
                } else {
                    asm::emit_movq_imm64_reg(self, imm, dest);
                }
            }
            MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
        }
    }

    pub fn load_float_const(&mut self, mode: MachineMode, dest: FReg, imm: f64) {
        let pos = self.pos() as i32;
        let inst_size = 8 + if dest.msb() != 0 { 1 } else { 0 };

        match mode {
            MachineMode::Float32 => {
                let off = self.dseg.add_f32(imm as f32);
                asm::movss_load(self, dest, Mem::Base(RIP, -(off + pos + inst_size)));
            }

            MachineMode::Float64 => {
                let off = self.dseg.add_f64(imm);
                asm::movsd_load(self, dest, Mem::Base(RIP, -(off + pos + inst_size)));
            }

            _ => unreachable!(),
        }
    }

    pub fn load_true(&mut self, dest: Reg) {
        asm::emit_movl_imm_reg(self, 1, dest);
    }

    pub fn load_false(&mut self, dest: Reg) {
        asm::emit_movl_imm_reg(self, 0, dest);
    }

    pub fn int_neg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        asm::emit_neg_reg(self, mode.w(), src);

        if dest != src {
            self.mov_rr(mode.is64(), dest.into(), src.into());
        }
    }

    pub fn int_not(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        match mode {
            MachineMode::Int8 => {
                asm::emit_not_reg_byte(self, src);
            }

            MachineMode::Int32 => {
                asm::emit_not_reg(self, 0, src);
            }

            MachineMode::Int64 => {
                asm::emit_not_reg(self, 1, src);
            }

            _ => unimplemented!(),
        };

        if dest != src {
            self.mov_rr(mode.is64(), dest.into(), src.into());
        }
    }

    pub fn bool_not(&mut self, dest: Reg, src: Reg) {
        asm::emit_xorb_imm_reg(self, 1, src);
        asm::emit_andb_imm_reg(self, 1, src);

        if dest != src {
            self.asm.movl_rr(dest.into(), src.into());
        }
    }

    pub fn float_add(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => asm::addss(self, lhs, rhs),
            MachineMode::Float64 => asm::addsd(self, lhs, rhs),
            _ => unimplemented!(),
        }

        if dest != lhs {
            self.copy_freg(mode, dest, lhs);
        }
    }

    pub fn float_sub(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => asm::subss(self, lhs, rhs),
            MachineMode::Float64 => asm::subsd(self, lhs, rhs),
            _ => unimplemented!(),
        }

        if dest != lhs {
            self.copy_freg(mode, dest, lhs);
        }
    }

    pub fn float_mul(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => asm::mulss(self, lhs, rhs),
            MachineMode::Float64 => asm::mulsd(self, lhs, rhs),
            _ => unimplemented!(),
        }

        if dest != lhs {
            self.copy_freg(mode, dest, lhs);
        }
    }

    pub fn float_div(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => asm::divss(self, lhs, rhs),
            MachineMode::Float64 => asm::divsd(self, lhs, rhs),
            _ => unimplemented!(),
        }

        if dest != lhs {
            self.copy_freg(mode, dest, lhs);
        }
    }

    pub fn float_neg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        let (fst, snd) = if mode == MachineMode::Float32 {
            (1i32 << 31, 0)
        } else {
            (0, 1i32 << 31)
        };

        // align MMX data to 16 bytes
        self.dseg.align(16);
        self.dseg.add_i32(0);
        self.dseg.add_i32(0);
        self.dseg.add_i32(snd);
        let disp = self.dseg.add_i32(fst);

        let pos = self.pos() as i32;
        let mem = Mem::Base(RIP, 0);

        match mode {
            MachineMode::Float32 => asm::xorps(self, src, mem),
            MachineMode::Float64 => asm::xorpd(self, src, mem),
            _ => unimplemented!(),
        }

        let after = self.pos() as i32;
        let len = after - pos;

        let offset = -(disp + pos + len);
        self.emit_u32_at(after - 4, offset as u32);

        if dest != src {
            self.copy_freg(mode, dest, src);
        }
    }

    pub fn float_sqrt(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => asm::sqrtss(self, dest, src),
            MachineMode::Float64 => asm::sqrtsd(self, dest, src),
            _ => unreachable!(),
        }
    }

    pub fn trap(&mut self, trap: Trap, pos: Position) {
        let vm = get_vm();
        self.load_int_const(MachineMode::Int32, REG_PARAMS[0], trap.int() as i64);
        self.raw_call(vm.trap_stub().to_ptr());
        self.emit_position(pos);
    }

    pub fn throw(&mut self, exception: Reg, pos: Position) {
        let vm = get_vm();
        self.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::exception_object_offset()),
            exception.into(),
        );
        self.raw_call(vm.throw_stub().to_ptr());
        self.emit_position(pos);
    }

    pub fn nop(&mut self) {
        self.asm.nop();
    }

    pub fn emit_label(&mut self, lbl: Label) {
        let value = self.labels[lbl.index()];

        match value {
            // backward jumps already know their target
            Some(idx) => {
                let current = self.pos() + 4;
                let target = idx;

                let diff = -((current - target) as i32);
                self.emit_u32(diff as u32);
            }

            // forward jumps do not know their target yet
            // we need to do this later...
            None => {
                let pos = self.pos();
                self.emit_u32(0);
                self.jumps.push(ForwardJump { at: pos, to: lbl });
            }
        }
    }

    pub fn fix_forward_jumps(&mut self) {
        for jmp in &self.jumps {
            let target = self.labels[jmp.to.0].expect("label not defined");
            let diff = (target - jmp.at - 4) as i32;

            let mut slice = &mut self.asm.code_mut()[jmp.at..];
            slice.write_u32::<LittleEndian>(diff as u32).unwrap();
        }
    }

    pub fn check_polling_page(&mut self, page: Address) {
        let disp = self.dseg.add_addr_reuse(page.to_ptr());
        let pos = self.pos() as i32;

        let scratch = self.get_scratch();
        self.load_constpool(*scratch, disp + pos);

        asm::testl_reg_mem(self, RAX, Mem::Base(*scratch, 0));
    }

    fn mov_rr(&mut self, x64: bool, lhs: AsmRegister, rhs: AsmRegister) {
        if x64 {
            self.asm.movq_rr(lhs, rhs);
        } else {
            self.asm.movl_rr(lhs, rhs);
        }
    }
}

impl MachineMode {
    fn w(self) -> u8 {
        match self {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => unreachable!(),
        }
    }

    fn is64(self) -> bool {
        match self {
            MachineMode::Int8 | MachineMode::Int32 => false,
            MachineMode::Int64 | MachineMode::Ptr => true,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct ForwardJump {
    at: usize,
    to: Label,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backward() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.bind_label(lbl);
        masm.emit_label(lbl);

        assert_eq!(vec![0xfc, 0xff, 0xff, 0xff], masm.data());
    }

    #[test]
    fn test_forward_with_gap() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.emit_label(lbl);
        masm.emit_u8(0x11);
        masm.bind_label(lbl);

        assert_eq!(vec![1, 0, 0, 0, 0x11], masm.data());
    }

    #[test]
    fn test_forward() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.emit_label(lbl);
        masm.bind_label(lbl);

        assert_eq!(vec![0, 0, 0, 0], masm.data());
    }

    #[test]
    fn test_backward_with_gap() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.bind_label(lbl);
        masm.emit_u8(0x33);
        masm.emit_label(lbl);

        assert_eq!(vec![0x33, 0xfb, 0xff, 0xff, 0xff], masm.data());
    }

    #[test]
    #[should_panic]
    fn test_label_undefined() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();

        masm.emit_label(lbl);
        masm.data();
    }
}
