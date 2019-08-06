use libc;

use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::OpenOptions;
use std::io::{self, BufWriter, Write};
use std::slice;
use std::sync::Arc;

use capstone::{Engine, Error};

use crate::baseline::asm::BaselineAssembler;
use crate::baseline::ast::{self, AstCodeGen};
use crate::baseline::cannon::CannonCodeGen;
use crate::baseline::expr::*;
use crate::baseline::fct::{CommentFormat, GcPoint, JitBaselineFct, JitFct};
use crate::baseline::info::JitInfo;
use crate::baseline::map::CodeDescriptor;
use crate::class::TypeParams;
use crate::cpu::{FREG_RESULT, REG_RESULT};
use crate::ctxt::VM;
use crate::ctxt::{Fct, FctId, FctSrc, VarId};
use crate::driver::cmd::{AsmSyntax, BaselineName};
use crate::gc::Address;
use crate::masm::*;
use crate::mem;
use crate::ty::MachineMode;

pub fn generate<'ast>(
    vm: &VM<'ast>,
    id: FctId,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> Address {
    let fct = vm.fcts.idx(id);
    let fct = fct.read();
    let src = fct.src();
    let mut src = src.write();

    generate_fct(vm, &fct, &mut src, cls_type_params, fct_type_params)
}

pub fn generate_fct<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    src: &mut FctSrc,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> Address {
    debug_assert!(cls_type_params
        .iter()
        .all(|ty| !ty.contains_type_param(vm),));
    debug_assert!(fct_type_params
        .iter()
        .all(|ty| !ty.contains_type_param(vm),));

    {
        let specials = src.specializations.read();
        let key = (cls_type_params.clone(), fct_type_params.clone());

        if let Some(&jit_fct_id) = specials.get(&key) {
            let jit_fct = vm.jit_fcts.idx(jit_fct_id);
            return jit_fct.fct_ptr();
        }
    }

    let ast = fct.ast;

    let jit_fct = match vm.args.flag_bc {
        Some(BaselineName::Cannon) => CannonCodeGen {
            vm: vm,
            fct: &fct,
            ast: ast,
            asm: BaselineAssembler::new(vm),
            scopes: Scopes::new(),
            src: src,

            lbl_break: None,
            lbl_continue: None,

            active_finallys: Vec::new(),
            active_upper: None,
            active_loop: None,
            lbl_return: None,

            cls_type_params: cls_type_params,
            fct_type_params: fct_type_params,
        }
        .generate(),
        _ => {
            let mut jit_info = JitInfo::new();
            ast::info::generate(
                vm,
                fct,
                src,
                &mut jit_info,
                cls_type_params,
                fct_type_params,
            );
            AstCodeGen {
                vm: vm,
                fct: &fct,
                ast: ast,
                asm: BaselineAssembler::new(vm),
                scopes: Scopes::new(),
                src: src,
                jit_info: jit_info,

                lbl_break: None,
                lbl_continue: None,

                active_finallys: Vec::new(),
                active_upper: None,
                active_loop: None,
                lbl_return: None,

                cls_type_params: cls_type_params,
                fct_type_params: fct_type_params,
            }
            .generate()
        }
    };

    if should_emit_asm(vm, &*fct) {
        dump_asm(
            vm,
            &*fct,
            &jit_fct,
            Some(&src),
            vm.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att),
        );
    }

    let fct_ptr = jit_fct.fct_ptr();
    let ptr_start = jit_fct.ptr_start();
    let ptr_end = jit_fct.ptr_end();

    debug_assert!(mem::is_aligned(ptr_start.to_usize(), 16));
    debug_assert!(mem::is_aligned(fct_ptr.to_usize(), 16));

    let jit_fct_id = {
        let mut jit_fcts = vm.jit_fcts.lock();
        let jit_fct_id = jit_fcts.len().into();
        jit_fcts.push(Arc::new(JitFct::Base(jit_fct)));

        jit_fct_id
    };

    {
        let mut specials = src.specializations.write();
        let key = (cls_type_params.clone(), fct_type_params.clone());
        specials.insert(key, jit_fct_id);
    }

    {
        let mut code_map = vm.code_map.lock();
        let cdata = CodeDescriptor::DoraFct(jit_fct_id);
        code_map.insert(ptr_start, ptr_end, cdata);
    }

    fct_ptr
}

#[cfg(target_arch = "x86_64")]
fn get_engine() -> Result<Engine, Error> {
    use capstone::{Arch, MODE_64};

    Engine::new(Arch::X86, MODE_64)
}

#[cfg(target_arch = "aarch64")]
fn get_engine() -> Result<Engine, Error> {
    use capstone::{Arch, MODE_ARM};

    Engine::new(Arch::Arm64, MODE_ARM)
}

pub fn dump_asm<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    jit_fct: &JitBaselineFct,
    fct_src: Option<&FctSrc>,
    asm_syntax: AsmSyntax,
) {
    use capstone::*;

    let buf: &[u8] =
        unsafe { slice::from_raw_parts(jit_fct.fct_ptr().to_ptr(), jit_fct.fct_len()) };

    let asm_syntax = match asm_syntax {
        AsmSyntax::Intel => 1,
        AsmSyntax::Att => 2,
    };

    let engine = get_engine().expect("cannot create capstone engine");
    if let Err(_) = engine.set_option(Opt::Syntax, asm_syntax) {
        panic!("capstone: syntax couldn't be set");
    }

    let mut w: Box<dyn Write> = if vm.args.flag_emit_asm_file {
        let pid = unsafe { libc::getpid() };
        let name = format!("code-{}.asm", pid);
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&name)
            .expect("couldn't append to asm file");

        Box::new(BufWriter::new(file))
    } else {
        Box::new(io::stdout())
    };

    let start_addr = jit_fct.fct_ptr().to_usize() as u64;
    let end_addr = jit_fct.fct_end().to_usize() as u64;

    let instrs = engine
        .disasm(buf, start_addr, jit_fct.fct_len())
        .expect("could not disassemble code");

    let name = fct.full_name(vm);

    writeln!(&mut w, "fun {} {:#x} {:#x}", &name, start_addr, end_addr).unwrap();

    if let Some(fct_src) = fct_src {
        for var in &fct_src.vars {
            let name = vm.interner.str(var.name);
            writeln!(&mut w, "  var `{}`: type {}", name, var.ty.name(vm)).unwrap();
        }

        if fct_src.vars.len() > 0 {
            writeln!(&mut w).unwrap();
        }
    }

    for instr in instrs {
        let addr = (instr.addr - start_addr) as i32;

        if let Some(gc_point) = jit_fct.gcpoint_for_offset(addr) {
            write!(&mut w, "\t\t  ; gc point = (").unwrap();
            let mut first = true;

            for &offset in &gc_point.offsets {
                if !first {
                    write!(&mut w, ", ").unwrap();
                }

                if offset < 0 {
                    write!(&mut w, "-").unwrap();
                }

                write!(&mut w, "{:x}", offset.abs()).unwrap();
                first = false;
            }

            writeln!(&mut w, ")").unwrap();
        }

        if let Some(comments) = jit_fct.get_comment(addr) {
            for comment in comments {
                if comment.is_newline() {
                    writeln!(&mut w).unwrap();
                    continue;
                }

                let cfmt = CommentFormat {
                    comment: comment,
                    vm: vm,
                    fct_src: fct_src,
                };

                writeln!(&mut w, "\t\t  ; {}", cfmt).unwrap();
            }
        }

        writeln!(
            &mut w,
            "  {:#06x}: {}\t\t{}",
            instr.addr, instr.mnemonic, instr.op_str
        )
        .unwrap();
    }

    writeln!(&mut w).unwrap();
}

pub fn register_for_mode(mode: MachineMode) -> ExprStore {
    if mode.is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CondCode {
    Zero,
    NonZero,
    Equal,
    NotEqual,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    UnsignedGreater,
    UnsignedGreaterEq,
    UnsignedLess,
    UnsignedLessEq,
}

pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    pub fn new() -> Scopes {
        Scopes {
            scopes: vec![Scope::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new())
    }

    pub fn pop_scope(&mut self) {
        assert!(self.scopes.pop().is_some());
        assert!(self.scopes.len() >= 1);
    }

    pub fn add_var(&mut self, id: VarId, offset: i32) {
        let scope = self.scopes.last_mut().unwrap();
        assert!(scope.vars.insert(id, offset).is_none());
    }

    pub fn add_var_offset(&mut self, offset: i32) {
        let scope = self.scopes.last_mut().unwrap();
        scope.offsets.push(offset);
    }
}

struct Scope {
    vars: HashMap<VarId, i32>,
    offsets: Vec<i32>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            vars: HashMap::new(),
            offsets: Vec::new(),
        }
    }
}

pub struct TempOffsets {
    offsets: HashSet<i32>,
}

impl TempOffsets {
    pub fn new() -> TempOffsets {
        TempOffsets {
            offsets: HashSet::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.offsets.is_empty()
    }

    pub fn insert(&mut self, offset: i32) {
        assert!(self.offsets.insert(offset));
    }

    pub fn remove(&mut self, offset: i32) {
        assert!(self.offsets.remove(&offset));
    }
}

pub fn create_gcpoint(vars: &Scopes, temps: &TempOffsets) -> GcPoint {
    let mut offsets = Vec::new();

    for scope in &vars.scopes {
        for (_, &offset) in &scope.vars {
            offsets.push(offset);
        }

        offsets.extend_from_slice(&scope.offsets);
    }

    for &offset in &temps.offsets {
        offsets.push(offset);
    }

    GcPoint::from_offsets(offsets)
}

#[derive(Copy, Clone, Debug)]
pub enum Next {
    Flow(Label),
    Return,
}

pub fn should_emit_debug(vm: &VM, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_debug {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}

pub fn should_emit_asm(vm: &VM, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_asm {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}

pub fn fct_pattern_match(vm: &VM, fct: &Fct, pattern: &str) -> bool {
    if pattern == "all" {
        return true;
    }

    let name = vm.interner.str(fct.name);

    for part in pattern.split(',') {
        if *name == part {
            return true;
        }
    }

    false
}

pub trait CodeGen<'v> {
    fn generate(self) -> JitBaselineFct;
}
