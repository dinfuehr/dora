use std::collections::hash_map::HashMap;
use std::mem::size_of;

use cpu::{emit, REG_FP, REG_PARAMS, REG_RESULT, REG_SP};
use ctxt::{Context, FctId, get_ctxt};
use jit::buffer::Buffer;
use jit::fct::JitFct;
use mem;
use stacktrace::StackFrameInfo;
use ty::{BuiltinType, MachineMode};

pub struct NativeFcts {
    map: HashMap<*const u8, JitFct>,
}

impl NativeFcts {
    pub fn new() -> NativeFcts {
        NativeFcts {
            map: HashMap::new()
        }
    }

    pub fn find_fct(&self, ptr: *const u8) -> Option<*const u8> {
        self.map.get(&ptr).map(|jit_fct| jit_fct.fct_start.raw() as *const u8)
    }

    pub fn insert_fct(&mut self, ptr: *const u8, fct: JitFct) -> *const u8 {
        self.map.entry(ptr).or_insert(fct).fct_start.raw() as *const u8
    }
}

pub fn generate<'a, 'ast: 'a>(ctxt: &'a Context<'ast>, ptr: *const u8,
                              return_type: BuiltinType, args: i32) -> JitFct {
    let ngen = NativeGen {
        ctxt: ctxt,
        ptr: ptr,
        buf: Buffer::new(),
        return_type: return_type,
        args: args,
    };

    ngen.generate()
}

struct NativeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    ptr: *const u8,
    buf: Buffer,

    return_type: BuiltinType,
    args: i32,
}

impl<'a, 'ast> NativeGen<'a, 'ast> where 'ast: 'a  {
    pub fn generate(mut self) -> JitFct {
        let save_return = self.return_type != BuiltinType::Unit;

        let framesize = size_of::<StackFrameInfo>() as i32
                        + if save_return { 8 } else { 0 }
                        + self.args * 8;

        let framesize = mem::align_i32(framesize, 16);

        let offset_return = 0;
        let offset_args = offset_return + if save_return { 8 } else { 0 };
        // let offset_sfi = offset_args + self.args * 8;

        emit::prolog(&mut self.buf, framesize);

        assert!(self.args <= REG_PARAMS.len() as i32);

        for (ind, &reg) in REG_PARAMS.iter().take(self.args as usize).enumerate() {
            emit::mov_reg_mem(&mut self.buf, MachineMode::Ptr, reg, REG_SP,
                              offset_args + ind as i32 * 8);
        }

        emit::mov_reg_reg(&mut self.buf, MachineMode::Ptr, REG_FP, REG_PARAMS[0]);
        emit::direct_call(&mut self.buf, start_native_call as *const u8);

        for (ind, &reg) in REG_PARAMS.iter().take(self.args as usize).enumerate() {
            emit::mov_mem_reg(&mut self.buf, MachineMode::Ptr, REG_SP,
                              offset_args + ind as i32 * 8, reg);
        }

        emit::direct_call(&mut self.buf, self.ptr);

        if save_return {
            emit::mov_reg_mem(&mut self.buf, MachineMode::Ptr, REG_RESULT, REG_SP, 0);
        }

        emit::direct_call(&mut self.buf, finish_native_call as *const u8);

        if save_return {
            emit::mov_mem_reg(&mut self.buf, MachineMode::Ptr, REG_SP, 0, REG_RESULT);
        }

        emit::epilog(&mut self.buf, framesize);

        self.buf.jit(FctId(0), framesize)
    }
}

fn start_native_call(fp: *const u8) {
    unsafe {
        // fp is framepointer of native stub

        // get framepointer of dora function and return address into dora
        let dora_ra = *(fp.offset(8) as *const usize);
        let dora_fp = *(fp as *const usize);

        let sfi_size = size_of::<StackFrameInfo>() as isize;
        let sfi: *mut StackFrameInfo = fp.offset(-sfi_size) as *mut StackFrameInfo;
        let sfi: &mut StackFrameInfo = &mut *sfi;

        sfi.sp = 0;
        sfi.fp = dora_fp;
        sfi.ra = dora_ra;
        sfi.xpc = sfi.ra - 1;

        let ctxt = get_ctxt();
        ctxt.push_sfi(sfi);
    }
}

fn finish_native_call() {
    get_ctxt().pop_sfi();
}