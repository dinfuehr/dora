use std::collections::hash_map::HashMap;

use ctxt::Context;
use jit::buffer::Buffer;
use jit::fct::JitFct;
use mem::ptr::Ptr;

struct NativeFcts {
    map: HashMap<Ptr, JitFct>,    
}

impl NativeFcts {
    pub fn find_fct(&self, ptr: Ptr) -> Option<Ptr> {
        self.map.get(&ptr).map(|jit_fct| jit_fct.fct_start)
    }

    pub fn insert_fct(&mut self, ptr: Ptr, fct: JitFct) -> Ptr {
        self.map.entry(ptr).or_insert(fct).fct_start
    }
}

struct NativeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    ptr: *const u8,
    buf: Buffer,
}

impl<'a, 'ast> NativeGen<'a, 'ast> where 'ast: 'a  {
    fn generate(&self) -> JitFct {
        panic!("unimplemented!")
    }
}