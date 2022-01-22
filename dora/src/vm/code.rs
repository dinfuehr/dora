use std::collections::HashSet;
use std::fmt;
use std::ptr;
use std::sync::Arc;

use crate::cpu::flush_icache;
use crate::gc::Address;
use crate::language::ty::SourceTypeArray;
use crate::masm::CodeDescriptor;
use crate::mem;
use crate::os;
use crate::utils::GrowableVec;
use crate::vm::FctDefinitionId;
use crate::vm::VM;

use dora_parser::Position;

#[derive(Debug, Clone)]
pub enum CodeKind {
    DoraFct(FctDefinitionId),
    CompileStub,
    TrapStub,
    AllocStub,
    VerifyStub,
    NativeStub(FctDefinitionId),
    DoraStub,
    GuardCheckStub,
    SafepointStub,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CodeId(usize);

impl CodeId {
    pub fn from(idx: usize) -> CodeId {
        CodeId(idx)
    }

    pub fn idx(self) -> usize {
        self.0
    }
}

impl From<usize> for CodeId {
    fn from(data: usize) -> CodeId {
        CodeId(data)
    }
}

impl GrowableVec<Code> {
    pub fn idx(&self, index: CodeId) -> Arc<Code> {
        self.idx_usize(index.0)
    }
}

pub fn install_code_stub(vm: &VM, code_descriptor: CodeDescriptor, kind: CodeKind) -> Arc<Code> {
    let code = install_code(vm, code_descriptor, kind);
    vm.add_code(code.clone());
    code
}

pub fn install_code(vm: &VM, code_descriptor: CodeDescriptor, kind: CodeKind) -> Arc<Code> {
    let object_header_size = mem::ptr_width_usize() * 4;
    let object_size =
        object_header_size + code_descriptor.constpool.size() as usize + code_descriptor.code.len();

    let object_start = vm.gc.alloc_code(object_size);
    let object_end = object_start.offset(object_size);

    let constpool_start = object_start.offset(object_header_size);
    let instruction_start = constpool_start.offset(code_descriptor.constpool.size() as usize);

    if object_start.is_null() {
        panic!("out of memory: not enough executable memory left!");
    }

    os::jit_writable();

    code_descriptor.constpool.finish(constpool_start.to_ptr());

    unsafe {
        ptr::copy_nonoverlapping(
            code_descriptor.code.as_ptr(),
            instruction_start.to_mut_ptr(),
            code_descriptor.code.len(),
        );
    }

    os::jit_executable();

    flush_icache(object_start.to_ptr(), object_size);

    Arc::new(Code {
        object_start,
        object_end,
        instruction_start,
        kind,
        lazy_compilation: code_descriptor.lazy_compilation,
        gcpoints: code_descriptor.gcpoints,
        comments: code_descriptor.comments,
        positions: code_descriptor.positions,
    })
}

pub struct Code {
    object_start: Address,
    object_end: Address,

    // pointer to beginning of function
    instruction_start: Address,

    kind: CodeKind,

    lazy_compilation: LazyCompilationData,
    gcpoints: GcPointTable,
    comments: CommentTable,
    positions: PositionTable,
}

impl Code {
    pub fn position_for_offset(&self, offset: u32) -> Option<Position> {
        self.positions.get(offset)
    }

    pub fn gcpoint_for_offset(&self, offset: u32) -> Option<&GcPoint> {
        self.gcpoints.get(offset)
    }

    pub fn object_start(&self) -> Address {
        self.object_start
    }

    pub fn object_end(&self) -> Address {
        self.object_end
    }

    pub fn fct_id(&self) -> FctDefinitionId {
        match self.kind {
            CodeKind::NativeStub(fct_id) => fct_id,
            CodeKind::DoraFct(fct_id) => fct_id,
            _ => panic!("no fctid found"),
        }
    }

    pub fn instruction_start(&self) -> Address {
        self.instruction_start
    }

    pub fn instruction_end(&self) -> Address {
        self.object_end
    }

    pub fn comment_for_offset(&self, offset: u32) -> Option<&String> {
        self.comments.get(offset)
    }

    pub fn lazy_for_offset(&self, offset: u32) -> Option<&LazyCompilationSite> {
        self.lazy_compilation.get(offset)
    }

    pub fn descriptor(&self) -> CodeKind {
        self.kind.clone()
    }
}

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Code {{ start: {:?}, end: {:?}, desc: {:?} }}",
            self.object_start(),
            self.object_end(),
            self.kind,
        )
    }
}

#[derive(Debug)]
pub struct GcPointTable {
    entries: Vec<(u32, GcPoint)>,
}

impl GcPointTable {
    pub fn new() -> GcPointTable {
        GcPointTable {
            entries: Vec::new(),
        }
    }

    pub fn get(&self, offset: u32) -> Option<&GcPoint> {
        let result = self
            .entries
            .binary_search_by_key(&offset, |&(offset, _)| offset);

        match result {
            Ok(idx) => Some(&self.entries[idx].1),
            Err(_) => None,
        }
    }

    pub fn insert(&mut self, offset: u32, gcpoint: GcPoint) {
        if let Some(last) = self.entries.last_mut() {
            debug_assert!(offset > last.0);
        }

        self.entries.push((offset, gcpoint));
    }
}

#[derive(Debug)]
pub struct GcPoint {
    pub offsets: Vec<i32>,
}

impl GcPoint {
    pub fn new() -> GcPoint {
        GcPoint {
            offsets: Vec::new(),
        }
    }

    pub fn merge(lhs: GcPoint, rhs: GcPoint) -> GcPoint {
        let mut offsets = HashSet::new();

        for offset in lhs.offsets {
            offsets.insert(offset);
        }

        for offset in rhs.offsets {
            offsets.insert(offset);
        }

        GcPoint::from_offsets(offsets.drain().collect())
    }

    pub fn from_offsets(offsets: Vec<i32>) -> GcPoint {
        GcPoint { offsets }
    }
}

pub struct CommentTable {
    entries: Vec<(u32, String)>,
}

impl CommentTable {
    pub fn new() -> CommentTable {
        CommentTable {
            entries: Vec::new(),
        }
    }

    pub fn get(&self, offset: u32) -> Option<&String> {
        let result = self
            .entries
            .binary_search_by_key(&offset, |&(offset, _)| offset);

        match result {
            Ok(idx) => Some(&self.entries[idx].1),
            Err(_) => None,
        }
    }

    pub fn insert(&mut self, offset: u32, comment: String) {
        if let Some(last) = self.entries.last_mut() {
            debug_assert!(offset >= last.0);
        }

        self.entries.push((offset, comment));
    }
}

#[derive(Debug)]
pub struct PositionTable {
    entries: Vec<(u32, Position)>,
}

impl PositionTable {
    pub fn new() -> PositionTable {
        PositionTable {
            entries: Vec::new(),
        }
    }

    pub fn insert(&mut self, offset: u32, position: Position) {
        if let Some(last) = self.entries.last() {
            debug_assert!(offset > last.0);
        }

        self.entries.push((offset, position));
    }

    pub fn get(&self, offset: u32) -> Option<Position> {
        let result = self
            .entries
            .binary_search_by_key(&offset, |&(offset, _)| offset);

        match result {
            Ok(idx) => Some(self.entries[idx].1),
            Err(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct LazyCompilationData {
    entries: Vec<(u32, LazyCompilationSite)>,
}

impl LazyCompilationData {
    pub fn new() -> LazyCompilationData {
        LazyCompilationData {
            entries: Vec::new(),
        }
    }

    pub fn insert(&mut self, offset: u32, info: LazyCompilationSite) {
        if let Some(last) = self.entries.last() {
            debug_assert!(offset > last.0);
        }

        self.entries.push((offset, info));
    }

    pub fn get(&self, offset: u32) -> Option<&LazyCompilationSite> {
        let result = self
            .entries
            .binary_search_by_key(&offset, |&(offset, _)| offset);

        match result {
            Ok(idx) => Some(&self.entries[idx].1),
            Err(_) => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum LazyCompilationSite {
    Direct(FctDefinitionId, i32, SourceTypeArray),
    Virtual(bool, FctDefinitionId, u32, SourceTypeArray),
}
