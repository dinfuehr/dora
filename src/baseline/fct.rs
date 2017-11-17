use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::ops::Index;
use std::ptr;

use class::{ClassDef, ClassDefId, FieldId, TypeParams};
use cpu::flush_icache;
use ctxt::{FctId, FctSrc, GlobalId, SemContext, VarId};
use dseg::DSeg;
use object::{Handle, Str};
use opt::fct::JitOptFct;
use utils::GrowableVec;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct JitFctId(usize);

impl JitFctId {
    pub fn from(idx: usize) -> JitFctId {
        JitFctId(idx)
    }

    pub fn idx(self) -> usize {
        self.0
    }
}

impl<'ast> Index<JitFctId> for GrowableVec<JitFct> {
    type Output = RefCell<JitFct>;

    fn index(&self, index: JitFctId) -> &RefCell<JitFct> {
        &self[index.0]
    }
}

impl From<usize> for JitFctId {
    fn from(data: usize) -> JitFctId {
        JitFctId(data)
    }
}

pub enum JitFct {
    Base(JitBaselineFct),
    Opt(JitOptFct),
}

impl JitFct {
    pub fn fct_id(&self) -> FctId {
        match self {
            &JitFct::Base(ref base) => base.fct_id,
            &JitFct::Opt(ref opt) => opt.fct_id(),
        }
    }

    pub fn fct_ptr(&self) -> *const u8 {
        match self {
            &JitFct::Base(ref base) => base.fct_ptr(),
            &JitFct::Opt(ref opt) => opt.fct_ptr(),
        }
    }

    pub fn to_base(&self) -> Option<&JitBaselineFct> {
        match self {
            &JitFct::Base(ref base) => Some(base),
            _ => None,
        }
    }
}

pub struct JitBaselineFct {
    code_start: *const u8,
    code_end: *const u8,

    pub fct_id: FctId,
    pub throws: bool,

    // pointer to beginning of function
    pub fct_start: *const u8,

    // machine code length in bytes
    fct_len: usize,

    pub framesize: i32,
    pub bailouts: Bailouts,
    pub nil_checks: HashSet<i32>,
    gcpoints: GcPoints,
    comments: Comments,
    linenos: LineNumberTable,
    pub exception_handlers: Vec<ExHandler>,
}

impl JitBaselineFct {
    pub fn from_buffer(
        ctxt: &SemContext,
        dseg: &DSeg,
        buffer: &[u8],
        bailouts: Bailouts,
        nil_checks: HashSet<i32>,
        gcpoints: GcPoints,
        framesize: i32,
        comments: Comments,
        linenos: LineNumberTable,
        fct_id: FctId,
        throws: bool,
        mut exception_handlers: Vec<ExHandler>,
    ) -> JitBaselineFct {
        let size = dseg.size() as usize + buffer.len();
        let ptr = ctxt.gc.alloc_code(size);

        dseg.finish(ptr);

        let fct_start;

        unsafe {
            fct_start = ptr.offset(dseg.size() as isize);
            ptr::copy_nonoverlapping(buffer.as_ptr(), fct_start as *mut u8, buffer.len());
        }

        flush_icache(ptr, size);

        for handler in &mut exception_handlers {
            let fct_start = fct_start as usize;

            handler.try_start = fct_start + handler.try_start;
            handler.try_end = fct_start + handler.try_end;
            handler.catch = fct_start + handler.catch;
        }

        JitBaselineFct {
            code_start: ptr,
            code_end: unsafe { ptr.offset(size as isize) },
            bailouts: bailouts,
            nil_checks: nil_checks,
            gcpoints: gcpoints,
            comments: comments,
            framesize: framesize,
            fct_start: fct_start,
            fct_len: buffer.len(),
            linenos: linenos,
            fct_id: fct_id,
            throws: throws,
            exception_handlers: exception_handlers,
        }
    }

    pub fn lineno_for_offset(&self, offset: i32) -> i32 {
        self.linenos.get(offset)
    }

    pub fn gcpoint_for_offset(&self, offset: i32) -> Option<&GcPoint> {
        self.gcpoints.get(offset)
    }

    pub fn nil_check_for_offset(&self, offset: i32) -> bool {
        self.nil_checks.contains(&offset)
    }

    pub fn ptr_start(&self) -> *const u8 {
        self.code_start
    }

    pub fn ptr_end(&self) -> *const u8 {
        self.code_end
    }

    pub fn fct_ptr(&self) -> *const u8 {
        self.fct_start
    }

    pub fn fct_end(&self) -> *const u8 {
        (self.fct_start as usize + self.fct_len) as *const u8
    }

    pub fn fct_len(&self) -> usize {
        self.fct_len
    }

    pub fn get_comment(&self, pos: i32) -> Option<&[Comment]> {
        self.comments.get(pos)
    }
}

impl fmt::Debug for JitBaselineFct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "JitBaselineFct {{ start: {:?}, end: {:?}, fct_id: {:?} }}",
            self.ptr_start(),
            self.ptr_end(),
            self.fct_id,
        )
    }
}

#[derive(Debug)]
pub struct GcPoints {
    points: HashMap<i32, GcPoint>,
}

impl GcPoints {
    pub fn new() -> GcPoints {
        GcPoints { points: HashMap::new() }
    }

    pub fn get(&self, offset: i32) -> Option<&GcPoint> {
        self.points.get(&offset)
    }

    pub fn insert(&mut self, offset: i32, gcpoint: GcPoint) {
        assert!(self.points.insert(offset, gcpoint).is_none());
    }
}

#[derive(Debug)]
pub struct GcPoint {
    pub offsets: Vec<i32>,
}

impl GcPoint {
    pub fn new() -> GcPoint {
        GcPoint { offsets: Vec::new() }
    }

    pub fn from_offsets(offsets: Vec<i32>) -> GcPoint {
        GcPoint { offsets: offsets }
    }
}

pub struct Comments {
    comments: HashMap<i32, Vec<Comment>>,
}

impl Comments {
    pub fn new() -> Comments {
        Comments { comments: HashMap::new() }
    }

    pub fn get(&self, pos: i32) -> Option<&[Comment]> {
        self.comments.get(&pos).map(|c| c.as_slice())
    }

    pub fn insert(&mut self, pos: i32, comment: Comment) {
        self.comments.entry(pos).or_insert(Vec::new()).push(comment);
    }
}

pub enum Comment {
    Lit(&'static str),
    LoadString(Handle<Str>),
    Alloc(ClassDefId),
    StoreVTable(ClassDefId),
    CallSuper(FctId),
    CallVirtual(FctId),
    CallDirect(FctId),
    StoreParam(VarId),
    Newline,
    StoreField(ClassDefId, FieldId),
    LoadField(ClassDefId, FieldId),
    StoreVar(VarId),
    LoadVar(VarId),
    LoadSelf(VarId),
    LoadGlobal(GlobalId),
    StoreGlobal(GlobalId),
    ReadPollingPage,
}

impl Comment {
    pub fn is_newline(&self) -> bool {
        match self {
            &Comment::Newline => true,
            _ => false,
        }
    }
}

pub struct CommentFormat<'a, 'ast: 'a> {
    pub comment: &'a Comment,
    pub fct_src: Option<&'a FctSrc>,
    pub ctxt: &'a SemContext<'ast>,
}

impl<'a, 'ast> fmt::Display for CommentFormat<'a, 'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.comment {
            &Comment::Lit(val) => write!(f, "{}", val),
            &Comment::LoadString(_) => write!(f, "load string"),
            &Comment::Alloc(cls_def_id) => {
                let name = self.ctxt.class_defs[cls_def_id].borrow().name(self.ctxt);
                write!(f, "allocate object of class {}", &name)
            }

            &Comment::StoreVTable(cls_def_id) => {
                let name = self.ctxt.class_defs[cls_def_id].borrow().name(self.ctxt);
                write!(f, "store vtable ptr for class {} in object", &name)
            }

            &Comment::CallSuper(fid) => {
                let fct = self.ctxt.fcts[fid].borrow();
                let name = fct.full_name(self.ctxt);

                write!(f, "call super {}", &name)
            }

            &Comment::CallVirtual(fid) => {
                let fct = self.ctxt.fcts[fid].borrow();
                let name = fct.full_name(self.ctxt);

                write!(f, "call virtual {}", &name)
            }

            &Comment::CallDirect(fid) => {
                let fct = self.ctxt.fcts[fid].borrow();
                let name = fct.full_name(self.ctxt);

                write!(f, "call direct {}", &name)
            }

            &Comment::StoreParam(vid) => {
                let var = &self.fct_src.unwrap().vars[vid];
                let name = self.ctxt.interner.str(var.name);

                write!(f, "store param {}", name)
            }

            &Comment::Newline => write!(f, ""),

            &Comment::StoreField(clsid, fid) => {
                let cls_def = self.ctxt.class_defs[clsid].borrow();
                let cname = cls_def.name(self.ctxt);

                let cls = self.ctxt.classes[cls_def.cls_id].borrow();
                let field = &cls.fields[fid];
                let fname = field.name;
                let fname = self.ctxt.interner.str(fname);

                write!(f, "store in {}.{}", cname, fname)
            }

            &Comment::LoadField(clsid, fid) => {
                let cls_def = self.ctxt.class_defs[clsid].borrow();
                let cname = cls_def.name(self.ctxt);

                let cls = self.ctxt.classes[cls_def.cls_id].borrow();
                let field = &cls.fields[fid];
                let fname = field.name;
                let fname = self.ctxt.interner.str(fname);

                write!(f, "load from {}.{}", cname, fname)
            }

            &Comment::StoreVar(vid) => {
                let var = &self.fct_src.unwrap().vars[vid];
                let name = self.ctxt.interner.str(var.name);

                write!(f, "store var {}", name)
            }

            &Comment::LoadVar(vid) => {
                let var = &self.fct_src.unwrap().vars[vid];
                let name = self.ctxt.interner.str(var.name);

                write!(f, "load var {}", name)
            }

            &Comment::StoreGlobal(gid) => {
                let glob = self.ctxt.globals[gid].borrow();
                let name = self.ctxt.interner.str(glob.name);

                write!(f, "store global {}", name)
            }

            &Comment::LoadGlobal(gid) => {
                let glob = &self.ctxt.globals[gid].borrow();
                let name = self.ctxt.interner.str(glob.name);

                write!(f, "load global {}", name)
            }

            &Comment::LoadSelf(_) => write!(f, "load self"),

            &Comment::ReadPollingPage => write!(f, "read polling page (safepoint)"),
        }
    }
}

#[derive(Debug)]
pub struct LineNumberTable {
    map: HashMap<i32, i32>,
}

impl LineNumberTable {
    pub fn new() -> LineNumberTable {
        LineNumberTable { map: HashMap::new() }
    }

    pub fn insert(&mut self, offset: i32, lineno: i32) {
        assert!(self.map.insert(offset, lineno).is_none());
    }

    pub fn get(&self, offset: i32) -> i32 {
        if let Some(value) = self.map.get(&offset) {
            *value
        } else {
            0
        }
    }
}

#[derive(Debug)]
pub struct ExHandler {
    pub try_start: usize,
    pub try_end: usize,
    pub catch: usize,
    pub offset: Option<i32>,
    pub catch_type: CatchType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CatchType {
    Any,
    Class(*const ClassDef),
}

#[derive(Debug)]
pub struct Bailouts {
    map: HashMap<i32, BailoutInfo>,
}

impl Bailouts {
    pub fn new() -> Bailouts {
        Bailouts { map: HashMap::new() }
    }

    pub fn insert(&mut self, offset: i32, info: BailoutInfo) {
        assert!(self.map.insert(offset, info).is_none());
    }

    pub fn get(&self, offset: i32) -> Option<&BailoutInfo> {
        self.map.get(&offset)
    }
}

#[derive(Clone, Debug)]
pub enum BailoutInfo {
    Compile(FctId, i32, TypeParams, TypeParams),
    VirtCompile(u32, TypeParams),
}
