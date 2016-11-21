use std::collections::HashMap;
use std::fmt;
use std::ptr;

use class::ClassId;
use ctxt::FctId;
use dseg::DSeg;
use mem::{CodeMemory, Ptr};

pub struct JitFct {
    code: CodeMemory,

    fct_id: FctId,

    // pointer to beginning of function
    fct_start: Ptr,

    // machine code length in bytes
    fct_len: usize,

    gcpoints: GcPoints,

    comments: Comments,

    linenos: LineNumberTable,

    pub exception_handlers: Vec<ExHandler>,
}

impl JitFct {
    pub fn from_buffer(fct_id: FctId, dseg: &DSeg, buffer: &[u8], gcpoints: GcPoints,
                comments: Comments, linenos: LineNumberTable,
                mut exception_handlers: Vec<ExHandler>) -> JitFct {
        let size = dseg.size() as usize + buffer.len();

        let code = CodeMemory::new(size);
        let ptr = code.ptr_start();

        dseg.finish(ptr.raw());

        let fct_start;

        unsafe {
            fct_start = ptr.offset(dseg.size() as isize);
            ptr::copy_nonoverlapping(buffer.as_ptr(), fct_start.raw() as *mut u8, buffer.len());
        }

        for handler in &mut exception_handlers {
            let fct_start: usize = fct_start.raw() as usize;

            handler.try_start = fct_start + handler.try_start;
            handler.try_end = fct_start + handler.try_end;
            handler.catch = fct_start + handler.catch;
        }

        JitFct {
            fct_id: fct_id,
            code: code,
            gcpoints: gcpoints,
            comments: comments,
            fct_start: fct_start,
            fct_len: buffer.len(),
            linenos: linenos,
            exception_handlers: exception_handlers,
        }
    }

    pub fn lineno_for_offset(&self, offset: i32) -> i32 {
        self.linenos.get(offset)
    }

    pub fn gcpoint_for_offset(&self, offset: i32) -> Option<&GcPoint> {
        self.gcpoints.get(offset)
    }

    pub fn code(self) -> CodeMemory {
        self.code
    }

    pub fn ptr_start(&self) -> Ptr {
        self.code.ptr_start()
    }

    pub fn ptr_end(&self) -> Ptr {
        self.code.ptr_end()
    }

    pub fn fct_id(&self) -> FctId {
        self.fct_id
    }

    pub fn fct_ptr(&self) -> Ptr {
        self.fct_start
    }

    pub fn fct_len(&self) -> usize {
        self.fct_len
    }

    pub fn get_comment(&self, pos: i32) -> Option<&Comment> {
        self.comments.get(pos)
    }
}

impl fmt::Debug for JitFct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "JitFct {{ start: {:?}, end: {:?} }}",
            self.ptr_start(), self.ptr_end())
    }
}

#[derive(Debug)]
pub struct GcPoints {
    points: HashMap<i32, GcPoint>
}

impl GcPoints {
    pub fn new() -> GcPoints {
        GcPoints {
            points: HashMap::new()
        }
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
    pub offsets: Vec<i32>
}

impl GcPoint {
    pub fn new() -> GcPoint {
        GcPoint {
            offsets: Vec::new()
        }
    }

    pub fn from_offsets(offsets: Vec<i32>) -> GcPoint {
        GcPoint {
            offsets: offsets
        }
    }
}

#[derive(Debug)]
pub struct Comments {
    comments: HashMap<i32, Comment>
}

impl Comments {
    pub fn new() -> Comments {
        Comments {
            comments: HashMap::new(),
        }
    }

    pub fn get(&self, pos: i32) -> Option<&Comment> {
        self.comments.get(&pos)
    }

    pub fn insert(&mut self, pos: i32, comment: Comment) {
        self.comments.insert(pos, comment);
    }
}

#[derive(Debug)]
pub enum Comment {
    LoadString,
    LoadFunction,
}

impl fmt::Display for Comment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Comment::LoadString => write!(f, "load string"),
            &Comment::LoadFunction => write!(f, "load function pointer"),
        }
    }
}

#[derive(Debug)]
pub struct LineNumberTable {
    map: HashMap<i32, i32>,
}

impl LineNumberTable {
    pub fn new() -> LineNumberTable {
        LineNumberTable {
            map: HashMap::new()
        }
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
    pub offset: i32,
    pub catch_type: CatchType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CatchType {
    Any, Class(ClassId),
}
