pub struct PosMap {
    files: Vec<FileMap>,
    starts: Vec<u32>,
    next: u32,
}

impl PosMap {
    pub fn new() -> PosMap {
        PosMap {
            files: Vec::new(),
            starts: Vec::new(),
            next: 0,
        }
    }

    pub fn add(&mut self, name: String, content: String) {
        let len = content.len();

        self.files.push(FileMap::new(name, content));
        self.starts.push(self.next);
        self.next += len as u32;
    }
}

pub struct FileMap {
    _name: String,
    _content: String,
    _lines: Vec<u32>,
}

impl FileMap {
    fn new(name: String, content: String) -> FileMap {
        FileMap {
            _name: name,
            _content: content,
            _lines: Vec::new(),
        }
    }
}
