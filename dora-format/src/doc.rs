use id_arena::Id;

#[allow(unused)]
pub type DocId = Id<Doc>;

#[allow(unused)]
pub enum Doc {
    Concat { children: Vec<DocId> },
    Nest { indent: u32, doc: DocId },
    Group { doc: DocId },
    Text { text: String },
    SoftLine,
    HardLine,
}
