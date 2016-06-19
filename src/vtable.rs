use class::Class;

#[derive(Debug)]
pub struct VTable<'ast> {
    classptr: *mut Class<'ast>,
    table: [usize; 1],
}
