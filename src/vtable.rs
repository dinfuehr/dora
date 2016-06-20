use class::Class;

#[derive(Debug)]
pub struct VTable<'ast> {
    pub classptr: *mut Class<'ast>,
}

impl<'ast> VTable<'ast> {
    pub fn classptr(&self) -> *mut Class<'ast> {
        self.classptr
    }

    pub fn class(&self) -> &mut Class<'ast> {
        unsafe { &mut *self.classptr }
    }
}
