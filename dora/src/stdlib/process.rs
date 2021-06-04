use crate::handle::Handle;
use crate::object::{Str, StrArray};

pub extern "C" fn spawn_process(_path: Handle<Str>, _arguments: Handle<StrArray>) {
    unimplemented!()
}
