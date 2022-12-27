use std::str::FromStr;
use std::{fs, path::PathBuf};

use crate::handle::{handle_scope, Handle};
use crate::object::{Ref, Str};
use crate::threads::parked_scope;
use crate::vm::get_vm;

pub extern "C" fn read_file_as_string(name: Handle<Str>) -> Ref<Str> {
    handle_scope(|| {
        let path = PathBuf::from_str(name.content_utf8());
        if path.is_err() {
            return Ref::null();
        }
        let path = path.unwrap();
        let content = parked_scope(|| fs::read_to_string(path));
        if content.is_err() {
            return Ref::null();
        }
        let content = content.unwrap();

        let vm = get_vm();
        Str::from_buffer(vm, content.as_bytes())
    })
}
