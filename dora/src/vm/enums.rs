use parking_lot::RwLock;

use std::collections::hash_map::HashMap;
use std::convert::TryInto;
use std::ops::Index;

use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::vm::FileId;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumId(u32);

impl From<usize> for EnumId {
    fn from(data: usize) -> EnumId {
        EnumId(data.try_into().unwrap())
    }
}

impl Index<EnumId> for Vec<RwLock<EnumData>> {
    type Output = RwLock<EnumData>;

    fn index(&self, index: EnumId) -> &RwLock<EnumData> {
        &self[index.0 as usize]
    }
}

#[derive(Debug)]
pub struct EnumData {
    pub id: EnumId,
    pub file: FileId,
    pub pos: Position,
    pub name: Name,
    pub values: Vec<Name>,
    pub name_to_value: HashMap<Name, u32>,
}
