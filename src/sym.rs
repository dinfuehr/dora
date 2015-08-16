use std::collections::HashMap;
use std::collections::hash_map::IterMut;
use std::collections::hash_map::Entry::{Vacant, Occupied};

use parser::ast::NodeId;

use parser::interner::Name;

#[derive(Debug)]
pub struct SymTable {
    map: HashMap<Name, Sym>
}

impl SymTable {
    // creates a new table
    pub fn new() -> SymTable {
        SymTable {
            map: HashMap::new()
        }
    }

    // finds symbol in table
    pub fn find(&self, name: Name) -> Option<&Sym> {
        self.map.get(&name)
    }

    // inserts symbol into the table
    pub fn insert(& mut self, name: Name, sym: Sym) -> Result<(), &Sym> {
        match self.map.entry(name) {
            Vacant(entry) => {
                entry.insert(sym);

                Ok(())
            }

            Occupied(old) => Err(old.into_mut())
        }
    }

    pub fn functions_mut(&mut self) -> SymFunctionIterMut {
        SymFunctionIterMut {
            iter: self.map.iter_mut()
        }
    }
}

#[derive(Debug)]
pub enum Sym {
    SymLocalVar(SymLocalVarType),
    SymFunction(SymFunctionType),
    SymType(SymTypeType),

    // only for testing purposes
    SymDummy(u8),
}

impl Sym {
    pub fn create_type(name: Name, builtin: BuiltinType) -> Sym {
        Sym::SymType(SymTypeType {
            name: name,
            builtin: builtin
        })
    }

    pub fn to_local_var(&self) -> Option<&SymLocalVarType> {
        match *self {
            Sym::SymLocalVar(ref sym) => Some(sym),
            _ => None
        }
    }

    pub fn to_function(&self) -> Option<&SymFunctionType> {
        match *self {
            Sym::SymFunction(ref sym) => Some(sym),
            _ => None
        }
    }

    pub fn to_type(&self) -> Option<&SymTypeType> {
        match *self {
            Sym::SymType(ref sym) => Some(sym),
            _ => None
        }
    }
}

#[derive(Debug)]
pub struct SymTypeType {
    pub name: Name,
    pub builtin: BuiltinType,
}

#[derive(Debug)]
pub enum Type {
    TypeBasic(TypeBasicType),
    TypeTuple(TypeTupleType),
    TypeArray(TypeArrayType),
    TypePtr(TypePtrType),
}

impl Type {
    pub fn create_unit() -> Type {
        Type::TypeTuple(TypeTupleType {
            subtypes: Vec::new()
        })
    }

    pub fn create_tuple(subtypes: Vec<Box<Type>>) -> Type {
        Type::TypeTuple(TypeTupleType {
            subtypes: subtypes
        })
    }

    pub fn create_basic(name: Name) -> Type {
        Type::TypeBasic(TypeBasicType {
            name: name
        })
    }

    pub fn create_ptr(subtype: Box<Type>) -> Type {
        Type::TypePtr(TypePtrType {
            subtype: subtype
        })
    }

    pub fn create_array(subtype: Box<Type>) -> Type {
        Type::TypeArray(TypeArrayType {
            subtype: subtype
        })
    }
}

#[derive(Debug)]
pub struct TypeBasicType {
    pub name: Name
}

#[derive(Debug)]
pub struct TypeTupleType {
    pub subtypes: Vec<Box<Type>>
}

#[derive(Debug)]
pub struct TypeArrayType {
    pub subtype: Box<Type>
}

#[derive(Debug)]
pub struct TypePtrType {
    pub subtype: Box<Type>
}

#[derive(Debug)]
pub enum BuiltinType {
    Int,
    Bool,
    Str,
}

#[derive(Debug)]
pub struct SymFunctionType {
    pub name: Name,
    pub return_type: Type,
    pub params: Vec<Param>,
    pub body: NodeId,
}

#[derive(Debug)]
pub struct Param {
    pub name: Name,
    pub data_type: Type
}

#[derive(Debug)]
pub struct SymLocalVarType {
    pub name: Name,
    pub data_type: Type,
    pub expr: Option<NodeId>,
}

struct SymFunctionIterMut<'a> {
    iter: IterMut<'a, Name, Sym>
}

impl<'a> Iterator for SymFunctionIterMut<'a> {
    type Item = &'a SymFunctionType;

    fn next(&mut self) -> Option<&'a SymFunctionType> {
        loop {
            let next = self.iter.next();

            match next {
                None => return None,

                Some(val) => {
                    if let Sym::SymFunction(ref fct) = *val.1 {
                        return Some(fct);
                    }
                }
            }
        }
    }
}

#[test]
fn test_insert_and_find_again() {
    let mut table = SymTable::new();

    assert!(table.insert(Name(1), Sym::SymDummy(1)).is_ok());
    assert!(table.find(Name(1)).is_some());
    assert!(table.find(Name(2)).is_none());
}

#[test]
fn test_insert_twice_into_same_level() {
    let mut table = SymTable::new();

    assert!(table.insert(Name(1), Sym::SymDummy(1)).is_ok());
    assert!(table.insert(Name(1), Sym::SymDummy(2)).is_err());
}

