use self::Type::*;

use mir::basic_block::*;

pub struct Function {
    name: String,
    local_vars: Vec<LocalVar>,
    params: Vec<Type>,
    blocks: Vec<BasicBlock>,
}

impl Function {
    pub fn new(name: &str) -> Function {
        Function {
            name: name.into(),
            local_vars: Vec::new(),
            params: Vec::new(),
            blocks: Vec::new()
        }
    }

    pub fn local_var(&self, ind: LocalVarId) -> &LocalVar {
        &self.local_vars[ind.0]
    }

    pub fn dump(&self) {
        println!("function {}", &self.name);

        for block in &self.blocks {
            block.dump(&self);
        }
    }
}

#[derive(Copy, Clone)]
pub struct LocalVarId(pub usize);

pub struct LocalVar {
    name: String,
    ty: Type,
}

impl LocalVar {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

pub enum Type {
    TyBool,
    TyInt(IntType),
    TyPtr(Box<Type>),
    TyStruct(StructType)
}

impl Type {
    pub fn to_string(&self) -> String {
        match *self {
            TyBool => "bool".into(),
            TyInt(inttype) => inttype.to_string().into(),
            TyPtr(ref subtype) => format!("*{}", subtype.to_string()),
            TyStruct(_) => "{struct}".into()
        }
    }
}

#[derive(Copy, Clone)]
pub enum IntType {
    UInt8,
    Int32,
    Int // size depending on architecture
}

impl IntType {
    pub fn to_string(&self) -> &'static str {
        match *self {
            IntType::UInt8 => "u8",
            IntType::Int32 => "i32",
            IntType::Int => "int"
        }
    }
}

struct StructType {
    size: usize,
    elems: Vec<StructElem>,
}

struct StructElem {
    name: String,
    ty: Type,
    offset: usize
}
