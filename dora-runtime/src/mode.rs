use crate::mem;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MachineMode {
    Int8,
    Int32,
    Int64,
    IntPtr,
    Float32,
    Float64,
    Ptr,
}

impl MachineMode {
    pub fn size(self) -> i32 {
        match self {
            MachineMode::Int8 => 1,
            MachineMode::Int32 => 4,
            MachineMode::Int64 => 8,
            MachineMode::IntPtr | MachineMode::Ptr => mem::ptr_width(),
            MachineMode::Float32 => 4,
            MachineMode::Float64 => 8,
        }
    }

    pub fn is_int8(self) -> bool {
        match self {
            MachineMode::Int8 => true,
            _ => false,
        }
    }

    pub fn is_float(self) -> bool {
        match self {
            MachineMode::Float32 | MachineMode::Float64 => true,
            _ => false,
        }
    }
}
