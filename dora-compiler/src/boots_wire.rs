mod deserializer;
mod serializer;

pub use deserializer::{
    ByteReader, decode_bytecode_trait_ty, decode_bytecode_type, decode_bytecode_type_array,
    decode_code_descriptor, decode_specialize_self,
};
pub use serializer::{
    ByteBuffer, encode_bytecode_type, encode_bytecode_type_array, encode_compilation_info,
    encode_const_value, encode_enum_data, encode_function_bytecode_data,
    encode_function_inlining_info, encode_struct_data,
};
