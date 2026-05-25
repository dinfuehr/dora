use std::path::Path;

use crate::Program;

pub fn decode_program_from_bytes(bytes: &[u8]) -> Result<Program, String> {
    let config = bincode::config::standard();
    let (program, decoded_len): (Program, usize) = bincode::decode_from_slice(bytes, config)
        .map_err(|err| format!("failed to decode AOT program: {err}"))?;

    if decoded_len != bytes.len() {
        return Err("encoded AOT program has trailing bytes".to_string());
    }

    Ok(program)
}

pub fn read_program_from_file(path: &Path) -> Result<Program, String> {
    let encoded_program = std::fs::read(path).map_err(|err| {
        format!(
            "failed to read encoded program input '{}': {err}",
            path.display()
        )
    })?;

    if encoded_program.is_empty() {
        return Err(format!(
            "missing encoded program input '{}'",
            path.display()
        ));
    }

    decode_program_from_bytes(&encoded_program)
}
