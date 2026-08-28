use std::path::Path;

use crate::{ArtifactKind, Program, decode_artifact, encode_artifact};

pub fn encode_program_to_vec(program: &Program) -> Vec<u8> {
    let payload = bincode::encode_to_vec(program, bincode::config::standard())
        .expect("program serialization failed");
    encode_artifact(ArtifactKind::PROGRAM, &payload)
}

pub fn decode_program_from_bytes(bytes: &[u8]) -> Result<Program, String> {
    let artifact = decode_artifact(bytes)?;
    if artifact.kind != ArtifactKind::PROGRAM {
        return Err(format!(
            "expected a Dora program artifact, found artifact kind {}",
            artifact.kind.as_u16()
        ));
    }

    let config = bincode::config::standard();
    let (program, decoded_len): (Program, usize) =
        bincode::decode_from_slice(artifact.payload, config)
            .map_err(|err| format!("failed to decode AOT program: {err}"))?;

    if decoded_len != artifact.payload.len() {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PackageId;

    #[test]
    fn program_round_trip() {
        let bytes = encode_program_to_vec(&empty_program());
        let program = decode_program_from_bytes(&bytes).expect("program should decode");

        assert!(program.packages.is_empty());
        assert_eq!(program.stdlib_package_id.index(), 0);
        assert_eq!(program.program_package_id.index(), 0);
        assert!(program.main_fct_id.is_none());
    }

    #[test]
    fn rejects_wrong_artifact_kind() {
        let payload = bincode::encode_to_vec(empty_program(), bincode::config::standard())
            .expect("program serialization failed");
        let bytes = encode_artifact(ArtifactKind::from_u16(17), &payload);
        let error = decode_program_from_bytes(&bytes).expect_err("artifact should be rejected");

        assert!(error.contains("expected a Dora program artifact"));
    }

    fn empty_program() -> Program {
        Program {
            packages: Vec::new(),
            modules: Vec::new(),
            functions: Vec::new(),
            function_intrinsics: Vec::new(),
            globals: Vec::new(),
            consts: Vec::new(),
            classes: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
            extensions: Vec::new(),
            aliases: Vec::new(),
            source_files: Vec::new(),
            stdlib_package_id: PackageId::from(0),
            program_package_id: PackageId::from(0),
            main_fct_id: None,
        }
    }
}
