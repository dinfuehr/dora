use std::fmt;

pub const ARTIFACT_MAGIC: &[u8; 8] = b"DORAART\0";
pub const ARTIFACT_FORMAT_VERSION: u16 = 1;

const VERSION_OFFSET: usize = ARTIFACT_MAGIC.len();
const KIND_OFFSET: usize = VERSION_OFFSET + size_of::<u16>();
const PAYLOAD_LEN_OFFSET: usize = KIND_OFFSET + size_of::<u16>();
const FINGERPRINT_OFFSET: usize = PAYLOAD_LEN_OFFSET + size_of::<u64>();
const PAYLOAD_OFFSET: usize = FINGERPRINT_OFFSET + 32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ArtifactKind(u16);

impl ArtifactKind {
    pub const PROGRAM: ArtifactKind = ArtifactKind(1);

    pub const fn from_u16(value: u16) -> ArtifactKind {
        ArtifactKind(value)
    }

    pub const fn as_u16(self) -> u16 {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArtifactFingerprint([u8; 32]);

impl ArtifactFingerprint {
    pub const fn from_bytes(bytes: [u8; 32]) -> ArtifactFingerprint {
        ArtifactFingerprint(bytes)
    }

    pub const fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }
}

impl fmt::Display for ArtifactFingerprint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for byte in self.0 {
            write!(f, "{byte:02x}")?;
        }

        Ok(())
    }
}

impl fmt::Debug for ArtifactFingerprint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct DecodedArtifact<'a> {
    pub version: u16,
    pub kind: ArtifactKind,
    pub fingerprint: ArtifactFingerprint,
    pub payload: &'a [u8],
}

pub fn encode_artifact(kind: ArtifactKind, payload: &[u8]) -> Vec<u8> {
    let payload_len = u64::try_from(payload.len()).expect("artifact payload too large");
    let fingerprint = calculate_artifact_fingerprint(kind, payload);
    let mut bytes = Vec::with_capacity(PAYLOAD_OFFSET + payload.len());

    bytes.extend_from_slice(ARTIFACT_MAGIC);
    bytes.extend_from_slice(&ARTIFACT_FORMAT_VERSION.to_le_bytes());
    bytes.extend_from_slice(&kind.as_u16().to_le_bytes());
    bytes.extend_from_slice(&payload_len.to_le_bytes());
    bytes.extend_from_slice(fingerprint.as_bytes());
    bytes.extend_from_slice(payload);

    bytes
}

pub fn decode_artifact(bytes: &[u8]) -> Result<DecodedArtifact<'_>, String> {
    if bytes.len() < ARTIFACT_MAGIC.len() {
        return Err("Dora artifact is truncated".to_string());
    }

    if &bytes[..ARTIFACT_MAGIC.len()] != ARTIFACT_MAGIC {
        return Err("encoded input is not a Dora artifact".to_string());
    }

    if bytes.len() < PAYLOAD_OFFSET {
        return Err("Dora artifact header is truncated".to_string());
    }

    let version = u16::from_le_bytes(
        bytes[VERSION_OFFSET..KIND_OFFSET]
            .try_into()
            .expect("invalid version field"),
    );
    if version != ARTIFACT_FORMAT_VERSION {
        return Err(format!(
            "unsupported Dora artifact format version {version}; expected {ARTIFACT_FORMAT_VERSION}"
        ));
    }

    let kind = ArtifactKind::from_u16(u16::from_le_bytes(
        bytes[KIND_OFFSET..PAYLOAD_LEN_OFFSET]
            .try_into()
            .expect("invalid artifact kind field"),
    ));
    let payload_len = u64::from_le_bytes(
        bytes[PAYLOAD_LEN_OFFSET..FINGERPRINT_OFFSET]
            .try_into()
            .expect("invalid payload length field"),
    );
    let payload_len = usize::try_from(payload_len)
        .map_err(|_| "Dora artifact payload length is too large".to_string())?;
    let expected_len = PAYLOAD_OFFSET
        .checked_add(payload_len)
        .ok_or_else(|| "Dora artifact payload length is too large".to_string())?;

    if bytes.len() < expected_len {
        return Err(format!(
            "Dora artifact payload is truncated: expected {payload_len} bytes, found {}",
            bytes.len() - PAYLOAD_OFFSET
        ));
    }
    if bytes.len() > expected_len {
        return Err("Dora artifact has trailing bytes".to_string());
    }

    let mut fingerprint_bytes = [0; 32];
    fingerprint_bytes.copy_from_slice(&bytes[FINGERPRINT_OFFSET..PAYLOAD_OFFSET]);
    let fingerprint = ArtifactFingerprint::from_bytes(fingerprint_bytes);
    let payload = &bytes[PAYLOAD_OFFSET..];
    let actual_fingerprint = calculate_artifact_fingerprint(kind, payload);

    if fingerprint != actual_fingerprint {
        return Err(format!(
            "Dora artifact fingerprint mismatch: expected {fingerprint}, computed {actual_fingerprint}"
        ));
    }

    Ok(DecodedArtifact {
        version,
        kind,
        fingerprint,
        payload,
    })
}

pub fn calculate_artifact_fingerprint(kind: ArtifactKind, payload: &[u8]) -> ArtifactFingerprint {
    let payload_len = u64::try_from(payload.len()).expect("artifact payload too large");
    let mut hasher = blake3::Hasher::new();
    hasher.update(ARTIFACT_MAGIC);
    hasher.update(&ARTIFACT_FORMAT_VERSION.to_le_bytes());
    hasher.update(&kind.as_u16().to_le_bytes());
    hasher.update(&payload_len.to_le_bytes());
    hasher.update(payload);
    ArtifactFingerprint::from_bytes(*hasher.finalize().as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_KIND: ArtifactKind = ArtifactKind::from_u16(42);
    const TEST_PAYLOAD: &[u8] = b"compiled Dora payload";

    #[test]
    fn round_trip() {
        let bytes = encode_artifact(TEST_KIND, TEST_PAYLOAD);
        let artifact = decode_artifact(&bytes).expect("artifact should decode");

        assert_eq!(artifact.version, ARTIFACT_FORMAT_VERSION);
        assert_eq!(artifact.kind, TEST_KIND);
        assert_eq!(artifact.payload, TEST_PAYLOAD);
        assert_eq!(
            artifact.fingerprint,
            calculate_artifact_fingerprint(TEST_KIND, TEST_PAYLOAD)
        );
    }

    #[test]
    fn rejects_wrong_magic() {
        let mut bytes = encode_artifact(TEST_KIND, TEST_PAYLOAD);
        bytes[0] ^= 0xff;

        assert_error_contains(&bytes, "not a Dora artifact");
    }

    #[test]
    fn rejects_unsupported_version() {
        let mut bytes = encode_artifact(TEST_KIND, TEST_PAYLOAD);
        bytes[VERSION_OFFSET..KIND_OFFSET].copy_from_slice(&2u16.to_le_bytes());

        assert_error_contains(&bytes, "unsupported Dora artifact format version 2");
    }

    #[test]
    fn rejects_truncated_header() {
        let bytes = encode_artifact(TEST_KIND, TEST_PAYLOAD);

        assert_error_contains(&bytes[..PAYLOAD_OFFSET - 1], "header is truncated");
    }

    #[test]
    fn rejects_truncated_payload() {
        let mut bytes = encode_artifact(TEST_KIND, TEST_PAYLOAD);
        bytes.pop();

        assert_error_contains(&bytes, "payload is truncated");
    }

    #[test]
    fn rejects_trailing_bytes() {
        let mut bytes = encode_artifact(TEST_KIND, TEST_PAYLOAD);
        bytes.push(0);

        assert_error_contains(&bytes, "has trailing bytes");
    }

    #[test]
    fn rejects_corrupted_payload() {
        let mut bytes = encode_artifact(TEST_KIND, TEST_PAYLOAD);
        *bytes.last_mut().expect("missing payload") ^= 0xff;

        assert_error_contains(&bytes, "fingerprint mismatch");
    }

    fn assert_error_contains(bytes: &[u8], expected: &str) {
        let error = decode_artifact(bytes).expect_err("artifact should be rejected");
        assert!(
            error.contains(expected),
            "expected error containing '{expected}', got '{error}'"
        );
    }
}
