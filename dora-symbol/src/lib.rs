const SYMBOL_PREFIX: &str = "dora_";

pub fn mangle_name(name: &str) -> String {
    let mut result = String::with_capacity(name.len() + SYMBOL_PREFIX.len());
    result.push_str(SYMBOL_PREFIX);

    for byte in name.bytes() {
        if byte.is_ascii_alphanumeric() {
            result.push(byte as char);
        } else {
            result.push('_');
            result.push(hex_digit(byte >> 4));
            result.push(hex_digit(byte & 0x0F));
        }
    }

    result
}

pub fn mangle_name_with_max_len(name: &str, max_len: usize) -> String {
    const HASH_SUFFIX_LEN: usize = 34;

    assert!(
        max_len >= HASH_SUFFIX_LEN,
        "maximum symbol length must be at least {HASH_SUFFIX_LEN}"
    );

    let symbol = mangle_name(name);
    if symbol.len() <= max_len {
        return symbol;
    }

    let hash = fnv1a_128(symbol.as_bytes());
    let suffix = format!("_H{hash:032X}");
    let prefix_len = max_len - suffix.len();
    debug_assert!(symbol.is_ascii());
    format!("{}{}", &symbol[..prefix_len], suffix)
}

pub fn demangle_name(name: &str) -> Option<String> {
    let body = name.strip_prefix(SYMBOL_PREFIX)?;
    let mut result = Vec::with_capacity(body.len());
    let bytes = body.as_bytes();
    let mut idx = 0;

    while idx < bytes.len() {
        let byte = bytes[idx];

        if byte == b'_' {
            let high = bytes.get(idx + 1).copied().and_then(hex_value)?;
            let low = bytes.get(idx + 2).copied().and_then(hex_value)?;
            result.push((high << 4) | low);
            idx += 3;
        } else if byte.is_ascii_alphanumeric() {
            result.push(byte);
            idx += 1;
        } else {
            return None;
        }
    }

    String::from_utf8(result).ok()
}

fn fnv1a_128(bytes: &[u8]) -> u128 {
    const OFFSET_BASIS: u128 = 0x6C62272E07BB014262B821756295C58D;
    const PRIME: u128 = 0x0000000001000000000000000000013B;

    let mut hash = OFFSET_BASIS;
    for &byte in bytes {
        hash ^= u128::from(byte);
        hash = hash.wrapping_mul(PRIME);
    }
    hash
}

fn hex_digit(value: u8) -> char {
    match value {
        0..=9 => (b'0' + value) as char,
        10..=15 => (b'A' + value - 10) as char,
        _ => unreachable!(),
    }
}

fn hex_value(value: u8) -> Option<u8> {
    match value {
        b'0'..=b'9' => Some(value - b'0'),
        b'a'..=b'f' => Some(value - b'a' + 10),
        b'A'..=b'F' => Some(value - b'A' + 10),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{demangle_name, mangle_name, mangle_name_with_max_len};

    #[test]
    fn mangle_name_escapes_separators() {
        assert_eq!(
            mangle_name("boots::interface::compile"),
            "dora_boots_3A_3Ainterface_3A_3Acompile"
        );
        assert_eq!(
            mangle_name("std::fatal_error[()]"),
            "dora_std_3A_3Afatal_5Ferror_5B_28_29_5D"
        );
    }

    #[test]
    fn demangle_name_roundtrips_mangled_names() {
        for name in [
            "main",
            "boots::interface::compile",
            "std::fatal_error[()]",
            "std::fatal_error[(): ()]",
            "std::primitives::<impl[()] Option[()]>::is_none",
            "std::traits::Add for std::string::String#add",
            "unicode::snowman::☃",
        ] {
            let mangled = mangle_name(name);
            assert_eq!(demangle_name(&mangled).as_deref(), Some(name));
        }
    }

    #[test]
    fn demangle_name_rejects_invalid_symbols() {
        assert_eq!(demangle_name("main"), None);
        assert_eq!(demangle_name("dora_std_"), None);
        assert_eq!(demangle_name("dora_std_3"), None);
        assert_eq!(demangle_name("dora_std_XY"), None);
    }

    #[test]
    fn mangle_name_with_max_len_preserves_short_symbols() {
        assert_eq!(
            mangle_name_with_max_len("std::string::length", 200),
            mangle_name("std::string::length")
        );
    }

    #[test]
    fn mangle_name_with_max_len_shortens_long_symbols_deterministically() {
        let name = format!("std::callable::Fn16[{}]", "Int64, ".repeat(32));
        let shortened = mangle_name_with_max_len(&name, 200);

        assert!(mangle_name(&name).len() > 200);
        assert_eq!(shortened.len(), 200);
        assert!(shortened.starts_with("dora_std_3A_3Acallable"));
        assert!(shortened.contains("_H"));
        assert_eq!(shortened, mangle_name_with_max_len(&name, 200));

        let other_name = format!("{name}other");
        assert_ne!(shortened, mangle_name_with_max_len(&other_name, 200));
    }
}
