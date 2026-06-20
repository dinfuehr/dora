use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::Path;

use crate::driver::flags::InitArgs;
use crate::driver::start::Result;

const DORA_PACKAGE_FILE: &str = "dora-package.toml";

pub fn command_init(args: InitArgs) -> Result<()> {
    fs::create_dir_all(&args.path)?;

    let package_name = args
        .name
        .unwrap_or_else(|| default_package_name(&args.path));
    let main_file = if args.lib {
        "src/lib.dora"
    } else {
        "src/main.dora"
    };
    let source = if args.lib {
        "fn hello() {}\n"
    } else {
        "fn main() { println(\"Hello world!\"); }\n"
    };
    let manifest = package_manifest(&package_name);
    let manifest_path = args.path.join(DORA_PACKAGE_FILE);
    let source_path = args.path.join(main_file);

    let parent = source_path
        .parent()
        .expect("generated source path should have parent directory");
    fs::create_dir_all(parent)?;

    write_new_file(&manifest_path, &manifest)?;
    write_new_file(&source_path, source)?;

    Ok(())
}

fn write_new_file(path: &Path, content: &str) -> Result<()> {
    let mut file = OpenOptions::new().write(true).create_new(true).open(path)?;
    file.write_all(content.as_bytes())?;

    Ok(())
}

fn default_package_name(path: &Path) -> String {
    path.file_name()
        .and_then(|name| name.to_str())
        .filter(|name| !name.is_empty())
        .unwrap_or("package")
        .to_string()
}

fn package_manifest(name: &str) -> String {
    format!(
        "[package]\nname = {}\npackages = []\n",
        toml_basic_string(name)
    )
}

fn toml_basic_string(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len() + 2);
    escaped.push('"');

    for ch in value.chars() {
        match ch {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '\u{08}' => escaped.push_str("\\b"),
            '\t' => escaped.push_str("\\t"),
            '\n' => escaped.push_str("\\n"),
            '\u{0C}' => escaped.push_str("\\f"),
            '\r' => escaped.push_str("\\r"),
            ch if ch.is_control() => {
                use std::fmt::Write as _;
                write!(&mut escaped, "\\u{:04X}", ch as u32).expect("write to string failed");
            }
            ch => escaped.push(ch),
        }
    }

    escaped.push('"');
    escaped
}
