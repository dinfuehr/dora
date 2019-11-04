use std::{
    env,
    error::Error,
    ffi::OsStr,
    fs::{self, copy, File},
    io::Write,
    path::Path,
};

const SOURCE_DIR: &str = "stdlib";

fn main() -> Result<(), Box<dyn Error>> {
    let out_dir = env::var("OUT_DIR")?;
    let out_path = Path::new(&out_dir);

    let root_dir = env::var("CARGO_MANIFEST_DIR")?;
    let root_path = Path::new(&root_dir);

    let copy_path = out_path.join("stdlib");
    let stdlib_bundle_path = out_path.join("dora_stdlib_bundle.rs");

    if copy_path.is_dir() {
        fs::remove_dir_all(&copy_path)?;
        fs::create_dir(&copy_path)?;
    } else {
        fs::create_dir(&copy_path)?;
    }

    if stdlib_bundle_path.is_file() {
        fs::remove_file(&stdlib_bundle_path)?;
    }

    let mut stdlib = File::create(&stdlib_bundle_path)?;

    writeln!(&mut stdlib, r#"["#,)?;

    for f in fs::read_dir(SOURCE_DIR)? {
        let f = f?;

        if !f.file_type()?.is_file() {
            continue;
        }

        match f.path().extension() {
            Some(extension) => {
                if extension != OsStr::new("dora") {
                    continue;
                }
            }
            None => continue,
        }

        copy(root_path.join(f.path()), out_path.join(f.path()))?;

        writeln!(
            &mut stdlib,
            r#"("{name}", include_str!("{name}")),"#,
            name = f.path().display(),
        )?;
    }

    writeln!(&mut stdlib, r#"];"#,)?;

    Ok(())
}
