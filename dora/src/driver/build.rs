use std::fs;
use std::path::{Path, PathBuf};

use crate::driver::flags::{BuildArgs, CommonFlags, CompileArgs};
use crate::driver::start::Result;
use crate::driver::{append_exe_suffix, compile::command_compile};

const DORA_PACKAGE_FILE: &str = "dora-package.toml";

pub fn command_build(args: BuildArgs) -> Result<()> {
    let package_dir = args.path;
    let manifest = read_package_manifest(&package_dir.join(DORA_PACKAGE_FILE))?;
    let source_file = binary_source_file(&package_dir, manifest.main.as_deref())?;
    let target_dir = package_dir.join("target");
    fs::create_dir_all(&target_dir)?;

    let output_file = target_dir.join(append_exe_suffix(PathBuf::from(&manifest.name)));
    let compile_args = CompileArgs {
        file: path_to_string(&source_file)?,
        output: Some(path_to_string(&output_file)?),
        compile_to_package_only: false,
        asm_only: false,
        target: None,
        gc: None,
        emit_graph: None,
        emit_graph_after_each_pass: false,
        test: false,
        verbose: false,
        emit_timings: false,
        internal_compile_stdlib: false,
        compiler: None,
        internal_compile_boots: false,
        cannon: false,
        common: CommonFlags::default(),
    };

    command_compile(compile_args)
}

struct PackageManifest {
    name: String,
    main: Option<String>,
}

fn read_package_manifest(path: &Path) -> Result<PackageManifest> {
    let content = fs::read_to_string(path)?;
    let table = content.parse::<toml::Table>()?;
    let package = table
        .get("package")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| format!("missing [package] table in '{}'", path.display()))?;

    let name = package
        .get("name")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| format!("missing package.name in '{}'", path.display()))?
        .to_string();
    let main = package
        .get("main")
        .and_then(toml::Value::as_str)
        .map(str::to_string);

    Ok(PackageManifest { name, main })
}

fn binary_source_file(package_dir: &Path, manifest_main: Option<&str>) -> Result<PathBuf> {
    if let Some(main) = manifest_main {
        let path = package_dir.join(main);
        if path.exists() {
            return Ok(path);
        }

        return Err(format!("binary source file not found at '{}'", path.display()).into());
    }

    let dora_main = package_dir.join("src/main.dora");
    if dora_main.exists() {
        return Ok(dora_main);
    }

    Err(format!("no binary source file found in '{}'", package_dir.display()).into())
}

fn path_to_string(path: &Path) -> Result<String> {
    Ok(path
        .to_str()
        .ok_or_else(|| format!("path is not valid UTF-8: '{}'", path.display()))?
        .to_string())
}
