use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Component, Path, PathBuf};

use crate::driver::flags::{BuildArgs, CommonFlags, CompileArgs};
use crate::driver::start::Result;
use crate::driver::{append_exe_suffix, compile::command_compile};

use self::manifest::{PackageDependency, PackageManifest, read_package_manifest};

mod manifest;

const DORA_PACKAGE_FILE: &str = "dora-package.toml";

pub fn command_build(args: BuildArgs) -> Result<()> {
    let package_dir = args.path;
    let PackageManifest {
        name,
        main,
        dependencies,
    } = read_package_manifest(&package_dir.join(DORA_PACKAGE_FILE))?;
    let source_file = binary_source_file(&package_dir, main.as_deref())?;
    let dependencies = collect_dependency_packages(&package_dir, dependencies)?;
    let target_dir = package_dir.join("target");
    fs::create_dir_all(&target_dir)?;

    let output_file = target_dir.join(append_exe_suffix(PathBuf::from(&name)));
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
        common: common_flags_for_dependencies(dependencies)?,
    };

    command_compile(compile_args)
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

fn collect_dependency_packages(
    package_dir: &Path,
    dependencies: Vec<PackageDependency>,
) -> Result<Vec<(String, PathBuf)>> {
    let mut packages = Vec::new();
    let mut seen_packages = HashMap::new();
    let mut visited = HashSet::new();
    let mut visiting = HashSet::new();
    // Package dependency aliases are local to their parent package. Use the
    // absolute canonical package directory as the identity for cycle detection.
    let package_id = fs::canonicalize(package_dir)?;
    visiting.insert(package_id.clone());
    let mut stack = vec![DependencyFrame {
        package_dir: package_dir.to_path_buf(),
        package_id,
        dependencies: dependencies.into_iter(),
    }];

    while let Some(frame) = stack.last_mut() {
        let Some(dependency) = frame.dependencies.next() else {
            let frame = stack.pop().expect("missing dependency frame");
            visiting.remove(&frame.package_id);
            visited.insert(frame.package_id);
            continue;
        };

        let dependency_dir = resolve_path(&frame.package_dir, &dependency.path);
        let dependency_id = fs::canonicalize(&dependency_dir)?;
        if visiting.contains(&dependency_id) {
            return Err(dependency_cycle_error(&stack, &dependency_id, &dependency_dir).into());
        }

        let dependency_manifest = read_package_manifest(&dependency_dir.join(DORA_PACKAGE_FILE))?;
        let source_file =
            library_source_file(&dependency_dir, dependency_manifest.main.as_deref())?;

        if let Some(existing) = seen_packages.get(&dependency.name) {
            if existing != &source_file {
                return Err(format!("duplicate dependency '{}'", dependency.name).into());
            }
            continue;
        }

        seen_packages.insert(dependency.name.clone(), source_file.clone());
        packages.push((dependency.name.clone(), source_file));

        if !visited.contains(&dependency_id) {
            visiting.insert(dependency_id.clone());
            stack.push(DependencyFrame {
                package_dir: dependency_dir,
                package_id: dependency_id,
                dependencies: dependency_manifest.dependencies.into_iter(),
            });
        }
    }

    Ok(packages)
}

struct DependencyFrame {
    package_dir: PathBuf,
    package_id: PathBuf,
    dependencies: std::vec::IntoIter<PackageDependency>,
}

fn dependency_cycle_error(
    stack: &[DependencyFrame],
    dependency_id: &Path,
    dependency_dir: &Path,
) -> String {
    let start = stack
        .iter()
        .position(|frame| frame.package_id == dependency_id)
        .expect("cycle package is not in dependency stack");
    let mut cycle = stack[start..]
        .iter()
        .map(|frame| display_package_path(&frame.package_dir))
        .collect::<Vec<_>>();
    cycle.push(display_package_path(dependency_dir));

    format!("cyclic package dependency: {}", cycle.join(" -> "))
}

fn display_package_path(path: &Path) -> String {
    // Cycle detection uses canonical paths, but diagnostics should stay close
    // to the paths the user wrote. Normalize only display noise like `..`.
    let mut normalized = PathBuf::new();

    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                if !normalized.pop() {
                    normalized.push(component.as_os_str());
                }
            }
            _ => normalized.push(component.as_os_str()),
        }
    }

    normalized.display().to_string()
}

fn library_source_file(package_dir: &Path, manifest_main: Option<&str>) -> Result<PathBuf> {
    if let Some(main) = manifest_main {
        let path = package_dir.join(main);
        if path.exists() {
            return Ok(path);
        }

        return Err(format!("package source file not found at '{}'", path.display()).into());
    }

    let dora_lib = package_dir.join("src/lib.dora");
    if dora_lib.exists() {
        return Ok(dora_lib);
    }

    Err(format!(
        "no library source file found in '{}'",
        package_dir.display()
    )
    .into())
}

fn resolve_path(base_dir: &Path, path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        base_dir.join(path)
    }
}

fn common_flags_for_dependencies(dependencies: Vec<(String, PathBuf)>) -> Result<CommonFlags> {
    let mut package_args = Vec::with_capacity(dependencies.len() * 2);
    for (name, path) in dependencies {
        package_args.push(name);
        package_args.push(path_to_string(&path)?);
    }

    Ok(CommonFlags::with_package_args(package_args))
}

fn path_to_string(path: &Path) -> Result<String> {
    Ok(path
        .to_str()
        .ok_or_else(|| format!("path is not valid UTF-8: '{}'", path.display()))?
        .to_string())
}
