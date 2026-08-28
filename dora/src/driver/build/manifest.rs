use std::fs;
use std::path::{Path, PathBuf};

use crate::driver::start::Result;

const PACKAGE_NAME_EXPECTATION: &str = "expected only ASCII letters, digits, '_' or '-'";

pub(super) struct PackageManifest {
    pub(super) name: String,
    pub(super) main: Option<String>,
    pub(super) dependencies: Vec<PackageDependency>,
}

pub(super) struct PackageDependency {
    pub(super) name: String,
    pub(super) path: PathBuf,
}

pub(super) fn read_package_manifest(path: &Path) -> Result<PackageManifest> {
    let content = fs::read_to_string(path)?;
    let table = content.parse::<toml::Table>()?;
    let package = table
        .get("package")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| format!("missing [package] table in '{}'", path.display()))?;

    let name = package
        .get("name")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| format!("missing package.name in '{}'", path.display()))?;
    if !is_valid_package_name(name) {
        return Err(format!(
            "invalid package name '{}' in '{}': {PACKAGE_NAME_EXPECTATION}",
            name,
            path.display(),
        )
        .into());
    }
    let name = name.to_string();
    let main = package
        .get("main")
        .and_then(toml::Value::as_str)
        .map(str::to_string);
    let dependencies = read_dependencies(&table, path)?;

    Ok(PackageManifest {
        name,
        main,
        dependencies,
    })
}

fn read_dependencies(table: &toml::Table, manifest_path: &Path) -> Result<Vec<PackageDependency>> {
    let Some(dependencies) = table.get("dependencies") else {
        return Ok(Vec::new());
    };
    let dependencies = dependencies.as_table().ok_or_else(|| {
        format!(
            "dependencies must be a table in '{}'",
            manifest_path.display()
        )
    })?;

    let mut result: Vec<PackageDependency> = Vec::new();
    for (name, dependency) in dependencies {
        if !is_valid_package_name(name) {
            return Err(format!(
                "invalid dependency name '{}' in '{}': {PACKAGE_NAME_EXPECTATION}",
                name,
                manifest_path.display(),
            )
            .into());
        }

        let path = dependency_path(dependency, manifest_path, name)?;
        let name = name.replace('-', "_");
        if result.iter().any(|dependency| dependency.name == name) {
            return Err(format!(
                "duplicate dependency name '{}' after replacing '-' with '_' in '{}'",
                name,
                manifest_path.display(),
            )
            .into());
        }
        result.push(PackageDependency { name, path });
    }

    Ok(result)
}

fn is_valid_package_name(name: &str) -> bool {
    !name.is_empty()
        && name
            .bytes()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'-')
}

fn dependency_path(
    dependency: &toml::Value,
    manifest_path: &Path,
    dependency_name: &str,
) -> Result<PathBuf> {
    if let Some(path) = dependency.as_str() {
        return Ok(PathBuf::from(path));
    }

    let dependency = dependency.as_table().ok_or_else(|| {
        format!(
            "dependency '{}' must be a string or table in '{}'",
            dependency_name,
            manifest_path.display()
        )
    })?;
    let path = dependency
        .get("path")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| {
            format!(
                "dependency '{}' is missing path in '{}'",
                dependency_name,
                manifest_path.display()
            )
        })?;

    Ok(PathBuf::from(path))
}

#[cfg(test)]
mod tests {
    use super::is_valid_package_name;

    #[test]
    fn valid_package_name() {
        assert!(is_valid_package_name("dependency"));
        assert!(is_valid_package_name("dependency-2"));
        assert!(is_valid_package_name("2dependency"));
        assert!(is_valid_package_name("_"));

        assert!(!is_valid_package_name(""));
        assert!(!is_valid_package_name("dependency/path"));
        assert!(!is_valid_package_name("dependency\\path"));
    }
}
