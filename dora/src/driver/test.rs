use std::path::PathBuf;
use std::process::Command;

use clap::Args;

use crate::driver::build::build_package;
use crate::driver::start::Result;

#[derive(Args)]
pub struct TestArgs {
    /// Package directory to test (default: current directory)
    pub path: Option<PathBuf>,
}

pub fn command_test(args: TestArgs) -> Result<()> {
    let package_dir = match args.path {
        Some(path) => path,
        None => std::env::current_dir()?,
    };
    let output_file = build_package(package_dir, true)?;
    let status = Command::new(&output_file).status()?;

    if !status.success() {
        return Err(format!("tests failed in '{}'", output_file.display()).into());
    }

    Ok(())
}
