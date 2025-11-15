$ErrorActionPreference = "Stop"

$build_profile = "release"

cargo build -p dora --profile $build_profile
cargo build -p dora-language-server --profile $build_profile

$install_dir_bin = Join-Path $Home ".local\bin"
$install_dir_share = Join-Path $Home ".local\share\dora"
$install_dir_pkgs = Join-Path $install_dir_share "pkgs"

# Remove only the share install so unrelated binaries are untouched.
if (Test-Path $install_dir_share) {
    Remove-Item -Path $install_dir_share -Recurse -Force
}

$null = New-Item -Path $install_dir_bin -ItemType "directory" -Force
$null = New-Item -Path $install_dir_pkgs -ItemType "directory" -Force

Copy-Item -Path "target/$build_profile/dora.exe" -Destination $install_dir_bin -Force
Copy-Item -Path "target/$build_profile/dora-language-server.exe" -Destination $install_dir_bin -Force

Copy-Item -Path "pkgs\std" -Destination $install_dir_pkgs -Recurse
Copy-Item -Path "pkgs\boots" -Destination $install_dir_pkgs -Recurse
