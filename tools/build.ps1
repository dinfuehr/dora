$ErrorActionPreference = "Stop"

$build_profile = "release"

cargo build -p dora --profile $build_profile
cargo build -p dora-language-server --profile $build_profile

$install_dir = "$Home\.dora"
$install_dir_bin = "$install_dir\bin"
$install_dir_pkgs = "$install_dir\pkgs"

# Remove old install
Remove-Item -Path $install_dir -Recurse -Force

$null = New-Item -Path $install_dir -ItemType "directory"
$null = New-Item -Path $install_dir_bin -ItemType "directory"
$null = New-Item -Path $install_dir_pkgs -ItemType "directory"

Copy-Item -Path "target/$build_profile/dora.exe" -Destination "$Home\.dora\bin"
Copy-Item -Path "target/$build_profile/dora-language-server.exe" -Destination "$Home\.dora\bin"

Copy-Item -Path "pkgs\std" -Destination $install_dir_pkgs -Recurse
Copy-Item -Path "pkgs\boots" -Destination $install_dir_pkgs -Recurse
