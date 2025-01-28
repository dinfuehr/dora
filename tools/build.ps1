$ErrorActionPreference = "Stop"

$build_profile = "release"

cargo build -p dora --profile $build_profile
cargo build -p dora-language-server --profile $build_profile

# Remove old install
Remove-Item -Path "$Home\.dora" -Recurse -Force

$null = New-Item -Path "$Home\.dora" -ItemType "directory"
$null = New-Item -Path "$Home\.dora\bin" -ItemType "directory"
$null = New-Item -Path "$Home\.dora\pkgs" -ItemType "directory"

Copy-Item -Path "target/$build_profile/dora.exe" -Destination "$Home\.dora\bin"
Copy-Item -Path "target/$build_profile/dora-language-server.exe" -Destination "$Home\.dora\bin"

Copy-Item -Path "pkgs\std" -Destination "$Home\.dora\pkgs" -Recurse
Copy-Item -Path "pkgs\boots" -Destination "$Home\.dora\pkgs" -Recurse
