$ErrorActionPreference = "Stop"

function Check-Exit-Code {
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }
}

cargo build
Check-Exit-Code

cargo test
Check-Exit-Code

cargo run -p dora -- --boots --report-all-warnings --emit-compiler --bootstrap-compiler tests/boots/hello.dora
Check-Exit-Code

.\tools\rt.ps1 $args
Check-Exit-Code

cargo run -p dora -- test --boots --test-boots --gc-verify tests/hello-world.dora
Check-Exit-Code
