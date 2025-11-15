$ErrorActionPreference = "Stop"

function Check-Exit-Code {
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }
}

cargo build --release
Check-Exit-Code

cargo test --release
Check-Exit-Code

cargo run -p dora --release -- --boots --report-all-warnings --emit-compiler --bootstrap-compiler tests/boots/hello.dora
Check-Exit-Code

.\tools\rt.ps1 --release $args
Check-Exit-Code

cargo run -p dora --release -- test --boots --test-boots --gc-verify tests/hello-world.dora
Check-Exit-Code
