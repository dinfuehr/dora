$ErrorActionPreference = "Stop"

param(
    [Parameter(Mandatory = $true)]
    [string] $Archive
)

if (-not (Test-Path -Path $Archive -PathType Leaf)) {
    Write-Error "Archive not found: $Archive"
}

$TempDir = New-Item -ItemType Directory -Path ([System.IO.Path]::GetTempPath()) -Name ("dora-install-" + [System.Guid]::NewGuid().ToString())
try {
    tar -xzf $Archive -C $TempDir.FullName

    $Staging = Join-Path $TempDir.FullName "dora"
    if (-not (Test-Path $Staging -PathType Container)) {
        $Staging = Get-ChildItem -Path $TempDir.FullName -Directory | Select-Object -First 1 | ForEach-Object { $_.FullName }
    }

    if (-not (Test-Path $Staging -PathType Container)) {
        Write-Error "Could not find extracted directory in $($TempDir.FullName)"
    }

    $BinSrc = Join-Path $Staging "bin"
    $ShareSrc = Join-Path $Staging "share"

    if (-not (Test-Path $BinSrc -PathType Container) -or -not (Test-Path $ShareSrc -PathType Container)) {
        Write-Error "Archive is missing bin/ or share/ directories."
    }

    $BinDest = Join-Path $env:USERPROFILE ".local\bin"
    $ShareDest = Join-Path $env:USERPROFILE ".local\share\dora"

    New-Item -ItemType Directory -Force -Path $BinDest | Out-Null
    Get-ChildItem -Path $BinSrc -File | ForEach-Object {
        Copy-Item -Path $_.FullName -Destination (Join-Path $BinDest $_.Name) -Force
    }
    Write-Output "Installed binaries into $BinDest"

    if (Test-Path $ShareDest) {
        Remove-Item -Recurse -Force $ShareDest
    }

    New-Item -ItemType Directory -Force -Path $ShareDest | Out-Null
    Copy-Item -Recurse -Force -Path (Join-Path $ShareSrc '*') -Destination $ShareDest
    Write-Output "Installed share contents into $ShareDest"
}
finally {
    Remove-Item -Recurse -Force $TempDir
}
