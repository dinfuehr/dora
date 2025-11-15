#!/usr/bin/env pwsh
$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRoot = Split-Path -Parent $scriptDir
$currentDir = (Get-Location).ProviderPath

if ([System.IO.Path]::GetFullPath($currentDir) -ne [System.IO.Path]::GetFullPath($repoRoot)) {
    Write-Error "Error: Run this script from $repoRoot"
    exit 1
}

uv run --project tools/pytester pytester @Args
