<#
.SYNOPSIS
    Copies the CI R installation (incl. its library) into the installer
    payload.

.DESCRIPTION
    The M2ara installer ships a complete R runtime, so the resulting payload
    directory contains:

        payload\R\        portable copy of the R installation used on the CI runner

    The R installation is copied together with everything that was installed
    into its library (see install-packages.R) and can be relocated, because R
    derives R_HOME from the position of its own binary.

.PARAMETER PayloadDir
    Target directory for the payload (created if missing).

.EXAMPLE
    ./prepare-runtime.ps1 -PayloadDir "$env:RUNNER_TEMP/payload"
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$PayloadDir
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

function Write-Section {
    param([string]$Text)
    Write-Host ''
    Write-Host "==> $Text" -ForegroundColor Cyan
}

function Invoke-Robocopy {
    param(
        [Parameter(Mandatory = $true)][string]$Source,
        [Parameter(Mandatory = $true)][string]$Destination,
        [string[]]$Extra = @()
    )
    $null = New-Item -ItemType Directory -Force -Path $Destination
    $arguments = @($Source, $Destination, '/E', '/NFL', '/NDL', '/NJH', '/NJS', '/R:2', '/W:2', '/MT:8') + $Extra
    & robocopy @arguments | Out-Null
    $exitCode = $LASTEXITCODE
    $global:LASTEXITCODE = 0
    if ($exitCode -ge 8) {
        throw "robocopy failed (exit code $exitCode): $Source -> $Destination"
    }
}

function Get-DirectorySizeMb {
    param([string]$Path)
    if (-not (Test-Path -LiteralPath $Path)) { return 0 }
    $bytes = (Get-ChildItem -LiteralPath $Path -Recurse -File -Force -ErrorAction SilentlyContinue |
        Measure-Object -Property Length -Sum).Sum
    if (-not $bytes) { return 0 }
    return [math]::Round($bytes / 1MB, 1)
}

$null = New-Item -ItemType Directory -Force -Path $PayloadDir

# --- R ----------------------------------------------------------------------
Write-Section 'Resolving R installation'
$rscriptCommand = Get-Command Rscript.exe -ErrorAction SilentlyContinue
if (-not $rscriptCommand) {
    throw 'Rscript.exe was not found on PATH. Run this after r-lib/actions/setup-r.'
}

$rHomeOutput = & $rscriptCommand.Source --vanilla -e "cat(normalizePath(R.home(), winslash = '/'))"
if ($LASTEXITCODE -ne 0) { throw 'the CI R installation could not be started' }

$rHome = ($rHomeOutput | Where-Object { $_ -and $_.Trim() } | Select-Object -Last 1)
if (-not $rHome) { throw 'could not resolve R.home()' }
$rHome = $rHome.Trim()

$rScriptX64 = Join-Path $rHome 'bin/x64/Rscript.exe'
if (-not (Test-Path -LiteralPath $rScriptX64)) {
    throw "unexpected R installation, bin/x64/Rscript.exe is missing: $rHome"
}
Write-Host "    R home: $rHome"
& $rScriptX64 --vanilla -e "cat(R.version.string, '\n')"

Write-Section 'Copying R into the payload'
$targetR = Join-Path $PayloadDir 'R'
if (Test-Path -LiteralPath $targetR) {
    Remove-Item -LiteralPath $targetR -Recurse -Force
}
# 'tests' and 'doc' only contain R's own test suites and manuals.
Invoke-Robocopy -Source $rHome -Destination $targetR -Extra @('/XD', 'tests', 'doc')

# --- MSVC runtime -----------------------------------------------------------
# A copied R installation does not carry the Visual C++ runtime with it; it is
# normally installed system wide by the R setup. Copy the DLLs next to R.dll so
# the app also starts on machines without the VC++ redistributable.
Write-Section 'Ensuring the MSVC runtime DLLs are available'
$rBinX64 = Join-Path $targetR 'bin/x64'
foreach ($dll in @('vcruntime140.dll', 'vcruntime140_1.dll', 'msvcp140.dll')) {
    $target = Join-Path $rBinX64 $dll
    if (Test-Path -LiteralPath $target) { continue }
    $source = Join-Path $env:SystemRoot "System32/$dll"
    if (Test-Path -LiteralPath $source) {
        Copy-Item -LiteralPath $source -Destination $target -Force
        Write-Host "    added $dll"
    } else {
        Write-Warning "    $dll not found - R may not start on machines without the VC++ runtime"
    }
}

# --- done -------------------------------------------------------------------
Write-Section 'Runtime ready'
Write-Host "    R: $(Get-DirectorySizeMb (Join-Path $PayloadDir 'R')) MB"
