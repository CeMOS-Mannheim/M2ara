<#
.SYNOPSIS
    Assembles the parts of the installer payload that do not come from the R
    installation: the Shiny app itself, the launcher, the bundled browser and
    the readme.

.DESCRIPTION
    Expected payload layout afterwards:

        payload\app\         M2ara Shiny application (app.R, components, functions, ...)
        payload\chrome\      Chrome for Testing (chrome.exe)
        payload\R\           portable R runtime    (prepare-runtime.ps1)
        payload\PACKAGES.txt package manifest      (install-packages.R)
        payload\run.R        launcher script started by M2ara.bat
        payload\M2ara.bat    entry point used by the shortcuts

.PARAMETER PayloadDir
    Payload directory that already contains R\ (see prepare-runtime.ps1).

.PARAMETER RepoDir
    Checkout of the M2ara repository.

.PARAMETER SkipChrome
    Do not bundle Chrome for Testing. The launcher then falls back to Microsoft
    Edge (app window) or the default browser of the user.

.PARAMETER ChromeChannel
    Chrome for Testing release channel: Stable, Beta, Dev or Canary.

.EXAMPLE
    ./prepare-payload.ps1 -PayloadDir "$env:RUNNER_TEMP/payload" -RepoDir $env:GITHUB_WORKSPACE
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$PayloadDir,

    [Parameter(Mandatory = $true)]
    [string]$RepoDir,

    [switch]$SkipChrome,

    [string]$ChromeChannel = 'Stable'
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

# RUNNER_TEMP only exists on GitHub Actions; fall back to the Windows temp
# directory so the script can also be used for local builds.
$tempRoot = $env:RUNNER_TEMP
if ([string]::IsNullOrWhiteSpace($tempRoot)) { $tempRoot = [System.IO.Path]::GetTempPath() }

# Files and directories of the repository that are shipped inside app\.
# DESCRIPTION and NAMESPACE are deliberately *not* copied: Shiny treats a
# directory with a DESCRIPTION file as an R package and warns about "Loading R/
# subdirectory for Shiny application". The version is written to VERSION.txt.
$AppItems = @(
    'app.R',
    'components',
    'functions',
    'helpfiles',
    'figures',        # referenced by manual.Rmd via knitr::include_graphics()
    'manual.Rmd',
    'manual.md',
    'req.txt',
    'LICENSE',
    'LICENSE.md'
)

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

function Install-ChromeForTesting {
    param(
        [Parameter(Mandatory = $true)][string]$Destination,
        [string]$Channel = 'Stable'
    )

    $endpoint = 'https://googlechromelabs.github.io/chrome-for-testing/last-known-good-versions-with-downloads.json'
    Write-Host "    reading $endpoint"
    $metadata = Invoke-RestMethod -Uri $endpoint -TimeoutSec 120
    $channelInfo = $metadata.channels.$Channel
    if (-not $channelInfo) {
        throw "channel '$Channel' is not part of the chrome-for-testing metadata"
    }

    $version = $channelInfo.version
    $download = $channelInfo.downloads.chrome |
        Where-Object { $_.platform -eq 'win64' } |
        Select-Object -First 1
    if (-not $download) { throw "no win64 download listed for Chrome $version" }

    $archive = Join-Path $tempRoot "chrome-win64-$version.zip"
    $extractDir = Join-Path $tempRoot "chrome-win64-$version"

    $downloaded = $false
    for ($attempt = 1; $attempt -le 3; $attempt++) {
        try {
            Write-Host "    downloading $($download.url) (attempt $attempt)"
            Invoke-WebRequest -Uri $download.url -OutFile $archive -TimeoutSec 1800
            $downloaded = $true
            break
        } catch {
            Write-Warning "    download failed: $($_.Exception.Message)"
            if ($attempt -eq 3) { throw }
            Start-Sleep -Seconds (10 * $attempt)
        }
    }
    if (-not $downloaded) { throw 'Chrome download failed' }

    if (Test-Path -LiteralPath $extractDir) {
        Remove-Item -LiteralPath $extractDir -Recurse -Force
    }
    $null = New-Item -ItemType Directory -Force -Path $extractDir
    # .NET instead of tar.exe: on some machines a GNU tar from Git/MSYS shadows
    # the Windows tar and cannot handle "C:\" paths.
    Add-Type -AssemblyName System.IO.Compression.FileSystem -ErrorAction SilentlyContinue
    [System.IO.Compression.ZipFile]::ExtractToDirectory($archive, $extractDir)

    $sourceDir = Join-Path $extractDir 'chrome-win64'
    if (-not (Test-Path -LiteralPath (Join-Path $sourceDir 'chrome.exe'))) {
        throw 'chrome.exe is missing in the Chrome for Testing archive'
    }
    Invoke-Robocopy -Source $sourceDir -Destination $Destination

    $info = @(
        "Chrome for Testing $version ($Channel)",
        "source: $($download.url)"
    )
    Set-Content -LiteralPath (Join-Path $Destination 'VERSION.txt') -Value $info -Encoding utf8

    Remove-Item -LiteralPath $extractDir -Recurse -Force
    Remove-Item -LiteralPath $archive -Force

    return $version
}

$null = New-Item -ItemType Directory -Force -Path $PayloadDir

# --- app --------------------------------------------------------------------
Write-Section 'Copying the M2ara application'
$appTarget = Join-Path $PayloadDir 'app'
if (Test-Path -LiteralPath $appTarget) {
    Remove-Item -LiteralPath $appTarget -Recurse -Force
}
$null = New-Item -ItemType Directory -Force -Path $appTarget

foreach ($item in $AppItems) {
    $source = Join-Path $RepoDir $item
    if (-not (Test-Path -LiteralPath $source)) {
        Write-Warning "    not in the repository, skipped: $item"
        continue
    }
    Copy-Item -LiteralPath $source -Destination $appTarget -Recurse -Force
}

# settings.csv is deliberately *not* shipped: it is a local, user specific
# configuration file (git-ignored) and the app falls back to its defaults.
if (Test-Path -LiteralPath (Join-Path $appTarget 'settings.csv')) {
    Remove-Item -LiteralPath (Join-Path $appTarget 'settings.csv') -Force
}

# --- launcher ---------------------------------------------------------------
Write-Section 'Installing the launcher'
Copy-Item -LiteralPath (Join-Path $RepoDir 'installer/run.R') -Destination (Join-Path $PayloadDir 'run.R') -Force
Copy-Item -LiteralPath (Join-Path $RepoDir 'installer/open-browser.ps1') -Destination (Join-Path $PayloadDir 'open-browser.ps1') -Force

# The version of the app is taken from DESCRIPTION and written as a plain file,
# because DESCRIPTION must not end up in app\ (see $AppItems above).
$appVersion = 'unknown'
$descriptionFile = Join-Path $RepoDir 'DESCRIPTION'
if (Test-Path -LiteralPath $descriptionFile) {
    $description = Get-Content -LiteralPath $descriptionFile -Raw
    if ($description -match '(?m)^Version:\s*(\S+)\s*$') { $appVersion = $Matches[1] }
}
Set-Content -LiteralPath (Join-Path $PayloadDir 'VERSION.txt') -Value $appVersion -Encoding ascii
Write-Host "    app version: $appVersion"

# cmd.exe requires CRLF line endings; the repository may be checked out with LF.
$batchTarget = Join-Path $PayloadDir 'M2ara.bat'
$batchContent = Get-Content -LiteralPath (Join-Path $RepoDir 'installer/launch.bat') -Raw
$batchContent = ($batchContent -replace "`r`n", "`n") -replace "`n", "`r`n"
[System.IO.File]::WriteAllText($batchTarget, $batchContent, [System.Text.Encoding]::ASCII)
Write-Host '    M2ara.bat and run.R installed'

# --- readme -----------------------------------------------------------------
$readme = @(
    'M2ara - stand-alone Windows installation',
    '',
    'Start the application with M2ara.bat (or with the shortcut in the start menu).',
    'Everything runs locally on this computer; no data is sent anywhere.',
    '',
    'Contents',
    '  app\         the M2ara Shiny application',
    '  R\           bundled R runtime incl. all packages (see PACKAGES.txt)',
    '  chrome\      bundled browser for the application window',
    '  run.R        launcher script started by M2ara.bat',
    '',
    'Documentation: https://github.com/CeMOS-Mannheim/M2ara',
    'Problems:      https://github.com/CeMOS-Mannheim/M2ara/issues',
    ''
)
Set-Content -LiteralPath (Join-Path $PayloadDir 'README.txt') -Value $readme -Encoding utf8

# --- browser ----------------------------------------------------------------
if ($SkipChrome) {
    Write-Section 'Skipping Chrome (fallback to Edge / default browser)'
} else {
    Write-Section "Bundling Chrome for Testing ($ChromeChannel)"
    $chromeTarget = Join-Path $PayloadDir 'chrome'
    if (Test-Path -LiteralPath $chromeTarget) {
        Remove-Item -LiteralPath $chromeTarget -Recurse -Force
    }
    $chromeVersion = Install-ChromeForTesting -Destination $chromeTarget -Channel $ChromeChannel
    Write-Host "    Chrome for Testing $chromeVersion"
}

# --- summary ----------------------------------------------------------------
Write-Section 'Payload assembled'
foreach ($part in @('app', 'R', 'chrome')) {
    $path = Join-Path $PayloadDir $part
    if (Test-Path -LiteralPath $path) {
        Write-Host ("    {0,-8} {1,8} MB" -f $part, (Get-DirectorySizeMb $path))
    }
}
$totalMb = Get-DirectorySizeMb $PayloadDir
Write-Host ("    {0,-8} {1,8} MB" -f 'total', $totalMb)
