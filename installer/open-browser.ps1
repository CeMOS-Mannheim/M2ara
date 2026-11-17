<#
.SYNOPSIS
    Opens M2ara in an application window of the bundled browser, with fallbacks.

.DESCRIPTION
    A browser started from a console window creates its window behind that
    console, and on some machines (virtual machines, remote sessions) it never
    shows a usable window. This script therefore starts the browser
    asynchronously (run.R does not wait for it), waits for a *visible* window,
    activates it and falls back step by step:

        1. the bundled browser (Chrome for Testing)
        2. Microsoft Edge, if a window of the bundled browser does not appear
        3. the default browser of the user, if both fail

    Every attempt is logged to %TEMP%\m2ara-browser.log so problems can be
    diagnosed from the user's machine.

    Manual test:

        powershell -NoProfile -ExecutionPolicy Bypass -File open-browser.ps1 `
            -Browser .\chrome\chrome.exe `
            -Url http://127.0.0.1:7183 `
            -UserDataDir .\browser-profile `
            -FallbackBrowser "C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe" `
            -OpenDefault

.PARAMETER Browser
    Browser started first: chrome.exe or msedge.exe.

.PARAMETER Url
    Address of the running M2ara instance.

.PARAMETER UserDataDir
    Optional profile directory for the first browser, so the app window never
    mixes with the browser the user has open.

.PARAMETER FallbackBrowser
    Optional second browser that is tried when the first one shows no window.

.PARAMETER TimeoutSeconds
    How long to wait for a visible window of each browser.

.PARAMETER OpenDefault
    Open the URL in the default browser when no browser could show a window.
#>
param(
    [Parameter(Mandatory = $true)][string]$Browser,
    [Parameter(Mandatory = $true)][string]$Url,
    [string]$UserDataDir = '',
    [string]$FallbackBrowser = '',
    [int]$TimeoutSeconds = 10,
    [switch]$OpenDefault
)

$ErrorActionPreference = 'Stop'

$logFile = Join-Path $env:TEMP 'm2ara-browser.log'

function Write-Log {
    param([string]$Message)
    try {
        $line = "{0}  {1}" -f (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), $Message
        Add-Content -LiteralPath $logFile -Value $line -Encoding utf8
    } catch {
        # logging must never break the launch
    }
}

try {
    Add-Type -Namespace M2ara -Name Window -MemberDefinition @'
[DllImport("user32.dll")] public static extern bool IsWindowVisible(IntPtr hWnd);
[DllImport("user32.dll")] public static extern bool SetForegroundWindow(IntPtr hWnd);
[DllImport("user32.dll")] public static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);
[DllImport("user32.dll")] public static extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int X, int Y, int cx, int cy, uint uFlags);
'@
} catch {
    # type already loaded
}

function Initialize-BrowserProfile {
    param([string]$Path)

    if (-not $Path) { return }
    if (-not (Test-Path -LiteralPath $Path)) {
        $null = New-Item -ItemType Directory -Force -Path $Path
    }

    # The first launch of a fresh Chrome profile wastes its initial navigation
    # on the first-run flow, so the app window would stay empty. A short
    # headless run initialises the profile up front; the real launch after that
    # is a normal (warm) start.
    if (Test-Path -LiteralPath (Join-Path $Path 'Default')) { return }

    Write-Log "initialising browser profile $Path"
    $warmupArguments = @(
        '--headless=new',
        '--disable-gpu',
        '--no-first-run',
        '--no-default-browser-check',
        '--disable-background-networking',
        "--user-data-dir=$Path",
        '--dump-dom',
        'about:blank'
    )

    try {
        $warmup = Start-Process -FilePath $Browser -ArgumentList $warmupArguments -PassThru -WindowStyle Hidden
        $null = $warmup | Wait-Process -Timeout 45 -ErrorAction SilentlyContinue
        $warmup.Refresh()
        if (-not $warmup.HasExited) {
            Write-Log 'profile warm-up timed out, stopping it'
            $warmup.Kill()
        } else {
            Write-Log "profile warm-up finished (exit code $($warmup.ExitCode))"
        }
    } catch {
        Write-Log "profile warm-up failed: $($_.Exception.Message)"
    }
}

function Start-AppWindow {
    param(
        [string]$Path,
        [int]$Timeout = 10
    )

    if (-not (Test-Path -LiteralPath $Path)) {
        Write-Log "browser not found: $Path"
        return 2
    }

    Initialize-BrowserProfile -Path $UserDataDir

    $arguments = @(
        "--app=$Url",
        '--window-size=1440,900',
        '--no-first-run',
        '--no-default-browser-check',
        # Chrome for Testing shows an "only for automated testing" infobar by
        # default; this switch hides it (documented by the Chromium project)
        '--disable-infobars'
    )
    if ($UserDataDir) {
        $arguments += "--user-data-dir=$UserDataDir"
    }

    try {
        $process = Start-Process -FilePath $Path -ArgumentList $arguments -PassThru
    } catch {
        Write-Log "could not start $Path : $($_.Exception.Message)"
        return 2
    }

    Write-Log "started $Path (pid $($process.Id))"

    $deadline = (Get-Date).AddSeconds($Timeout)
    while ((Get-Date) -lt $deadline) {
        Start-Sleep -Milliseconds 250

        if ($process.HasExited) {
            # the browser handed the request to an already running instance
            Write-Log "$Path exited, assuming an already running instance"
            return 1
        }

        $process.Refresh()
        $handle = $process.MainWindowHandle
        if ($handle -ne [IntPtr]::Zero -and [M2ara.Window]::IsWindowVisible($handle)) {
            try {
                # SW_RESTORE = 9
                $null = [M2ara.Window]::ShowWindow($handle, 9)
                # Windows does not let a background process take the foreground,
                # so raise the window via TOPMOST/NOTOPMOST instead (0x0003 =
                # SWP_NOSIZE | SWP_NOMOVE).
                $null = [M2ara.Window]::SetWindowPos($handle, [IntPtr](-1), 0, 0, 0, 0, 0x0003)
                $null = [M2ara.Window]::SetWindowPos($handle, [IntPtr](-2), 0, 0, 0, 0, 0x0003)
                $null = [M2ara.Window]::SetForegroundWindow($handle)
            } catch {
                # bringing the window to the front is best effort only
            }
            Write-Log "$Path shows a visible window"
            return 0
        }
    }

    Write-Log "$Path did not show a visible window within $Timeout seconds"
    return 2
}

$result = Start-AppWindow -Path $Browser -Timeout $TimeoutSeconds

if ($result -eq 2 -and $FallbackBrowser) {
    Write-Log "trying fallback browser $FallbackBrowser"
    $result = Start-AppWindow -Path $FallbackBrowser -Timeout ([Math]::Max(8, $TimeoutSeconds - 2))
}

if ($result -eq 2 -and $OpenDefault) {
    Write-Log 'opening the default browser'
    try {
        Start-Process $Url
        $result = 3
    } catch {
        Write-Log "could not open the default browser: $($_.Exception.Message)"
    }
}

exit $result
