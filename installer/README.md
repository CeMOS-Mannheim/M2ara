# Windows installer

The stand-alone Windows installer (`M2ara_<version>.exe`) is built automatically
by the GitHub Actions workflow
[`build-installer.yml`](../.github/workflows/build-installer.yml) whenever a
**release is published**. The installer is attached to that release as an asset
and additionally kept as a workflow artifact for 14 days.

Users do not need R: the installer ships a portable R runtime with all packages
from [`req.txt`](../req.txt) plus `MALDIcellassay` and Chrome for Testing (the
application window).

## Cutting a release

1. Make sure `DESCRIPTION` (`Version:`) matches the intended version and that
   `main` is green. Note that a release can only be built if this workflow is
   present on the **default branch** - GitHub always takes `release` workflows
   from there.
2. Create a tag with the version number, e.g. `1.5.0` (the previous releases use
   the bare version, `v1.5.0` works as well).
3. Create the release and **publish** it (drafts do not trigger the workflow).
4. The workflow checks out the tag, builds the installer and uploads
   `M2ara_1.5.0.exe` to the release. Expect roughly 20-35 minutes; the job
   summary lists file size, SHA256 and the bundled Chrome/R versions.

Everything that runs before the upload is also used for the `workflow_dispatch`
trigger, so the build can be tested without publishing a release:

* run the workflow manually (no inputs) and download the `M2ara-<version>-windows-x64`
  artifact;
* set `release_tag` to attach the result to an existing release;
* set `bundle_chrome` to `false` for a fast test build without the ~340 MB
  browser download (the launcher then uses Edge or the default browser).

## How the payload is assembled

```
payload\
  R\             portable R runtime with all packages and the M2ara package (R\library)
  chrome\        Chrome for Testing (chrome.exe)
  run.R          launcher: sets the library path, starts the app, opens the browser
  open-browser.ps1  starts the browser window, brings it to the front, falls back to Edge
  M2ara.bat      entry point used by the shortcuts
  VERSION.txt    version of the bundled app
  PACKAGES.txt   manifest of the bundled R and package versions
  README.txt     user facing note
```

The browser is started asynchronously: `run.R` returns immediately so the Shiny
server can answer the first request, while `open-browser.ps1` waits for a
**visible** window, activates it and falls back to Microsoft Edge and finally to
the default browser if no window appears (log:
`%TEMP%\m2ara-browser.log`).

`installer\M2ara.iss` packages that directory with [Inno Setup](https://jrsoftware.org/isinfo.php)
into a per-user installer (`%LOCALAPPDATA%\M2ara`, no admin rights required).

Steps and scripts:

| Step | Script |
| --- | --- |
| copy the CI R installation | `scripts/prepare-runtime.ps1` |
| install `req.txt` packages and `MALDIcellassay` into the payload library | `scripts/install-packages.R` |
| download Chrome, copy the app, install the launcher | `scripts/prepare-payload.ps1` |
| open the browser window (fallback chain, foreground) | `open-browser.ps1` |
| verify that the payload starts the app and renders the manual | `scripts/smoke-test.R` |
| optional installer icon from `figures/logo_resized.png` | `scripts/make-ico.py` |

## Configuration

All knobs live at the top of the workflow (`env:`):

| Variable | Default | Meaning |
| --- | --- | --- |
| `R_VERSION` | `release` | R version bundled in the installer. `release` follows the current R release, which has Windows binaries on CRAN and therefore keeps the build fast. Pin an exact version (`4.6.2`) for a reproducible payload. |
| `R_SNAPSHOT` | `2026-11-17` | Frozen [Posit Package Manager](https://packagemanager.posit.co/) snapshot. It must be the snapshot that `pak.lock` was created from, otherwise the package check fails. |
| `CHROME_CHANNEL` | `Stable` | Channel of [Chrome for Testing](https://chromelabs.github.io/chrome-for-testing/) |
| `MALDI_CELLASSAY_REF` | *(empty)* | Git ref of `CeMOS-Mannheim/MALDIcellassay`. Empty means: use the commit pinned in `pak.lock`. |

The R version used for the Docker image and mentioned in the README (`4.3.2`) is
**not** used for the installer: CRAN no longer provides Windows binaries for it,
so every package would have to be compiled from source during the build.

## Reproducible builds (`pak.lock`)

[`pak.lock`](../pak.lock) pins every R package (currently 149) with its version,
download URL and SHA256, and pins `MALDIcellassay` to a specific commit.

The workflows install from the frozen CRAN snapshot in `R_SNAPSHOT` - that
snapshot contains exactly the versions from the lockfile - and
`tools/check-lockfile.R` compares the result with `pak.lock`. A difference fails
the build, so a release cannot silently ship other versions than the ones that
were tested.

Updating dependencies:

```powershell
# 1) pick a new snapshot date and recreate the lockfile from it
$snapshot = "2026-12-01"
Rscript -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/$snapshot')); pak::lockfile_create(pkg = unique(c(readLines('req.txt'), 'CeMOS-Mannheim/MALDIcellassay')), lockfile = 'pak.lock')"

# 2) update R_SNAPSHOT in both workflows (.github/workflows/build-installer.yml and docker-publish.yml)
# 3) commit pak.lock + workflows and run the installer workflow manually
```

The weekly canary build (`schedule`, Mondays 04:00 UTC) builds the installer from
the default branch without uploading it. It catches drift in the runner image or
in the snapshot before a release is cut.

## Local build (Windows)

Prerequisites: R (matching `R_VERSION`), Inno Setup 6, Python with Pillow and
robocopy (part of Windows). Pandoc is **not** needed: `app.R` only runs
`knitr::knit("manual.Rmd")`, which writes markdown directly.

```powershell
$payload = "$env:TEMP\payload"
./installer/scripts/prepare-runtime.ps1 -PayloadDir $payload
& "$payload\R\bin\x64\Rscript.exe" --vanilla ./installer/scripts/install-packages.R $payload $PWD
./installer/scripts/prepare-payload.ps1 -PayloadDir $payload -RepoDir $PWD
& "$payload\R\bin\x64\Rscript.exe" --vanilla ./installer/scripts/smoke-test.R $payload
python ./installer/scripts/make-ico.py ./figures/logo_resized.png ./installer/M2ara.ico

& "${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe" `
    /DMyAppVersion=1.5.0 `
    /DMyAppVersionInfo=1.5.0.0 `
    "/DPayloadDir=$($payload -replace '\\','/')" `
    "/DOutputDir=$($env:TEMP -replace '\\','/')" `
    /DMyIcon="$PWD\installer\M2ara.ico" `
    installer\M2ara.iss
```

The setup is written to `%TEMP%\M2ara_1.5.0.exe`.

## Notes

* The app window is a Chrome for Testing app window. Its "only for automated
  testing" infobar is suppressed with `--disable-infobars` (documented switch of
  the Chromium project), and `open-browser.ps1` raises the window above other
  windows, because Windows does not let a background process take the
  foreground. A fresh browser profile is initialised with a short headless run
  first: otherwise Chrome for Testing spends the first navigation on its
  first-run flow and the app window stays empty. If no visible window appears,
  the launcher falls back to Edge and then to the default browser; what happened
  is logged to `%TEMP%\m2ara-browser.log`.
* The installer is roughly 250-320 MB, most of it the bundled R packages and
  Chrome for Testing. Chrome is bundled so the app does not depend on the
  browser installed on the user's machine; Chrome for Testing is an official,
  non-auto-updating Chrome build, which keeps the bundled version predictable.
* `windows-latest` already ships Inno Setup 6 (and R), so the workflow installs
  Inno Setup through Chocolatey only if it is really missing.
* The run summary lists the installer size and SHA256 as well as the bundled R,
  Chrome for Testing and MALDIcellassay versions.
* If `DESCRIPTION` and the release tag disagree, the workflow only warns - the
  tag wins for the installer version.

## Upgrades and the AppId

`M2ara.iss` uses a fixed `AppId`. If you still have the AppId used for the
previously published installers (1.4.2 and older), put it into
`installer\M2ara.iss` instead: with the old AppId an upgrade replaces the
previous installation, with a new one both versions appear side by side in
"Apps & features" (they share `%LOCALAPPDATA%\M2ara`).

## Code signing

The installer is **not** code signed, so Windows SmartScreen shows a warning on
first start (same as with the 1.4.2 installer). If a certificate becomes
available, sign the compiled `M2ara_<version>.exe` with `signtool` after the
"Compile installer" step and before the upload step.
