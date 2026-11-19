# M2ara 1.5.0 (unreleased)

## Processing

- M2ara is an installable R package now (`M2ara::runApp()`, `M2ara::app()`), the
  builds are reproducible (`pak.lock`, frozen CRAN snapshot, pinned
  MALDIcellassay commit).
- Processing runs in a background process: the app stays responsive, shows the
  current step, the percentage and the feature counter, and a run can be
  cancelled.
- Results are cached per data set and settings: unchanged settings and files
  return the previous result immediately, changed settings re-process without
  reading the spectra again.
- One button: **Process spectra** loads the spectra (when needed) and processes
  them. Switching the folder invalidates the loaded spectra and the result.
- The recalibration check draws centroided data as sticks.

## Feature comparison

- Up to four features can be compared: the concentration-response curves are
  overlaid (colour = *m/z*, optional normalisation) and the peak profiles are
  shown as a 2 x 2 grid that keeps the concentration colours.
- Features are selected with the checkboxes in the first column (survives
  paging, sorting and filtering) or with a single click on a row.
- Sorting by the checkbox column puts the selected features first.

## Export

- **Export results (ZIP)** writes the peak table, the fitting parameters, the
  intensity matrix, the settings and a `run_info.json` with the software
  versions into one archive.

## Usability

- Keyboard shortcuts: `Ctrl+Enter` (process), `Ctrl+F` (m/z search), `Ctrl+S`
  (save plot), `Ctrl+E` (export), `Ctrl+D`/`Esc` (deselect all),
  `Ctrl+Shift+1-4` (toggle a feature), `Ctrl+/` (overview).
- **Copy link to this view** puts settings, folder and compared features into a
  shareable link; **Open shared view** applies such a link to another
  installation, independent of host and port.
- The app keeps running across page reloads.
- mzML files that contain chromatograms no longer flood the console with
  warnings.

## Build and CI

- `pak.lock` with a frozen CRAN snapshot and a pinned MALDIcellassay commit;
  `tools/install-maldi-from-lock.R`, `tools/check-lockfile.R` and
  `tools/check-dependencies.R` keep the environment reproducible.
- The Docker image builds in about ten minutes (cached R library, build cache),
  a weekly canary build catches drift.
- The Windows installer is built from the package and attached to the release.

## Fixes

- Settings are passed to the server explicitly; download handlers no longer
  touch the reactive inputs.
- The byte order mark was removed from `DESCRIPTION`.
- Settings of older installations are migrated to `settings.csv` in the
  installation root.
