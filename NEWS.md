# M2ara 1.6.0

## Concentrations

- A sample sheet (CSV/TSV) maps file names to concentrations; every spectrum of a
  file inherits its concentration, so files with several spots work. The sheet can
  sit in the data folder as `samples.csv` or be filled in the app.
- **Set concentrations ...** in the sidebar opens a dialog with one field per file,
  prefilled where the file name or a sheet already says it. Files can be left out
  of the analysis with a checkbox: they are dropped before processing, so they do
  not appear in the plate map, the PCA or the statistics. The assignment can be
  written into the folder as `samples.csv`.
- The file format is detected from the selected folder (mzML or Bruker flex), so the
  format menu and the mapping upload are gone from the settings.

## Dataset card

- The card shows the last two folders, the number of files (spectra and features
  once known), the format, where the concentrations come from, and a coloured state:
  not read yet, loaded, settings changed, up to date, failed.
- A failed load says why and which files are affected, in the card and in the
  notification. "Last run" appears only for a run that produced a result; a cached
  result shows when it was computed.

## Clustering

- Runs in a background process with progress per k and can be cancelled.
- Curves with a poor fit (weighted goodness of fit < 0.8) or without rise are left
  out and reported; up to 2000 features are clustered.
- Distance: shape only (SBD) or shape + potency (Euclidean). Centroid: mean, pam or
  shape (DBA), each labelled with the distance it belongs to.
- Metrics: ASW, Dunn, Davies-Bouldin and WRSS; a k is suggested from the votes of
  silhouette, Dunn and Davies-Bouldin.
- Clusters use the Okabe-Ito colours that the compared features do not use, ordered
  by cluster label, so a cluster keeps its colour when k changes.

## Interface

- The sidebar is grouped (Data, Preprocess, Peaks, Normalize, Align) with short
  labels; the help of each field opens as a modal.
- The peak table shows readable numbers (`m/z` four decimals, SNR one, scores two,
  R2 three, intensities three significant digits) without changing the values.
- Buttons that cannot act are disabled; a failed run does not lock
  **Process spectra** any more.
- **Save plot** and **Export results (ZIP)** sit in one row above the plots. The
  settings tab is grouped (Settings, Cache and sharing) and says where
  `settings.csv` is and when it was saved last.
- A single selected feature is drawn black again, and **Normalize curves** works for
  it as well.

## Cache and state

- One cache entry per dataset with a small sidecar (key and time), written
  atomically; the settings tab shows how many datasets are cached and how much space
  they use.
- Every state change goes through a state machine that reports unexpected jumps in
  the log; the processing settings are checked before a run starts.

## Manual

- New screenshots of the interface, the QC tab, the PCA, the cluster tab and the
  settings, and a table of contents with working anchors.
- `tools/make-manual-screenshots.R` recreates the screenshots and
  `tools/screenshots.md` explains how and what to check.
# M2ara 1.5.0

## Processing

- M2ara is an installable R package now (`M2ara::runApp()`, `M2ara::app()`), the
  builds are reproducible (`pak.lock`, frozen CRAN snapshot, pinned
  MALDIcellassay commit).
- Processing runs in a background process: the app stays responsive, shows the
  current step, the percentage and the feature counter, and a run can be
  cancelled.
- One button: **Process spectra** loads the spectra (when needed) and processes
  them. Changing settings only re-processes; switching the folder invalidates
  the loaded spectra and the result.
- The result cache is keyed by the data, the settings, the version of
  M2ara/MALDIcellassay and a cache schema, so results of an older version are
  never reused. Cached results make a restart instant; **Re-process from
  scratch** in the `Settings` tab reads the files again and ignores the cache.
- The recalibration check draws centroided data as sticks.

## Plate map

- The spot of an mzML spectrum is read from its id (`spotID="chip=0 spot=K11"`)
  and used for the target plate view, so the plate map is no longer limited to
  Bruker flex data.
- Metrics that need a selected feature or a computed PCA show a hint instead of
  failing, and the log scale is only offered for metrics where it is meaningful
  (concentration, total peak intensity, normalization factor, selected m/z).

## Feature comparison

- Up to four features can be compared: the concentration-response curves are
  overlaid (colour = *m/z*, optional normalisation) and the peak profiles are
  shown as a 2 x 2 grid that keeps the concentration colours.
- Features are selected with the checkboxes in the first column (the selection
  survives paging, sorting and filtering) or with a single click on a row.
  Sorting by the checkbox column puts the selected features first.
- **Deselect all** clears the comparison; at most four features are compared.

## Sparse PCA

- The explained variance is relative to the total variance, the largest loading
  of every component is made positive, constant features are dropped and NA
  intensities are treated as 0.
- Empty components are reported and no longer offered as plot axes; the x and y
  axes exclude each other.
- The loadings carry the feature index, so they are joined to the peak table by
  construction; the loadings plot shows at most ten features per sign (twenty in
  total) and states how many of the active features are shown.
- The number of active features per component is shown together with the
  parameters of the run, and the L1 penalty defaults to 1e-2.
- An optional `log10(x + 1)` transform of the intensity matrix can be selected,
  but not on top of the sqrt preprocessing.

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
