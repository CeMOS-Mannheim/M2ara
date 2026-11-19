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
