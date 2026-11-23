# Screenshots for the manual and the README

How the pictures in `inst/figures` that show the app are made, and what to look at
when they are redone. The script is `tools/make-manual-screenshots.R`; this note
explains the why behind its steps.

## When

After a change that is visible in the interface: a new or moved control, a new tab,
a changed plot, changed labels. Not needed for logic-only changes.

## Requirements

- `shinytest2` and `magick`
- the mzML test data of the package (`testdata_mzML.zip`, see
  `tests/testthat/helper-setup.R`), so the manual does not need a private data set
- `NOT_CRAN=true` (the script sets it)

## What the script does, and why

1. **Copy the test data to a neutral folder** (`<temp>/M2ara_example/mzML`).
   The dataset card shows the folder path, and a temp path in a manual figure looks
   broken.
2. **Start the app with `tests/testthat/settings_mzML_data.csv`.**
   These settings fit the test data; the app's own `settings.csv` would fail on it
   (e.g. a normalization mass outside the measured range).
3. **Process and select the feature with the best CRS.**
   The manual should show a real dose-response curve, not a flat one. The script
   reads the CRS column of the peak table and clicks that row.
4. **Screenshot the tabs.**
   - main tab → `interface.png` (whole page) and `gui_example.png` (cut to the
     window height, like the README had it)
   - QC → `qc.png`, cut to the tab, **without the sidebar** (the sidebar shows the
     folder, which is irrelevant for the figure)
   - PCA → `pca_scores.png` and `pca_loadings.png`; **PCA is computed first**
     ("Run PCA"), otherwise the plots are empty, and `simpleLoadings` is off so both
     loadings plots appear with their titles
   - Cluster → `clustering_curves.png` and `clustering_metrics.png` (one screenshot,
     cut in two)
   - Settings → `settings.png`
   - folder dialog → `selectFolder.png`, taken **last** (it covers the page) and as
     the element `.modal-content` so only the dialog is in the picture
5. **Cut the figures.** Crop offsets are in the script; they exist because of the
   following, all of which were bugs in earlier versions:

   | figure | offset | why |
   | --- | --- | --- |
   | `clustering_curves.png` | height 520 | otherwise the numbers under the slider are cut |
   | `clustering_metrics.png` | start 512 | below the controls, above the metric titles |
   | `pca_loadings.png` | start 551 | otherwise a line of the controls above is visible |
   | `qc.png` | x 355, width 1350 | sidebar and empty space outside the tab |
   | `gui_example.png` | height 1000 | the README shows the window, not the scrolled page |

6. **Window widths matter.** 2200 px for the main window, 2600 px for the PCA and
   cluster screenshots: with a narrower window "mean (Euclidean)" wraps onto two
   lines and the titles of the loadings plots are cut.

## Checklist before committing

- [ ] `clustering_curves.png`: the scale under "Number of cluster" is complete
- [ ] `clustering_metrics.png`: no controls above, "Suggested k" and the four metric
      titles visible
- [ ] `pca_scores.png`: points and ellipses visible (PCA was computed)
- [ ] `pca_loadings.png`: both titles ("Feature importance for PC1/PC2 …") complete,
      axis titles visible
- [ ] `interface.png` / `gui_example.png`: "Binning [ppm]" and "Tol. [mDa]" on one
      line, card shows the folder name (not a temp path)
- [ ] `settings.png`: both groups visible
- [ ] `selectFolder.png`: only the dialog, nothing cut at the top
- [ ] `qc.png`: plate map visible. The mzML test data has **no spot ids**
      (`spotID=…`), so the plate map is hidden; `qc.png` therefore comes from
      `exampledata/generated`, which carries spots. Use data with spot ids, or take
      the QC figure without the plate map and say so in the text.

## What stays manual

`interface.png` is the only figure with annotations (numbers 1–4 and coloured boxes),
see the note in the manual. The script produces the plain screenshot; the labels are
added by hand. `pipeline.png`, `curve_figure.png`, `Workflow_figure.png` and
`logo_resized.png` are drawings/logos, not screenshots.

## Pitfalls

- **File locks**: writing a PNG that an editor has open fails with
  "cannot open the connection". The script writes to a temp file and copies with
  retries.
- **Timing**: after clicking "Process spectra" the table keeps its placeholder rows
  for a while. Wait until the first cell of the mzML column starts with a digit
  (`wait_for_results()`), do not use a fixed sleep.
- **Clustering** takes ~45 s up to k = 10; the script waits, then screenshots.

## Rendering the manual

After replacing figures: `Rscript tools/render-manual.R` (writes `inst/manual.md`),
then commit both the figures and the rendered manual.
