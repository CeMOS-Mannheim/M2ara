downloadHandlerPlots <- function(res, mzIdx, errorbars, zoom, plot_ready) {
  downloadHandler(
    filename = function()  {
      if (plot_ready) {
        paste0(basename(getDirectory(res)),
               "_mz", round(getMzFromMzIdx(res,
                                           mzIdx), 2),
               ".png")
      }
    },
    content = function(file) {
      if (plot_ready) {
        # generate plots at download time (avoids out-of-scope variable issues)
        p_curve <- plotCurves(res,
                              mzIdx = mzIdx,
                              errorbars = errorbars) +
          labs(title = paste0("m/z = ",
                              round(getMzFromMzIdx(res, mzIdx), 2)))

        p_peak <- plotPeak(res,
                           mzIdx = mzIdx,
                           tol = zoom) +
          labs(title = NULL)

        p_main <- p_curve + p_peak

        ggsave(file,
               plot = p_main,
               device = "png",
               scale = 1.8,
               bg = "white",
               dpi = 600,
               width = 183,
               height = 122,
               units = "mm")
      } else {
        warning("Nothing to download. Load and process data.")
      }
    })
}

downloadHandlerTable <- function(res, stats, plot_ready, name) {
  downloadHandler(
    filename = function()  {
      if (plot_ready) {
        paste0(basename(getDirectory(res)),
               "_",
               name,
               ".csv")
      }
    },
    content = function(file) {
      if (plot_ready) {
        readr::write_excel_csv(file = file, x = stats)
      } else {
        warning("Nothing to download. Load and process data.")
      }
    })
}
