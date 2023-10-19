downloadHandlerPlots <- function(res, selected_row, p_curve, p_peak, plot_ready) {
  downloadHandler(
    filename = function()  {
      if (plot_ready == "TRUE") {
        paste0(basename(getDirectory(res)),
               "_mz", round(getMzFromMzIdx(res,
                                           selected_row), 2),
               ".png")
      }
    },
    content = function(file) {
      if (plot_ready == "TRUE") {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        p_main <- ggarrange(p_curve, p_peak)
        ggsave(file,
               plot = p_main,
               device = device,
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

downloadHandlerTable <- function(res, stats, plot_ready) {
  downloadHandler(
    filename = function()  {
      if (plot_ready == "TRUE") {
        paste0(basename(getDirectory(res)),
               "_peakTable",
               ".csv")
      }
    },
    content = function(file) {
      if (plot_ready == "TRUE") {
        write_excel_csv(file = file, x = stats)
      } else {
        warning("Nothing to download. Load and process data.")
      }
    })
}
