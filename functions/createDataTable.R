createDataTable <- function(stats, plot_ready) {
  DT::renderDataTable({
    # check if data is already prepared and if not show dummy table
    if (plot_ready == "TRUE") {
      tableData <- stats %>%
        mutate_if(is.numeric, function(x) {
          round(x, 3)
        })

    } else {
      #dummy data
      tableData <- tibble(mz = c("load", rep("", 9)),
                          mzIdx = c("data", rep("", 9)),
                          pIC50 = c("to display", rep("", 9)),
                          R2 = c("peak", rep("", 9)),
                          log2FC = c("table", rep("", 9)))
    }

    tableData

  },
  server = TRUE,
  filter = "bottom",
  options = list(searching = TRUE,
                 lengthChange = FALSE,
                 paging = TRUE,
                 pageLength = 20,
                 autoWidth = TRUE,
                 rownames = FALSE),
  selection = list(mode = "single",
                   selected = 1)
  )
}
