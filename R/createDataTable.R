createDataTable <- function(stats, plot_ready) {
  DT::renderDataTable({
    # check if data is already prepared and if not show dummy table
    if (plot_ready == "FALSE") {
      #dummy data
      return(tibble(mz = c("load", rep("", 9)),
                    mzIdx = c("data", rep("", 9)),
                    pEC50 = c("to display", rep("", 9)),
                    R2 = c("peak", rep("", 9)),
                    log2FC = c("table", rep("", 9))))
    }
    return(stats)

  },
  server = TRUE,
  filter = "bottom",
  options = list(searching = TRUE,
                 lengthChange = FALSE,
                 paging = TRUE,
                 pageLength = 20,
                 autoWidth = TRUE,
                 rownames = FALSE,
                 # the compared features are tinted in their plot colour; the
                 # mapping is updated by a custom message and applied on every
                 # draw, so it survives paging and filtering
                 rowCallback = DT::JS(
                   "function(row, data) {
                      var styles = window.m2araRowColours || {};
                      var mz = parseFloat(data[0]);
                      if (isNaN(mz)) { return; }
                      var style = styles[mz.toFixed(2)];
                      if (style) {
                        row.style.backgroundColor = style.background;
                        row.style.borderLeft = '4px solid ' + style.colour;
                      } else {
                        row.style.backgroundColor = '';
                        row.style.borderLeft = '';
                      }
                    }")
  ),
  selection = list(mode = "multiple",
                   selected = 1)
  )
}
