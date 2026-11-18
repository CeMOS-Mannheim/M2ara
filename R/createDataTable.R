createDataTable <- function(stats, plot_ready) {
  DT::renderDataTable({
    # check if data is already prepared and if not show dummy table
    if (plot_ready == "FALSE") {
      #dummy data
      return(cbind(
        tibble(" " = rep("", 10)),
        tibble(mz = c("load", rep("", 9)),
               mzIdx = c("data", rep("", 9)),
               pEC50 = c("to display", rep("", 9)),
               R2 = c("peak", rep("", 9)),
               log2FC = c("table", rep("", 9)))
      ))
    }

    cbind(tibble(" " = rep("", nrow(stats))), stats)

  },
  server = TRUE,
  filter = "bottom",
  options = list(
    searching = TRUE,
    lengthChange = FALSE,
    paging = TRUE,
    pageLength = 20,
    autoWidth = FALSE,
    rownames = FALSE,
    # column 0 is the checkbox for the feature comparison; the state lives in
    # the browser (window.m2araSelected, keyed by mzIdx), so paging/filtering
    # re-applies it without a server round trip
    columnDefs = list(
      list(
        targets = 0,
        orderable = FALSE,
        searchable = FALSE,
        className = "dt-center m2ara-check-col",
        width = "18px",
        render = DT::JS(
          "function(data, type, row) {
             if (type !== 'display') { return data; }
             var sel = window.m2araSelected || [];
             var idx = row[2];
             var checked = sel.indexOf(idx) >= 0;
             var full = sel.length >= 4;
             var dis = (!checked && full) ? ' disabled' : '';
             var chk = checked ? ' checked' : '';
             return '<input type=\"checkbox\" class=\"m2ara-check\" value=\"' +
                    idx + '\"' + chk + dis + ' />';
           }"
        )
      )
    ),
    # tint the rows of the compared features with their plot colour
    rowCallback = DT::JS(
      "function(row, data) {
         var sel = window.m2araSelected || [];
         var pos = sel.indexOf(data[2]);
         var colours = ['#D55E00', '#009E73', '#CC79A7', '#0072B2'];
         if (pos >= 0 && sel.length === 1) {
           row.style.backgroundColor = 'rgba(0, 0, 0, 0.06)';
           row.style.borderLeft = '4px solid #999999';
         } else if (pos >= 0) {
           var hex = colours[pos];
           var r = parseInt(hex.substr(1, 2), 16);
           var g = parseInt(hex.substr(3, 2), 16);
           var b = parseInt(hex.substr(5, 2), 16);
           row.style.backgroundColor = 'rgba(' + r + ',' + g + ',' + b + ',0.18)';
           row.style.borderLeft = '4px solid ' + hex;
         } else {
           row.style.backgroundColor = '';
           row.style.borderLeft = '';
         }
       }"
    )
  ),
  selection = "none"
  )
}
