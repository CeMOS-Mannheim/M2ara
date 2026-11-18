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
  server = FALSE,
  filter = "bottom",
  rownames = FALSE,
  options = list(
    searching = TRUE,
    lengthChange = FALSE,
    paging = TRUE,
    pageLength = 20,
    autoWidth = FALSE,
    # column 0 is the checkbox for the feature comparison; the state lives in
    # the browser (window.m2araSelected, keyed by mzIdx), so paging/filtering
    # re-applies it without a server round trip
    columnDefs = list(
      list(
        targets = 0,
        orderable = TRUE,
        searchable = FALSE,
        className = "dt-center m2ara-check-col",
        width = "20px",
        render = DT::JS(
          "function(data, type, row) {
             var sel = window.m2araSelected || [];
             var idx = parseInt(row[2], 10);
             // sorting puts the selected features first
             if (type === 'sort') { return sel.indexOf(idx) >= 0 ? '0' : '1'; }
             if (type !== 'display') { return data; }
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
    # tint the rows (feature colour when comparing, neutral for a single one)
    rowCallback = DT::JS(
      "function(row, data) {
         var sel = window.m2araSelected || [];
         var pos = sel.indexOf(parseInt(data[2], 10));
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
    ),
    # after every draw (paging, sorting, filtering, re-render) restore the
    # checkboxes from the browser state and keep the server in sync
    drawCallback = DT::JS(
      "function(settings) {
         if (window.m2araUpdateCheckboxes) { window.m2araUpdateCheckboxes(); }
         if (window.Shiny) {
           Shiny.setInputValue('mzTable_selected',
                               (window.m2araSelected || []).slice(),
                               {priority: 'event'});
         }
       }"
    )
  ),
  selection = "none"
  )
}
