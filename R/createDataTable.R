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

    shown <- displayStats(stats)
    # readable column names; the values behind them are unchanged
    names(shown) <- vapply(names(shown), columnLabel, character(1))

    cbind(tibble(" " = rep("", nrow(stats))), shown)

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
# The table is for reading, not for computing: m/z keeps its decimals, the rest
# is rounded so a column of similar numbers can be compared at a glance. Only the
# displayed copy is rounded, the values behind it stay untouched.
displayStats <- function(stats) {
  out <- as.data.frame(stats)
  if (nrow(out) == 0L) {
    return(out)
  }

  roundColumn <- function(name, digits) {
    if (name %in% names(out) && is.numeric(out[[name]])) {
      out[[name]] <<- round(out[[name]], digits)
    }
  }
  sigColumn <- function(name, digits) {
    if (name %in% names(out) && is.numeric(out[[name]])) {
      out[[name]] <<- signif(out[[name]], digits)
    }
  }

  roundColumn("mz", 4)
  roundColumn("SNR", 1)
  roundColumn("pEC50", 2)
  roundColumn("R2", 3)
  roundColumn("log2FC", 2)

  for (nm in c("Z'", "V'", "SSMD", "FZ", "FV", "FS", "CRS", "CRS_pEC50")) {
    roundColumn(nm, 2)
  }
  for (nm in c("TIC", "peak_intensity", "Norm_factor", "intensity")) {
    sigColumn(nm, 3)
  }

  out
}

# the full name of a column, shown as tooltip in the header
columnHelp <- function(name) {
  help <- c(
    "mz" = "m/z of the feature [Da]",
    "mzIdx" = "index of the feature in the dataset",
    "pEC50" = "-log10 of the half maximal concentration",
    "R2" = "goodness of fit of the curve",
    "log2FC" = "log2 fold change between the highest and lowest concentration",
    "Z'" = "Z' factor (assay quality)",
    "V'" = "V' factor (assay quality, robust)",
    "SSMD" = "strictly standardised mean difference",
    "FZ" = "Z' of the fitted curve",
    "FV" = "V' of the fitted curve",
    "FS" = "SSMD of the fitted curve",
    "CRS" = "curve response score (0-100)",
    "cluster" = "cluster from the clustering tab"
  )
  if (name %in% names(help)) help[[name]] else NULL
}

# help texts of all columns as JS array
tableColumnHelp <- function(stats) {
  columns <- c(" ", if (is.data.frame(stats)) names(stats) else character(0))
  vapply(columns, function(nm) {
    txt <- columnHelp(nm)
    if (is.null(txt)) "" else txt
  }, character(1))
}

columnHelpJs <- function(help) {
  paste0("\"", gsub("\"", "\\\\\"", help), "\"", collapse = ", ")
}

# readable column names for the table (the values behind it are unchanged)
columnLabel <- function(name) {
  labels <- c(
    "mz" = "m/z [Da]",
    "mzIdx" = "feature",
    "R2" = "R2 (fit)",
    "log2FC" = "log2 FC",
    "Z'" = "Z (assay)",
    "V'" = "V (assay)",
    "FZ" = "Z (fit)",
    "FV" = "V (fit)",
    "FS" = "SSMD (fit)",
    "CRS" = "CRS (0-100)",
    "SNR" = "SNR",
    "cluster" = "cluster"
  )
  if (name %in% names(labels)) labels[[name]] else name
}