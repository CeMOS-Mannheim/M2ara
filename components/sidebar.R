appSidebar <- function(defaults) {
  s <- sidebarPanel(
    useShinyjs(),
    #### select dir ####
    fluidRow(
      shinyDirButton('dir',
                     '  Select folder',
                     'Please select a folder',
                     multiple = FALSE,
                     icon = icon("search"),
                     style='padding:6px; font-size:80%',
                     class = "btn-default"),
      actionButton("load", "  Load spectra ",
                   icon = icon("upload"), style='padding:6px; font-size:80%'),
      use_busy_spinner(spin = "half-circle",
                       position = "top-left",
                       height = "100px", width = "100px",
                       margins = c(100, 100))),
    #### concentration unit ####
    fluidRow(
      column(6,
             selectInput(inputId = "concUnits", label = "Conc. unit",
                         choices = c("M", "mM", "µM", "nM", "pM"),
                         selected = defaults$concUnits, multiple = FALSE, width = "80%"))
    ),
    #### preprocessing ####
    fluidRow(
      h5("Preprocessing:") %>%
        helper(type = "markdown", content = "preprocessing"),
      column(6,
             checkboxInput("smooth", "Smooth", value = defaults$smooth)),
      column(6,
             checkboxInput("rmBl", "Remove baseline", value = defaults$rmBl))
    ),
    fluidRow(
      column(6,
             checkboxInput("sqrtTrans", "Sqrt-transform", value = defaults$sqrtTrans)),
      column(6,
             checkboxInput("monoisotopicFilter", "Monoisotopic", value = defaults$monoisotopicFilter))
    ),
    #### peak detection ####
    fluidRow(
      h5("Peak detection:") %>%
        helper(type = "markdown", content = "peakdetection"),
      column(6,
             numericInput("SNR", label = "S/N-ratio", min = 1, step = 1, value = defaults$SNR)),
      column(6)
    ),
    #### normalization/var. filter
    fluidRow(
      column(6,
             radioButtons(inputId = "normMeth",
                          label = "Normalization method",
                          selected = defaults$normMeth,
                          choices = c("mz", "TIC", "PQN", "median", "none")) %>%
               helper(type = "markdown", content = "normalization")),
      column(6,
             radioButtons("VarFilterMethod", label = "Variance filtering",
                          selected = defaults$VarFilterMethod,
                          choices = c("mean", "q75","median", "q25", "none")) %>%
               helper(type = "markdown", content = "filtering"))),
    #### recal./norm. method ####
    fluidRow(
      h5("m/z for recal. and mz-norm.:"),
      checkboxInput("SinglePointRecal", "Single point recal.", value = defaults$SinglePointRecal) %>%
        helper(type = "markdown", content = "recalibration"),
      column(5,
             numericInput("normTol", label = "tol. [Da]", min = 0, value = defaults$normTol, step = 0.05)),
      column(7,
             numericInput("normMz", label = "m/z [Da]", min = 0, value = defaults$normMz, step = 0.05))),
    #### alignment/binning ####
    fluidRow(
      h5("Aligment / binning:")  %>%
        helper(type = "markdown", content = "alignment"),
      column(6,
             numericInput("alignTol",
                          label = "align tol. [Da]",
                          min = 0,
                          value = defaults$alignTol,
                          step = 0.05) ),
      column(6,
             numericInput("binTol",
                          label = "bin tol. [ppm]",
                          min = 0,
                          value = defaults$binTol,
                          step = 5))
    ),
    #### process button/info text
    actionButton("process", "Process spectra",
                 icon = icon("redo"), style='padding:6px; font-size:80%'),
    textOutput("info1", inline = FALSE),
    textOutput("info2", inline = FALSE),
    textOutput("info3", inline = FALSE),
    width = 2L)
  return(s)
}
