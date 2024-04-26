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
      createActionButton(inputId = "load",
                         label = "  Load spectra ",
                         icon = "upload"),
      use_busy_spinner(spin = "half-circle",
                       position = "top-left",
                       height = "100px", width = "100px",
                       margins = c(100, 100))
      ),
    #### concentration unit ####
    fluidRow(
      column(6,
             selectInput(inputId = "concUnits",
                         label = "Conc. unit",
                         choices = c("M", "mM", "µM", "nM", "pM"),
                         selected = defaults$concUnits, multiple = FALSE, width = "80%")
      )
    ),

    #### preprocessing ####
    checkboxGroupInput(inputId = "preproc_settings",
                       label = "Preprocess",
                       choices = c("Smooth" = "smooth",
                                   "Remove baseline" = "rmBl",
                                   "Sqrt-trans." = "sqrtTrans",
                                   "Monoisotopic" = "monoisotopicFilter"),
                       selected = unlist(defaults$preproc_settings)),

    #### peak detection ####
    fluidRow(
      column(6,
             selectInput(inputId = "avgMethod",
                         label = "Aggregate",
                         selected = defaults$avgMethod,
                         choices = c("mean", "median", "sum"))
      ),
      column(6,
             numericInput("SNR",
                          label = "SNR",
                          min = 1,
                          step = 1,
                          value = defaults$SNR) %>%
               helper(type = "markdown", content = "peakdetection")
      )
    ),
    #### normalization/var. filter
    fluidRow(
      column(6,
             selectInput(inputId = "normMeth",
                         label = "Normalize",
                         selected = defaults$normMeth,
                         choices = c("mz", "TIC", "PQN", "median", "none")) %>%
               helper(type = "markdown", content = "normalization")),
      column(6,
             selectInput("VarFilterMethod", label = "Var. filter",
                         selected = defaults$VarFilterMethod,
                         choices = c("mean", "q75","median", "q25", "none")) %>%
               helper(type = "markdown", content = "filtering"))),
    #### recal./norm. method ####
    fluidRow(
      h5("m/z for recal. and mz-norm.:"),
      checkboxInput("SinglePointRecal", "Recalibrate", value = defaults$SinglePointRecal) %>%
        helper(type = "markdown", content = "recalibration"),
      column(7,
             numericInput("normMz", label = "m/z [Da]", min = 0, value = defaults$normMz, step = 0.05)),
      column(5,
             numericInput("normTol", label = "tol. [Da]", min = 0, value = defaults$normTol, step = 0.05))),
    #### alignment/binning ####
    fluidRow(
      h5("Align/bin:")  %>%
        helper(type = "markdown", content = "alignment"),
      column(6,
             numericInput("alignTol",
                          label = HTML("align tol.<br>[mDa]"),
                          min = 0,
                          value = defaults$alignTol,
                          step = 1) ),
      column(6,
             numericInput("binTol",
                          label = HTML("bin tol.<br>[ppm]"),
                          min = 0,
                          value = defaults$binTol,
                          step = 5))
    ),
    #### process button/info text
    createActionButton(inputId = "process",
                       label = "Process spectra",
                       icon = "redo"),
    textOutput("info1", inline = FALSE),
    textOutput("info2", inline = FALSE),
    textOutput("info3", inline = FALSE),
    width = 2L)
  return(s)
}
