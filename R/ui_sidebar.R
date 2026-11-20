# a labelled group of controls (preprocess, aggregate, ...), so the sidebar does
# not read as one long list
sidebarGroup <- function(title, help = NULL, ...) {
  div(
    class = "m2ara-group",
    div(class = "m2ara-group-title", title,
        if (!is.null(help)) {
          tags$span(
            class = "m2ara-group-help",
            title = help,
            icon("circle-question")
          )
        }
    ),
    list(...)
  )
}

appSidebar <- function(defaults) {
  s <- sidebarPanel(
    useShinyjs(),
    use_busy_spinner(spin = "half-circle",
                     position = "top-left",
                     height = "100px", width = "100px",
                     margins = c(100, 100)),

    #### data ####
    sidebarGroup(
      "Data",
      help = "The folder with the spectra and the unit of the concentrations.",
      shinyDirButton('dir',
                     'Select folder',
                     'Please select a folder',
                     multiple = FALSE,
                     icon = icon("search"),
                     style = 'padding:6px; font-size:80%',
                     class = "btn-default"),
      div(style = "margin-top: 6px;",
          selectInput(inputId = "concUnits",
                      label = "Concentration unit",
                      choices = c("M", "mM", "uM", "nM", "pM"),
                      selected = defaults$concUnits, multiple = FALSE,
                      width = "100%"))
    ),

    #### preprocessing ####
    sidebarGroup(
      "Preprocess",
      help = paste("Smoothing, baseline removal and the sqrt transform are applied",
                   "to the spectra before peaks are detected."),
      checkboxGroupInput(inputId = "preproc_settings",
                         label = NULL,
                         choices = c("Smooth" = "smooth",
                                     "Remove baseline" = "rmBl",
                                     "Sqrt transform" = "sqrtTrans",
                                     "Monoisotopic" = "monoisotopicFilter"),
                         selected = unlist(defaults$preproc_settings))
    ),

    #### peak detection ####
    sidebarGroup(
      "Peaks",
      help = paste("How the replicates are aggregated and how strict a peak has to",
                   "be (signal to noise ratio)."),
      selectInput(inputId = "avgMethod",
                  label = "Aggregate",
                  selected = defaults$avgMethod,
                  choices = c("mean", "median", "sum"),
                  width = "100%"),
      numericInput("SNR",
                   label = "Signal to noise (SNR)",
                   min = 1,
                   max = 100,
                   step = 1,
                   value = defaults$SNR,
                   width = "100%")
    ),

    #### normalization / filter / calibration ####
    sidebarGroup(
      "Normalize",
      help = paste("Normalisation of the intensities, the variance filter and the",
                   "m/z used for the single point recalibration."),
      selectInput(inputId = "normMeth",
                  label = "Method",
                  selected = defaults$normMeth,
                  choices = c("mz", "TIC", "median", "none"),
                  width = "100%"),
      selectInput("VarFilterMethod", label = "Variance filter",
                  selected = defaults$VarFilterMethod,
                  choices = c("mean", "q75", "median", "q25", "none"),
                  width = "100%"),
      checkboxInput("SinglePointRecal", "Recalibrate", value = defaults$SinglePointRecal),
      numericInput("normMz", label = "m/z [Da]", min = 0, max = 10000,
                   value = defaults$normMz, step = 0.05, width = "100%"),
      numericInput("normTol", label = "Tolerance [Da]", min = 0, max = 10,
                   value = defaults$normTol, step = 0.05, width = "100%")
    ),

    #### alignment/binning ####
    sidebarGroup(
      "Align",
      help = paste("Tolerance for aligning the peaks across spectra and the binning",
                   "of the m/z axis."),
      numericInput("alignTol",
                   label = "Tolerance [mDa]",
                   min = 0,
                   max = 1000,
                   value = defaults$alignTol,
                   step = 1,
                   width = "100%"),
      numericInput("binTol",
                   label = "Binning [ppm]",
                   min = 0,
                   max = 1000,
                   value = defaults$binTol,
                   step = 5,
                   width = "100%")
    ),

    #### process button/info text
    createActionButton(inputId = "process",
                       label = "Process spectra",
                       icon = "redo"),
    # the status is also shown while the spectra are read (synchronous), the
    # cancel button only exists for the background job
    conditionalPanel(
      condition = "output.processBusy",
      div(style = "margin-bottom: 8px;",
          uiOutput("processStatus"),
          conditionalPanel(
            condition = "output.processRunning",
            actionButton("cancelProcess", "Cancel run",
                         icon = icon("stop"),
                         class = "btn-sm",
                         style = "margin-top: 4px;")
          )
      )
    ),
    uiOutput("datasetCard"),
    textOutput("info3", inline = FALSE),
    width = 2L)
  return(s)
}
