# a labelled group of controls (preprocess, aggregate, ...), so the sidebar does
# not read as one long list
sidebarGroup <- function(title, ...) {
  div(
    class = "m2ara-group",
    div(class = "m2ara-group-title", title),
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
      shinyDirButton('dir',
                     'Select folder',
                     'Please select a folder',
                     multiple = FALSE,
                     icon = icon("search"),
                     style = 'padding:6px; font-size:80%',
                     class = "btn-default"),
      div(style = "margin-top: 6px;",
          selectInput(inputId = "concUnits",
                      label = "Conc. unit",
                      choices = c("M", "mM", "uM", "nM", "pM"),
                      selected = defaults$concUnits, multiple = FALSE,
                      width = "80%"))
    ),

    #### concentration assignment ####
    conditionalPanel(
      condition = "output.hasFolder",
      actionButton("setConcentrations", "Set concentrations ...",
                   icon = icon("table"),
                   style = 'padding:6px; font-size:80%; margin-top: 6px;')
    ),

    #### preprocessing ####
    sidebarGroup(
      "Preprocess",
      checkboxGroupInput(inputId = "preproc_settings",
                         label = NULL,
                         choices = c("Smooth" = "smooth",
                                     "Remove baseline" = "rmBl",
                                     "Sqrt transform" = "sqrtTrans",
                                     "Monoisotopic" = "monoisotopicFilter"),
                         selected = unlist(defaults$preproc_settings)) %>%
        helper(type = "markdown", content = "preprocessing")
    ),

    #### peak detection ####
    sidebarGroup(
      "Peaks",
      fluidRow(
        column(6,
               selectInput(inputId = "avgMethod",
                           label = "Aggregate",
                           selected = defaults$avgMethod,
                           choices = c("mean", "median", "sum"))),
        column(6,
               numericInput("SNR",
                            label = "SNR",
                            min = 1,
                            max = 100,
                            step = 1,
                            value = defaults$SNR) %>%
                 helper(type = "markdown", content = "peakdetection"))
      )
    ),

    #### normalization / filter / calibration ####
    sidebarGroup(
      "Normalize",
      fluidRow(
        column(6,
               selectInput(inputId = "normMeth",
                           label = "Method",
                           selected = defaults$normMeth,
                           choices = c("mz", "TIC", "median", "none")) %>%
                 helper(type = "markdown", content = "normalization")),
        column(6,
               selectInput("VarFilterMethod", label = "Var. filter",
                           selected = defaults$VarFilterMethod,
                           choices = c("mean", "q75", "median", "q25", "none")) %>%
                 helper(type = "markdown", content = "filtering"))
      ),
      checkboxInput("SinglePointRecal", "Recalibrate",
                    value = defaults$SinglePointRecal) %>%
        helper(type = "markdown", content = "recalibration"),
      fluidRow(
        column(7,
               numericInput("normMz", label = "m/z [Da]", min = 0, max = 10000,
                            value = defaults$normMz, step = 0.05)),
        column(5,
               numericInput("normTol", label = "Tol. [Da]", min = 0, max = 10,
                            value = defaults$normTol, step = 0.05))
      )
    ),

    #### alignment/binning ####
    sidebarGroup(
      "Align",
      fluidRow(
        column(6,
               numericInput("alignTol",
                            label = "Tol. [mDa]",
                            min = 0,
                            max = 1000,
                            value = defaults$alignTol,
                            step = 1) %>%
                 helper(type = "markdown", content = "alignment")),
        column(6,
               numericInput("binTol",
                            label = "Binning [ppm]",
                            min = 0,
                            max = 1000,
                            value = defaults$binTol,
                            step = 5))
      )
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
