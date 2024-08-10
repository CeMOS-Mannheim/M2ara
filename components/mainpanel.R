source("components/mainTab.R")
source("components/qcTab.R")
source("components/pcaTab.R")
source("components/clustTab.R")

appMainPanel <- function(defaults) {
  m <- mainPanel(
    shiny::tabsetPanel(type = "tabs",
                       mainTab(),
                       qcTab(),
                       pcaTab(),
                       clustTab(),
                       #### Settings ####
                       tabPanel("Settings",
                                fluidRow(
                                  column(3,
                                         createActionButton(inputId = "saveSettings",
                                                            label = "Save settings",
                                                            icon = "floppy-disk")
                                  )
                                ),
                                fluidRow(
                                  column(3,
                                         selectInput(inputId = "fileFormat",
                                                     label = "File format",
                                                     choices = c("Bruker flex (.fid)" = "bruker", "mzML" = "mzml"),
                                                     selected = defaults$fileFormat, multiple = FALSE, width = "80%")
                                  ),
                                  column(3,
                                         fileInput(inputId = "mappingFile",
                                                   label = "Conc. mapping",
                                                   multiple = FALSE,
                                                   accept = ".txt",
                                                   buttonLabel = "Select (txt)",
                                                   width = "80%"))
                                ),
                                fluidRow(
                                  column(3,
                                         numericInput(inputId = "halfWindowSize",
                                                      label = "Peak window size",
                                                      value = defaults$halfWindowSize,
                                                      width = "80%")
                                  ),
                                  column(3,
                                         selectInput(inputId = "peakMethod",
                                                      label = "Peak method",
                                                      choices = c("SuperSmoother", "MAD"),
                                                      selected =  defaults$peakMethod,
                                                      width = "80%")
                                  )
                                ),
                                fluidRow(
                                  column(3,
                                         checkboxInput(inputId = "checkEmpty",
                                                      label = "Exclude empty spectra",
                                                      value = defaults$checkEmpty,
                                                      width = "80%")
                                  )
                                ),
                                fluidRow(
                                  column(3,
                                  downloadButton(outputId = "downloadFittingParameter",
                                                 label = "Save fitting param.",
                                                 icon = icon("download"),
                                                 style = 'padding:6px; font-size:80%')
                                  )
                                )
                       ),
                       #### Manual tab ####
                       tabPanel("Manual", htmltools::includeMarkdown("manual.md")
                       )
    )
  )
  return(m)
}
