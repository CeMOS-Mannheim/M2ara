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
                                  column(2,
                                         createActionButton(inputId = "saveSettings",
                                                            label = "Save settings",
                                                            icon = "floppy-disk")
                                  )
                                ),
                                fluidRow(
                                  column(2,
                                         selectInput(inputId = "fileFormat",
                                                     label = "File format",
                                                     choices = c("Bruker flex (.fid)" = "bruker", "mzML" = "mzml"),
                                                     selected = defaults$fileFormat, multiple = FALSE, width = "80%")
                                  )
                                ),
                                fluidRow(
                                  column(2,
                                         numericInput(inputId = "halfWindowSize",
                                                      label = "Window size peak detection",
                                                      value = defaults$halfWindowSize,
                                                      width = "80%")
                                  )
                                ),
                                fluidRow(
                                  column(2,
                                         checkboxInput(inputId = "checkEmpty",
                                                      label = "Exclude empty spectra",
                                                      value = defaults$checkEmpty,
                                                      width = "80%")
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
