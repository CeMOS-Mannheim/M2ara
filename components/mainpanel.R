source("components/mainTab.R")
source("components/qcTab.R")
source("components/pcaTab.R")
source("components/lassoTab.R")
source("components/hcTab.R")

appMainPanel <- function(defaults) {
  m <- mainPanel(
    shiny::tabsetPanel(type = "tabs",
                       mainTab(),
                       qcTab(),
                       pcaTab(),
                       lassoTab(),
                       clustTab(),
                       #### Settings ####
                       tabPanel("Settings",
                                fluidRow(
                                  column(2,
                                         createActionButton(inputId = "saveSettings",
                                                            label = "Save settings",
                                                            icon = "floppy-disk")
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
