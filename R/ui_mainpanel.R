appMainPanel <- function(defaults) {
  m <- mainPanel(
    shiny::tabsetPanel(type = "tabs",
                       mainTab(defaults),
                       qcTab(defaults),
                       pcaTab(defaults),
                       clustTab(defaults),
                       #### Settings ####
                       tabPanel("Settings",
                                div(class = "m2ara-group", style = "margin-bottom: 10px;",
                                    div(class = "m2ara-group-title", "Settings"),
                                    fluidRow(
                                      column(3,
                                             createActionButton(inputId = "saveSettings",
                                                                label = "Save settings",
                                                                icon = "floppy-disk"),
                                             tags$div(
                                               tags$small(textOutput("settingsInfo")),
                                               style = "margin-top: 4px; color: #777;"
                                             )
                                      ),
                                      column(3,
                                             selectInput(inputId = "fileFormat",
                                                         label = "File format",
                                                         choices = c("Bruker flex (.fid)" = "bruker",
                                                                     "mzML" = "mzml"),
                                                         selected = defaults$fileFormat,
                                                         multiple = FALSE, width = "100%")
                                      ),
                                      column(3,
                                             fileInput(inputId = "mappingFile",
                                                       label = "Conc. mapping",
                                                       multiple = FALSE,
                                                       accept = ".txt",
                                                       buttonLabel = "Select (txt)",
                                                       width = "100%")
                                      )
                                    ),
                                    fluidRow(
                                      column(3,
                                             numericInput(inputId = "halfWindowSize",
                                                          label = "Peak window size",
                                                          value = defaults$halfWindowSize,
                                                          min = 1,
                                                          max = 100,
                                                          width = "100%")
                                      ),
                                      column(3,
                                             selectInput(inputId = "peakMethod",
                                                         label = "Peak method",
                                                         choices = c("SuperSmoother", "MAD"),
                                                         selected = defaults$peakMethod,
                                                         width = "100%")
                                      ),
                                      column(3,
                                             checkboxInput(inputId = "checkEmpty",
                                                          label = "Exclude empty spectra",
                                                          value = defaults$checkEmpty)
                                      )
                                    )
                                ),
                                div(class = "m2ara-group", style = "margin-bottom: 10px;",
                                    div(class = "m2ara-group-title", "Cache and sharing"),
                                    fluidRow(
                                      column(3,
                                             actionButton("clearCache",
                                                          "Clear result cache",
                                                          icon = icon("trash"),
                                                          style = 'padding:6px; font-size:80%'),
                                             tags$div(
                                               tags$small(textOutput("cacheInfo")),
                                               style = "margin-top: 4px; color: #777;"
                                             )
                                      ),
                                      column(3,
                                             actionButton("forceProcess",
                                                          "Read files again (ignore cache)",
                                                          icon = icon("rotate"),
                                                          style = 'padding:6px; font-size:80%'),
                                             tags$div(style = "height: 10px;")
                                      ),
                                      column(3,
                                             actionButton("copyLink",
                                                          "Copy link to this view",
                                                          icon = icon("link"),
                                                          style = 'padding:6px; font-size:80%'),
                                             tags$div(style = "height: 10px;")
                                      ),
                                      column(3,
                                             tags$label("Open shared view", `for` = "m2araOpenInput",
                                                        style = "font-weight: normal; font-size: 80%;"),
                                             tags$div(style = "display: flex; gap: 4px;",
                                                      tags$input(id = "m2araOpenInput", type = "text",
                                                                 placeholder = "paste a share link",
                                                                 style = "flex: 1; font-size: 80%;"),
                                                      tags$button(id = "m2araOpenButton", type = "button",
                                                                  class = "btn btn-default btn-sm", "Open"))
                                      )
                                    )
                                )
                       ),
                       #### Manual tab ####
                       tabPanel("Manual",
                                withMathJax(htmltools::HTML(
                                  markdown::markdownToHTML(
                                    system.file("manual.md", package = "M2ara"),
                                    fragment.only = TRUE
                                  )
                                )))
    )
  )
  return(m)
}
