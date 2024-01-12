mainTab <- function() {
  #### Main tab ####
  tabPanel("Main",
           tags$head(
             tags$style('
    ul.nav-pills{
      display: flex !important;
      font-size: 80%
    }')
           ),
    tabsetPanel(type = "pills",
                # curve and peak plot
                tabPanel("Curve",
                         fluidRow(
                           column(2,
                                  div(
                                    selectInput(inputId = "errorbars",
                                                label = "Errorbars",
                                                choices = c("None" = "none",
                                                            "Std. Dev." = "sd",
                                                            "SEM" = "sem"),
                                                selected = defaults$errorbars,
                                                multiple = FALSE),
                                    style = "width:70%"
                                  )
                           ),
                           column(1,
                                  downloadButton(outputId = "downloadPlot",
                                                 label = "Save plot",
                                                 style='padding:6px; font-size:80%; margin-top:28px',
                                                 icon = icon("download"))
                           ),
                           column(3),
                           column(2,
                                  sliderInput(inputId = "zoom",
                                              label = "m/z range",
                                              min = 0.1,
                                              max = 25,
                                              value = defaults$zoom,
                                              ticks = FALSE))
                         ),
                         column(6,
                                plotlyOutput('curve') %>%
                                  withSpinner(color="#0dc5c1")),
                         column(6,
                                plotlyOutput('peak') %>%
                                  withSpinner(color="#0dc5c1")),
                ),
                tabPanel("Score",
                         fluidRow(
                           column(2,
                                  div(
                                    selectInput(inputId = "metric",
                                                label = "Metric",
                                                choices = c("Score",
                                                            "Z'",
                                                            "V'",
                                                            "SSMD",
                                                            "log2FC",
                                                            "pIC50"),
                                                selected = defaults$errorbars,
                                                multiple = FALSE),
                                    style = "width:70%"
                                  )
                           )
                         ),
                         column(12,
                                plotlyOutput("scorePlot") %>%
                                  withSpinner(color="#0dc5c1"))
                )
    ),
    hr(),
    fluidRow(
      column(12,
             fluidRow(
               dataTableOutput("mzTable") %>%
                 withSpinner(color="#0dc5c1")
             ),
             fluidRow(
               downloadButton(outputId = "downloadTable",
                              label = "Save as CSV",
                              icon = icon("download"),
                              style = 'padding:6px; font-size:80%')
             )
      )
    ),
    width = 9)
}
