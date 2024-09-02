qcTab <- function() {
  ### QC tab ####
  tabPanel("QC",
           fluidRow(
             h4("Recalibration check"),
             plotlyOutput('checkRecal') %>%
               withSpinner(color="#0dc5c1")
           ),

           fluidRow(
             column(2,
                    hidden(
                      selectInput("plateStat",
                                  label = "Metric",
                                  choices = c("Concentration",
                                              "Total Peak Intensity",
                                              "Normalization factor",
                                              "Recal-shift",
                                              "Selected-mz",
                                              "PC-x",
                                              "PC-y",
                                              "Outlier-mz",
                                              "Outlier-all"),
                                  multiple = FALSE,
                                  selected = defaults$plateStat)
                    )
             ),
             column(2,
                    hidden(
                      checkboxInput("plateScale",
                                    label = "Log10 scale",
                                    value = defaults$plateScale)
                    )
             )
           ),

           fluidRow(
             column(6,
                    fluidRow(
                      plotOutput("platemap")
                    ),
             ),
             column(6,
                    uiOutput(outputId = "summaryText")
             )
           )
  )

}
