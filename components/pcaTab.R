pcaTab <- function() {
  #### PCA tab ####
  tabPanel("PCA",
           h4("PCA of single spectra"),
           plotlyOutput('pca') %>%
             withSpinner(color="#0dc5c1"),
           fluidRow(
             column(2,
                    createActionButton(inputId = "doPca",
                                       label = "Perform PCA",
                                       icon = "chart-line")
             ),
             column(2,
                    sliderInput(inputId = "pcaAlpha",
                                label = "Log(L1-Penalty)",
                                min = -10,
                                max = 0,
                                value = defaults$pcaAlpha,
                                step = 0.1)),
             column(2,
                    sliderInput(inputId = "pcaBeta",
                                label = "Log(L2-Penalty)",
                                min = -10,
                                max = 0,
                                value = defaults$pcaBeta,
                                step = 0.1)),
             column(1,
                    selectInput(inputId = "pcaX",
                                label = "x-axis",
                                choices = paste0("PC", 1:5),
                                multiple = FALSE,
                                selected = defaults$pcaX)
             ),
             column(1,
                    selectInput(inputId = "pcaY",
                                label = "y-axis",
                                choices = paste0("PC", 1:5),
                                multiple = FALSE,
                                selected = defaults$pcaY)
             ),
             column(1,
                    selectInput(inputId = "pcaEllipse",
                                label = "Ellipse size",
                                choices = c(0.5, 0.67, 0.95, 0.99),
                                multiple = FALSE,
                                selected = defaults$pcaEllipse)),
             column(1,
                    materialSwitch(
                      inputId = "simpleLoadings",
                      label = "Summarise loadings",
                      value = defaults$simpleLoadings,
                      status = "primary")
             ),
             column(2,
                    createActionButton(inputId = "pca2peaksTable",
                                       label = "Send to Peak Table",
                                       icon = "share-from-square"))
           ),
           fluidRow(
             column(6,
                    plotlyOutput('pcaLoading1') %>%
                      withSpinner(color="#0dc5c1")),
             column(6,
                    plotlyOutput('pcaLoading2') %>%
                      withSpinner(color="#0dc5c1"))
           )
  )
}
