appMainPanel <- function() {
  m <- mainPanel(
    shiny::tabsetPanel(type = "tabs",

                       #### Main tab ####
                       tabPanel("Main",
                                fluidRow(
                                  column(1,
                                         materialSwitch(
                                           inputId = "errorbars",
                                           label = "Errorbars",
                                           value = FALSE,
                                           status = "primary")
                                  ),
                                  column(1,
                                         downloadButton(outputId = "downloadPlot",
                                                        label = "Save plot",
                                                        style='padding:6px; font-size:80%',
                                                        icon = icon("download"))
                                  ),
                                  column(4),
                                  column(2,
                                         sliderInput(inputId = "zoom",
                                                     label = "zoom",
                                                     min = 0.1, max = 25, value = 4, ticks = FALSE))
                                ),
                                column(6,
                                       plotlyOutput('curve') %>%
                                         withSpinner(color="#0dc5c1")),
                                column(6,
                                       plotlyOutput('peak') %>%
                                         withSpinner(color="#0dc5c1")),
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
                                                          icon = icon("download"))
                                         )
                                  )
                                ),
                                width = 9),
                       #### QC tab ####
                       tabPanel("QC",
                                fluidRow(
                                  h4("Recalibration check"),
                                  plotlyOutput('checkRecal') %>%
                                    withSpinner(color="#0dc5c1")),
                                fluidRow(
                                  column(2,
                                         selectInput("plateStat",
                                                     label = "Metric",
                                                     choices = c("Concentration",
                                                                 "Total Peak Intensity",
                                                                 "Normalization factor",
                                                                 "Recal-shift",
                                                                 "Selected-mz"),
                                                     multiple = FALSE,
                                                     selected = "Recal-shift")),
                                  column(2, checkboxInput("plateScale", label = "Log10 scale"))
                                ),
                                fluidRow(
                                  column(6,
                                         plotOutput("platemap")),
                                  column(6,
                                         uiOutput(outputId = "summaryText"))
                                )
                       ),
                       #### PCA tab ####
                       tabPanel("PCA",
                                h4("PCA of single spectra"),
                                plotlyOutput('pca') %>%
                                  withSpinner(color="#0dc5c1"),
                                fluidRow(
                                  column(2,
                                         actionButton(inputId = "doPca",
                                                      label = "Perform PCA",
                                                      icon = icon("chart-line"))
                                  ),
                                  column(2,
                                         sliderInput(inputId = "pcaAlpha",
                                                     label = "Log(L1-Penalty)",
                                                     min = -10,
                                                     max = 0,
                                                     value = -3,
                                                     step = 0.1)),
                                  column(2,
                                         sliderInput(inputId = "pcaBeta",
                                                     label = "Log(L2-Penalty)",
                                                     min = -10,
                                                     max = 0,
                                                     value = -3,
                                                     step = 0.1)),
                                  column(1,
                                         selectInput(inputId = "pcaX",
                                                     label = "x-axis",
                                                     choices = paste0("PC", 1:5),
                                                     multiple = FALSE,
                                                     selected = "PC1")
                                  ),
                                  column(1,
                                         selectInput(inputId = "pcaY",
                                                     label = "y-axis",
                                                     choices = paste0("PC", 1:5),
                                                     multiple = FALSE,
                                                     selected = "PC2")
                                  ),
                                  column(1,
                                         selectInput(inputId = "pcaEllipse",
                                                     label = "Ellipse size",
                                                     choices = c(0.5, 0.67, 0.95, 0.99),
                                                     multiple = FALSE,
                                                     selected = 0.67)),
                                  column(1,
                                         materialSwitch(
                                           inputId = "simpleLoadings",
                                           label = "Summarise loadings",
                                           value = FALSE,
                                           status = "primary")
                                  ),
                                  column(2,
                                         actionButton(inputId = "pca2peaksTable",
                                                      label = "Send to Peak Table",
                                                      icon = icon("share-from-square")))
                                ),
                                fluidRow(
                                  column(6,
                                         plotlyOutput('pcaLoading1') %>%
                                           withSpinner(color="#0dc5c1")),
                                  column(6,
                                         plotlyOutput('pcaLoading2') %>%
                                           withSpinner(color="#0dc5c1"))
                                )
                       ),
                       #### LASSO tab #####
                       tabPanel("LASSO",
                                h4("Feature importance using LASSO-model")  %>%
                                  helper(type = "markdown", content = "lasso", size = "l"),
                                fluidRow(
                                  column(6,
                                         plotlyOutput("glmTruePred") %>%
                                           withSpinner(color="#0dc5c1")),
                                  column(6,
                                         plotlyOutput("glmVi") %>%
                                           withSpinner(color="#0dc5c1"))
                                ),
                                fluidRow(
                                  column(2,
                                         actionButton(inputId = "doGLM",
                                                      label = "Fit model",
                                                      icon = icon("chart-line"))
                                  )
                                ),
                                fluidRow(
                                  column(2,
                                         sliderInput(inputId = "penalty",
                                                     label = "Log(L1-Penalty)",
                                                     min = -10,
                                                     max = 0,
                                                     value = -5,
                                                     step = 0.1)
                                  ),
                                  column(2,
                                         checkboxInput(inputId = "sigmoidModel",
                                                       label = "Sigmoid-fit",
                                                       value = FALSE)
                                  )
                                ),
                                fluidRow(
                                  column(2,
                                         actionButton(inputId = "resetPenalty",
                                                      label = "Reset penalty",
                                                      icon = icon("chart-line"))
                                  ),
                                  column(2,
                                         actionButton(inputId = "lasso2peaksTable",
                                                      label = "Send to Peak Table",
                                                      icon = icon("share-from-square"))
                                  )
                                )
                       ),
                       #### HClust tab ####
                       tabPanel("HClust",
                                h4("Hierarchical clustering"),
                                fluidRow(
                                  column(6,
                                         plotlyOutput("hclustPlot")),
                                  column(6,
                                         plotlyOutput("clustCurvesPlot"))
                                ),
                                fluidRow(
                                  column(2,
                                         actionButton(inputId = "doHC",
                                                      label = "Perform HC",
                                                      icon = icon("circle-nodes"))),
                                  column(2,
                                         sliderInput(inputId = "num_cluster",
                                                     label = "Number of cluster",
                                                     min = 2,
                                                     max = 25,
                                                     value = 4,
                                                     step = 1)),
                                  column(2,
                                         selectInput(inputId = "hcDist",
                                                     choices = c("Euclidean",
                                                                 "Manhattan",
                                                                 "correlation",
                                                                 "cosine"),
                                                     selected = "Euclidean",
                                                     label = "Distance metric",
                                                     multiple = FALSE)),
                                  column(2,
                                         selectInput(inputId = "hcMethod",
                                                     choices = c("complete",
                                                                 "ward.D2",
                                                                 "average"),
                                                     selected = "average",
                                                     label = "Cluster method",
                                                     multiple = FALSE)),
                                  column(2, actionButton(inputId = "hc2peaksTable",
                                                         label = "Send to Peak Table",
                                                         icon = icon("share-from-square")))
                                ),
                                fluidRow(
                                  column(6,
                                         plotlyOutput("optNumClust"))
                                )
                       ),
                       #### Manual tab ####
                       tabPanel("Manual", htmltools::includeMarkdown("manual.md")
                       )
    )
  )
  return(m)
}
