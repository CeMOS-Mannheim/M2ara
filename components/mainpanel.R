appMainPanel <- function(defaults) {
  m <- mainPanel(
    shiny::tabsetPanel(type = "tabs",

                       #### Main tab ####
                       tabPanel("Main",
                                fluidRow(
                                  column(1,
                                         materialSwitch(
                                           inputId = "errorbars",
                                           label = "Errorbars",
                                           value = defaults$errorbars,
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
                                                                 "Selected-mz",
                                                                 "PC-x",
                                                                 "PC-y",
                                                                 "LASSO-error"),
                                                     multiple = FALSE,
                                                     selected = defaults$plateStat)),
                                  column(2, checkboxInput("plateScale",
                                                          label = "Log10 scale",
                                                          value = defaults$plateScale))
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
                                  ),
                                  column(2,
                                         checkboxInput(inputId = "sigmoidModel",
                                                       label = "Sigmoid-fit",
                                                       value = defaults$sigmoidModel,
                                                       width = "60%")
                                  ),
                                  column(2,
                                         checkboxInput(inputId = "elasticNet",
                                                       label = "Elastic net",
                                                       value = defaults$elasticNet,
                                                       width = "60%")
                                  ),
                                  column(2,
                                         checkboxInput(inputId = "corFilter",
                                                       label = "Corr. filter",
                                                       value = defaults$corrFilter,
                                                       width = "60%")
                                  )
                                ),
                                fluidRow(
                                  column(2,
                                         fluidRow(
                                           sliderInput(inputId = "penalty",
                                                       label = "Log(Penalty)",
                                                       min = -10,
                                                       max = 0,
                                                       value = defaults$penalty,
                                                       step = 0.1)
                                         ),
                                         fluidRow(
                                           textOutput("mixture")
                                         )
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
                                                     value = defaults$num_cluster,
                                                     step = 1)),
                                  column(2,
                                         selectInput(inputId = "hcDist",
                                                     choices = c("Euclidean",
                                                                 "Manhattan",
                                                                 "correlation",
                                                                 "cosine"),
                                                     selected = defaults$hcDist,
                                                     label = "Distance metric",
                                                     multiple = FALSE)),
                                  column(2,
                                         selectInput(inputId = "hcMethod",
                                                     choices = c("complete",
                                                                 "ward.D2",
                                                                 "average"),
                                                     selected = defaults$hcMethod,
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
                       #### Settings ####
                       tabPanel("Settings",
                                fluidRow(
                                  column(2,
                                         actionButton("saveSettings",
                                                      style='padding:6px; font-size:80%',
                                                      label = "Save settings",
                                                      icon = icon("floppy-disk"))
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
