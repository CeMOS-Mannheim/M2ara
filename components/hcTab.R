hcTab <- function() {
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
                    createActionButton(inputId = "doHC",
                                       label = "Perform HC",
                                       icon = "circle-nodes")),
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
                                            "average",
                                            "median",
                                            "centroid",
                                            "single"),
                                selected = defaults$hcMethod,
                                label = "Cluster method",
                                multiple = FALSE)),
             column(2,
                    createActionButton(inputId = "hc2peaksTable",
                                       label = "Send to Peak Table",
                                       icon = "share-from-square")),
             column(2,
                    checkboxInput(inputId = "hcUseFitted",
                                  label = "use fitted",
                                  value = FALSE))
           ),
           fluidRow(
             column(6,
                    plotlyOutput("optNumClust"))
           )
  )
}
