clustTab <- function() {
  #### clustering tab ####
  tabPanel("Cluster",
           h4("Curve clustering"),
           fluidRow(
             column(6,
                    plotlyOutput("hclustPlot")),
             column(6,
                    plotlyOutput("clustCurvesPlot"))
           ),
           fluidRow(
             column(2,
                    createActionButton(inputId = "doHC",
                                       label = "Perform clustering",
                                       icon = "circle-nodes")),
             column(2,
                    sliderInput(inputId = "num_cluster",
                                label = "Number of cluster",
                                min = 2,
                                max = 15,
                                value = defaults$num_cluster,
                                step = 1)),
             column(2,
                    createActionButton(inputId = "hc2peaksTable",
                                       label = "Send to Peak Table",
                                       icon = "share-from-square"))
           ),
           fluidRow(
             column(12,
                    plotlyOutput("optNumClust"))
           )
  )
}
