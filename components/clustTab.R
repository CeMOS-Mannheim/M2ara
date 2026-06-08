clustTab <- function(defaults) {
  #### clustering tab ####
  tabPanel("Cluster",
           h4("Curve clustering"),
           fluidRow(
             column(6,
                    plotlyOutput("clustPlot")),
             column(6,
                    plotlyOutput("clustCurvesPlot"))
           ),
           fluidRow(
             column(2,
                    createActionButton(inputId = "doClust",
                                       label = "Perform clustering",
                                       icon = "circle-nodes")),
             column(2,
                    sliderInput(inputId = "num_cluster",
                                label = "Number of cluster",
                                min = 2,
                                max = 10,
                                value = defaults$num_cluster,
                                step = 1)),
             column(2,
                    selectInput(inputId = "centroid_method",
                                label = "Centroid",
                                choices = c("mean (fast)" = "mean",
                                            "pam (accurate, slow)" = "pam"),
                                selected = defaults$centroid_method)),
             column(2,
                    createActionButton(inputId = "clust2peaksTable",
                                       label = "Send to Peak Table",
                                       icon = "share-from-square"))
           ),
           fluidRow(
             column(12,
                    plotlyOutput("optNumClust"))
           )
  )
}
