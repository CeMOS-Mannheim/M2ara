clustTab <- function(defaults) {
  #### clustering tab ####
  tabPanel("Cluster",
           h4("Curve clustering"),
           fluidRow(
             column(6,
                    plotOutput("clustPlot", height = "360px")),
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
           # status and cancel while the clustering runs in the background
           conditionalPanel(
             condition = "output.clustRunning",
             fluidRow(
               column(12,
                      uiOutput("clustStatus"),
                      actionButton("cancelClust", "Cancel clustering",
                                   icon = icon("stop"),
                                   class = "btn-sm",
                                   style = "margin-top: 4px;"))
             )
           ),
           fluidRow(
             column(12,
                    plotlyOutput("optNumClust"))
           )
  )
}
