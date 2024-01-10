lassoTab <- function() {
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
                    createActionButton(inputId = "doGLM",
                                       label = "Fit model",
                                       icon = "chart-line")
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
                                  value = defaults$corFilter,
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
                    createActionButton(inputId = "resetPenalty",
                                       label = "Reset penalty",
                                       icon = "chart-line")
             ),
             column(2,
                    createActionButton(inputId = "lasso2peaksTable",
                                       label = "Send to Peak Table",
                                       icon = "share-from-square")
             )
           )
  )
}
