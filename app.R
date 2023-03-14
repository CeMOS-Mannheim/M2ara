#### tidy up ####
# clear work space
rm(list = ls())
gc()

#### package check on start up ####
# If a package is installed, it will be loaded. If any
# are not, the missing package(s) will be installed
# from CRAN and then loaded.

## First specify the packages of interest
packages = c("tidyverse", "tidymodels", "shiny",  "vip", "shinyFiles",
             "MALDIquant", "MALDIquantForeign", "DT", "plotly",
             "shinycssloaders", "shinyhelper", "knitr", "shinybusy",
             "shinythemes", "shinyWidgets", "devtools", "ggpubr", "dendextend",
             "glmnet", "proxy", "sparsepca", "platetools", "ggdendro", "zoo",
             "fs", "cluster")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# special case for packages not on CRAN
if (!require("MALDIcellassay", character.only = TRUE)) {
  devtools::install_github("CeMOS-Mannheim/MALDIcellassay", dependencies = TRUE)
  library(MALDIcellassay, character.only = TRUE)
}

knit("manual.Rmd", quiet = TRUE)

#### load UI functions
source("sidebar.R")
source("mainpanel.R")

#### UI ####
ui <- fluidPage(
  title = "Peak Explorer",
  lang = "en",
  theme = shinytheme("flatly"),

  # shiny sidebar layout
  sidebarLayout(
    #### Sidebar ####
    sidebarPanel = appSidebar(),

    #### Main panel ####
    mainPanel = appMainPanel()
  )
)

#### Sever ####
server <- function(input, output) {

  #### load functions ####
  source("preprocessFunction.R")
  source("plotFunctions.R")
  source("generatePCA.R")
  source("fitGLM.R")
  source("getVolumes.R")
  source("hclust.R")
  source("getVolumes.R")
  source("generateSummaryText.R")
  source("helpers.R")

  #### variables ####
  p_main <- ggplot()
  observe_helpers(withMathJax = TRUE)
  res <- reactiveVal()
  stats <- reactiveVal()
  pspec <- reactiveVal()
  info_state <- reactiveVal("inital")

  dir_set <- reactiveVal("FALSE")
  show_plot <- reactiveVal("FALSE")
  loaded <- reactiveVal("FALSE")

  #### main #####
  RV <<- reactiveValues(res = NULL,
                        stats_original = NULL,
                        stats = NULL,
                        specIdx = 1,
                        maxSpecIdx = 1,
                        pspec = NULL,
                        pca = NULL,
                        model = NULL,
                        hc = NULL,
                        opt = NULL)

  vol <- tolower(getVolumes())
  names(vol) <- str_remove(vol, ":")

  #### choose dir ####
  shinyDirChoose(input,
                 'dir',
                 roots = vol,
                 allowDirCreate = FALSE,
                 defaultRoot = names(vol)[1])

  observeEvent(input$dir, {
    # check if folder was selected
    # prepare info massage
    selected_dir <<- parseDirPath(vol, input$dir)
    if(length(selected_dir)>0) {
      info_state("dir_set")
    }
  })

  #### Text massage logic ####
  observeEvent(info_state(), {
    if(info_state() == "inital") {
      output$info1 <- renderText("Please select\na folder.")
      output$info2 <- renderText("")
      output$info3 <- renderText("")
    }
    if(info_state() == "dir_set") {
      output$info1 <- renderText("Selected:")
      output$info2 <- renderText(selected_dir)
      output$info3 <- renderText("Press load button.")
    }
    if(info_state() == "loaded") {
      output$info1 <- renderText("Loaded:")
      output$info2 <- renderText(selected_dir)
      output$info3 <- renderText("Press process button.")
    }
    if(info_state() == "processed") {
      output$info1 <- renderText(("Dataset:"))
      output$info2 <- renderText(selected_dir)
      output$info3 <- renderText("If you change settings, press process again to apply them.")
    }
  })

  #### load spectra ####
  observeEvent(input$load, {
    if(info_state() == "dir_set") {
      show_spinner()

      # check if all spectra names are numeric/concentrations
      # for later: if pos and neg ctrls are included
      # checkSpecNames needs to return indices of the numeric folders
      if(!checkSpecNames(selected_dir)) {
        spec_raw <- loadSpectra(selected_dir)
        info_state("loaded")
      } else {
        warning("Found folder names that could not be converted to numeric.
                All folders/spectra need to have concentrations as names.\n")
        hide_spinner()
        return()
      }

      # make sure that the concentrations are in acending order
      conc <- as.numeric(names(spec_raw))
      spec_raw <- spec_raw[order(conc)]

      cat("check for empty spectra...\n")
      conc <- names(spec_raw)

      # MAD would be faster but may fail in some circumstances...
      peaks <- detectPeaks(spec_raw, SNR = input$SNR, method = "SuperSmoother")
      filPeaks <- vapply(peaks,
                         function(x) {
                           ifelse(length(mz(x)) > 0, TRUE, FALSE)
                         },
                         FUN.VALUE = TRUE)
      cat(sum(filPeaks), "/", length(spec_raw), "spectra retained.\n")
      peaks_fil <- peaks[filPeaks]

      spec_raw <<- spec_raw[filPeaks]
      names(spec_raw) <- conc[filPeaks]
      hide_spinner()
    }
  })

  #### process spectra ####
  observeEvent(input$process, {
    if(info_state() == "loaded" | info_state() == "processed") {
      show_spinner()

      cat("start processing...\n")
      spec_prc <- preprocess(spectra = spec_raw,
                             sqrtTransform = input$sqrtTrans,
                             smooth = input$smooth,
                             rmBaseline = input$rmBl)

      res <- suppressMessages(
        suppressWarnings(
          fitCurve(spec = spec_prc,
                   dir = selected_dir,
                   conc = NA,
                   unit = input$concUnits,
                   normMeth = input$normMeth,
                   SinglePointRecal = input$SinglePointRecal,
                   normMz = input$normMz,
                   normTol = input$normTol,
                   binTol = input$binTol * 1e-6, # convert to ppm
                   alignTol = input$alignTol,
                   SNR = input$SNR,
                   varFilterMethod = input$VarFilterMethod,
                   saveIntensityMatrix = FALSE,
                   plot = FALSE)
        )
      )

      cat("processing done\n")

      stats <-   getPeakStatistics(res, FALSE) %>%
        mutate(mz = as.numeric(mz)) %>%
        group_by(mz, mzIdx) %>%
        summarise(
          pIC50 = first(pIC50),
          R2 = first(R2),
          min = min(mean),
          max = max(mean),
          log2FC = log2(first(fc_window)),
          `abs. log2FC` = abs(log2FC)
        ) %>%
        # left_join(getFittingParameters(res, summarise = TRUE), by = join_by(mz)) %>%
        # select(-npar) %>%
        ungroup()

      RV <<- reactiveValues(res = res,
                            stats_original = stats, # copy of original stats for updates
                            stats = stats,
                            specIdx = 1,
                            maxSpecIdx = length(getAvgPeaks(res)),
                            pspec = generateSpecPlots(res),
                            pca = NULL,
                            model = NULL)

      info_state("processed")
      show_plot("TRUE")
      updateActionButton(inputId = "process", label = "Re-process")
      hide_spinner()
    }
  })

  #### Peak table ####
  # first initialization
  output$mzTable <- DT::renderDataTable({
    # check if data is already prepared and if not show dummy table
    if(show_plot() == "TRUE") {
      tableData <- RV$stats %>%
        mutate_if(is.numeric, function(x)
        {
          round(x, 3)
        })

    } else {
      tableData <- tibble(mz = c("load", rep("", 9)),
                          mzIdx = c("data", rep("", 9)),
                          pIC50 = c("to display", rep("", 9)),
                          R2 = c("peak", rep("", 9)),
                          log2FC = c("table", rep("", 9)))
    }

    tableData

  },
  server = TRUE,
  filter = "bottom",
  options = list(searching = TRUE,
                 lengthChange = FALSE,
                 paging = TRUE,
                 pageLength = 20,
                 autoWidth = TRUE,
                 rownames= FALSE),
  selection = list(mode = 'single',
                   selected = 1)
  )

  # on reprocess
  # not sure if this duplicate is needed
  observeEvent(input$process, {
    output$mzTable <- DT::renderDataTable({
      if(show_plot() == "TRUE") {
        RV$stats %>%
          mutate_if(is.numeric, function(x)
          {
            round(x, 2)
          })
      }
    },
    server = TRUE,
    filter = "bottom",
    options = list(searching = TRUE,
                   lengthChange = FALSE,
                   paging = TRUE,
                   pageLength = 20,
                   autoWidth = TRUE,
                   rownames= FALSE),
    selection = list(mode = 'single',
                     selected = 1)
    )
  })

  #### curve and peak plots ####
  output$curve <- renderPlotly({
    if(show_plot() == "TRUE") {
      p_curve <<- plotCurves(RV$res,
                             mzIdx = input$mzTable_rows_selected[1],
                             errorbars = input$errorbars) +
        labs(title = paste0("m/z = ",
                            round(
                              getMzFromMzIdx(RV$res,
                                             input$mzTable_rows_selected[1]),
                              2)))

      ggplotly(p_curve)
    } else {
      # dummy plot
      p_curve <- ggplot(tibble(label = "Load data\nto display plot",
                               x = 1,
                               y = 1),
                        aes(x = x, y = y, label = label)) +
        geom_text(size = 5) +
        theme_light(base_size = 14)

      ggplotly(p_curve)
    }

  })

  output$peak <- renderPlotly({
    if(show_plot() == "TRUE") {
      p_peak <<- plotPeak(RV$res,
                          mzIdx = input$mzTable_rows_selected[1],
                          tol = input$zoom) +
        labs(title = NULL)
      ggplotly(p_peak)
    } else {
      # dummy plot
      p_peak <- ggplot(tibble(label = "Load data\nto display plot",
                              x = 1,
                              y = 1),
                       aes(x = x, y = y, label = label)) +
        geom_text(size = 5) +
        theme_light(base_size = 14)
      ggplotly(p_peak)
    }

  })

  #### QC tab ####
  #  recal check
  output$checkRecal <- renderPlotly({
    if(show_plot() == "TRUE") {
      p <- checkRecalibration(RV$res, idx = 1:length(getAvgSpectra(RV$res)))
      ggplotly(p)
    } else {
      p <- ggplot(tibble(label = "Load data\nto display plot",
                         x = 1,
                         y = 1),
                  aes(x = x, y = y, label = label)) +
        geom_text(size = 5) +
        theme_light(base_size = 14)
      ggplotly(p)
    }

  })

  # platemap
  output$platemap <- renderPlot({
    if(show_plot() == "TRUE") {
      plateMapPlot(RV$res,
                   stat = input$plateStat,
                   log10 = input$plateScale,
                   mz_idx = input$mzTable_rows_selected[1])
    }
  })

  # summary
  output$summaryText <-  output$myText <- renderUI({
    text <- paste0(generateSummaryText(RV$res), collapse = "<br>")
    HTML(text)
  })

  #### PCA tab ####
  # default plot for PCA
  output$pca <- renderPlotly({
    p <- ggplot(tibble(label = "Load data\nto display plot",
                       x = 1,
                       y = 1),
                aes(x = x, y = y, label = label)) +
      geom_text(size = 5) +
      theme_light(base_size = 14)

    ggplotly(p)
  })

  observeEvent(input$doPca, {
    if(!is.null(RV$res)) {
      cat("pcaAlpha =", 10^input$pcaAlpha, "\n")
      cat("pcaBeta =", 10^input$pcaBeta, "\n")
      RV$pca <- generatePCA(RV$res,
                            num_PC = 5,
                            alpha = 10^input$pcaAlpha,
                            beta = 10^input$pcaBeta,
                            verbose = FALSE)
    }

  })

  observeEvent(input$doPca, {
    output$pca <- renderPlotly({
      if(!is.null(RV$pca)) {
        p <- pcaPlot(pca = RV$pca,
                     conc = factor(getConc(RV$res)),
                     x = input$pcaX,
                     y = input$pcaY,
                     ellipseLevel = as.numeric(input$pcaEllipse),
                     spots = getSpots(RV$res))

      } else {
        p <- ggplot(tibble(label = "Load data\nto display plot",
                           x = 1,
                           y = 1),
                    aes(x = x, y = y, label = label)) +
          geom_text(size = 5) +
          theme_light(base_size = 14)
      }
      ggplotly(p)
    })
  })

  output$pcaLoading1 <- renderPlotly({
    if(show_plot() == "TRUE" & !is.null(RV$pca)) {
      p <- loadingsPlot(RV$pca, pc = input$pcaX, simple = input$simpleLoadings) +
        labs(title = paste("Feature importance for", input$pcaX))
      if(input$simpleLoadings) {
        p <- p +
          labs(title = paste("Top-Features for", input$pcaX))
      }

      ggplotly(p)

    } else {
      p <- ggplot(tibble(label = "Load data\nto display plot",
                         x = 1,
                         y = 1),
                  aes(x = x, y = y, label = label)) +
        geom_text(size = 5) +
        theme_light(base_size = 14)
      ggplotly(p)
    }
  })

  output$pcaLoading2 <- renderPlotly({
    if(show_plot() == "TRUE" & !is.null(RV$pca)) {
      p <- loadingsPlot(RV$pca, pc = input$pcaY, simple = input$simpleLoadings) +
        labs(title = paste("Feature importance for", input$pcaY))
      if(input$simpleLoadings) {
        p <- p + labs(title = paste("Top-Features for", input$pcaY))
      }

      ggplotly(p)

    } else {
      p <- ggplot(tibble(label = "Load data\nto display plot",
                         x = 1,
                         y = 1),
                  aes(x = x, y = y, label = label)) +
        geom_text(size = 5) +
        theme_light(base_size = 14)
      ggplotly(p)
    }
  })

  observeEvent(input$pca2peaksTable, {
    if(show_plot() == "TRUE" & !is.null(RV$pca)) {
      loadings <- extractLoadings(RV$pca, input$pcaX, input$pcaY)

      RV$stats <- RV$stats_original %>%
        left_join(loadings, by = join_by(mzIdx))
    }
  })

  #### LASSO tab #####

  output$glmTruePred <- renderPlotly({
    # dummy plot
    p <- tibble(label = "Fit model\nto display plot",
                .pred = 1,
                truth = 1) %>%
      ggplot(aes(x = .pred, y = truth, label = label)) +
      geom_text() +
      coord_obs_pred() +
      labs(x = "Log-predicted conc.",
           y = "Log-true conc.") +
      theme_minimal(base_size = 14)

    return(ggplotly(p))
  })

  output$glmVi <- renderPlotly({
    # dummy plot
    p <- tibble(label = "Fit model\nto display plot",
                Variable = 1,
                imp = 1) %>%
      ggplot(aes(x = Variable, y = imp, label = label)) +
      coord_flip() +
      geom_text() +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") +
      labs(x = "m/z",
           y = "Variable importance")
    return(ggplotly(p))
  })

  observeEvent(input$doGLM, {
    if(info_state() == "processed") {

      cat("starting model fit...\n")
      show_spinner()
      RV$model <- fitGLM(RV$res, sigmoid = input$sigmoidModel)
      updateSliderInput(inputId = "penalty", value = log10(RV$model$penalty))
      cat("model fitted.\n")
      hide_spinner()

      output$glmTruePred <- renderPlotly({
        p <- glmRegPlot(model = RV$model, penalty = input$penalty)

        return(ggplotly(p))
      })

      observeEvent(input$penalty, {
        output$glmVi <- renderPlotly({

          p <- viPlot(getVi(RV$model, input$penalty))

          return(ggplotly(p))
        })
      })
    }
  })

  observeEvent(input$resetPenalty, {
    if(!is.null(RV$model)) {
      updateSliderInput(inputId = "penalty", value = log10(RV$model$penalty))
    } else {
      cat("RV$model was NULL\n")
    }
  })

  observeEvent(input$lasso2peaksTable, {
    if(!is.null(RV$model)) {

      View(getVi(RV$model, penalty = input$penalty))

      vi <- getVi(RV$model, penalty = input$penalty) %>%
        mutate(Variable = readr::parse_number(Variable)) %>%
        mutate(`Lasso importance` = ifelse(Sign == "POS", Importance, -Importance)) %>%
        mutate(mz = as.numeric(Variable),
               `Lasso importance` = round(`Lasso importance`, digits = 4)) %>%
        select(mz, `Lasso importance`) %>%
        mutate(mzIdx = match.closest(x = mz, table = getAllMz(RV$res), tolerance = 0.1)) %>%
        filter(!`Lasso importance` == 0) %>%
        select(-mz)

      RV$stats <- RV$stats_original %>%

        left_join(vi, by = join_by(mzIdx))
      cat("Updated peak table with lasso data.\n")
    }
  })

  #### HClust tab #####
  observeEvent(input$doHC, {
    output$hclustPlot <- renderPlotly({
      if(show_plot() == "TRUE") {
        show_spinner()
        RV$hc <- doHClust(RV$res,
                          cut = input$num_cluster,
                          dist = input$hcDist,
                          clustMethod = input$hcMethod)
        p <- plotDendro(RV$hc$dend)
        hide_spinner()
        return(ggplotly(p))
      }
    })

    output$clustCurvesPlot <- renderPlotly({
      if(show_plot() == "TRUE" & !is.null(RV$hc)) {
        show_spinner()
        p <- plotClusterCurves(dend = RV$hc$dend,
                               tintmat = RV$hc$tintmat)
        hide_spinner()
        return(ggplotly(p))
      }
    })

    output$optNumClust <- renderPlotly({
      if(show_plot() == "TRUE" & !is.null(RV$hc)) {
        show_spinner()
        p <- optimalNumClustersPlot(RV$hc$opt,
                                    sel_k = input$num_cluster)
        hide_spinner()
        return(ggplotly(p))
      }
    })
  })

  observeEvent(input$hc2peaksTable, {
    if(show_plot() == "TRUE" & !is.null(RV$hc)) {
      clusters <- extractClusters(RV$hc$dend)
      RV$stats <- RV$stats_original %>%
        left_join(clusters, by = join_by(mz))
      cat("Updated peak table with HC data.\n")
    }
  })

  #### download handler ####
  output$downloadPlot = downloadHandler(
    filename = function()  {
      if(show_plot() == "TRUE") {
        paste0(basename(getDirectory(RV$res)),
               "_mz", round(getMzFromMzIdx(RV$res, input$mzTable_rows_selected[1]), 2),
               ".png")
      }
    },
    content = function(file) {
      if(show_plot() == "TRUE") {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        p_main <- ggarrange(p_curve, p_peak)
        ggsave(file, plot = p_main, device = device, scale = 1.8, bg = "white")
      } else {
        warning("Nothing to download. Load and process data.")
      }
    })

  output$downloadTable = downloadHandler(
    filename = function()  {
      if(show_plot() == "TRUE") {
        paste0(basename(getDirectory(RV$res)),
               "_peakTable",
               ".csv")
      }
    },
    content = function(file) {
      if(show_plot() == "TRUE") {
        write_excel_csv(file = file, x = RV$stats)
      } else {
        warning("Nothing to download. Load and process data.")
      }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
