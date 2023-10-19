#### Sever ####
server <- function(input, output) {

  #### load functions ####
  source("functions/preprocessFunction.R")
  source("functions/plotFunctions.R")
  source("functions/generatePCA.R")
  source("functions/fitGLM.R")
  source("functions/getVolumes.R")
  source("functions/hclust.R")
  source("functions/getVolumes.R")
  source("functions/generateSummaryText.R")
  source("functions/helpers.R")
  source("functions/doFitCurve.R")
  source("functions/infoStateMassageHandler.R")
  source("functions/fitCurveErrorHandler.R")

  #### variables ####
  observe_helpers(withMathJax = TRUE)
  info_state <- reactiveVal("inital")

  show_plot <- reactiveVal("FALSE")

  #### main #####
  appData <<- reactiveValues(selected_dir = NULL,
                             res = NULL, # MALDIcellassay object
                             spec_all = NULL, # all spectra
                             spec_idx = NULL, # indices of non-empty spectra
                             preprocessing = NULL, # preprocessing
                             stats_original = NULL, # original stats
                             stats = NULL,
                             pca = NULL,
                             model = NULL,
                             hc = NULL,
                             opt = NULL)

  vol <- tolower(getVolumes())
  names(vol) <- str_remove(vol, ":")

  defaults <- defaultsSettingsHandler(userSavedSettings = "settings.conf")

  # check if "dir" is set in defaults
  if (!is.null(defaults$dir)) {
    appData$selected_dir <- defaults$dir
    cat("Dir set from loaded default value.\n")
    info_state("dir_set")
  }

  #### choose dir ####
  shinyDirChoose(input,
                 "dir",
                 roots = vol,
                 allowDirCreate = FALSE,
                 defaultRoot = names(vol)[1])

  observeEvent(input$dir, {
    # check if folder was selected
    # prepare info massage
    appData$selected_dir <- parseDirPath(vol, input$dir)
    if (length(appData$selected_dir) > 0) {
      info_state("dir_set")
    }
  })

  #### Text massage logic ####
  observeEvent(info_state(), {
    output <- infoStateMassageHandler(info_state = info_state(),
                                      output = output)
  })

  #### load spectra ####
  # disable load button if no dir is set
  disable("load")
  # disable process button if no spectra are loaded
  disable("process")

  # enable buttons when state is reached
  observeEvent(info_state(), {
    if (info_state() == "dir_set") {
      enable("load")
    }
    if (info_state() == "loaded") {
      enable("process")
    }
  })

  observeEvent(input$load, {
    if (info_state() == "dir_set") {
      show_spinner()

      # check if all spectra names are numeric/concentrations
      # for later: if pos and neg ctrls are included
      # checkSpecNames needs to return indices of the numeric folders
      if (!checkSpecNames(appData$selected_dir)) {
        spec_raw <- loadSpectra(appData$selected_dir)
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
      appData$spec_all <- spec_raw

      # MAD would be faster but may fail in some circumstances...
      peaks <- detectPeaks(appData$spec_all,
                           SNR = input$SNR,
                           method = "SuperSmoother")
      appData$spec_idx <- vapply(peaks,
                                 function(x) {
                                   ifelse(length(mz(x)) > 0, TRUE, FALSE)
                                 },
                                 FUN.VALUE = TRUE)
      cat(sum(appData$spec_idx), "/", length(appData$spec_all),
          "spectra retained.\n")

      hide_spinner()
    }
  })

  #### process spectra ####
  observeEvent(input$process, {
    if (!info_state() %in% c("intial", "dir_set")) {
      show_spinner()

      cat("start processing...\n")
      spec_prc <- preprocess(spectra = appData$spec_all[appData$spec_idx],
                             sqrtTransform = input$sqrtTrans,
                             smooth = input$smooth,
                             rmBaseline = input$rmBl)

      res <- doFitCurve(appData = appData,
                        spec = spec_prc,
                        input = input)

      if(!fitCurveErrorHandler(res = res,
                               input = input,
                               info_state = info_state)) {
        return()
      }

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
        ungroup()

      # write everything needed into appData
      appData$res <- res
      appData$preprocessing <- data.frame(
        smooth = input$smooth,
        rmBl = input$rmBl,
        sqrtTrans = input$sqrtTrans,
        monoisotopicFilter = input$monoisotopicFilter)
      appData$stats_original <- stats # copy of original stats for updates
      appData$stats <- stats

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
    if (show_plot() == "TRUE") {
      tableData <- appData$stats %>%
        mutate_if(is.numeric, function(x) {
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
                 rownames = FALSE),
  selection = list(mode = "single",
                   selected = 1)
  )

  # on reprocess
  # not sure if this duplicate is needed
  observeEvent(input$process, {
    output$mzTable <- DT::renderDataTable({
      if (show_plot() == "TRUE") {
        appData$stats %>%
          mutate_if(is.numeric, function(x) {
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
                   rownames = FALSE),
    selection = list(mode = "single",
                     selected = 1)
    )
  })

  #### curve and peak plots ####
  output$curve <- renderPlotly({
    if (show_plot() == "TRUE") {
      p_curve <<- plotCurves(appData$res,
                             mzIdx = input$mzTable_rows_selected[1],
                             errorbars = input$errorbars) +
        labs(title = paste0("m/z = ",
                            round(
                              getMzFromMzIdx(appData$res,
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
    if (show_plot() == "TRUE") {
      p_peak <<- plotPeak(appData$res,
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
    if (show_plot() == "TRUE") {
      p <- checkRecalibration(appData$res,
                              idx = seq_along(getAvgSpectra(appData$res)))
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
    if (show_plot() == "TRUE") {
      plateMapPlot(appData,
                   stat = input$plateStat,
                   PCs = c(input$pcaX, input$pcaY),
                   penalty = input$penalty,
                   log10 = input$plateScale,
                   mz_idx = input$mzTable_rows_selected[1])
    }
  })

  # summary
  observeEvent(input$process, {
    output$summaryText <- renderUI({
      if (info_state() == "processed") {
        smooth <-  appData$preprocessing$smooth
        rmBl <- appData$preprocessing$rmBl
        sqrtTrans <- appData$preprocessing$sqrtTrans
        monoisotopicFilter <- appData$preprocessing$monoisotopicFilter

        text <- markdown(
          paste0(
            generateSummaryText(appData$res,
                                smooth = smooth,
                                rmBl = rmBl,
                                sqrtTrans = sqrtTrans,
                                monoisotopicFilter = monoisotopicFilter),
            collapse = "<br>"))

        text
      }
    })
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
    if (!is.null(appData$res)) {
      cat("pcaAlpha =", 10^input$pcaAlpha, "\n")
      cat("pcaBeta =", 10^input$pcaBeta, "\n")
      appData$pca <- generatePCA(appData$res,
                                 num_PC = 5,
                                 alpha = 10^input$pcaAlpha,
                                 beta = 10^input$pcaBeta,
                                 verbose = FALSE)
    }

  })

  observeEvent(input$doPca, {
    output$pca <- renderPlotly({
      if (!is.null(appData$pca)) {
        p <- pcaPlot(pca = appData$pca,
                     conc = factor(getConc(appData$res)),
                     x = input$pcaX,
                     y = input$pcaY,
                     ellipseLevel = as.numeric(input$pcaEllipse),
                     spots = getSpots(appData$res))

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
    if (show_plot() == "TRUE" & !is.null(appData$pca)) {
      p <- loadingsPlot(appData$pca,
                        pc = input$pcaX,
                        simple = input$simpleLoadings) +
        labs(title = paste("Feature importance for", input$pcaX))
      if (input$simpleLoadings) {
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
    if (show_plot() == "TRUE" & !is.null(appData$pca)) {
      p <- loadingsPlot(appData$pca,
                        pc = input$pcaY,
                        simple = input$simpleLoadings) +
        labs(title = paste("Feature importance for", input$pcaY))
      if (input$simpleLoadings) {
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
    if (show_plot() == "TRUE" & !is.null(appData$pca)) {
      loadings <- extractLoadings(appData$pca,
                                  input$pcaX,
                                  input$pcaY)

      appData$stats <- appData$stats_original %>%
        left_join(loadings, by = join_by(mzIdx))
    }
  })

  #### LASSO tab #####
  # inital dummy text output
  output$mixture <- renderText({
    paste("Mixture = 1")
  })
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
    if (info_state() == "processed") {

      cat("starting model fit...\n")
      show_spinner()
      appData$model <- fitGLM(appData$res,
                              sigmoid = input$sigmoidModel,
                              elasticNet = input$elasticNet,
                              corFilter = input$corFilter)
      updateSliderInput(inputId = "penalty",
                        value = log10(appData$model$penalty))
      if (input$elasticNet) {
        output$mixture <- renderText({
          paste("Mixture =", round(appData$model$mixture, 2))
        })
      } else {
        output$mixture <- renderText({
          paste("Mixture = 1")
        })
      }

      cat("model fitted.\n")
      hide_spinner()

      output$glmTruePred <- renderPlotly({
        p <- glmRegPlot(model = appData$model, penalty = input$penalty)

        return(ggplotly(p))
      })

      observeEvent(input$penalty, {
        output$glmVi <- renderPlotly({

          p <- viPlot(getVi(appData$model, input$penalty))

          return(ggplotly(p))
        })
      })
    }
  })

  observeEvent(input$resetPenalty, {
    if (!is.null(appData$model)) {
      updateSliderInput(inputId = "penalty",
                        value = log10(appData$model$penalty))
    } else {
      cat("appData$model was NULL\n")
    }
  })

  observeEvent(input$lasso2peaksTable, {
    if (!is.null(appData$model)) {

      vi <- getVi(appData$model, penalty = input$penalty) %>%
        mutate(Variable = readr::parse_number(Variable)) %>%
        mutate(`Lasso importance` = ifelse(Sign == "POS",
                                           Importance,
                                           -Importance)) %>%
        mutate(mz = as.numeric(Variable),
               `Lasso importance` = round(`Lasso importance`, digits = 4)) %>%
        select(mz, `Lasso importance`) %>%
        mutate(mzIdx = match.closest(x = mz,
                                     table = getAllMz(appData$res),
                                     tolerance = 0.1)) %>%
        filter(!`Lasso importance` == 0) %>%
        select(-mz)

      appData$stats <- appData$stats_original %>%

        left_join(vi, by = join_by(mzIdx))
      cat("Updated peak table with lasso data.\n")
    }
  })

  #### HClust tab #####
  observeEvent(input$doHC, {
    output$hclustPlot <- renderPlotly({
      if (show_plot() == "TRUE") {
        show_spinner()
        appData$hc <- doHClust(appData$res,
                               cut = input$num_cluster,
                               dist = input$hcDist,
                               clustMethod = input$hcMethod)
        p <- plotDendro(appData$hc$dend)
        hide_spinner()
        return(ggplotly(p))
      }
    })

    output$clustCurvesPlot <- renderPlotly({
      if (show_plot() == "TRUE" & !is.null(appData$hc)) {
        show_spinner()
        p <- plotClusterCurves(dend = appData$hc$dend,
                               tintmat = appData$hc$tintmat)
        hide_spinner()
        return(ggplotly(p))
      }
    })

    output$optNumClust <- renderPlotly({
      if (show_plot() == "TRUE" & !is.null(appData$hc)) {
        show_spinner()
        p <- optimalNumClustersPlot(appData$hc$opt,
                                    sel_k = input$num_cluster)
        hide_spinner()
        return(ggplotly(p))
      }
    })
  })

  observeEvent(input$hc2peaksTable, {
    if (show_plot() == "TRUE" & !is.null(appData$hc)) {
      clusters <- extractClusters(appData$hc$dend) %>%
        mutate(mzIdx = match.closest(x = mz,
                                     table = getAllMz(appData$res),
                                     tolerance = 0.1)) %>%
        select(-mz)

      appData$stats <- appData$stats_original %>%
        left_join(clusters, by = join_by(mzIdx))
      cat("Updated peak table with HC data.\n")
    }
  })

  #### save settings ####
  observeEvent(input$saveSettings, {

    inputList <- reactiveValuesToList(input)

    # filter inputs
    classes <- vapply(inputList, function(x) class(x)[1], character(1))
    sel_class <- "mzTable|shiny"
    sel_name <-  "mzTable|shiny|plotly|dir"

    fil_inputList <- inputList[!grepl(sel_class, classes)]
    fil_inputList <- fil_inputList[!grepl(sel_name, names(fil_inputList))]

    dir <- as.character(parseDirPath(vol, input$dir))

    # add dir value
    if (info_state() == "dir_set" & length(dir) > 0) {

      fil_inputList$dir <- as.character(parseDirPath(vol, input$dir))
    }


    write.csv(fil_inputList, file = "settings.conf", row.names = FALSE)
    cat("Inputs written.\n")
  })

  #### download handler ####
  output$downloadPlot <- downloadHandler(
    filename = function()  {
      if (show_plot() == "TRUE") {
        paste0(basename(getDirectory(appData$res)),
               "_mz", round(getMzFromMzIdx(appData$res,
                                           input$mzTable_rows_selected[1]), 2),
               ".png")
      }
    },
    content = function(file) {
      if (show_plot() == "TRUE") {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        p_main <- ggarrange(p_curve, p_peak)
        ggsave(file,
               plot = p_main,
               device = device,
               scale = 1.8,
               bg = "white",
               dpi = 600,
               width = 183,
               height = 122,
               units = "mm")
      } else {
        warning("Nothing to download. Load and process data.")
      }
    })

  output$downloadTable <- downloadHandler(
    filename = function()  {
      if (show_plot() == "TRUE") {
        paste0(basename(getDirectory(appData$res)),
               "_peakTable",
               ".csv")
      }
    },
    content = function(file) {
      if (show_plot() == "TRUE") {
        write_excel_csv(file = file, x = appData$stats)
      } else {
        warning("Nothing to download. Load and process data.")
      }
    })
}
