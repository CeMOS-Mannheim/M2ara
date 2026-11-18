# Server ####
appServer <- function(input, output, session, defaults = NULL) {
  ## variables ####
  observe_helpers(withMathJax = TRUE,
                  help_dir = system.file("helpfiles", package = "M2ara"))

  ## main ####
  appData <- emptyAppDataObject()
  # background processing job and its cache key
  process_job <- reactiveVal(NULL)
  process_cache_key <- reactiveVal(NULL)

  appData <- selectDir(appData, input, output, defaults = defaults)

  observeEvent(input$preproc_settings, {
    appData <- setPreprocessSettings(input, appData)
  })

  ### Text massage logic ####
  observeEvent(list(appData$info_state, appData$selected_dir), {
    output <- infoStateMassageHandler(info_state = appData$info_state,
                                      output = output,
                                      selected_dir = appData$selected_dir)
  })

  ### load spectra ####
  handleButtonStatus(appData)

  ### download buttons (disabled until data is ready) ####
  shinyjs::disable("downloadPlot")
  shinyjs::disable("exportRun")
  
  

  observeEvent(appData$show_plot, {
    if (appData$show_plot) {
      shinyjs::enable("downloadPlot")
      shinyjs::enable("exportRun")
      
      
    } else {
      shinyjs::disable("downloadPlot")
      shinyjs::disable("exportRun")
      
      
    }
  })

  observeEvent(input$load, {
    appData <- loadSpectraData(input, appData, mapping = appData$mapping)
  })

  ### process spectra ####
  observeEvent(input$process, {
    # a second job while one is running would orphan the first one
    if (!is.null(process_job())) {
      showNotification("Processing is already running.", type = "warning", duration = 4)
      return()
    }

    # guard against missing/empty numeric inputs
    req(input$SNR, input$normMz, input$normTol, input$alignTol, input$binTol, input$halfWindowSize,
        cancelOutput = FALSE)

    # change concentrations according to unit
    appData <- setConcentrationUnit(appData, input)

    if (!appData$info_state %in% c("initial", "dir_set")) {
      show_spinner()
      showNotification("Processing started. Please wait.",
                       duration = 5,
                       type = "default")
      message(MALDIcellassay:::timeNow(), " start processing...\n")
      settings <- processingSettings(input)
      cache_key <- processCacheKey(appData$selected_dir, settings,
                                   appData$preprocessing, appData$centroided)
      process_cache_key(cache_key)

      res <- processCacheGet(cache_key)
      if (!is.null(res)) {
        message(MALDIcellassay:::timeNow(), " results loaded from cache\n")
        showNotification("Results loaded from the cache.",
                         duration = 5, type = "message")
        appData <- storeResults(appData, res = res, input,
                                stats = getStatistics(res))
        updateActionButton(inputId = "process", label = "Re-process")
        hide_spinner()
        return()
      }

      # no cached result: run the pipeline in a background process, the poll
      # observer below reports progress and collects the result
      job <- tryCatch(
        processRunAsync(
          spec_all = appData$spec_all,
          spec_idx = appData$spec_idx,
          centroided = appData$centroided,
          preprocessing = appData$preprocessing,
          settings = settings,
          dir = appData$selected_dir
        ),
        error = function(e) {
          message("could not start the processing job: ", conditionMessage(e))
          NULL
        }
      )

      if (is.null(job)) {
        appData$info_state <- "fitErrorOther"
        hide_spinner()
        showNotification("Processing could not be started.",
                         type = "error", duration = 10)
        return()
      }

      process_job(job)
    }
  })

  ### processing job: progress, result and cancel ####
  output$processRunning <- reactive({ !is.null(process_job()) })
  outputOptions(output, "processRunning", suspendWhenHidden = FALSE)

  output$processStatus <- renderText({
    job <- process_job()
    if (is.null(job)) "" else processStageLabel(processJobProgress(job))
  })

  observe({
    job <- process_job()
    if (is.null(job)) return()

    if (!processJobDone(job)) {
      invalidateLater(500)
      return()
    }

    out <- processJobResult(job)
    processJobCleanup(job)
    process_job(NULL)
    hide_spinner()

    if (is.null(out)) {
      appData$info_state <- "fitErrorOther"
      showNotification("Processing failed. Please check the settings.",
                       type = "error", duration = 10)
      return()
    }

    if (!isTRUE(out$ok)) {
      appData$info_state <- switch(out$error,
                                   recal = "fitErrorRecal",
                                   norm = "fitErrorNorm",
                                   "fitErrorOther")
      return()
    }

    processCachePut(process_cache_key(), out$res)
    appData <- storeResults(appData, res = out$res, input,
                            stats = getStatistics(out$res))
    updateActionButton(inputId = "process", label = "Re-process")
    showNotification("Processing finished.", type = "message", duration = 3)
  })

  observeEvent(input$cancelProcess, {
    job <- process_job()
    if (is.null(job)) return()

    processJobCancel(job)
    processJobCleanup(job)
    process_job(NULL)
    hide_spinner()
    showNotification("Processing cancelled.", type = "warning", duration = 5)
  })

  ### Peak table ####

  observeEvent(input$clearCache, {
    removed <- clearProcessCache()
    showNotification(
      sprintf("Removed %d cached result(s) (%.1f MB).",
              removed, processCacheSize() / 1024^2),
      type = "message", duration = 5
    )
  })
  # first initialization
  output$mzTable <- createDataTable(appData$stats,
                                    plot_ready = appData$show_plot)

  # on reprocess or if data is send from PCA or clustering
  observeEvent(appData$stats, {
    output$mzTable <- createDataTable(appData$stats,
                                      plot_ready = appData$show_plot)
  })

  ### curve and peak plots ####
  output$curve <- renderPlotly({
    if (appData$show_plot) {
      p_curve <- plotCurves(appData$res,
                            mzIdx = input$mzTable_rows_selected[1],
                            errorbars = input$errorbars) +
        labs(title = paste0("m/z = ",
                            round(
                              getMzFromMzIdx(appData$res,
                                             input$mzTable_rows_selected[1]),
                              2)))

      ggplotly(p_curve)
    } else {
      dummyPlot()
    }
  })

  output$peak <- renderPlotly({
    if (appData$show_plot) {
      p_peak <- plotPeak(appData$res,
                         mzIdx = input$mzTable_rows_selected[1],
                         tol = input$zoom,
                         centroided = isTRUE(appData$centroided),
                         showPeaks = isTRUE(input$showPeaks)) +
        labs(title = NULL)
      ggplotly(p_peak)
    } else {
      dummyPlot()
    }
  })

  ### score plot ####
  output$scorePlot <- renderPlotly({
    if (appData$show_plot) {
      scorePlot(appData$stats, metric = input$metric)
    } else {
      dummyPlot()
    }
  })

  ### QC tab ####
  # re-calibration check
  output$checkRecal <- renderPlotly({
    if (appData$show_plot) {
      p <- checkRecalibration(appData$res,
                              idx = seq_along(getAvgSpectra(appData$res)),
                              centroided = isTRUE(appData$centroided))
      ggplotly(p)
    } else {
      dummyPlot()
    }
  })

  # platemap
  output$platemap <- renderPlot({
    if (appData$show_plot & input$fileFormat == "bruker") {
      shinyjs::show(id = "plateStat")
      shinyjs::show(id = "plateScale")
      plateMapPlot(appData,
                   stat = input$plateStat,
                   PCs = c(input$pcaX, input$pcaY),
                   log10 = input$plateScale,
                   mz_idx = input$mzTable_rows_selected[1])
    } else {
      shinyjs::hide(id = "plateStat")
      shinyjs::hide(id = "plateScale")
    }
  })

  # summary
  observeEvent(input$process, {
    output$summaryText <- renderUI({
      if (appData$info_state == "processed") {
        generateSummaryText(appData$res,
                            smooth = appData$preprocessing$smooth,
                            rmBl = appData$preprocessing$rmBl,
                            sqrtTrans = appData$preprocessing$sqrtTrans,
                            monoFil = appData$preprocessing$monoisotopicFilter,
                            concUnit = input$concUnits)
      }
    })
  })

  ### PCA tab ####
  # default plot for PCA
  output$pca <- renderPlotly({
    dummyPlot()
  })

  observeEvent(input$doPca, {
    if (!is.null(appData$res)) {
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
        pcaPlot(pca = appData$pca,
                conc = factor(getConc(appData$res)),
                x = input$pcaX,
                y = input$pcaY,
                ellipseLevel = as.numeric(input$pcaEllipse),
                spots = getSpots(appData$res))
      } else {
        dummyPlot()
      }
    })
  })

  output$pcaLoading1 <- renderPlotly({
    if (appData$show_plot & !is.null(appData$pca)) {
      p <- loadingsPlot(appData$pca,
                        pc = input$pcaX,
                        simple = input$simpleLoadings)

      return(p)
    } else {
      dummyPlot()
    }
  })

  output$pcaLoading2 <- renderPlotly({
    if (appData$show_plot & !is.null(appData$pca)) {
      p <- loadingsPlot(appData$pca,
                        pc = input$pcaY,
                        simple = input$simpleLoadings)

      return(p)

    } else {
      dummyPlot()
    }
  })

  observeEvent(input$pca2peaksTable, {
    if (appData$show_plot & !is.null(appData$pca)) {
      loadings <- extractLoadings(appData$pca,
                                  appData$res,
                                  input$pcaX,
                                  input$pcaY)

      appData$stats <- appData$stats_original %>%
        left_join(loadings, by = join_by(mzIdx))
    }
  })

  ### clustering tab #####
  observeEvent(input$doClust, {
    if (appData$show_plot) {
      withProgress(
        message = "Clustering curves...",
        detail = paste0("Fitting models (k=2..10, centroid=", input$centroid_method, ")"),
        value = 0.1,
        expr = {
          appData$clust <- clusterCurves(appData$res, nClusters = 10,
                                          centroidMethod = input$centroid_method)
        }
      )
      if (is.null(appData$clust)) {
        showNotification("Clustering failed. Check console for details.",
                         duration = 10, type = "error")
      }
    }
  })

  output$clustPlot <- renderPlotly({
    if (appData$show_plot & !is.null(appData$clust)) {
      show_spinner()
      p <- plotClusters(appData$clust, k = input$num_cluster)
      hide_spinner()
      return(p)
    }
  })

  output$clustCurvesPlot <- renderPlotly({
    if (appData$show_plot & !is.null(appData$clust)) {
      show_spinner()
      p <- plotTraj(appData$clust, k = input$num_cluster)
      hide_spinner()
      return(p)
    }
  })

  output$optNumClust <- renderPlotly({
    if (appData$show_plot & !is.null(appData$clust)) {
      show_spinner()
      p <- plotClusterMetrics(appData$clust)
      hide_spinner()
      return(p)
    }
  })

  observeEvent(input$clust2peaksTable, {
    if (appData$show_plot & !is.null(appData$clust)) {

      clusters <- extractLaClusters(appData$clust, k = input$num_cluster)
      appData$stats <- appData$stats_original %>%
        left_join(clusters, by = join_by(mzIdx))
      message("Updated peak table with clustering data.\n")
    }
  })

  ### save settings ####
  observeEvent(input$saveSettings, {
    saveSettings(input, filename = "settings.csv", info_state = appData$info_state)

  })

  ### download handler ####
  output$downloadPlot <- downloadHandlerPlots(res = appData$res,
                                              mzIdx = input$mzTable_rows_selected[1],
                                              errorbars = input$errorbars,
                                              zoom = input$zoom,
                                              centroided = isTRUE(appData$centroided),
                                              showPeaks = isTRUE(input$showPeaks),
                                              plot_ready = appData$show_plot)

  # one archive with the tables, the settings, run information and an overview
  # figure of the top ranked features
  output$exportRun <- downloadHandler(
    filename = function() {
      if (isTRUE(appData$show_plot)) {
        paste0(basename(getDirectory(appData$res)), "_M2ara_export.zip")
      }
    },
    content = function(file) {
      if (!isTRUE(appData$show_plot)) {
        warning("Nothing to export. Load and process data.")
        return(invisible(NULL))
      }

      settings_file <- file.path(tempdir(), "m2ara_settings_export.csv")
      saveSettings(input,
                   filename = settings_file,
                   info_state = appData$info_state)

      exportRun(
        file = file,
        res = appData$res,
        stats = appData$stats,
        settings = settings_file,
        centroided = isTRUE(appData$centroided),
        plot_ready = appData$show_plot
      )
    }
  )

  # mappig of concentrations by external txt
  observeEvent(input$mappingFile, {
    mapping <- processMappingFile(input$mappingFile)
    if(is.null(mapping)) {
      appData$info_state <- "errorMapping"
      return()
    }
    appData$mapping <- mapping
  })

  exportTestValues(numSpec = length(appData$spec_all),
                   isSpectrumList = MALDIquant::isMassSpectrumList(appData$spec_all),
                   infoState =  appData$info_state,
                   pca =  appData$pca,
                   clust = appData$clust)

  session$onSessionEnded(function() {
    stopApp()
  })

}
