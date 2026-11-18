# Server ####
appServer <- function(input, output, session, defaults = NULL) {
  ## variables ####
  observe_helpers(withMathJax = TRUE,
                  help_dir = system.file("helpfiles", package = "M2ara"))

  ## main ####
  appData <- emptyAppDataObject()
  # settings and features of a share link (if the app was opened with one)
  url_state <- appDefaultsFromUrl(defaults, shiny::isolate(session$clientData$url_search))
  appData$pending_selection <- url_state$features
  if (isTRUE(url_state$active)) {
    # the merged defaults also provide the folder of the link
    defaults <- url_state$defaults
    showNotification("Settings restored from the link.", duration = 8, type = "message")
  }

  # background processing job and its cache key
  process_job <- reactiveVal(NULL)
  process_status <- reactiveVal(NULL)
  process_console_bucket <- reactiveVal(-1L)
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

  # settings that are applied while the spectra are read: changing them means
  # the spectra have to be read again on the next run
  observeEvent(list(input$fileFormat, input$mappingFile, input$checkEmpty), {
    if (!is.null(appData$spec_all)) {
      appData$spec_all <- NULL
      appData$spec_idx <- NULL
      appData$info_state <- if (is.null(appData$selected_dir)) "initial" else "dir_set"
    }
  }, ignoreInit = TRUE)

  ### process spectra: loads the spectra if necessary and processes them ####
  observeEvent(input$process, {
    # a second job while one is running would orphan the first one
    if (!is.null(process_job())) {
      showNotification("Processing is already running.", type = "warning", duration = 4)
      return()
    }

    # guard against missing/empty numeric inputs
    req(input$SNR, input$normMz, input$normTol, input$alignTol, input$binTol, input$halfWindowSize,
        cancelOutput = FALSE)


    if (appData$info_state %in% c("dir_set", "loaded", "processed")) {
      # settings as shown in the UI, used by the export (no shiny input
      # access in the download handler, that could block the server)
      appData$settings_snapshot <- settingsDataFrame(input)

      settings <- processingSettings(input)
      cache_key <- processCacheKey(appData$selected_dir, settings,
                                   appData$preprocessing)
      process_cache_key(cache_key)

      # 1) unchanged data and settings: the cached result is used
      cached <- processCacheGet(cache_key)
      if (!is.null(cached)) {
        appData$res <- cached$res
        appData$centroided <- cached$centroided
        message(MALDIcellassay:::timeNow(), " results loaded from cache\n")
        showNotification("Results loaded from the cache.", duration = 5, type = "message")
        appData <- storeResults(appData, res = cached$res, input,
                                stats = getStatistics(cached$res))
        session$sendCustomMessage("m2ara_reset_selection", list())
        applyPendingSelection()
        updateActionButton(inputId = "process", label = "Re-process")
        return()
      }

      # 2) the spectra for this folder are not in memory yet: read them first
      if (is.null(appData$spec_all)) {
        show_spinner()
        message(MALDIcellassay:::timeNow(), " loading spectra...\n")
        appData <- loadSpectraData(input, appData, mapping = appData$mapping)
        if (!identical(appData$info_state, "loaded")) {
          hide_spinner()
          return()
        }
      }

      # change concentrations according to the unit of the UI
      appData <- setConcentrationUnit(appData, input)

      # 3) process in a background process
      show_spinner()
      showNotification("Processing started. Please wait.",
                       duration = 5,
                       type = "default")
      message(MALDIcellassay:::timeNow(), " start processing...\n")


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

      process_status(list(percent = 0, label = "Starting ..."))
      process_console_bucket(-1L)
      process_job(job)
    }
  })

  ### processing job: progress, result and cancel ####
  output$processRunning <- reactive({ !is.null(process_job()) })
  outputOptions(output, "processRunning", suspendWhenHidden = FALSE)

  output$processStatus <- renderUI({
    status <- process_status()
    if (is.null(status)) {
      return(NULL)
    }

    tagList(
      div(class = "progress", style = "height: 16px; margin-bottom: 4px;",
          div(class = "progress-bar progress-bar-striped active",
              role = "progressbar",
              style = sprintf("width: %d%%;", max(2, status$percent)),
              sprintf("%d%%", status$percent))),
      tags$small(status$label)
    )
  })

  observe({
    job <- process_job()
    if (is.null(job)) return()

    if (!processJobDone(job)) {
      status <- processJobStatus(job)
      label <- processStageLabel(status$stage)
      if (!is.na(status$total) && status$total > 0) {
        label <- sprintf("%s %d/%d", label, status$done, status$total)
      }
      process_status(list(percent = status$percent, label = label))

      # console output, but only every 10 percent
      bucket <- as.integer(status$percent / 10)
      if (bucket > process_console_bucket()) {
        process_console_bucket(bucket)
        message(sprintf("  %s (%d%%)", label, status$percent))
      }

      invalidateLater(500)
      return()
    }

    out <- processJobResult(job)
    job_log <- processJobLog(job)
    processJobCleanup(job)
    process_job(NULL)
    process_status(NULL)
    process_console_bucket(-1L)
    hide_spinner()

    if (is.null(out)) {
      appData$info_state <- "fitErrorOther"
      if (nzchar(job_log)) {
        message("processing failed:\n", job_log)
        showNotification(paste0("Processing failed: ",
                                strsplit(job_log, "\n")[[1]][1]),
                         type = "error", duration = 15)
      } else {
        showNotification("Processing failed. Please check the settings.",
                         type = "error", duration = 15)
      }
      return()
    }

    if (!isTRUE(out$ok)) {
      appData$info_state <- switch(out$error,
                                   recal = "fitErrorRecal",
                                   norm = "fitErrorNorm",
                                   "fitErrorOther")
      return()
    }

    processCachePut(process_cache_key(),
                    list(res = out$res, centroided = isTRUE(appData$centroided)))
    appData <- storeResults(appData, res = out$res, input,
                            stats = getStatistics(out$res))
    session$sendCustomMessage("m2ara_reset_selection", list())
    applyPendingSelection()
    updateActionButton(inputId = "process", label = "Re-process")
    showNotification("Processing finished.", type = "message", duration = 3)
  })

  observeEvent(input$cancelProcess, {
    job <- process_job()
    if (is.null(job)) return()

    processJobCancel(job)
    processJobCleanup(job)
    process_job(NULL)
    process_status(NULL)
    process_console_bucket(-1L)
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
  # at most 4 features can be compared
  selected_features <- reactive({
    sel <- input$mzTable_selected
    sel <- sel[!is.na(sel)]
    utils::head(sel, 4)
  })
  # a share link may carry the compared features; apply them once results
  # are available
  applyPendingSelection <- function() {
    pending <- appData$pending_selection
    if (length(pending) == 0L) {
      return(invisible(NULL))
    }
    appData$pending_selection <- integer(0)
    keep <- pending[pending >= 1L & pending <= nrow(appData$stats)]
    if (length(keep) > 0L) {
      session$sendCustomMessage("m2ara_set_selection",
                                as.list(utils::head(keep, 4)))
    }
    invisible(NULL)
  }


  # share link: current settings, folder and compared features in the URL
  observeEvent(input$copyLink, {
    state <- appStateFromInput(input,
                                dir = appData$selected_dir,
                                features = selected_features())
    url <- appShareUrl(session, state)
    shiny::updateQueryString(paste0("?state=",
                                    utils::URLencode(encodeAppState(state), reserved = TRUE)),
                             mode = "replace", session = session)
    session$sendCustomMessage("m2ara_copy_link", list(url = url))
    showNotification("Link copied to the clipboard.", type = "message", duration = 5)
  })

  observeEvent(input$clearSelection, {
    session$sendCustomMessage("m2ara_reset_selection", list())
  })

  output$curve <- renderPlotly({

    if (appData$show_plot) {
      sel <- selected_features()

      if (length(sel) == 0L) {
        return(dummyPlot("Select a feature\nin the table"))
      }

      if (length(sel) > 1L) {
        return(ggplotly(plotCurvesOverlay(appData$res, sel,
                                          errorbars = input$errorbars,
                                          normalize = isTRUE(input$normalizeCompare))))
      }

      p_curve <- plotCurves(appData$res,
                            mzIdx = selected_features()[1],
                            errorbars = input$errorbars) +
        labs(title = paste0("m/z = ",
                            round(
                              getMzFromMzIdx(appData$res,
                                             selected_features()[1]),
                              2)))

      ggplotly(p_curve)
    } else {
      dummyPlot()
    }
  })

  # the peak panel grows with the number of compared features: two features are
  # one row, three and four are a 2 x 2 grid
  output$peakUi <- renderUI({
    sel <- selected_features()
    if (length(sel) > 1L) {
      rows <- ceiling(length(sel) / 2)
      height <- if (rows == 1L) "340px" else paste0(300 * rows, "px")
      plotOutput("peakGrid", height = height)
    } else {
      plotly::plotlyOutput("peak", height = "auto")
    }
  })

  output$peakGrid <- renderPlot({
    sel <- selected_features()
    if (!appData$show_plot || length(sel) < 2L) {
      return(NULL)
    }
    peakGrid(appData$res, sel,
             tol = input$zoom,
             centroided = isTRUE(appData$centroided),
             showPeaks = isTRUE(input$showPeaks))
  },
  # a minimum device size avoids the transient "figure margins too large"
  # error while the panel is still being laid out
  width = function() {
    w <- session$clientData[["output_peakGrid_width"]]
    if (is.null(w) || !is.finite(w)) 640 else max(480, w)
  },
  height = function() {
    rows <- ceiling(max(1, length(selected_features())) / 2)
    if (rows == 1L) 340 else 300 * rows
  })

  output$peak <- renderPlotly({
    if (appData$show_plot) {
      if (length(selected_features()) == 0L) {
        return(dummyPlot("Select a feature\nin the table"))
      }

      p_peak <- plotPeak(appData$res,
                         mzIdx = selected_features()[1],
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
                   mz_idx = selected_features()[1])
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
                                              mzIdx = selected_features()[1],
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
      utils::write.csv(appData$settings_snapshot, settings_file, row.names = FALSE)

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
                   showPlot = appData$show_plot,
                   pca =  appData$pca,
                   clust = appData$clust)

  session$onSessionEnded(function() {
    stopApp()
  })

}
