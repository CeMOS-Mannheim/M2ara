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
  loading <- reactiveVal(FALSE)
  process_status <- reactiveVal(NULL)
  process_console_bucket <- reactiveVal(-1L)
  process_cache_key <- reactiveVal(NULL)

  appData <- selectDir(appData, input, output, defaults = defaults)

  observeEvent(input$preproc_settings, {
    appData <- setPreprocessSettings(input, appData)
  })

  ### assign concentrations by hand #####
  output$hasFolder <- reactive({ !is.null(appData$selected_dir) && nzchar(appData$selected_dir) })
  outputOptions(output, "hasFolder", suspendWhenHidden = FALSE)

  conc_data <- reactiveVal(NULL)

  observeEvent(input$setConcentrations, {
    req(appData$selected_dir)
    table <- concentrationTable(appData$selected_dir,
                                detectDataFormat(appData$selected_dir),
                                mapping = appData$mapping,
                                excluded = appData$excluded_files)
    if (nrow(table) == 0L) {
      showNotification("No spectra in this folder.", type = "error", duration = 10)
      return()
    }
    conc_data(table)
    showModal(concentrationDialog(table))
  })

  observeEvent(input$applyConcentrations, {
    table <- concentrationDialogTable(conc_data(), input)
    problem <- concentrationTableProblem(table)
    if (!is.null(problem)) {
      showNotification(problem, type = "error", duration = 15)
      return()
    }

    sheet <- includedSheet(table)
    appData$mapping <- list(kind = "sheet", sheet = sheet)
    appData$mapping_source <- "sample sheet"
    appData$excluded_files <- excludedFiles(table)

    if (isTRUE(input$concWriteSheet)) {
      path <- file.path(appData$selected_dir, "samples.csv")
      ok <- tryCatch({
        utils::write.csv(sheet, path, row.names = FALSE)
        TRUE
      }, error = function(e) FALSE)
      if (!ok) {
        showNotification("Could not write samples.csv.", type = "error", duration = 10)
      }
    }

    removeModal()

    # the assignment is what the last load was missing, so the app is ready to
    # process again (the card must not keep saying "last load failed")
    state <- appData$info_state
    if (startsWith(state, "loadError") || identical(state, "errorMapping")) {
      setInfoState(appData, "dir_set")
    }

    # the spectra have to be read again when they were already loaded
    if (isTRUE(hadSpectra)) {
      updateActionButton(inputId = "process", label = "Re-process")
    }

    showNotification(
      sprintf(paste("Concentrations for %d file(s) taken over%s - press Process",
                    "spectra to use them%s."),
              nrow(sheet),
              if (length(appData$excluded_files) > 0L)
                sprintf(", %d file(s) left out", length(appData$excluded_files)) else "",
              if (isTRUE(hadSpectra)) ", the spectra are read again" else ""),
      type = "message", duration = 8)
  })
  ### enable/disable buttons #####
  # a button that cannot do anything should not look clickable
  observe({
    # pressing Process is also how a failed load is retried, so a usable folder
    # is enough - the error states must not lock the button
    ready <- hasUsableDir(appData$selected_dir)
    busy <- isTRUE(loading()) || !is.null(process_job())
    shinyjs::toggleState("process", condition = ready && !busy)
  })

  observe({
    hasData <- isTRUE(appData$show_plot)
    shinyjs::toggleState("doClust",
                         condition = hasData && is.null(clust_job()))
    shinyjs::toggleState("clust2peaksTable",
                         condition = hasData && !is.null(appData$clust))
  })
  ### dataset card in the sidebar #####
  output$datasetCard <- renderUI({
    datasetCardUi(appData, input,
                    running = !is.null(process_job()),
                    loading = isTRUE(loading()))
  })

  ### Text massage logic ####
  observeEvent(list(appData$info_state, appData$selected_dir, appData$load_problem), {
    output <- infoStateMassageHandler(info_state = appData$info_state,
                                       problem = appData$load_problem,
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
  observeEvent(list(input$fileFormat, input$checkEmpty), {
    if (!is.null(appData$spec_all)) {
      appData$spec_all <- NULL
      appData$spec_idx <- NULL
      setInfoState(appData, if (is.null(appData$selected_dir)) "initial" else "dir_set")
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
      # remembered, so the sidebar can tell whether the shown result still
      # belongs to the settings in the form (the run time is only set when a run
      # actually produced a result)
      appData$run_signature <- list(settings = settings,
                                    preprocessing = appData$preprocessing,
                                    concUnits = input$concUnits,
                                    mapping = mappingHash(appData$mapping),
                                    excluded = sort(as.character(appData$excluded_files)))

      # a value the widgets accept can still break the pipeline
      problems <- validateProcessingSettings(settings)
      if (length(problems) > 0L) {
        showNotification(paste(problems, collapse = " "), type = "error",
                         duration = 20)
        return()
      }
      cache_key <- processCacheKey(appData$selected_dir, settings,
                                   appData$preprocessing,
                                   mapping = appData$mapping)
      process_cache_key(cache_key)

      # 1) unchanged data and settings: the cached result is used (unless a
      # forced run was requested)
      cached <- if (isTRUE(appData$force_reprocess)) NULL else processCacheGet(cache_key, appData$selected_dir)
      appData$force_reprocess <- FALSE
      if (!is.null(cached)) {
        # keep the sidecar of the entry up to date, so the sidebar can tell that
        # a cached result exists
        created <- file.mtime(processCachePath(appData$selected_dir))
        processCacheWriteMeta(cache_key, appData$selected_dir, created = created)
        appData$res <- cached$res
        appData$centroided <- cached$centroided
        # the effective concentration sheet belongs to the result
        appData$samples <- cached$samples
        appData$mapping_source <- cached$mapping_source
        message(m2araTimeNow(), " results loaded from cache\n")
        showNotification("Results loaded from the cache.", duration = 5, type = "message")
        appData <- storeResults(appData, res = cached$res, input,
                                stats = getStatistics(cached$res))
        # "Last run" is when the result was computed, not when it was loaded
        appData$run_time <- created
        session$sendCustomMessage("m2ara_reset_selection", list())
        applyPendingSelection()
        updateActionButton(inputId = "process", label = "Re-process")
        return()
      }

      # 2) the spectra for this folder are not in memory yet: read them first
      if (is.null(appData$spec_all)) {
        showM2araSpinner()
        # the files are read synchronously, so at least say what is going on
        loading(TRUE)
        process_status(list(percent = 0,
                            label = spectraLoadingLabel(appData$selected_dir,
                                                       input$fileFormat)))
        message(m2araTimeNow(), " loading spectra...\n")
        appData <- loadSpectraData(input, appData, mapping = appData$mapping)
        loading(FALSE)
        process_status(NULL)
        if (!identical(appData$info_state, "loaded")) {
          hideM2araSpinner()
          return()
        }
      }

      # change concentrations according to the unit of the UI
      appData <- setConcentrationUnit(appData, input)

      # the normalization m/z has to be inside the measured range
      if (is.null(appData$mz_range)) {
        appData$mz_range <- spectraMassRange(appData$spec_all)
      }
      mzRange <- appData$mz_range
      if (!is.null(mzRange) &&
          (settings$normMz < mzRange[1L] || settings$normMz > mzRange[2L])) {
        showNotification(
          sprintf(paste("The normalization m/z (%.2f) is outside the measured range",
                        "(%.1f - %.1f Da). The run continues, but check the setting."),
                  settings$normMz, mzRange[1L], mzRange[2L]),
          type = "warning", duration = 15)
      }

      # 3) process in a background process
      showM2araSpinner()
      showNotification("Processing started. Please wait.",
                       duration = 5,
                       type = "message")
      message(m2araTimeNow(), " start processing...\n")


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
        setInfoState(appData, "fitErrorOther")
        hideM2araSpinner()
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

  # busy while loading or processing
  output$processBusy <- reactive({ isTRUE(loading()) || !is.null(process_job()) })
  outputOptions(output, "processBusy", suspendWhenHidden = FALSE)

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
    hideM2araSpinner()

    if (is.null(out)) {
      setInfoState(appData, "fitErrorOther")
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
      setInfoState(appData, switch(out$error,
                                   recal = "fitErrorRecal",
                                   norm = "fitErrorNorm",
                                   "fitErrorOther"))
      return()
    }

    processCachePut(process_cache_key(),
                    value = list(res = out$res,
                                 centroided = isTRUE(appData$centroided)),
                    dir = appData$selected_dir)
    cache_refresh(cache_refresh() + 1L)
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
    hideM2araSpinner()
    showNotification("Processing cancelled.", type = "warning", duration = 5)
  })

  ### Peak table ####

  # where the settings live and when they were saved last
  settings_saved <- reactiveVal(NULL)
  output$settingsInfo <- renderText({
    path <- file.path(getwd(), "settings.csv")
    saved <- settings_saved()
    if (is.null(saved) || !file.exists(path)) {
      return("settings.csv (saved when you press the button)")
    }
    sprintf("settings.csv - saved %s", format(saved, "%Y-%m-%d %H:%M"))
  })
  # what is in the result cache
  cache_refresh <- reactiveVal(0L)
  output$cacheInfo <- renderText({
    cache_refresh()
    processCacheSummary()
  })

  # read the files again and ignore the cache, e.g. after an update of M2ara
  observeEvent(input$forceProcess, {
    if (is.null(appData$selected_dir) || length(appData$selected_dir) == 0L) {
      showNotification("Select a folder first.", type = "warning", duration = 5)
      return()
    }

    appData$spec_all <- NULL
    appData$spec_idx <- NULL
    appData$force_reprocess <- TRUE

    showNotification("Cache bypassed - the spectra are read and processed again.",
                     type = "message", duration = 6)
    shinyjs::click("process")
  })

  observeEvent(input$clearCache, {
    removed <- clearProcessCache()
    cache_refresh(cache_refresh() + 1L)
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
    local_host <- shiny::isolate(session$clientData$url_hostname) %in%
      c("127.0.0.1", "localhost", "::1")
    session$sendCustomMessage("m2ara_show_link", list(url = url, local = local_host))
    showNotification("Share link ready - use the box in the lower right corner.", type = "message", duration = 6)
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

      # one feature uses the same overlay, so "normalize curves" works here too;
      # a single curve stays black like in the original curve plot
      p_curve <- plotCurvesOverlay(appData$res, sel,
                                   errorbars = input$errorbars,
                                   normalize = isTRUE(input$normalizeCompare),
                                   colour = "black") +
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

  # spots come from Bruker flex folders and from the mzML spectrum ids
  spotsAvailable <- reactive({
    if (is.null(appData$res)) {
      return(FALSE)
    }
    spots <- getSpots(appData$res)
    length(spots) > 0L && any(!is.na(spots) & nzchar(spots))
  })

  output$platemap <- renderPlot({
    if (isTRUE(appData$show_plot) && isTRUE(spotsAvailable())) {
      shinyjs::show(id = "plateStat")
      shinyjs::toggle(id = "plateScale",
                      condition = input$plateStat %in% plateLogMetrics())
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
  # the two axes must show different components
  pcaComponents <- reactive({
    if (is.null(appData$pca)) {
      return(paste0("PC", 1:5))
    }
    nonEmptyComponents(appData$pca)
  })

  observeEvent(input$pcaX, {
    keep <- pcaComponents()
    if (!is.null(input$pcaY) && identical(input$pcaY, input$pcaX) && length(keep) > 1L) {
      updateSelectInput(session, "pcaY", selected = setdiff(keep, input$pcaX)[1])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pcaY, {
    keep <- pcaComponents()
    if (!is.null(input$pcaX) && identical(input$pcaX, input$pcaY) && length(keep) > 1L) {
      updateSelectInput(session, "pcaX", selected = setdiff(keep, input$pcaY)[1])
    }
  }, ignoreInit = TRUE)

  output$pca <- renderPlotly({
    dummyPlot()
  })

  # the preprocessing may already apply a sqrt transform; a log transform on
  # top of it would be a double transform, so it is only offered without it
  pcaTransformChoices <- reactive({
    if (isTRUE(handlePreprocSettings(input$preproc_settings, "sqrtTrans"))) {
      return(c("as processed (sqrt)" = "as_processed"))
    }
    c("as processed" = "as_processed", "log10(x + 1)" = "log10")
  })

  observeEvent(input$doPca, {
    if (is.null(appData$res)) {
      return()
    }

    choices <- pcaTransformChoices()
    updateSelectInput(session, "pcaTransform",
                      choices = choices,
                      selected = if (input$pcaTransform %in% unname(choices)) input$pcaTransform else choices[[1]])

    withProgress(message = "Performing sparse PCA", value = 0.5, {
      pca <- tryCatch(
        generatePCA(appData$res,
                    num_PC = 5,
                    alpha = 10^input$pcaAlpha,
                    beta = 10^input$pcaBeta,
                    transform = if (length(choices) == 1L) "as_processed" else input$pcaTransform,
                    verbose = FALSE),
        error = function(e) e
      )
    })

    if (inherits(pca, "error")) {
      showNotification(paste("PCA failed:", conditionMessage(pca)),
                       type = "error", duration = 10)
      return()
    }

    appData$pca <- pca

    if (pca$dropped > 0L) {
      showNotification(sprintf("%d constant feature(s) were dropped for the PCA.",
                               pca$dropped), type = "message", duration = 6)
    }
    if (pca$naReplaced > 0L) {
      showNotification(sprintf("%d missing intensity value(s) were treated as 0.",
                               pca$naReplaced), type = "message", duration = 6)
    }
    if (any(pca$active == 0L)) {
      empty <- names(pca$active)[pca$active == 0L]
      showNotification(
        paste0("Component(s) ", paste(empty, collapse = ", "),
               " have no active feature - the L1 penalty is too high."),
        type = "warning", duration = 10)
    }

    # the axes have to differ; a run with only one component must not leave the
    # y axis on PC1 forever
    keep <- nonEmptyComponents(pca)
    sel_x <- if (input$pcaX %in% keep) input$pcaX else keep[1]
    others <- setdiff(keep, sel_x)

    if (length(others) == 0L) {
      sel_y <- sel_x
      showNotification("Only one component contains features - reduce the L1 penalty to get more.",
                       type = "warning", duration = 8)
    } else {
      sel_y <- if (input$pcaY %in% others) input$pcaY else others[1]
    }

    updateSelectInput(session, "pcaX", choices = keep, selected = sel_x)
    updateSelectInput(session, "pcaY", choices = keep, selected = sel_y)
  })

  output$pcaSparsity <- renderText({
    pca <- appData$pca
    if (is.null(pca)) {
      return("")
    }

    total <- length(pca$mzIdx)
    counts <- paste(names(pca$active),
                    paste0(pca$active, "/", total),
                    sep = " ", collapse = " \u00b7 ")

    # the numbers belong to the last run, the sliders may already differ
    changed <- !isTRUE(all.equal(pca$alpha, 10^input$pcaAlpha))
    hint <- if (changed) {
      "  - settings changed, press 'Perform PCA' again"
    } else {
      ""
    }

    paste0("last run: ", counts,
           "  (L1 = ", signif(pca$alpha, 2),
           ", ", pca$transform,
           "; active = |loading| \u2265 1 % of the component max)", hint)
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
  clust_job <- reactiveVal(NULL)
  clust_status <- reactiveVal(NULL)
  clust_cancelled <- reactiveVal(FALSE)

  output$clustRunning <- reactive({ !is.null(clust_job()) })
  outputOptions(output, "clustRunning", suspendWhenHidden = FALSE)

  output$clustStatus <- renderUI({
    status <- clust_status()
    if (is.null(status)) {
      return(NULL)
    }
    tags$div(
      style = "font-size: 85%; margin-bottom: 4px;",
      status
    )
  })

  observeEvent(input$doClust, {
    req(isTRUE(appData$show_plot))
    if (!is.null(clust_job())) {
      showNotification("Clustering is already running.", type = "warning")
      return()
    }

    # sampling the fitted curves is fast, the clustering runs in the background
    curves <- curveClusterData(appData$res)
    if (is.null(curves)) {
      showNotification(
        paste("No usable curves - cannot run clustering.",
              "At least 10 curves with a reasonable fit are needed."),
        type = "error", duration = 15)
      return()
    }

    features <- attr(curves, "features")
    if (features < 10L) {
      showNotification(
        sprintf("Only %d usable curves - the clustering needs at least 10.", features),
        type = "error", duration = 15)
      return()
    }

    if (features > maxClusterFeatures) {
      showNotification(
        sprintf(paste("This would cluster %d features, the limit is %d.",
                      "Reduce the number of features with the Var. filter first."),
                features, maxClusterFeatures),
        type = "error", duration = 20)
      return()
    }

    excluded <- c(if (attr(curves, "excludedFit") > 0L)
                    sprintf("%d with a poor fit", attr(curves, "excludedFit")),
                  if (attr(curves, "excludedFlat") > 0L)
                    sprintf("%d without rise", attr(curves, "excludedFlat")))
    if (length(excluded) > 0L) {
      showNotification(sprintf("%d curves are left out before the clustering: %s.",
                               features, paste(excluded, collapse = ", ")),
                       type = "warning", duration = 10)
    }

    clust_cancelled(FALSE)
    appData$clust <- NULL
    appData$clust_distance <- input$clustDistance
    appData$clust_centroid <- input$centroid_method
    clust_status(clusterStageLabel("queued"))
    clust_job(clusterCurvesAsync(curves, nClusters = 10,
                                 centroidMethod = input$centroid_method,
                                 distance = input$clustDistance))
    showNotification("Clustering started. You can cancel it below.",
                     type = "message", duration = 5)
  })

  # poll the background job
  observe({
    job <- clust_job()
    if (is.null(job)) {
      return()
    }

    invalidateLater(500)

    if (!clusterJobDone(job)) {
      clust_status(clusterStageLabel(clusterJobProgress(job)))
      return()
    }

    models <- clusterJobResult(job)
    cancelled <- isTRUE(clust_cancelled())
    log <- clusterJobLog(job)
    clusterJobCleanup(job)
    clust_job(NULL)
    clust_status(NULL)

    if (cancelled) {
      showNotification("Clustering cancelled.", type = "message", duration = 5)
      return()
    }

    if (is.null(models)) {
      showNotification(
        paste0("Clustering failed. ", if (nzchar(log)) log else "See the console."),
        type = "error", duration = 15)
      return()
    }

    appData$clust <- models
    showNotification("Clustering finished.", type = "message", duration = 5)
  })


  observeEvent(input$cancelClust, {
    job <- clust_job()
    if (!is.null(job)) {
      clust_cancelled(TRUE)
      clusterJobCancel(job)
      clust_status("Cancelling clustering ...")
    }
  })

  output$clustPlot <- renderPlotly({
    if (appData$show_plot & !is.null(appData$clust)) {
      plotClusters(appData$clust, k = input$num_cluster)
    }
  })

  output$clustCurvesPlot <- renderPlotly({
    if (appData$show_plot & !is.null(appData$clust)) {
      plotTraj(appData$clust, k = input$num_cluster)
    }
  })

  # the shown result belongs to the settings it was computed with
  output$clustOutdated <- renderText({
    if (is.null(appData$clust)) {
      return("")
    }

    changed <- c(if (!identical(appData$clust_distance, input$clustDistance))
                   sprintf("distance (%s -> %s)", appData$clust_distance, input$clustDistance),
                 if (!identical(appData$clust_centroid, input$centroid_method))
                   sprintf("centroid (%s -> %s)", appData$clust_centroid, input$centroid_method))

    if (length(changed) == 0L) {
      return("")
    }

    sprintf("The plots were computed with a different %s. Press \"Perform clustering\" to apply the new setting.",
            paste(changed, collapse = " and "))
  })
  output$clustSuggestion <- renderText({
    if (is.null(appData$clust)) {
      return("")
    }
    suggestClustersText(appData$clust)
  })

  output$optNumClust <- renderPlotly({
    if (appData$show_plot & !is.null(appData$clust)) {
      plotClusterMetrics(appData$clust)
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
    settings_saved(Sys.time())
    showNotification("Settings saved to settings.csv.", type = "message", duration = 5)
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
        plot_ready = appData$show_plot,
        samples = appData$samples,
        samples_source = appData$mapping_source
      )
    }
  )

  # concentration mapping by an external sample sheet


  exportTestValues(numSpec = length(appData$spec_all),
                   isSpectrumList = MALDIquant::isMassSpectrumList(appData$spec_all),
                   infoState =  appData$info_state,
                   showPlot = appData$show_plot,
                   pca =  appData$pca,
                   clust = appData$clust)

  # shut down when the browser is gone, but not on a page reload
  registerSession()
  session$onSessionEnded(function() {
    unregisterSession(function() shiny::stopApp())
  })

}
