# Sever ####
server <- function(input, output, session) {

  ## load ext. functions ####
  source("functions/loadAllFunctions.R")
  loadAllFunctions()

  ## variables ####
  observe_helpers(withMathJax = TRUE)

  ## main ####
  appData <<- emptyAppDataObject()
  vol <- getVolumes()

  # check if "dir" is set in defaults
  if (!is.null(defaults$dir)) {
    appData$selected_dir <- defaults$dir
    message("Dir set from loaded default value.\n")
    appData$info_state <- "dir_set"
  }

  ### choose dir ####
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
      appData$info_state <- "dir_set"
    }
  })

  observeEvent(input$preproc_settings, {
    appData$preprocessing$smooth <-
      handlePreprocSettings(input$preproc_settings,
                            "smooth")

    appData$preprocessing$rmBl <-
      handlePreprocSettings(input$preproc_settings,
                            "rmBl")

    appData$preprocessing$sqrtTrans <-
      handlePreprocSettings(input$preproc_settings,
                            "sqrtTrans")

    appData$preprocessing$monoisotopicFilter <-
      handlePreprocSettings(input$preproc_settings,
                            "monoisotopicFilter")
  })

  ### Text massage logic ####
  observeEvent(appData$info_state, {
    output <- infoStateMassageHandler(info_state = appData$info_state,
                                      output = output)
  })

  ### load spectra ####
  # disable load button if no dir is set
  disable("load")
  # disable process button if no spectra are loaded
  disable("process")

  # enable buttons when state is reached
  observeEvent(appData$info_state, {
    if (appData$info_state == "dir_set") {
      enable("load")
    }
    if (appData$info_state == "loaded") {
      enable("process")
    }
  })

  observeEvent(input$load, {
    if (appData$info_state == "dir_set") {
      show_spinner()

      # check if all spectra names are numeric/concentrations
      # for later: if pos and neg ctrls are included
      # checkSpecNames needs to return indices of the numeric folders
      if (!checkSpecNames(appData$selected_dir)) {
        switch(input$fileFormat,
               "bruker" = {
                 spec_raw <- loadSpectra(appData$selected_dir)
               },
               "mzml" = {
                 spec_raw <- loadSpectraMzML(appData$selected_dir)
               })

        appData$info_state <- "loaded"
      } else {
        warning("Found folder names that could not be converted to numeric.
                All folders/spectra need to have concentrations as names.\n")
        hide_spinner()
        return()
      }

      # make sure that the concentrations are in acending order
      spec_raw <- spec_raw[order(as.numeric(names(spec_raw)))]

      appData$org_conc <- as.numeric(names(spec_raw))

      appData$spec_all <- spec_raw
      if(input$checkEmpty) {
        message(MALDIcellassay:::timeNow(),  " check for empty spectra...\n")

        # MAD would be faster but may fail in some circumstances...
        peaks <- detectPeaks(appData$spec_all,
                             SNR = input$SNR,
                             method = "SuperSmoother")
        appData$spec_idx <- vapply(peaks,
                                   function(x) {
                                     ifelse(length(mz(x)) > 0, TRUE, FALSE)
                                   },
                                   FUN.VALUE = TRUE)
        message(MALDIcellassay:::timeNow(), " ",
                sum(appData$spec_idx), "/", length(appData$spec_all),
                " spectra retained.\n")
      } else {
        appData$spec_idx <- rep(TRUE, length(appData$spec_all))
      }


      hide_spinner()
    }
  })

  ### process spectra ####
  observeEvent(input$process, {
    if(!input$concUnits == "M") {
      message("Changing concentrations to ", input$concUnits, ".\n")
    }

    unitFactor <- switch (input$concUnits,
                          "M" = 1,
                          "mM" = 1e-3,
                          "µM" = 1e-6,
                          "nM" = 1e-9,
                          "pM" = 1e-12,
                          "fM" = 1e-15)

    # change concentrations according to unit
    names(appData$spec_all) <- appData$org_conc * unitFactor

    if (!appData$info_state %in% c("intial", "dir_set")) {
      show_spinner()
      message(MALDIcellassay:::timeNow(), " start processing...\n")
      appData$spec_all <- MALDIcellassay:::.repairMetaData(appData$spec_all)

      prc <- preprocess(spectra = appData$spec_all[appData$spec_idx],
                        sqrtTransform = appData$preprocessing$sqrtTrans,
                        smooth = appData$preprocessing$smooth,
                        rmBaseline = appData$preprocessing$rmBl,
                        SNR = input$SNR,
                        singlePointRecal = input$SinglePointRecal,
                        normMz = input$normMz,
                        normTol = input$normTol,
                        normMeth = input$normMeth,
                        alignTol = input$alignTol * 1e-3,
                        halfWindowSize = input$halfWindowSize)

      if(is.null(prc)) {
        # on error stop here
        appData$info_state <- "RefMzError"
        hide_spinner()
        return()
      }


      #### average spectra ####
      message(MALDIcellassay:::timeNow(), " calculating ", input$avgMethod, " spectra... \n")
      avg <- MALDIcellassay:::.aggregateSpectra(spec = prc$spec,
                                                averageMethod = input$avgMethod,
                                                SNR = input$SNR,
                                                monoisotopicFilter = appData$preprocessing$monoisotopicFilter,
                                                binTol = input$binTol * 1e-6,
                                                normMz = input$normMz,
                                                normTol = input$normTol)
      ### single spectra data
      # this is the single spectra data but based on the same signals as in the
      # avg spectra
      # doing it like this gives us greater sensitivity (signal detection on
      # avg spectra) while still enabling statistics on single spectra
      singlePeaks <- extractIntensity(mz = as.numeric(colnames(avg$intmat)),
                                      peaks = prc$singlePeaks,
                                      spec = prc$spec,
                                      tol = input$normTol)

      # fit curves
      message(MALDIcellassay:::timeNow(), " fitting curves... \n")
      fits <- calculateCurveFit(intmat = avg$intmat,
                                idx = filterVariance(apply(avg$intmat, 2, var),
                                                     method = input$VarFilterMethod))

      # peak statistics
      stat_df <- calculatePeakStatistics(curveFits = fits,
                                         singlePeaks = singlePeaks,
                                         spec = prc$spec)

      appData$res <- new("MALDIassay",
                         avgSpectra = avg$avgSpec,
                         avgPeaks = avg$avgPeaksBinned,
                         singlePeaks = singlePeaks,
                         singleSpecSpots = extractSpots(prc$spec),
                         normFactors = prc$normFac,
                         mzShifts = prc$mzShift,
                         fits = fits,
                         stats = stat_df,
                         included_specIdx = prc$idx,
                         settings = list(
                           Conc = as.numeric(names(singlePeaks)),
                           normMz = input$normMz,
                           normTol = input$normTol,
                           varFilterMethod = input$VarFilterMethod,
                           monoisotopicFilter = handlePreprocSettings(input$preproc_settings,
                                                                      "monoisotopicFilter"),
                           alignTol = input$alignTol * 1e-3,
                           SNR = input$SNR,
                           normMeth = input$normMeth,
                           binTol = input$binTol * 1e-6,
                           SinglePointRecal = input$SinglePointRecal,
                           dir = appData$selected_dir
                         )
      )

      if(!fitCurveErrorHandler(appData = appData,
                               input = input)) {
        return()
      }

      message(MALDIcellassay:::timeNow(),  " processing done\n")

      # write everything needed into appData
      appData <- storeResults(appData,
                              res = appData$res,
                              input,
                              stats = getStatistics(appData$res))

      appData$info_state <- "processed"
      appData$show_plot <- TRUE
      updateActionButton(inputId = "process", label = "Re-process")
      hide_spinner()
    }
  })

  ### Peak table ####
  # first initialization
  output$mzTable <- createDataTable(appData$stats,
                                    plot_ready = appData$show_plot)



  # on reprocess or if data is send from PCA, LASSO, HC
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
                          tol = input$zoom) +
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

  #### QC tab ####
  #  recal check
  output$checkRecal <- renderPlotly({
    if (appData$show_plot) {
      p <- checkRecalibration(appData$res,
                              idx = seq_along(getAvgSpectra(appData$res)))
      ggplotly(p)
    } else {
      dummyPlot()

    }

  })

  # platemap
  output$platemap <- renderPlot({
    if (appData$show_plot) {
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
      if (appData$info_state == "processed") {
        generateSummaryText(appData$res,
                            smooth = appData$preprocessing$smooth,
                            rmBl = appData$preprocessing$rmBl,
                            sqrtTrans = appData$preprocessing$sqrtTrans,
                            monoFil = appData$preprocessing$monoisotopicFilter)
      }
    })
  })

  #### PCA tab ####
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

  #### HClust tab #####
  observeEvent(input$doHC, {
    if (appData$show_plot) {
      show_spinner()

      appData$hc <- clusterCurves(appData$res, nClusters = 15)
      hide_spinner()
    }
  })
  output$hclustPlot <- renderPlotly({
    if (appData$show_plot & !is.null(appData$hc)) {
      plotClusters(appData$hc, k = input$num_cluster)

    }
  })

  output$clustCurvesPlot <- renderPlotly({
    if (appData$show_plot & !is.null(appData$hc)) {
      show_spinner()

      p <- plotTraj(appData$hc, k = input$num_cluster)

      hide_spinner()
      return(p)
    }
  })

  output$optNumClust <- renderPlotly({
    if (appData$show_plot & !is.null(appData$hc)) {
      show_spinner()
      p <- plotClusterMetrics(appData$hc)

      hide_spinner()
      return(p)
    }
  })


  observeEvent(input$hc2peaksTable, {
    if (appData$show_plot & !is.null(appData$hc)) {

      clusters <- extractLaClusters(appData$hc, k = input$num_cluster)
      appData$stats <- appData$stats_original %>%
        left_join(clusters, by = join_by(mzIdx))
      message("Updated peak table with HC data.\n")
    }
  })

  #### save settings ####
  observeEvent(input$saveSettings, {
    saveSettings(input, filename = "settings.csv", info_state = appData$info_state)

  })

  #### download handler ####
  output$downloadPlot <- downloadHandlerPlots(res = appData$res,
                                              selected_row = input$mzTable_rows_selected[1],
                                              p_curve = p_curve,
                                              p_peak = p_peak,
                                              plot_ready = appData$show_plot)

  output$downloadTable <- downloadHandlerTable(res = appData$res,
                                               stats = appData$stats,
                                               plot_ready = appData$show_plot)

  exportTestValues(numSpec = length(appData$spec_all),
                   isSpectrumList = MALDIquant::isMassSpectrumList(appData$spec_all),
                   infoState =  appData$info_state)

  session$onSessionEnded(function() {
    stopApp()
  })
}
