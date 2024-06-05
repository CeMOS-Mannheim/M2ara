# Sever ####
server <- function(input, output, session) {
  ## load ext. functions ####
  source("functions/loadAllFunctions.R")
  loadAllFunctions()

  ## variables ####
  observe_helpers(withMathJax = TRUE)

  ## main ####
  appData <<- emptyAppDataObject()

  appData <- selectDir(appData, input)

  observeEvent(input$preproc_settings, {
    appData <- setPreprocessSettings(input, appData)
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
    appData <- loadSpectraData(input, appData)
  })

  ### process spectra ####
  observeEvent(input$process, {
    # change concentrations according to unit
    appData <- setConcentrationUnit(appData, input)

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

  ### QC tab ####
  # re-calibration check
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
      show_spinner()
      appData$clust <- clusterCurves(appData$res, nClusters = 15)
      hide_spinner()
    }
  })

  output$clustPlot <- renderPlotly({
    if (appData$show_plot & !is.null(appData$clust)) {
      show_spinner()
      plotClusters(appData$clust, k = input$num_cluster)
      hide_spinner()
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
                                              selected_row = input$mzTable_rows_selected[1],
                                              p_curve = p_curve,
                                              p_peak = p_peak,
                                              plot_ready = appData$show_plot)

  output$downloadTable <- downloadHandlerTable(res = appData$res,
                                               stats = appData$stats,
                                               plot_ready = appData$show_plot)

  exportTestValues(numSpec = length(appData$spec_all),
                   isSpectrumList = MALDIquant::isMassSpectrumList(appData$spec_all),
                   infoState =  appData$info_state,
                   pca =  appData$pca,
                   clust = appData$clust)

  session$onSessionEnded(function() {
    stopApp()
  })
}
