#### tidy up ####
# clear workspace
rm(list = ls())
gc()

#### package check on startup ####
# If a package is installed, it will be loaded. If any
# are not, the missing package(s) will be installed
# from CRAN and then loaded.

## First specify the packages of interest
packages = c("tidyverse", "tidymodels", "shiny",  "vip", "shinyFiles", "tidyverse", "MALDIquant",
             "MALDIquantForeign", "DT", "plotly", "shinycssloaders",
             "shinyhelper", "knitr", "shinybusy", "shinythemes", "shinyWidgets",
             "devtools", "ggpubr")

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

#### UI ####
ui <- fluidPage(
  title = "Peak Explorer", theme = shinytheme("flatly"),

  #### Sidebar ####

  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        shinyDirButton('dir', '  Select folder', 'Please select a folder',
                       multiple = FALSE,
                       icon = icon("search"),
                       style='padding:6px; font-size:80%',
                       class = "btn-default"),
        actionButton("load", "  Load spectra ",
                     icon = icon("upload"), style='padding:6px; font-size:80%'),
        use_busy_spinner(spin = "half-circle",
                         position = "top-left",
                         height = "100px", width = "100px",
                         margins = c(100, 100))),
      fluidRow(
        column(6,
               selectInput(inputId = "concUnits", label = "Conc. unit",
                           choices = c("M", "mM", "µM", "nM", "pM"),
                           selected = "M", multiple = FALSE, width = "80%"))
      ),

      fluidRow(
        h5("Preprocessing:") %>%
          helper(type = "markdown", content = "preprocessing"),
        column(6,
               checkboxInput("smooth", "Smooth spectra", value = TRUE)),
        column(6,
               checkboxInput("rmBl", "Remove baseline", value = TRUE))
      ),
      fluidRow(

        h5("Peak detection:") %>%
          helper(type = "markdown", content = "peakdetection"),
        column(6,
               numericInput("SNR", label = "S/N-ratio", min = 1, step = 1, value = 3)),
        column(6)
      ),
      fluidRow(
        column(6,
               radioButtons(inputId = "normMeth",
                            label = "Normalization method",
                            selected = "mz",
                            choices = c("mz", "TIC", "PQN", "median", "none")) %>%
                 helper(type = "markdown", content = "normalization")),
        column(6,
               radioButtons("VarFilterMethod", label = "Variance filtering",
                            selected = "none",
                            choices = c("mean", "median", "q25", "q75", "none")) %>%
                 helper(type = "markdown", content = "filtering"))),

      fluidRow(
        h5("m/z for recal. and mz-norm.:"),
        checkboxInput("SinglePointRecal", "Single point recal.", value = TRUE) %>%
          helper(type = "markdown", content = "recalibration"),
        column(5,
               numericInput("normTol", label = "tol. [Da]", min = 0, value = 0.1, step = 0.05)),
        column(7,
               numericInput("normMz", label = "m/z [Da]", min = 0, value = 760.585, step = 0.05))),
      fluidRow(
        h5("Aligment / binning:")  %>%
          helper(type = "markdown", content = "alignment"),
        column(6,
               numericInput("alignTol", label = "align tol. [Da]", min = 0, value = 0.01, step = 0.05) ),
        column(6,
               numericInput("binTol", label = "bin tol. [ppm]", min = 0, value = 200, step = 5))
      ),
      actionButton("process", "Process spectra",
                   icon = icon("redo", ), style='padding:6px; font-size:80%'),
      textOutput("info1", inline = FALSE),
      textOutput("info2", inline = FALSE),
      textOutput("info3", inline = FALSE),
      width = 2L),

    #### Main panel ####

    mainPanel = mainPanel(
      shiny::tabsetPanel(type = "tabs",

                         #### Main tab ####

                         tabPanel("Main",
                                  column(6,
                                         plotlyOutput('curve') %>%
                                           withSpinner(color="#0dc5c1")),
                                  column(6,
                                         plotlyOutput('peak') %>%
                                           withSpinner(color="#0dc5c1")),
                                  fluidRow(
                                    column(1,
                                           materialSwitch(
                                             inputId = "errorbars",
                                             label = "Errorbars",
                                             value = FALSE,
                                             status = "primary")
                                    ),
                                    column(1,
                                           downloadButton(outputId = "downloadPlot",
                                                          label = "Save plot",
                                                          icon = icon("download"))
                                    ),
                                    column(4),
                                    column(2,
                                           sliderInput(inputId = "zoom", label = "displayed m/z-range",
                                                       min = 0.1, max = 25, value = 4, ticks = FALSE))
                                  ),
                                  hr(),
                                  fluidRow(
                                    column(12,
                                           fluidRow(
                                             dataTableOutput("mzTable") %>%
                                               withSpinner(color="#0dc5c1")
                                           ),
                                           fluidRow(
                                             downloadButton(outputId = "downloadTable",
                                                            label = "Save as CSV",
                                                            icon = icon("download"))
                                           )
                                    )
                                  ),
                                  width = 9),

                         #### QC tab ####

                         tabPanel("QC",
                                  h4("Recalibration check"),
                                  plotlyOutput('checkRecal') %>%
                                    withSpinner(color="#0dc5c1")),

                         #### PCA tab ####

                         tabPanel("PCA",
                                  h4("PCA of single spectra"),
                                  plotlyOutput('pca') %>%
                                    withSpinner(color="#0dc5c1"),
                                  fluidRow(
                                    column(2,
                                           actionButton(inputId = "doPca",
                                                        label = "Perform PCA",
                                                        icon = icon("chart-line"))
                                    ),
                                    column(1,
                                           selectInput(inputId = "pcaX",
                                                       label = "x-axis",
                                                       choices = paste0("PC", 1:5),
                                                       multiple = FALSE,
                                                       selected = "PC1")
                                    ),
                                    column(1,
                                           selectInput(inputId = "pcaY",
                                                       label = "y-axis",
                                                       choices = paste0("PC", 1:5),
                                                       multiple = FALSE,
                                                       selected = "PC2")
                                    ),
                                    column(1,
                                           selectInput(inputId = "pcaEllipse",
                                                       label = "Ellipse size",
                                                       choices = c(0.5, 0.67, 0.95, 0.99),
                                                       multiple = FALSE,
                                                       selected = 0.67)),
                                    column(1,
                                           materialSwitch(
                                             inputId = "simpleLoadings",
                                             label = "Summarise loadings",
                                             value = FALSE,
                                             status = "primary")
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           plotlyOutput('pcaLoading1') %>%
                                             withSpinner(color="#0dc5c1")),
                                    column(6,
                                           plotlyOutput('pcaLoading2') %>%
                                             withSpinner(color="#0dc5c1"))
                                  )
                         ),
                         tabPanel("LASSO",
                                  h4("Feature importance using LASSO-model")  %>%
                                    helper(type = "markdown", content = "lasso", size = "l"),
                                  fluidRow(
                                    column(6,
                                           plotlyOutput("glmTruePred")),
                                    column(6,
                                           plotlyOutput("glmVi"))
                                  ),
                                  fluidRow(
                                    column(3,
                                           actionButton(inputId = "doGLM",
                                                        label = "Fit model",
                                                        icon = icon("chart-line"))
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           sliderInput(inputId = "penalty", label = "Log-Penalty", min = -10, max = 2, value = -5, step = 0.1)
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           actionButton(inputId = "resetPenalty",
                                                        label = "Reset penalty to tuned value",
                                                        icon = icon("chart-line"))
                                    ),
                                  )
                         ),


                         #### Manual tab ####

                         tabPanel("Manual", htmltools::includeMarkdown("manual.md")
                         )

      )

    )
  )
)

#### Sever ####

server <- function(input, output) {
  #### helpers ####

  preprocess <- function(spectra, smooth, rmBaseline,
                         smoothHalfWindowSize = 3,
                         smoothMethod = "SavitzkyGolay",
                         rmBlMethod = "TopHat") {
    nm <- names(spectra)
    if(!smooth & !rmBaseline) {
      cat("No preprocessing selected. Returning unprocessed spectra.\n")
      return(spectra)
    }

    if(smooth) {
      cat("smoothing...\n")
      spec_prc <- suppressWarnings(
        smoothIntensity(spectra,
                        method = smoothMethod,
                        halfWindowSize = smoothHalfWindowSize)
      )
    } else {
      spec_prc <- spectra
    }
    if(rmBaseline) {
      cat("removing baseline...\n")
      spec_prc <- suppressWarnings(
        removeBaseline(spec_prc,
                       method = rmBlMethod)
      )
    }

    names(spec_prc) <- nm
    return(spec_prc)
  }

  generateSpecPlots <- function(res) {
    allPeaks <- getAvgPeaks(res)
    avgSpec <- mergeMassPeaks(allPeaks)

    mzRange <- range(mass(avgSpec))
    specNames <- c("Global Average",
                   paste(as.character(unique(getConc(res))), "M"))

    l <- c(avgSpec, allPeaks)

    p <- lapply(1:length(specNames), FUN = function(i) {

      df <- tibble(mz = mass(l[[i]]),
                   int = intensity(l[[i]]))
      pspec <- ggplot(df, aes(x = mz, ymin = 0, ymax = int)) +
        geom_linerange() +
        theme_light(base_size = 14) +
        scale_x_continuous(limits = mzRange) +
        labs(x = "m/z",
             y = "Intensity",
             title = specNames[i])
      return(pspec)
    })
    return(p)
  }

  generatePCA <- function(intmat, num_PC) {
    cat("Performing PCA...\n")
    scaled <- scale(intmat)

    pca <- prcomp(scaled, rank. = num_PC)

    scores <- as_tibble(pca$x)
    loadings <- as_tibble(pca$rotation, rownames = NA)
    vars <- pca$sdev^2
    percExp<- vars/sum(vars)*100

    cat("Done!\n")
    return(list(scores = scores,
                loadings = loadings,
                explaindVar = percExp))
  }

  pcaPlot <- function(pca, conc, x, y, ellipseLevel, spots) {
    xnum <- parse_number(x)
    ynum <- parse_number(y)

    exp <- round(pca[[3]], 1)

    df <- tibble(
      xax = pca[[1]] %>% pull(xnum),
      yax = pca[[1]] %>% pull(ynum),
      c = conc,
      spot = spots
    )

    df %>%
      ggplot(aes(x = xax,
                 y = yax,
                 col = c,
                 text = spot)) +
      geom_point() +
      stat_ellipse(aes(group = c), level = ellipseLevel) +
      scale_color_viridis_d(end = 0.75, option = "C") +
      theme_light(base_size = 14) +
      labs(col = "Conc. [M]",
           x = paste0(x, " (", exp[xnum], "% expl. var.)"),
           y = paste0(y, " (", exp[ynum], "% expl. var.)"))
  }

  loadingsPlot <- function(pca, pc, simple = TRUE, n = 10) {
    pcnum <- parse_number(pc)

    df <- tibble(mz = rownames(pca[[2]]),
                 val = pca[[2]] %>% pull(pcnum)) %>%
      mutate(mz = round(as.numeric(mz), 2)) %>%
      arrange(desc(val)) %>%
      mutate(sign = ifelse(val > 0, "pos", "neg"))
    if(simple) {
      df_red <- slice_head(df, n = n) %>%
        bind_rows(slice_tail(df, n = n)) %>%
        mutate(mz = factor(mz)) %>%
        mutate(mz = fct_reorder(mz, .x = val))

      p <- df_red %>%
        ggplot(aes(x = mz,
                   y = val,
                   fill = sign)) +
        geom_col(show.legend = FALSE) +
        geom_hline(yintercept = 0, alpha = 0.75, linetype = "dashed") +
        theme_light(base_size = 14) +
        coord_flip() +
        labs(x = "m/z [Da]",
             y = paste0(pc, " Loading")) +
        theme(legend.position = "none")

      return(p)
    } else {
      p <- df %>%
        ggplot(aes(x = mz,
                   y = val,
                   col = sign)) +
        geom_linerange(aes(ymin = 0, ymax = val), show.legend = FALSE) +
        theme_light(base_size = 14) +
        labs(x = "m/z [Da]",
             y = paste0(pc, " Loading")) +
        theme(legend.position = "none")
      return(p)
    }
  }

  fitGLM <- function(res) {

    df <- getSinglePeaks(res) %>%
      intensityMatrix() %>%
      as_tibble(intmat) %>%
      mutate(conc = getConc(res))

    rec <- recipe(df, conc ~.) %>%
      step_log(all_outcomes(), base = 10) %>%
      step_normalize(all_predictors()) %>%
      step_YeoJohnson(all_predictors()) %>%
      step_corr(all_predictors())

    df_rdy <- prep(rec) %>%
      bake(new_data = NULL)

    glm <- linear_reg(penalty = tune(), mixture = 1) %>%
      set_engine("glmnet")

    tune_rs <- tune_grid(object = glm,
                         preprocessor = rec,
                         resamples = bootstraps(df,
                                                times = 5,
                                                strata = "conc",
                                                pool = 0.25),
                         grid = grid_regular(penalty(range = c(-5,0)),  levels = 10))

    best_penalty <- select_by_one_std_err(tune_rs, desc(penalty), metric = "rsq")

    return(list(model = glm,
                prepData = df_rdy,
                penalty = pull(best_penalty, penalty)))
  }

  getVolumes <- function(exclude = NULL) {
    osSystem <- Sys.info()["sysname"]
    if (osSystem == "Darwin") {
      volumes <- dir_ls("/Volumes")
      names(volumes) <- basename(volumes)
    }
    else if (osSystem == "Linux") {
      volumes <- c(Computer = "/")
      if (isTRUE(dir_exists("/media"))) {
        media <- dir_ls("/media")
        names(media) <- basename(media)
        volumes <- c(volumes, media)
      }
    }
    else if (osSystem == "Windows") {
      wmic <- paste0(Sys.getenv("SystemRoot"), "\\System32\\Wbem\\WMIC.exe")
      if (!file.exists(wmic)) {
        message("\nThe wmic program does not seem to be in the default location")
        message("Please report this problem and include output from the command")
        message("'where wmic' to https://github.com/thomasp85/shinyFiles/issues")
        volumes <- Sys.getenv("HOMEDRIVE")
        volNames <- ""
      }
      else {
        volumes <- system(paste(wmic, "logicaldisk get Caption"),
                          intern = TRUE, ignore.stderr = TRUE)
        volumes <- sub(" *\\r$", "", volumes)
        keep <- !tolower(volumes) %in% c("caption", "")
        volumes <- volumes[keep]
        volNames <- system(paste(wmic, "/FAILFAST:1000 logicaldisk get VolumeName"),
                           intern = TRUE, ignore.stderr = TRUE)
        volNames <- sub(" *\\r$", "", volNames)
        volNames <- volNames[keep]
        volNames <- paste0(volNames, ifelse(volNames ==
                                              "", "", " "))
      }
      volNames <- paste0(volNames, "(", volumes, ")")
      names(volumes) <- volNames
      #volumes <- gsub(":$", ":/", volumes)
    }
    else {
      stop("unsupported OS")
    }
    if (!is.null(exclude)) {
      volumes <- volumes[!names(volumes) %in% exclude]
    }
    volumes
  }



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

  roots <- c(c = "c:", d = "d:", e = "e:",f = "f:",
             x = "x:", y = "y:", z = "z:")

  #### main #####
  RV <<- reactiveValues(res = NULL,
                        stats = NULL,
                        specIdx = 1,
                        maxSpecIdx = 1,
                        pspec = NULL,
                        pca = NULL,
                        model = NULL)

  vol <- tolower(getVolumes())
  names(vol) <- str_remove(vol, ":")
  #### choose dir ####
  shinyDirChoose(input,
                 'dir',
                 roots = roots,
                 allowDirCreate = FALSE, defaultRoot = names(vol)[1])

  observeEvent(input$dir, {
    # check if folder was selected
    # prepare info massage
    selected_dir <<- parseDirPath(roots, input$dir)
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
      spec_raw <- loadSpectra(selected_dir)
      info_state("loaded")
      cat("check for empty spectra...\n")
      conc <- names(spec_raw)
      peaks <- detectPeaks(spec_raw, SNR = input$SNR, method = "MAD")
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
        mutate(mz = round(as.numeric(mz), 3)) %>%
        group_by(mz, mzIdx) %>%
        summarise(
          pIC50 = first(pIC50),
          R2 = first(R2),
          wgof = first(wgof),
          min = min(mean),
          max = max(mean),
          log2FC = log2(first(fc_window))
        ) %>%
        left_join(getFittingParameters(res, summarise = TRUE)) %>%
        mutate(symetric = ifelse(npar < 5, TRUE, FALSE)) %>%
        select(-npar) %>%
        ungroup() %>%
        mutate_if(is.numeric, function(x)
        {
          round(x, 2)
        })

      RV <<- reactiveValues(res = res,
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

  #### data table ####
  # first initialization
  output$mzTable <- DT::renderDataTable({
    # check if data is already prepared and if not show dummy table
    if(show_plot() == "TRUE") {
      tableData <- RV$stats %>%
        select(-mzIdx)
    } else {
      tableData <- tibble(mz = c("load", rep("", 25)),
                          mzIdx = c("data", rep("", 25)),
                          pIC50 = c("to", rep("", 25)),
                          R2 = c("display", rep("", 25)),
                          wgof = c("peak", rep("", 25)),
                          log2FC = c("table", rep("", 25)))
    }

    tableData

  },
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
        RV$stats
      }
    },
    server = TRUE,
    filter = "bottom",
    options = list(searching = TRUE,
                   lengthChange = FALSE,
                   paging = TRUE),
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
      p_peak <- ggplot(tibble(label = "Load data\nto display plot",
                              x = 1,
                              y = 1),
                       aes(x = x, y = y, label = label)) +
        geom_text(size = 5) +
        theme_light(base_size = 14)
      ggplotly(p_peak)
    }

  })

  #### QC tab: recal check ####
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
      RV$pca <- generatePCA(intensityMatrix(getSinglePeaks(RV$res)),
                            num_PC = 20)
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
    if(show_plot() == "TRUE") {
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
    if(show_plot() == "TRUE") {
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



  #### LASSO tab #####

  output$glmTruePred <- renderPlotly({
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
      RV$model <- fitGLM(RV$res)
      updateSliderInput(inputId = "penalty", value = log10(RV$model$penalty))
      cat("model fitted.\n")
      hide_spinner()
      output$glmTruePred <- renderPlotly({
        p <- RV$model$model %>%
          finalize_model(tibble(penalty = 10^input$penalty)) %>%
          fit(data = RV$model$prepData, formula = conc ~.) %>%
          predict(RV$model$prepData) %>%
          mutate(truth = pull(RV$model$prepData, conc)) %>%
          ggplot(aes(x = .pred, y = truth)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm") +
          ggpubr::stat_regline_equation(aes(label = paste0("R²adj.=", ..adj.rr..))) +
          coord_obs_pred() +
          labs(x = "Log-predicted conc.",
               y = "Log-true conc.") +
          theme_minimal(base_size = 14)

        return(ggplotly(p))
      })


      observeEvent(input$penalty, {
        output$glmVi <- renderPlotly({

          vi <- RV$model$model %>%
            finalize_model(tibble(penalty = 10^input$penalty)) %>%
            fit(data = RV$model$prepData, formula = conc ~.) %>%
            vip::vi_model(lambda = 10^input$penalty)

          NumNonZeoroCoef <- vi %>%
            filter(Importance > 0) %>%
            pull(Importance) %>%
            length()

          p <- vi  %>%
            arrange(desc(Importance)) %>%
            slice_head(n = 20) %>%
            mutate(Variable = round(readr::parse_number(Variable), 3)) %>%
            mutate(imp = ifelse(Sign == "POS", Importance, -Importance)) %>%
            mutate(Variable = fct_reorder(as.factor(Variable), imp)) %>%
            ggplot(aes(x = Variable, y = imp, fill = Sign)) +
            geom_col() +
            coord_flip() +
            geom_text(aes(x = 1, y = 0, label = paste0("# non-zero coef. features =", NumNonZeoroCoef))) +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none") +
            labs(x = "m/z",
                 y = "Variable importance")
          return(ggplotly(p))
        })
      })




    }
  })

  observeEvent(input$resetPenalty, {
    cat("button pressed\n")
    if(!is.null(RV$model)) {
      cat("RV$model was different from NULL\n")
      updateSliderInput(inputId = "penalty", value = log10(RV$model$penalty))
    } else {
      cat("RV$model was NULL\n")
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
