library(shiny)
library(shinycssloaders)
library(tidyverse)
library(psych)
library(grid)
library(gridExtra)
library(Hmisc)
library(GPArotation)

ui <- fluidPage(
  titlePanel(
    "Exploratory Factor Analysis"
  ),
  sidebarLayout(
    sidebarPanel(
      # tags$head(tags$style(type="text/css", "
      #        #loadmessage {
      #                      position: fixed;
      #                      top: 0px;
      #                      left: 0px;
      #                      width: 100%;
      #                      padding: 5px 0px 5px 0px;
      #                      text-align: center;
      #                      font-weight: bold;
      #                      font-size: 100%;
      #                      color: #000000;
      #                      z-index: 105;
      #                      }
      #                      ")),
      # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
      #                  tags$div("Loading... Please wait.",id="loadmessage")),
      fileInput("fileinput", label = "Import data file (.csv, .sav):"),
      uiOutput("mainInput")
    ),
    mainPanel(
      uiOutput("mainOutput")
    )
  )
)

server <- function(input, output, session) {
  filedata <- reactive({
    rdata <- input$fileinput
    path <- rdata$datapath
    
    if (is.null(rdata)){
      return(NULL)
    } else if (substr(path, nchar(path) - 2, nchar(path)) == 'csv') {
      read.csv(path, stringsAsFactors = FALSE)
    } else if (substr(path, nchar(path) - 2, nchar(path)) == 'sav') {
      as.data.frame(foreign::read.spss(path))
    }
  })
  
  output$mainOutput <- renderUI({
    df <- filedata()
    if(is.null(df)){
      return(NULL)
    } else {
      tabsetPanel(id = "outputTabs",
                  tabPanel("Data Table",
                           dataTableOutput("dataView"))
      )
    }
  })
  
  output$dataView <- renderDataTable(
    filedata(),
    options = list(pageLength = 10))
  
  output$mainInput <- renderUI({
    df <- filedata()
    if (is.null(df)){
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel("Variables",
                 p(),
                 selectInput("select_vars", label = "Select Variables",
                             choices = colnames(df), multiple = TRUE),
                 fluidRow(
                   column(5, actionButton("select_all", label = "Select All")),
                   column(5, actionButton("select_none", label = "Select None"))
                 )),
        tabPanel("Parallel Analysis",
                 p(),
                 numericInput('pa_niter', label = "Number of simulated analyses to perform:",
                              min = 0, max = 500, step = 10, value = 100),
                 numericInput('pa_quant', label = "Quantile of simulated/resampled eigenvalues:",
                              min = 0, max = 1, value = .95, step = .01),
                 helpText('(If 0, the mean of the simulated/resampled eigenvalues is used.)'),
                 radioButtons('pa_vartype', label = 'Type of variables:',
                              choices = c('Continuous' = 1,
                                          'Categorical' = 2)),
                 actionButton("run_pa", label = 'Run Parallel Analysis')),
        tabPanel("Extraction",
                 p(),
                 selectInput('extraction_methods', label = "Method:",
                             choices = c("Principal Component Analysis",
                                         "Exploratory Factor Analysis"),
                             selected = "Exploratory Factor Analysis"),
                 uiOutput('method_options'))
      )
    }
  })
  
  observeEvent(input$select_all, {
    updateSelectInput(session, inputId = "select_vars",
                      selected = colnames(filedata()))
  })
  
  observeEvent(input$select_none, {
    updateSelectInput(session, inputId = "select_vars",
                      selected = "")
  })
  
  output$method_options <- renderUI({
    if (input$extraction_methods == "Principal Component Analysis"){
      tagList(
        numericInput('pca_nfactor', label = "Number of factors to extract:", value = 1,
                     min = 1, step = 1),
        selectInput('pca_matrix', label = "Analyze:",
                    choices = c("Pearson correlation matrix" = "cor",
                                "Tetrachoric correlation matrix" = "tet",
                                "Polychoric correlation matrix" = "poly")),
        selectInput('pca_rotation', label = "Rotation method:",
                    choices = c("None" = "none",
                                "Varimax" = "varimax",
                                "Promax" = "promax",
                                "Oblimin" = "oblimin")),
        actionButton('pca_run', 'Run PCA')
      )
    } else {
      tagList(
        numericInput('efa_nfactor', label = "Number of factors to extract:", value = 1,
                     min = 1, step = 1),
        selectInput('efa_matrix', label = "Analyze:",
                    choices = c("Pearson correlation matrix" = "cor",
                                "Tetrachoric correlation matrix" = "tet",
                                "Polychoric correlation matrix" = "poly")),
        selectInput('efa_rotation', label = "Rotation method:",
                    choices = c("None" = "none",
                                "Varimax" = "varimax",
                                "Promax" = "promax",
                                "Oblimin" = "oblimin")),
        selectInput('efa_method', label = "Factoring method:",
                    choices = c("Maximum likelihood factor analysis" = "ml",
                                "Weighted least squares solution" = "wls",
                                "Generalized weighted least squares" = "gls",
                                "Principal factor solution" = "pa"),
                    selected = "pa"),
        actionButton('efa_run', 'Run EFA')
      )
    }
  })
  
  observeEvent(input$run_pa, {
    if (length(input$select_vars) < 2){
      showNotification("At least two variables must be selected.", action = NULL,
                       duration = 5, closeButton = TRUE,
                       id = NULL, type = 'warning',
                       session = session)
    } else {
      removeTab(inputId = 'outputTabs', target = 'Parallel Analysis')
      appendTab(inputId = 'outputTabs', select = TRUE,
                tab = {
                  tabPanel('Parallel Analysis',
                           tags$head(tags$style(type='text/css',
                                                '#pa_summary {font-size: 18px}',
                                                '#pa_eigen {font-size: 18px}')),
                           h3('Parallel Analysis Summary'),
                           verbatimTextOutput('pa_summary'),
                           h3('Eigenvalues'),
                           verbatimTextOutput('pa_eigen'),
                           h3('Scree Plots'),
                           withSpinner(plotOutput('pa_scree')))
                })
    }
  })
  
  pa_results <- eventReactive(input$run_pa, {
    if (length(input$select_vars) < 2){
      return(NULL)
    } else {
      df <- filedata()[input$select_vars]
      if (input$pa_vartype == 1) {
        pa_results <- fa.parallel(df, n.iter = input$pa_niter, quant = input$pa_quant,
                                  fm = 'ml', plot = FALSE)
      } else {
        pa_results <- fa.parallel(df, n.iter = input$pa_niter, quant = input$pa_quant,
                                  cor = 'poly', fm = 'ml', plot = FALSE)
      }
    }
  })
  
  # observeEvent(input$run_pa, {
  #   showModal(modalDialog(p("Running paralell analysis. This might take a while."),
  #                         title = "Calculating...", easyClose = FALSE,
  #                         footer = NULL))
  #   pa_results()
  #   removeModal()
  # })
  
  output$pa_summary <- renderPrint({
    pa_results()
  })
  
  output$pa_eigen <- renderPrint({
    round(data.frame('Original factors' = pa_results()$fa.value,
                     'Simulated data' = pa_results()$fa.sim,
                     'Original components' = pa_results()$pc.value,
                     'Simulated data' = pa_results()$pc.sim), 2)
  })
  
  output$pa_scree <- renderPlot({
    df1 <- data.frame('PCv' = pa_results()$pc.value,
                      'PCs' = pa_results()$pc.sim,
                      'PCsr' = pa_results()$pc.simr,
                      'ncomp' = seq(length(pa_results()$fa.values)))
    
    df2 <- data.frame('FAv' = pa_results()$fa.value,
                      'FAs' = pa_results()$fa.sim,
                      'FAsr' = pa_results()$fa.simr,
                      'ncomp' = seq(length(pa_results()$fa.values)))
    
    p1 <- df1 %>% gather(key = "type", value = 'val', 1:3) %>% 
      mutate(type = factor(type, levels = c('PCv', 'PCs', "PCsr"),
                           labels = c('PC Actual Data',
                                      'PC Simulated Data',
                                      'PC Resampled Data'))) %>%
      ggplot() +
      geom_line(aes(x = ncomp, y = val, col = type, linetype = type)) +
      geom_point(aes(x = ncomp, y = val, col = type)) +
      labs(title = 'Parallel Analysis (PC) Scree Plot',
           y = 'Eigenvales',
           x = 'Component Number',
           color = 'Legend',
           linetype = 'Legend') +
      scale_x_continuous(breaks = 1:nrow(df1), labels = 1:nrow(df1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
    
    p2 <- df2 %>% gather(key = "type", value = 'val', 1:3) %>% 
      mutate(type = factor(type, levels = c('FAv', 'FAs', "FAsr"),
                           labels = c('FA Actual Data',
                                      'FA Simulated Data',
                                      'FA Resampled Data'))) %>%
      ggplot() +
      geom_line(aes(x = ncomp, y = val, col = type, linetype = type)) +
      geom_point(aes(x = ncomp, y = val, col = type)) +
      labs(title = 'Parallel Analysis (FA) Scree Plot',
           y = 'Eigenvales',
           x = 'Factor Number',
           color = 'Legend',
           linetype = 'Legend') +
      scale_x_continuous(breaks = 1:nrow(df2), labels = 1:nrow(df2)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
    
    grid.arrange(p2, p1, ncol = 2)
    
  })
  
  observeEvent(input$pca_run, {
    if (length(input$select_vars) < 2){
      showNotification("At least two variables must be selected.", action = NULL,
                       duration = 5, closeButton = TRUE,
                       id = NULL, type = 'warning',
                       session = session)
    } else {
      removeTab(inputId = 'outputTabs', target = 'PCA')
      appendTab(inputId = 'outputTabs', select = TRUE,
                tab = {
                  tabPanel('PCA',
                           tags$head(tags$style(type='text/css',
                                                '#pca_spec {font-size: 18px}',
                                                '#kmo_bartlett {font-size: 18px}',
                                                '#pca_variance {font-size: 18px}',
                                                '#pca_loadings {font-size: 18px}',
                                                '#pca_pattern {font-size: 18px}',
                                                '#pca_structure {font-size: 18px}',
                                                '#pca_correlation {font-size: 18px}',
                                                '#pca_uniqueness {font-size: 18px}')),
                           fluidRow(
                             column(8, h2('Principal Component Analysis')),
                             column(4, align = 'right',
                                    downloadButton('download_pca', 'Save summary'))
                           ),
                           verbatimTextOutput('pca_spec'),
                           h3('KMO and Bartlett\'s Test'),
                           verbatimTextOutput('kmo_bartlett'),
                           h3('Total Variance Explained'),
                           verbatimTextOutput('pca_variance'),
                           if (input$pca_rotation %in% c('none', 'varimax')){
                             tagList(
                               h3('Factor Loadings'),
                               verbatimTextOutput('pca_loadings')
                             )
                           } else {
                             tagList(
                               h3('Factor Pattern Matrix'),
                               verbatimTextOutput('pca_pattern'),
                               h3('Factor Structure Matrix'),
                               verbatimTextOutput('pca_structure'),
                               h3('Factor Corrleation Matrix'),
                               verbatimTextOutput('pca_correlation')
                             )
                           },
                           h3('Communality/Uniqueness'),
                           verbatimTextOutput('pca_uniqueness'),
                           h3('Scree Plot'),
                           withSpinner(plotOutput('pca_scree')))
                })
    }
  })
  
  PCA_results <- eventReactive(input$pca_run, {
    if (length(input$select_vars) < 2){
      return(NULL)
    } else {
      df <- filedata()[input$select_vars]
      if (input$pca_matrix == 'cor'){
        results <- principal(df, nfactors = input$pca_nfactor,
                             rotate = input$pca_rotation)
      } else if (input$pca_matrix == 'tet'){
        tet <- tetrachoric(df)
        results <- principal(tet$rho, nfactors = input$pca_nfactor,
                             rotate = input$pca_rotation)
      } else {
        po <- polychoric(df)
        results <- principal(po$rho, nfactors = input$pca_nfactor,
                             rotate = input$pca_rotation)
      }
      
    }
  })
  
  # observeEvent(input$pca_run, {
  #   showModal(modalDialog(p("Running PCA. This might take a while."),
  #                         title = "Calculating...", easyClose = FALSE,
  #                         footer = NULL))
  #   PCA_results()
  #   removeModal()
  # })
  
  PCA_spec <- eventReactive(input$pca_run, {
    n1 <- paste('Number of factors:', input$pca_nfactor)
    n2 <- paste('Correlation:', switch(input$pca_matrix,
                                       'cor' = 'Pearson',
                                       'tet' = 'Tetrachoric',
                                       'poly' = 'Polychoric'))
    n3 <- paste('Rotation method:', capitalize(input$pca_rotation))
    c(n1, n2, n3)
  })
  
  output$pca_spec <- renderPrint({
    writeLines(paste(PCA_spec(), collapse = '\n'))
  })
  
  kmo_bartlett <- eventReactive(input$pca_run, {
    df <- filedata()[input$select_vars]
    sample_size <- nrow(df)
    if (input$pca_matrix == 'cor'){
      cor_matrix <- cor(df)
    } else if (input$pca_matrix == 'tet'){
      cor_matrix <- tetrachoric(df)$rho
    } else {
      cor_matrix <- polychoric(df)$rho
    }
    kmo <- round(KMO(df)$MSA, 2)
    bart <- cortest.bartlett(cor_matrix, sample_size)
    if (round(bart$p.value, 3) == 0){
      p.value <- '<.001'
    } else {
      p.value <- round(bart$p.value, 3)
    }
    n1 <- paste('Kaiser_Meyer_Olkin Measure of Sampling Adequacy:', kmo)
    n2 <- paste('Bartlett\'s Test of Sphericity      - Chi square:',
                round(bart$chisq, 2))
    n3 <- paste('                                   - p.value   :',
                p.value)
    n4 <- paste('                                   - df        :',
                round(bart$df, 2))
    c(n1,' ',n2, n3, n4)
  })
  
  output$kmo_bartlett <- renderPrint({
    writeLines(paste(kmo_bartlett(), collapse = '\n'))
  })
  
  PCA_variance <- eventReactive(input$pca_run, {
    eigen <- PCA_results()$values[1:input$pca_nfactor]
    round(data.frame('Eigenvalue' = eigen,
                     'Prop_Explained' = eigen / sum(eigen),
                     'Cum_Explained' = cumsum(eigen / sum(eigen))),3)
  })
  
  output$pca_variance <- renderPrint({
    PCA_variance()
  })
  
  PCA_loadings <- eventReactive(input$pca_run, {
    if (isolate(input$pca_rotation %in% c('none', 'varimax'))){
      loadings <- PCA_results()$loadings
      if (isolate(input$pca_nfactor == 1)) {
        round(data.frame('PC1' = loadings[1:length(isolate(input$select_vars)), 1]),3)
      } else {
        round(loadings[1:length(isolate(input$select_vars)), 1:isolate(input$pca_nfactor)], 3)
      }
    } else {
      return(NULL)
    }
  })
  
  output$pca_loadings <- renderPrint({
    PCA_loadings()
  })
  
  PCA_pattern <- eventReactive(input$pca_run, {
    if (isolate(input$pca_rotation %in% c('none', 'varimax'))){
      return(NULL)
    } else {
      pattern <- PCA_results()$loadings
      if (isolate(input$pca_nfactor == 1)) {
        round(data.frame('PC1' = pattern[1:length(isolate(input$select_vars)), 1]),3)
      } else {
        round(pattern[1:length(isolate(input$select_vars)), 1:isolate(input$pca_nfactor)], 3)
      }
    }
  })
  
  output$pca_pattern <- renderPrint({
    PCA_pattern()
  })
  
  PCA_structure <- eventReactive(input$pca_run, {
    if (isolate(input$pca_rotation %in% c('none', 'varimax'))){
      return(NULL)
    } else {
      structure <- PCA_results()$Structure
      if (isolate(input$pca_nfactor == 1)) {
        round(data.frame('PC1' = structure[1:length(isolate(input$select_vars)), 1]),3)
      } else {
        round(structure[1:length(isolate(input$select_vars)), 1:isolate(input$pca_nfactor)], 3)
      }
    }
  })
  
  output$pca_structure <- renderPrint({
    PCA_structure()
  })
  
  PCA_correlation <- eventReactive(input$pca_run, {
    PCA_results()$Phi
  })
  
  output$pca_correlation <- renderPrint({
    PCA_correlation()
  })
  
  PCA_uniqueness <- eventReactive(input$pca_run, {
    commu <- PCA_results()$communality
    uniq <- PCA_results()$uniqueness
    data.frame('Communality' = commu,
               'Uniqueness' = uniq)
  })
  
  output$pca_uniqueness <- renderPrint({
    PCA_uniqueness()
  })
  
  output$pca_scree <- renderPlot({
    values <- PCA_results()$values
    df <- data.frame("Component" = seq(length(values)),
                     "Eigenvalue" = values)
    
    ggplot(df, aes(x = Component, y = Eigenvalue)) +
      geom_point(size = 3) +
      geom_path(size = 1.2) +
      labs(y = "Eigenvalues",
           x = "Components",
           title = "Scree plot") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,df$Eigenvalue[1]*1.1)) +
      scale_x_continuous(breaks = seq(nrow(df))) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
  })
  
  output$download_pca <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      chars <- c('Principal Component Analysis', '',
                 PCA_spec()[1], PCA_spec()[2], PCA_spec()[3], '',
                 'Total Variance Explained', '',
                 'KMO and Bartlett\'s Test',
                 kmo_bartlett()[1], kmo_bartlett()[3],
                 kmo_bartlett()[4], kmo_bartlett()[5],'',
                 capture.output(PCA_variance()))
      if (!is.null(PCA_loadings())) {
        chars <- c(chars, '','Factor Loadings', '',
                   capture.output(PCA_loadings()))
      }
      if (!is.null(PCA_pattern())){
        chars <- c(chars, '', 'Factor Pattern Matrix', '',
                   capture.output(PCA_pattern()))
      }
      if (!is.null(PCA_structure())){
        chars <- c(chars, '', 'Factor Structure Matrix', '',
                   capture.output(PCA_structure()))
      }
      if (!is.null(PCA_correlation())){
        chars <- c(chars, '', 'Factor Correlation Matrix', '',
                   capture.output(PCA_correlation()))
      }
      if (!is.null(PCA_uniqueness())){
        chars <- c(chars, '', 'Communality/Uniqueness', '',
                   capture.output(PCA_uniqueness()))
      }
      writeLines(chars, file, sep = "\r\n")
    }
  )
  
  observeEvent(input$efa_run, {
    if (length(input$select_vars) < 2){
      showNotification("At least two variables must be selected.", action = NULL,
                       duration = 5, closeButton = TRUE,
                       id = NULL, type = 'warning',
                       session = session)
    } else {
      removeTab(inputId = 'outputTabs', target = 'EFA')
      appendTab(inputId = 'outputTabs', select = TRUE,
                tab = {
                  tabPanel('EFA',
                           tags$head(tags$style(type='text/css',
                                                '#efa_spec {font-size: 18px}',
                                                '#efa_kmo_bartlett {font-size: 18px}',
                                                '#efa_variance {font-size: 18px}',
                                                '#efa_loadings {font-size: 18px}',
                                                '#efa_pattern {font-size: 18px}',
                                                '#efa_structure {font-size: 18px}',
                                                '#efa_correlation {font-size: 18px}',
                                                '#efa_uniqueness {font-size: 18px}')),
                           fluidRow(
                             column(8, h2('Exploratory Component Analysis')),
                             column(4, downloadButton('download_efa', 'Save summary'),
                                    align = 'right')),
                           verbatimTextOutput('efa_spec'),
                           h3('KMO and Bartlett\'s Test'),
                           verbatimTextOutput('efa_kmo_bartlett'),
                           h3('Total Variance Explained'),
                           verbatimTextOutput('efa_variance'),
                           if (input$efa_rotation %in% c('none', 'varimax')){
                             tagList(
                               h3('Factor Loadings'),
                               verbatimTextOutput('efa_loadings')
                             )
                           } else {
                             tagList(
                               h3('Factor Pattern Matrix'),
                               verbatimTextOutput('efa_pattern'),
                               h3('Factor Structure Matrix'),
                               verbatimTextOutput('efa_structure'),
                               h3('Factor Corrleation Matrix'),
                               verbatimTextOutput('efa_correlation')
                             )
                           },
                           h3('Communality/Uniqueness'),
                           verbatimTextOutput('efa_uniqueness'),
                           h3('Scree Plot'),
                           withSpinner(plotOutput('efa_scree')))
                })
    }
  })
  
  EFA_results <- eventReactive(input$efa_run, {
    if (length(input$select_vars) < 2){
      return(NULL)
    } else {
      df <- filedata()[input$select_vars]
      results <- fa(df, nfactors = input$efa_nfactor, rotate = input$efa_rotation,
                    fm = input$efa_method, cor = input$efa_matrix)
    }
  })
  
  # observeEvent(input$efa_run, {
  #   showModal(modalDialog(p("Running EFA. This might take a while."),
  #                         title = "Calculating...", easyClose = FALSE,
  #                         footer = NULL))
  #   EFA_results()
  #   removeModal()
  # })
  
  EFA_spec <- eventReactive(input$efa_run, {
    n1 <- paste('Number of factors:', input$efa_nfactor)
    n2 <- paste('Rotation method:', capitalize(input$efa_rotation))
    n3 <- paste('Correlation:', switch(input$efa_matrix,
                                   'cor' = 'Pearson',
                                   'tet' = 'Tetrachoric',
                                   'poly' = 'Polychoric'))
    n4 <- paste('Estimation Method:', switch(input$efa_method,
                                             'ml' = "Maximum likelihood",
                                             'wls' = "Weighted least squares",
                                             'gls' = "Generalized weighted least squares",
                                             'pa' = 'Principal factor solution'))
    c(n1, n2, n3, n4)
  })
  
  output$efa_spec <- renderPrint({
    writeLines(paste(EFA_spec(), collapse = '\n'))
  })
  
  efa_kmo_bartlett <- eventReactive(input$efa_run, {
    df <- filedata()[input$select_vars]
    sample_size <- nrow(df)
    if (input$efa_matrix == 'cor'){
      cor_matrix <- cor(df)
    } else if (input$efa_matrix == 'tet'){
      cor_matrix <- tetrachoric(df)$rho
    } else {
      cor_matrix <- polychoric(df)$rho
    }
    kmo <- round(KMO(df)$MSA, 2)
    bart <- cortest.bartlett(cor_matrix, sample_size)
    if (round(bart$p.value, 3) == 0){
      p.value <- '<.001'
    } else {
      p.value <- round(bart$p.value, 3)
    }
    n1 <- paste('Kaiser_Meyer_Olkin Measure of Sampling Adequacy:', kmo)
    n2 <- paste('Bartlett\'s Test of Sphericity      - Chi square:',
                round(bart$chisq, 2))
    n3 <- paste('                                   - p.value   :',
                p.value)
    n4 <- paste('                                   - df        :',
                round(bart$df, 2))
    c(n1,' ',n2, n3, n4)
  })
  
  output$efa_kmo_bartlett <- renderPrint({
    writeLines(paste(efa_kmo_bartlett(), collapse = '\n'))
  })
  
  EFA_variance <- eventReactive(input$efa_run, {
    eigen <- EFA_results()$values[1:isolate(input$efa_nfactor)]
    round(data.frame('Eigenvalue' = eigen,
                     'Prop_Explained' = eigen / length(input$select_vars),
                     'Cum_Explained' = cumsum(eigen / length(input$select_vars))),3)
  })
  
  output$efa_variance <- renderPrint({
    EFA_variance()
  })
  
  EFA_loadings <- eventReactive(input$efa_run, {
    if (isolate(input$efa_rotation %in% c('none', 'varimax'))){
      loadings <- EFA_results()$loadings
      if (isolate(input$efa_nfactor == 1)) {
        round(data.frame('PC1' = loadings[1:length(isolate(input$select_vars)), 1]),3)
      } else {
        round(loadings[1:length(isolate(input$select_vars)), 1:isolate(input$efa_nfactor)], 3)
      }
    } else {
      return(NULL)
    }
  })
  
  output$efa_loadings <- renderPrint({
    EFA_loadings()
  })
  
  EFA_pattern <- eventReactive(input$efa_run, {
    if (isolate(input$efa_rotation %in% c('none', 'varimax'))){
      return(NULL)
    } else {
      pattern <- EFA_results()$loadings
      if (isolate(input$efa_nfactor == 1)) {
        round(data.frame('PC1' = pattern[1:length(isolate(input$select_vars)), 1]),3)
      } else {
        round(pattern[1:length(isolate(input$select_vars)), 1:isolate(input$efa_nfactor)], 3)
      }
    }
  })
  
  output$efa_pattern <- renderPrint({
    EFA_pattern()
  })
  
  EFA_structure <- eventReactive(input$efa_run, {
    if (isolate(input$efa_rotation %in% c('none', 'varimax'))){
      return(NULL)
    } else {
      structure <- EFA_results()$Structure
      if (isolate(input$efa_nfactor == 1)) {
        round(data.frame('PC1' = structure[1:length(isolate(input$select_vars)), 1]),3)
      } else {
        round(structure[1:length(isolate(input$select_vars)), 1:isolate(input$efa_nfactor)], 3)
      }
    }
  })
  
  output$efa_structure <- renderPrint({
    EFA_structure()
  })
  
  EFA_correlation <- eventReactive(input$efa_run, {
    EFA_results()$Phi
  })
  
  output$efa_correlation <- renderPrint({
    EFA_correlation()
  })
  
  EFA_uniqueness <- eventReactive(input$efa_run, {
    commu <- EFA_results()$communality
    uniq <- EFA_results()$uniqueness
    data.frame('Communality' = commu,
               'Uniqueness' = uniq)
  })
  
  output$efa_uniqueness <- renderPrint({
    EFA_uniqueness()
  })
  
  output$efa_scree <- renderPlot({
    values <- EFA_results()$e.values
    df <- data.frame("Component" = seq(length(values)),
                     "Eigenvalue" = values)
    
    ggplot(df, aes(x = Component, y = Eigenvalue)) +
      geom_point(size = 3) + 
      geom_path(size = 1.2) +
      labs(y = "Eigenvalues",
           x = "Components",
           title = "Scree plot") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,df$Eigenvalue[1]*1.1)) +
      scale_x_continuous(breaks = seq(nrow(df))) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
  })
  
  output$download_efa <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      chars <- c('Exploratory Factor Analysis', '',
                 EFA_spec()[1], EFA_spec()[2], 
                 EFA_spec()[3], EFA_spec()[4], '',
                 'KMO and Bartlett\'s Test',
                 efa_kmo_bartlett()[1], efa_kmo_bartlett()[3],
                 efa_kmo_bartlett()[4], efa_kmo_bartlett()[5],'',
                 'Total Variance Explained', '',
                 capture.output(EFA_variance()))
      if (!is.null(EFA_loadings())) {
        chars <- c(chars, '','Factor Loadings', '',
                   capture.output(EFA_loadings()))
      }
      if (!is.null(EFA_pattern())){
        chars <- c(chars, '', 'Factor Pattern Matrix', '',
                   capture.output(EFA_pattern()))
      }
      if (!is.null(EFA_structure())){
        chars <- c(chars, '', 'Factor Structure Matrix', '',
                   capture.output(EFA_structure()))
      }
      if (!is.null(EFA_correlation())){
        chars <- c(chars, '', 'Factor Correlation Matrix', '',
                   capture.output(EFA_correlation()))
      }
      if (!is.null(EFA_uniqueness())){
        chars <- c(chars, '', 'Communality/Uniqueness', '',
                   capture.output(EFA_uniqueness()))
      }
      writeLines(chars, file, sep = "\r\n")
    }
  )
}

shinyApp(ui = ui, server = server)
