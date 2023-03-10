library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(plotly)

options(shiny.maxRequestSize = 60 * 1024 ^ 2)

summaryColNames <- c(
    "Sample",
    "Tissue",
    "Peak Group",
    "Labeled Element",
    "Animal",
    "Genotype",
    "Age (weeks)",
    "Sex",
    "Diet",
    "Feeding Status",
    "Treatment",
    "Studies",
    "Infusate",
    "Tracer Compound"
  )

numericColNames <- c(
  "Total Abundance",
  "Enrichment Fraction",
  "Enrichment Abundance",
  "Normalized Labeling",
  "Body Weight (g)",
  "Infusion Rate (ul/min/g)",
  "Average Ra (nM/m/g)",
  "Average Rd (nM/m/g)",
  "Average Ra (nM/m)",
  "Average Rd (nM/m)",
  "Intact Ra (nM/m/g)",
  "Intact Rd (nM/m/g)",
  "Intact Ra (nM/m)",
  "Intact Rd (nM/m)",
  "Tracer Concentration (mM)",
  "Time Collected (m)"
)

function(input, output, session) {
  
  # The getHeader reactive function parses the second header
  # line of the input file from Tracebase and extracts the
  #'selectedtemplate' value. Options are 'pgtemplate' for
  #'PeakGroups, 'pdtemplate' for PeakData or 'fctemplate'
  # for FCirc outputs. Output type is stored as a string
  
  output$getHeader <- renderText({
    if (is.null(input$fileIn)) {
      return(NULL)
    } else if (grepl(
      "pgtemplate",
      gsub(
        '^.*selectedtemplate\\s*|\\s*searches.*$',
        '',
        readLines(input$fileIn$datapath, n = 2)[2]
      ),
      fixed = TRUE
    )) {
      return("peakGroups")
    } else if (grepl(
      "pdtemplate",
      gsub(
        '^.*selectedtemplate\\s*|\\s*searches.*$',
        '',
        readLines(input$fileIn$datapath, n = 2)[2]
      ),
      fixed = TRUE
    )) {
      return("peakData")
    } else if (grepl(
      "fctemplate",
      gsub(
        '^.*selectedtemplate\\s*|\\s*searches.*$',
        '',
        readLines(input$fileIn$datapath, n = 2)[2]
      ),
      fixed = TRUE
    )) {
      return("fCirc")
    }
    
  })
  
  
  # This observeEvent statement parses the file input header to determine data type
  # then hides tabPanels which are not associated with the inputted datatype
  # The remaining displayed tabPanels are only those useful for that datatype
  # Ex. when Fcirc dataype is input, peakGroups and peakData tabs are hidden
  
  observeEvent(input$fileIn, {
    if (grepl(
      "pgtemplate",
      gsub(
        '^.*selectedtemplate\\s*|\\s*searches.*$',
        '',
        readLines(input$fileIn$datapath, n = 2)[2]
      ),
      fixed = TRUE
    )) {
      hideTab(inputId = "dataPanels",
              target = "Fcirc Plots")
      hideTab(inputId = "dataPanels",
              target = "Fcirc Stats")
      showTab(inputId = "dataPanels",
              target = "Enrichment Plots")
      showTab(inputId = "dataPanels",
              target = "Enrichment Stats")
      showTab(inputId = "dataPanels",
              target = "Pool Sizes")
    } else if (grepl(
      "fctemplate",
      gsub(
        '^.*selectedtemplate\\s*|\\s*searches.*$',
        '',
        readLines(input$fileIn$datapath, n = 2)[2]
      ),
      fixed = TRUE
    )) {
      hideTab(inputId = "dataPanels",
              target = "Enrichment Plots")
      hideTab(inputId = "dataPanels",
              target = "Enrichment Stats")
      hideTab(inputId = "dataPanels",
              target = "Pool Sizes")
      showTab(inputId = "dataPanels",
              target = "Fcirc Plots")
      showTab(inputId = "dataPanels",
              target = "Fcirc Stats")
    }
  })
  
  
  # Load data using reactive function. This reactive function returns a dataframe
  # of the input file. Functions that call getData() will receive the dataframe
  # and will only rerun fread() when the input field has been updated.
  # This function also recodes "None" to NA and reclasses variables
  
  getData <- reactive({
    req(input$fileIn)
    dataIn <- as.data.frame(fread(input$fileIn$datapath))
    dataIn[dataIn == "None"] <- NA
    if("Infusate" %in% colnames(dataIn)){
      dataIn$Infusate <- make.names(dataIn$Infusate) 
    }
    dataIn[colnames(dataIn) %in% summaryColNames] <-
      lapply(dataIn[colnames(dataIn) %in% summaryColNames], as.character)
    dataIn[colnames(dataIn) %in% numericColNames] <-
      lapply(dataIn[colnames(dataIn) %in% numericColNames], as.numeric)
    return(dataIn)
  })
  
  
  
  # Generate selectInput (pickerInput) elements for all character variables from
  # the user data upload. These elements are used for the user to filter
  # data and exclude levels of factor variables
  
  output$generateFilters <- renderUI({
    lapply(names(Filter(is.character, getData())), function(i) {
      filterLevels <- unique(getData()[, i])
      shinyWidgets::pickerInput(
        inputId = paste(i),
        label = paste(i),
        choices = filterLevels,
        multiple = T,
        options = list(`actions-box` = T),
        selected = filterLevels
      )
    })
  })
  
  
  # Reactive function which executes filtering based on user selection from the
  # generateFilters elements
  
  filterFunction <- reactive({
    purrr::reduce(names(Filter(is.character, getData())), function(.x, .y) {
      filter(.x, .data[[ .y ]] %in% input[[.y]])  
    },
    .init = getData()
    )
  })
  
  
  # dataSummary output generates a count table of the number of levels in each
  # "factor" variable (all possible defined in "summaryColNames" variable).
  # This table is then rendered below the file input widget after a file
  # has been loaded and reacts to filtering
  
  output$dataSummary <- renderTable({
    filterFunction() %>%
      select(any_of(summaryColNames)) %>%
      map(unique) %>%
      map(length) %>%
      do.call(what = cbind.data.frame, args = .)  %>%
      t() %>%
      as.data.frame() %>%
      rename(UniqueValues = V1)
  }, rownames = T)
  
  output$dataTableView <- renderTable({
    head(filterFunction())
  })
  
  
  # The 4 observe functions below react to user inputs which 
  # dictate the variables to be plotted on the x axis, y axis
  # and variables used for fill color and faceting in the plot1 
  # (Peak Groups Enrichment) output
  # The two coder functions take the T/F value for faceting and scaling
  
  observe({
    updateSelectInput(session, "plot1_x",
                      choices = names(getData()),
                      selected = character(0))
  })
  
  observe({
    updateSelectInput(session, "plot1_y",
                      choices = names(getData()),
                      selected = character(0))
  })
  
  observe({
    updateSelectInput(session, "plot1_facet1",
                      choices = names(getData()),
                      selected = character(0))
  })
  
  observe({
    updateSelectInput(session, "fillEnrichPlot",
                      choices = names(getData()),
                      selected = character(0))
  })
  
  facetCoder <- eventReactive(input$renderPlot1, {
     if(input$check_facet1) FALSE else TRUE 
  })
  
  scaleCoder <- eventReactive(input$renderPlot1, {
    if(input$scales_plot1) FALSE else TRUE
  }) 
  
  
  # plot1 output generates plots for isotope enrichment-based parameters from 
  # the Peak Groups output data type. Mainly intended for "Total Abundance",
  # "Enrichment Fraction", "Enrichment Abundance" or "Normalized Labeling"
  # values. The user dictates the x and y axis parameters via dropdown
  # user inputs. User can also optionally provide a faceting parameter
  # and allow for free y axis scales between facets via checkbox inputs.
  # One plot per infusate is automatically generated.
  
  output$plot1 <- renderUI({
    plotOutputList <- lapply(make.names(unique(filterFunction()$Infusate)),
                             function(i){
                               plotname <- paste("Plot", i, sep = "_")
                               plotOutput(plotname, height = 600)
                             })
    do.call(tagList, plotOutputList)
  })
  
  observeEvent(input$renderPlot1, {
    for (i in make.names(unique(filterFunction()$Infusate))) {
      local({
        iCurrent <- i
        plotname <- paste("Plot", iCurrent, sep = "_")
        
        output[[plotname]] <- renderPlot({
          if (input$fillEnrichPlot == "") {
            if (facetCoder()) {
              filterFunction() %>%
                filter(Infusate == iCurrent) %>%
                ggplot(aes_string(
                  x = as.name(input$plot1_x),
                  y = as.name(input$plot1_y)
                )) +
                geom_boxplot() + ggtitle(paste(iCurrent))
            } else{
              if (scaleCoder()) {
                filterFunction() %>%
                  filter(Infusate == iCurrent) %>%
                  ggplot(aes_string(
                    x = as.name(input$plot1_x),
                    y = as.name(input$plot1_y)
                  )) +
                  geom_boxplot() + ggtitle(paste(iCurrent)) +
                  facet_wrap(paste("~`", input$plot1_facet1, "`", sep = ""))
              } else{
                filterFunction() %>%
                  filter(Infusate == iCurrent) %>%
                  ggplot(aes_string(
                    x = as.name(input$plot1_x),
                    y = as.name(input$plot1_y)
                  )) +
                  geom_boxplot() + ggtitle(paste(iCurrent)) +
                  facet_wrap(paste("~`", input$plot1_facet1, "`", sep = ""),
                             scale = "free")
              }
            }
          } else{
            if (facetCoder()) {
              filterFunction() %>%
                filter(Infusate == iCurrent) %>%
                ggplot(aes_string(
                  x = as.name(input$plot1_x),
                  y = as.name(input$plot1_y),
                  fill = as.name(input$fillEnrichPlot)
                )) +
                geom_boxplot() + ggtitle(paste(iCurrent))
            } else{
              if (scaleCoder()) {
                filterFunction() %>%
                  filter(Infusate == iCurrent) %>%
                  ggplot(aes_string(
                    x = as.name(input$plot1_x),
                    y = as.name(input$plot1_y),
                    fill = as.name(input$fillEnrichPlot)
                  )) +
                  geom_boxplot() + ggtitle(paste(iCurrent)) +
                  facet_wrap(paste("~`", input$plot1_facet1, "`", sep = ""))
              } else{
                filterFunction() %>%
                  filter(Infusate == iCurrent) %>%
                  ggplot(aes_string(
                    x = as.name(input$plot1_x),
                    y = as.name(input$plot1_y),
                    fill = as.name(input$fillEnrichPlot)
                  )) +
                  geom_boxplot() + ggtitle(paste(iCurrent)) +
                  facet_wrap(paste("~`", input$plot1_facet1, "`", sep = ""),
                             scale = "free")
              }
            }
          }

        })
      })
    }
  })
  
  
  # Enrichment stats
  
  observe({
    updateSelectInput(session, "enrichmentDependent",
                      choices = names(getData()))
  })
  
  observe({
    updateSelectInput(session, "enrichmentIndependent",
                      choices = names(getData()))
  })
  
  observe({
    updateSelectInput(session, "enrichNest",
                      choices = names(getData()))
  })
  
  enrichInteractCoder <- eventReactive(input$calcEnrichStats, {
    if(input$enrichmentInteractionBox) FALSE else TRUE 
  })
  
  enrichNestCoder <- eventReactive(input$calcEnrichStats, {
    if(input$enrichNestBox) FALSE else TRUE 
  })
  
  # Generate model functions for enrichment statistics
  
  enrichmentFormula <- reactive({
    if(enrichInteractCoder()){
      if(length(input$enrichmentIndependent) == 1){
        paste0("`", input$enrichmentDependent, "` ~ `", input$enrichmentIndependent, "`") %>%
          as.formula()
      } else if(length(input$enrichmentIndependent) == 2){
        paste0("`", input$enrichmentDependent, "` ~ `", input$enrichmentIndependent[1],
               "` + `", input$enrichmentIndependent[2], "`") %>%
          as.formula()
      }
    } else {
      if(length(input$enrichmentIndependent) == 1){
        paste0("`", input$enrichmentDependent, "` ~ `", input$enrichmentIndependent, "`") %>%
          as.formula()
      } else if(length(input$enrichmentIndependent) == 2){
        paste0("`", input$enrichmentDependent, "` ~ `", input$enrichmentIndependent[1],
               "` * `", input$enrichmentIndependent[2], "`") %>%
          as.formula()
      }
    }
  })
  
  enrichmentFunction <- possibly(function(dat) {
    lm(formula = enrichmentFormula(), data = dat) %>%
      broom::tidy()
  }, otherwise = NULL)
  
  output$enrichStatTable <- DT::renderDataTable(DT::datatable({
    req(input$calcEnrichStats)
    if (enrichNestCoder()) {
      filterFunction() %>%
        nest(statData = -Infusate) %>%
        mutate(df = purrr::map(statData, enrichmentFunction)) %>%
        unnest(df) %>%
        select(-statData) %>%
        filter(term != "(Intercept)")
    } else{
      filterFunction() %>%
        nest(statData = -c(Infusate, input$enrichNest)) %>%
        mutate(df = purrr::map(statData, enrichmentFunction)) %>%
        unnest(df) %>%
        select(-statData) %>%
        filter(term != "(Intercept)")
    }
  }))
  
  # Pool sizes
  observe({
    updateSelectInput(session, "poolSizeTissue",
                      choices = unique(filterFunction()$Tissue))
  })
  
  observe({
    updateSelectInput(session, "poolSizeGroupVar",
                      choices = names(getData()))
  })
  
  poolSizeFormula <- reactive({
    paste0("log2(`", input$poolSizeNumVar, "`) ~ `", input$poolSizeGroupVar, "`") %>%
      as.formula()
  })
  
  poolSizeFunction <- possibly(function(dat) {
    lm(formula = poolSizeFormula(), data = dat) %>%
      broom::tidy()
  }, otherwise = NULL)
  
  poolSizeStats <- reactive({
    req(input$calcPoolSize)
    filterFunction() %>%
      filter(Tissue == input$poolSizeTissue) %>%
      nest(statData = -`Measured Compound(s)`) %>%
      mutate(df = purrr::map(statData, poolSizeFunction)) %>%
      unnest(df) %>%
      select(-statData) %>%
      filter(term != "(Intercept)")
  })
  
  output$poolSizeStatTable <- DT::renderDataTable(DT::datatable({
    req(input$calcPoolSize)
    poolSizeStats()
  }))
  
  output$poolSizeVolcano <- renderPlotly({
    req(input$calcPoolSize)
    p <- ggplot(data = poolSizeStats(),
                aes(x = estimate,
                    y = -log10(p.value),
                    text = paste("Metabolite:", `Measured Compound(s)`),
                    key = `Measured Compound(s)`,
                    color = p.value < 0.05)) +
      geom_hline(yintercept = 1.3, linetype = "dashed") +
      geom_point() +
      labs(x = "Estimate (log2 fold change)",
           y = "-log10 p-value",
           color = "Sig")
    
    p <- ggplotly(p, source = "poolVolcano")
    print(p)
  })
  
  
  output$poolSizeBarplot <- renderPlot({
    req(input$calcPoolSize)
    event.data <- event_data("plotly_click",
                             source = "poolVolcano")
    
    if (is.null(event.data)) {
      p <- filterFunction() %>%
        filter(`Measured Compound(s)` == "lysine") %>%
        ggplot(aes_string(
          x = "Tissue",
          y = "`Total Abundance`",
          fill = paste("`", input$poolSizeGroupVar, "`", sep = "")
        )) +
        geom_boxplot()
      print(p)
    } else{
      p <- filterFunction() %>%
        filter(`Measured Compound(s)` == event.data$key) %>%
        ggplot(aes_string(
          x = "Tissue",
          y = "`Total Abundance`",
          fill = paste("`", input$poolSizeGroupVar, "`", sep = "")
        )) +
        geom_boxplot()
      print(p)
    }
  })
  
  # The 4 observe functions below react to user inputs which
  # dictate the variables to be plotted on the x axis, y axis
  # and variables used for fill color and faceting in the 
  # Fcirc_plot1 (Fcirc main plot) output
  # The two coder functions take the T/F value for faceting and scaling
  
  observe({
    updateSelectInput(session, "plotFcirc1_x",
                      choices = names(getData()),
                      selected = character(0))
  })
  
  observe({
    updateSelectInput(session, "plotFcirc1_y",
                      choices = names(getData()),
                      selected = character(0))
  })
  
  observe({
    updateSelectInput(session, "Fcirc_facet1",
                      choices = names(getData()),
                      selected = character(0))
  })
  
  observe({
    updateSelectInput(session, "fillFcircPlot",
                      choices = names(getData()),
                      selected = character(0))
  })
  
  facetCoderFcirc <- eventReactive(input$renderPlotFcirc1, {
    if(input$facetCheck_Fcirc1) FALSE else TRUE 
  })
  
  scaleCoderFcirc <- eventReactive(input$renderPlotFcirc1, {
    if(input$scalesCheck_Fcirc1) FALSE else TRUE
  }) 
  
  
  # plot1 output generates plots for isotope enrichment-based parameters from 
  # the Peak Groups output data type. Mainly intended for "Total Abundance",
  # "Enrichment Fraction", "Enrichment Abundance" or "Normalized Labeling"
  # values. The user dictates the x and y axis parameters via dropdown
  # user inputs. User can also optionally provide a faceting parameter
  # and allow for free y axis scales between facets via checkbox inputs.
  # One plot per infusate is automatically generated.
  
  output$Fcirc_plot1 <- renderUI({
    plotOutputListFcirc <- lapply(make.names(unique(filterFunction()$`Labeled Element`)),
                             function(i){
                               plotname <- paste("Plot", i, sep = "_")
                               plotOutput(plotname, height = 600)
                             })
    do.call(tagList, plotOutputListFcirc)
  })
  
  observeEvent(input$renderPlotFcirc1, {
    for (i in make.names(unique(filterFunction()$`Labeled Element`))) {
      local({
        iCurrent <- i
        plotname <- paste("Plot", iCurrent, sep = "_")
        
        output[[plotname]] <- renderPlot({
          if (input$fillFcircPlot == "") {
            if (facetCoderFcirc()) {
              filterFunction() %>%
                filter(`Labeled Element` == iCurrent) %>%
                ggplot(aes_string(
                  x = as.name(input$plotFcirc1_x),
                  y = as.name(input$plotFcirc1_y)
                )) +
                geom_boxplot() + ggtitle(paste(iCurrent))
            } else{
              if (scaleCoderFcirc()) {
                filterFunction() %>%
                  filter(`Labeled Element` == iCurrent) %>%
                  ggplot(aes_string(
                    x = as.name(input$plotFcirc1_x),
                    y = as.name(input$plotFcirc1_y)
                  )) +
                  geom_boxplot() + ggtitle(paste(iCurrent)) +
                  facet_wrap(paste("~`", input$Fcirc_facet1, "`", sep = ""))
              } else{
                filterFunction() %>%
                  filter(`Labeled Element` == iCurrent) %>%
                  ggplot(aes_string(
                    x = as.name(input$plotFcirc1_x),
                    y = as.name(input$plotFcirc1_y)
                  )) +
                  geom_boxplot() + ggtitle(paste(iCurrent)) +
                  facet_wrap(paste("~`", input$Fcirc_facet1, "`", sep = ""),
                             scale = "free")
              }
            }
          } else{
            if (facetCoderFcirc()) {
              filterFunction() %>%
                filter(`Labeled Element` == iCurrent) %>%
                ggplot(aes_string(
                  x = as.name(input$plotFcirc1_x),
                  y = as.name(input$plotFcirc1_y),
                  fill = as.name(input$fillFcircPlot)
                )) +
                geom_boxplot() + ggtitle(paste(iCurrent))
            } else{
              if (scaleCoderFcirc()) {
                filterFunction() %>%
                  filter(`Labeled Element` == iCurrent) %>%
                  ggplot(aes_string(
                    x = as.name(input$plotFcirc1_x),
                    y = as.name(input$plotFcirc1_y),
                    fill = as.name(input$fillFcircPlot)
                  )) +
                  geom_boxplot() + ggtitle(paste(iCurrent)) +
                  facet_wrap(paste("~`", input$Fcirc_facet1, "`", sep = ""))
              } else{
                filterFunction() %>%
                  filter(`Labeled Element` == iCurrent) %>%
                  ggplot(aes_string(
                    x = as.name(input$plotFcirc1_x),
                    y = as.name(input$plotFcirc1_y),
                    fill = as.name(input$fillFcircPlot)
                  )) +
                  geom_boxplot() + ggtitle(paste(iCurrent)) +
                  facet_wrap(paste("~`", input$Fcirc_facet1, "`", sep = ""),
                             scale = "free")
              }
            }
          }
          
          
          
        })
      })
    }
  })
  
  # Fcirc stats
  
  observe({
    updateSelectInput(session, "fcircDependent",
                      choices = names(getData()))
  })
  
  observe({
    updateSelectInput(session, "fcircIndependent",
                      choices = names(getData()))
  })
  
  observe({
    updateSelectInput(session, "fcircNest",
                      choices = names(getData()))
  })
  
  fcircInteractCoder <- eventReactive(input$calcFcircStats, {
    if(input$fcircInteractionBox) FALSE else TRUE 
  })
  
  fcircNestCoder <- eventReactive(input$calcFcircStats, {
    if(input$fcircNestBox) FALSE else TRUE 
  })
  
  # Generate model functions for Fcirc statistics
  
  fcircFormula <- reactive({
    if(fcircInteractCoder()){
      if(length(input$fcircIndependent) == 1){
        paste0("`", input$fcircDependent, "` ~ `", input$fcircIndependent, "`") %>%
          as.formula()
      } else if(length(input$fcircIndependent) == 2){
        paste0("`", input$fcircDependent, "` ~ `", input$fcircIndependent[1],
               "` + `", input$fcircIndependent[2], "`") %>%
          as.formula()
      }
    } else {
      if(length(input$fcircIndependent) == 1){
        paste0("`", input$fcircDependent, "` ~ `", input$fcircIndependent, "`") %>%
          as.formula()
      } else if(length(input$fcircIndependent) == 2){
        paste0("`", input$fcircDependent, "` ~ `", input$fcircIndependent[1],
               "` * `", input$fcircIndependent[2], "`") %>%
          as.formula()
      }
    }
  })
  
  fcircFunction <- possibly(function(dat) {
    lm(formula = fcircFormula(), data = dat) %>%
      broom::tidy()
  }, otherwise = NULL)
  
  output$fcircStatTable <- DT::renderDataTable(DT::datatable({
    req(input$calcFcircStats)
    if (fcircNestCoder()) {
      filterFunction() %>%
        nest(statData = -`Labeled Element`) %>%
        mutate(df = purrr::map(statData, fcircFunction)) %>%
        unnest(df) %>%
        select(-statData) %>%
        filter(term != "(Intercept)")
    } else{
      filterFunction() %>%
        nest(statData = -c(`Labeled Element`, input$fcircNest)) %>%
        mutate(df = purrr::map(statData, fcircFunction)) %>%
        unnest(df) %>%
        select(-statData) %>%
        filter(term != "(Intercept)")
    }
  }))
  
  
  outputOptions(output, "getHeader", suspendWhenHidden = FALSE)
  
}