library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(shinydashboard)

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
    "Tracer Compound",
    "Time Collected (m)"
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
  "Tracer Concentration (mM)"
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
  
  # The 3 observe functions below react to user inputs which 
  # dictate the variables to be plotted on the x axis, y axis
  # and variable used for faceting in the plot1 (Peak Groups) output
  
  observe({
    updateSelectInput(session, "plot1_x",
                      choices = names(getData()))
  })
  
  observe({
    updateSelectInput(session, "plot1_y",
                      choices = names(getData()))
  })
  
  observe({
    updateSelectInput(session, "plot1_facet1",
                      choices = names(getData()))
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
          if (facetCoder()) {
            filterFunction() %>%
              filter(Infusate == iCurrent) %>%
              ggplot(aes_string(x = as.name(input$plot1_x), 
                                y = as.name(input$plot1_y))) +
              geom_boxplot() + ggtitle(paste(iCurrent))
          } else{
            if (scaleCoder()) {
              filterFunction() %>%
                filter(Infusate == iCurrent) %>%
                ggplot(aes_string(x = as.name(input$plot1_x), 
                                  y = as.name(input$plot1_y))) +
                geom_boxplot() + ggtitle(paste(iCurrent)) +
                facet_wrap(input$plot1_facet1)
            } else{
              filterFunction() %>%
                filter(Infusate == iCurrent) %>%
                ggplot(aes_string(x = as.name(input$plot1_x), 
                                  y = as.name(input$plot1_y))) +
                geom_boxplot() + ggtitle(paste(iCurrent)) +
                facet_wrap(input$plot1_facet1, scale = "free")
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
  
  enrichInteractCoder <- eventReactive(input$calcEnrichStats, {
    if(input$enrichmentInteractionBox) FALSE else TRUE 
  })
  
  # Generate model functions for enrichment statistics
  enrichmentRegFxn <- reactive({
    if (enrichInteractCoder) {
      if (length(input$enrichmentIndependent) == 1) {
        possibly(function(dat) {
          lm(input$enrichmentDependent ~ input$enrichmentDependent,
             data = dat) %>%
            broom::tidy()
        })
      } else if(length(input$enrichmentIndependent) == 2) {
        possibly(function(dat) {
          lm(input$enrichmentDependent ~ input$enrichmentDependent[1] +
               input$enrichmentDependent[2],
             data = dat) %>%
            broom::tidy()
        })
      }
    } else {
      if (length(input$enrichmentIndependent) == 1) {
        possibly(function(dat) {
          lm(input$enrichmentDependent ~ input$enrichmentDependent,
             data = dat) %>%
            broom::tidy()
        })
      } else if(length(input$enrichmentIndependent) == 2) {
        possibly(function(dat) {
          lm(input$enrichmentDependent ~ input$enrichmentDependent[1] *
               input$enrichmentDependent[2],
             data = dat) %>%
            broom::tidy()
        })
      }
    }
  })
  
  # Perform stats
  observeEvent(input$calcEnrichStats,{
               output$enrichStatTable <- getData() %>%
                 nest(infusateData = -Infusate) %>%
                 mutate(df = map(infusateData, enrichmentRegFxn)) %>%
                 unnest(df) %>%
                 select(-infusateData)
               })
  
  
  
  outputOptions(output, "getHeader", suspendWhenHidden = FALSE)
  
}