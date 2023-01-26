library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

options(shiny.maxRequestSize = 60 * 1024 ^ 2)

summaryColNames <-
  c(
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
  
  # Load data using reactive functio. This reactive function returns a dataframe
  # of the input file. Functions that call getData() will receive the dataframe
  # and will only rerun fread() when the input field has been updated.
  # This function also recodes "None" to NA and reclasses variables
  
  getData <- reactive({
    req(input$fileIn)
    dataIn <- as.data.frame(fread(input$fileIn$datapath))
    dataIn[dataIn == "None"] <- NA
    dataIn[colnames(dataIn) %in% summaryColNames] <-
      lapply(dataIn[colnames(dataIn) %in% summaryColNames], factor)
    dataIn[colnames(dataIn) %in% numericColNames] <-
      lapply(dataIn[colnames(dataIn) %in% numericColNames], as.numeric)
    return(dataIn)
  })
  
  # dataSummary output generates a count table of the number of levels in each
  # "factor" variable (all possible defined in "summaryColNames" variable).
  # This table is then rendered below the file input widget after a file
  # has been loaded
  
  output$dataSummary <- renderTable({
    getData() %>%
      select(any_of(summaryColNames)) %>%
      map(levels) %>%
      map(length) %>%
      do.call(what = cbind.data.frame, args = .)  %>%
      t() %>%
      as.data.frame() %>%
      rename(UniqueValues = V1)
  }, rownames = T)
  
  observeEvent(input$renderPlot1, {
    output$plot1 <- renderPlot(if (input$check_facet1 == F) {
      getData() %>%
        ggplot(aes_string(
          x = as.name(input$plot1_x),
          y = as.name(input$plot1_y)
        )) +
        geom_boxplot()
    } else{
      if (input$scales_plot1 == F) {
        getData() %>%
          ggplot(aes_string(
            x = as.name(input$plot1_x),
            y = as.name(input$plot1_y)
          )) +
          facet_wrap( ~ get(input$plot1_facet1)) +
          geom_boxplot()
      } else{
        getData() %>%
          ggplot(aes_string(
            x = as.name(input$plot1_x),
            y = as.name(input$plot1_y)
          )) +
          facet_wrap( ~ get(input$plot1_facet1),
                      scales = "free") +
          geom_boxplot()
      }
    })
  })
  
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
  
  outputOptions(output, "getHeader", suspendWhenHidden = FALSE)
  
}