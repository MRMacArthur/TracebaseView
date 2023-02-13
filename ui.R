library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

fluidPage(titlePanel("TracebaseViews"),
          sidebarLayout(
            sidebarPanel(
              width = 2,
              conditionalPanel(
                condition = "input.dataPanels == 'Data Input'",
                fileInput(
                  "fileIn",
                  "Load Tracebase Output File",
                  multiple = F,
                  accept = c(".tsv")
                ),
                br(),
                tableOutput("dataSummary")),
              conditionalPanel(
                condition = "input.dataPanels == 'Enrichment Plots'",
                h4("Required parameters"),
                selectInput(
                  inputId = "plot1_x",
                  label = "X Axis",
                  choices = c("")
                ),
              selectInput(
                inputId = "plot1_y",
                label = "Y Axis",
                choices = c("")
              ),
              h4("Optional parameters"),
              selectInput(
                inputId = "fillEnrichPlot",
                label = "Fill color",
                choices = c("")
              ),
              checkboxInput("check_facet1",
                            "Check for 1 facet",
                            value = F),
              checkboxInput("scales_plot1",
                            "Check for free scales",
                            value = F),
              selectInput(
                inputId = "plot1_facet1",
                label = "Facet",
                choices = c("")
              ),
              actionButton("renderPlot1", "Render Plot")
            ),
            conditionalPanel(
              condition = "input.dataPanels == 'Enrichment Stats'",
              checkboxInput("enrichmentInteractionBox",
                            "Check for interaction terms",
                            value = F),
              selectInput("enrichmentTransform",
                          "Select data transformation",
                          choices = c("None",
                                      "Log2",
                                      "Log10",
                                      "SqRoot")),
              selectInput(
                inputId = "enrichmentDependent",
                label = "Select dependent variable",
                choices = c("")
              ),
              selectInput(
                inputId = "enrichmentIndependent",
                label = "Select independent variable",
                choices = c(""),
                multiple = T
              ),
              checkboxInput("enrichNestBox",
                            "Check for nested models",
                            value = F),
              selectInput(
                inputId = "enrichNest",
                label = "Select nesting variable",
                choices = c("")
              ),
              actionButton("calcEnrichStats", "Calculate statistics")
            ),
            conditionalPanel(
              condition = "input.dataPanels == 'Fcirc Plots'",
              h4("Required parameters"),
              selectInput(
                inputId = "plotFcirc1_x",
                label = "X Axis",
                choices = c("")
              ),
              selectInput(
                inputId = "plotFcirc1_y",
                label = "Y Axis",
                choices = c("")
              ),
              h4("Optional parameters"),
              selectInput(
                inputId = "fillFcircPlot",
                label = "Fill color",
                choices = c("")
              ),
              checkboxInput("facetCheck_Fcirc1",
                            "Check for 1 facet",
                            value = F),
              checkboxInput("scalesCheck_Fcirc1",
                            "Check for free scales",
                            value = F),
              selectInput(
                inputId = "Fcirc_facet1",
                label = "Facet",
                choices = c("")
              ),
              actionButton("renderPlotFcirc1", "Render Plot")
            ),
            conditionalPanel(
              condition = "input.dataPanels == 'Fcirc Stats'",
              checkboxInput("fcircInteractionBox",
                            "Check for interaction terms",
                            value = F),
              selectInput("fcircTransform",
                          "Select data transformation",
                          choices = c("None",
                                      "Log10")),
              selectInput(
                inputId = "fcircDependent",
                label = "Select dependent variable",
                choices = c("")
              ),
              selectInput(
                inputId = "fcircIndependent",
                label = "Select independent variable",
                choices = c(""),
                multiple = T
              ),
              checkboxInput("fcircNestBox",
                            "Check for nested models",
                            value = F),
              selectInput(
                inputId = "fcircNest",
                label = "Select nesting variable",
                choices = c("")
              ),
              actionButton("calcFcircStats", "Calculate statistics")
            ),),
            mainPanel(
              tabsetPanel(
              tabPanel(
                "Data Input",
                fluidRow(column(3,
                                verbatimTextOutput("getHeader"),
                                h3("Filter Data"),
                                uiOutput("generateFilters")),
                         column(9,
                                h3("Preview of Data"),
                                tableOutput("dataTableView")))
              ),
              tabPanel("Enrichment Plots",
                       uiOutput("plot1")),
              tabPanel("Enrichment Stats",
                       DT::dataTableOutput("enrichStatTable")),
              tabPanel("Fcirc Plots",
                       uiOutput("Fcirc_plot1")),
              tabPanel("Fcirc Stats",
                       DT::dataTableOutput("fcircStatTable")),
              id = "dataPanels"
            ))
          ))
