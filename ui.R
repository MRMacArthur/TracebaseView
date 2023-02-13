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
              actionButton("calcEnrichStats", "Calculate statistics")
            ),
            conditionalPanel(
              condition = "input.dataPanels == 'Fcirc Plots'",
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
            )),
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
                       h3("STUFF HERE")),
              tabPanel("Fcirc Stats",
                       h3("STUFF HERE")),
              id = "dataPanels"
            ))
          ))
