library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(shinydashboard)

fluidPage(titlePanel("TracebaseViews"),
          sidebarLayout(
            sidebarPanel(
              width = 2,
              fileInput(
                "fileIn",
                "Load Tracebase Output File",
                multiple = F,
                accept = c(".tsv")
              ),
              br(),
              tableOutput("dataSummary"),
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
            mainPanel(tabsetPanel(
              tabPanel("Data Input",
                verbatimTextOutput("getHeader"),
                h3("Filter Data"),
                uiOutput("generateFilters"),
                h3("Preview of Data"),
                tableOutput("dataTableView")
              ),
              tabPanel("Plots",
                       uiOutput("plot1"))
            ))
          ))
