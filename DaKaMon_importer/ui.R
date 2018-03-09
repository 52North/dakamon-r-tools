# UI DaKaMon import
library(shiny)
library(DT)
library(shinyjs)
library(httr)

SOSWebApp <- "http://localhost:8080/52n-sos-webapp/"
verbose <- TRUE
BGencode <- 0
BGchar <- "< BG"

ui <- fluidPage(
  useShinyjs(),
  titlePanel("DaKaMon Importer"),
  
  #############
  ###  FoI  ###
  #############
  
  tabsetPanel(
    tabPanel("Basic data upload",
             sidebarLayout(
               sidebarPanel(fluidRow(column(6, textInput("sepFoI", "Column separator:", value = ";", width = "80%")),
                                     column(6, textInput("decFoI", "Decimal separator:", value = ".", width = "80%"))),
                            fileInput("csvFileFoI", "Select a FoI file for upload."),
                            checkboxInput("owFoI", "Overwrite ALL data?", FALSE), 
                            uiOutput("foiValidationOut"),
                            uiOutput("DBConsistencyTxtOut"),
                            uiOutput("DBConsistencyActionOut"), # DBConsistencyActionOut
                            width = 2),
               mainPanel(dataTableOutput('tableFoI'))
               )),
    
    
    ##############
    ###  Data  ###
    ##############

    tabPanel("Time series data upload",
             sidebarLayout(
               sidebarPanel(fluidRow(column(6, textInput("dataSep", "Column separator:", value = ";", width = "80%")),
                                     column(6, textInput("dataDec", "Decimal separator:", value = ".", width = "80%"))),
                            fileInput("dataCsvFile", "Select a data file for upload."),
                            selectInput("dataBG", "Detection threshold row:", choices = c(1:20), selected = "1"),
                            selectInput("dataUoM", "Unit of measurement row:", choices = c(1:20), selected = "2"),
                            selectInput("dataStgr", "Element group row:", choices = c(1:20), selected = "3"),
                            textInput("dataBGchar", "Detection limit character:", value = BGchar),
                            checkboxInput("dataOW", "Overwrite ALL data?", FALSE), 
                            uiOutput("dataValidationOut"),
                            uiOutput("dataDBConsistencyTxtOut"),
                            uiOutput("dataDBConsistencyActionOut"),
                            width=2),
               mainPanel(dataTableOutput('tableData'))
             )))
)
