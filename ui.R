# UI DaKaMon import
library(shiny)
library(DT)

library("rpostgis")
library("httr")

# db <- dbConnect("PostgreSQL", host="localhost", dbname="sos", user="postgres", password="postgres", port="5432")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("DaKaMon Importer"),
  
  #############
  ###  FoI  ###
  #############
  
  tabsetPanel(
    tabPanel("FoI generation",
             sidebarLayout(
               sidebarPanel(fluidRow(column(6, textInput("sepFoI", "Column separator:", value = ";", width = "80%")),
                                     column(6, textInput("decFoI", "Decimal separator:", value = ".", width = "80%"))),
                            fileInput("csvFileFoI", "Select a FoI file for upload."),
                            selectInput("UoMFoI", "Unit of measurement row:", choices = c(1:20), selected = "1"),
                            textInput("exclRowFoI", "Exclude rows:"),
                            textInput("exclColFoI", "Exclude columns:"),
                            checkboxInput("owFoI", "Overwrite FoI?", FALSE), 
                            uiOutput("foiValidationOut"),
                            uiOutput("DBConsistencyTxtOut"),
                            uiOutput("DBConsistencyActionOut"),
                            width = 2),
               mainPanel(dataTableOutput('tableFoI'))
               )),
    
    
    ##############
    ###  Data  ###
    ##############

    tabPanel("Data upload",
             sidebarLayout(
               sidebarPanel(fluidRow(column(6, textInput("sepData", "Column separator:", value = ";", width = "80%")),
                                     column(6, textInput("decData", "Decimal separator:", value = ".", width = "80%"))),
                            fileInput("csvFileData", "Select a data file for upload."),
                            selectInput("UoMData", "Unit of measurement row:", choices = c(1:20), selected = "1"),
                            selectInput("BgData", "Detection threshold row:", choices = c(1:20), selected = "2"),
                            selectInput("StgrData", "Element group row:", choices = c(1:20), selected = "3"),
                            textInput("exclRowData", "Exclude rows:"),
                            textInput("exclColData", "Exclude columns:"),
                            checkboxInput("owData", "Overwrite Data?", FALSE), 
                            uiOutput("DataValidationOut"),
                            uiOutput("DataDBConsistencyTxtOut"),
                            uiOutput("DataDBConsistencyActionOut"),
                            width=2),
               mainPanel(dataTableOutput('tableData'))
             )))
)
