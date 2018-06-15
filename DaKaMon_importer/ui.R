# UI DaKaMon import
library(shiny)
library(DT)
library(shinyjs)
library(httr)

if(!require(readr)) {
  install.packages("readr", quiet=TRUE)
  library(readr)
}

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
    tabPanel("Kläranlagen und Verfahrensschritte anlegen",
             sidebarLayout(
               sidebarPanel(textInput("sepFoI", "Spaltentrennzeichen:", value = ";", width = "80%"),
                            textInput("decFoI", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                            fileInput("csvFileFoI", "Kläranlagen csv-Datei", 
                                      buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                            checkboxInput("owFoI", "Alle Daten überschreiben?", FALSE), 
                            uiOutput("foiValidationOut"),
                            uiOutput("DBConsistencyTxtOut"),
                            uiOutput("DBConsistencyActionOut"), # DBConsistencyActionOut
                            width = 2),
               mainPanel(dataTableOutput('tableFoI'))
               )),
    
    
    ##############
    ###  Data  ###
    ##############

    tabPanel("Zeitreihen anlegen",
             sidebarLayout(
               sidebarPanel(textInput("dataSep", "Spaltentrennzeichen:", value = ";", width = "80%"),
                            textInput("dataDec", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                            fileInput("dataCsvFile", "Beobachtungs csv-Datei", 
                                      buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                            selectInput("dataBG", "Zeile mit Bestimmungsgrenze:", choices = c(1:20), selected = "1"),
                            selectInput("dataUoM", "Zeile mit Maßeinheit:", choices = c(1:20), selected = "2"),
                            selectInput("dataStgr", "Zeile mit Elementgruppe:", choices = c(1:20), selected = "3"),
                            textInput("dataBGchar", "Unter Bestimmungsgrenze:", value = BGchar),
                            checkboxInput("dataOW", "Alle Daten überschreiben?", FALSE), 
                            uiOutput("dataValidationOut"),
                            uiOutput("dataDBConsistencyTxtOut"),
                            uiOutput("dataDBConsistencyActionOut"),
                            width=2),
               mainPanel(dataTableOutput('tableData'))
             )))
)
