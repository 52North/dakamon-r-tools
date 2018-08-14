# UI DaKaMon import
library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(rjson)
library(RPostgreSQL)

# guess an encoding
library(readr)

# consts and config:

source("conf.R")

## UI

ui <- navbarPage("Datenimport",
                 navbarMenu("Messdaten",
                            ## Ort
                            tabPanel("Ort anlegen", useShinyjs(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput("csvFileOrt", "CSV-Datei mit Orten", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owOrt", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("OrtValidationOut"),
                                         uiOutput("OrtDBConsistencyOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tableOrt'))
                                     )),
                            
                            ## Probenahmestelle (PNS)
                            tabPanel("Probenahmestelle anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput("csvFilePNS", "CSV-Datei mit Probenahmestellen", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,                                           
                                         checkboxInput("owPNS", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("PNSValidationOut"),
                                         uiOutput("PNSDBConsistencyTxtOut"),
                                         uiOutput("PNSDBConsistencyActionOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tablePNS'))
                                     )),
                            
                            ## Parameter (PAR)
                            tabPanel("Parameter anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # textInput("sepPAR", "Spaltentrennzeichen:", value = ";", width = "80%"),
                                         # textInput("decPAR", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                                         fileInput("csvFilePAR", "CSV-Datei mit Parametern", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,                     
                                         checkboxInput("owPAR", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("PARValidationOut"),
                                         uiOutput("PARDBConsistencyTxtOut"),
                                         uiOutput("PARDBConsistencyActionOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tablePAR'))
                                     )),
                            
                            ## Probe
                            tabPanel("Probe anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # textInput("decProbe", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                                         fileInput("csvFileProbe", "CSV-Datei mit Proben", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,  
                                         checkboxInput("owProbe", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("ProbeValidationOut"),
                                         uiOutput("ProbeDBConsistencyTxtOut"),
                                         uiOutput("ProbeDBConsistencyActionOut"), # DBConsistencyActionOut
                                         width = 2),
                                       mainPanel(dataTableOutput('tableProbe'))
                                     )),
                            
                            ##  Messungen hochladen
                            tabPanel("Messungen hochladen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # textInput("dataSep", "Spaltentrennzeichen:", value = ";", width = "80%"),
                                         # textInput("dataDec", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                                         fileInput("dataCsvFile", "CSV-Datei mit Messungen", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         #   selectInput("dataBG", "Zeile mit Bestimmungsgrenze:", choices = c(1:20), selected = "1"),
                                         #   selectInput("dataUoM", "Zeile mit Maßeinheit:", choices = c(1:20), selected = "2"),
                                         #   selectInput("dataStgr", "Zeile mit Elementgruppe:", choices = c(1:20), selected = "3"),
                                         # textInput("dataBGchar", "Unter Bestimmungsgrenze:", value = BGchar),
                                         csvInfo, br(),
                                         paste0("Werte unterhalb der Bestimmungsgrenze müssel als \"", BGchar, "\" und unterhalb der Nachweisgrenze als \"", NGchar, "\" in der CSV-Datei abgelegt sein."),
                                         checkboxInput("dataOW", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("dataValidationOut"),
                                         uiOutput("dataDBConsistencyTxtOut"),
                                         uiOutput("dataDBConsistencyActionOut"),
                                         width=2),
                                       mainPanel(dataTableOutput('tableData'))
                                     ))),
                 
                 ######################
                 ### Literaturdaten ###
                 ######################
                 
                 navbarMenu("Literaturdaten",
                            ## Referenz (Ref)
                            tabPanel("Referenz anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput("csvFileRef", "CSV-Datei mit Referenzen", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owRef", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("RefValidationOut"),
                                         uiOutput("RefDBConsistencyOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tableRef'))
                                     )),
                            
                            tabPanel("Literaturdaten hochladen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput("csvFileLit", "CSV-Datei mit Literaturwerten", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owProbe", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("LitValidationOut"),
                                         uiOutput("LitDBConsistencyOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tableLit'))
                                     ))),
                 
                 ## Dateien
                 tabPanel("Dateien hochladen",
                          selectInput("DateiSelType", "Wähle eine Kategorie", 
                                      choices = list(Messdaten=list("Ort", "Probenahmestelle", "Parameter", "Probe"), 
                                                     Literaturdaten=list("Literatur"))),
                          uiOutput("DateiKategorieElementeOut"),
                          fileInput("DateiFile", "Hochzuladende Datei", 
                                    buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt"),
                          checkboxInput("owDatei", "Datei überschreiben?", FALSE), 
                          uiOutput("DateiValidationOut"),
                          uiOutput("DateiDBConsistencyOut"))
)


server <- function(input, output) {
  
  source("server_Ort.R", local = TRUE)$value
  
  source("server_Probenahme.R", local = TRUE)$value
  
  source("server_Parameter.R", local = TRUE)$value
  
  source("server_Probe.R", local = TRUE)$value
  
  source("server_Messungen.R", local = TRUE)$value
  
  # Lit
  # Datei upload
}


shinyApp(ui, server)
