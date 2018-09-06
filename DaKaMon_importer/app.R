# UI DaKaMon import
library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(rjson)
library(RPostgreSQL)
library(pool)

source("conf.R")

## UI

ui <- navbarPage("Datenimport",
                 navbarMenu("Messdaten",
                            ## Ort
                            tabPanel("Ort anlegen", useShinyjs(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("ortFileEnc", "Encoding der Datei", 
                                                     list('UTF-8 (linux)'="UTF-8", 'ISO-8859-1 (windows)'="ISO-8859-1", 'Windows-1252'="windows-1252")),
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
                                         selectInput("pnsFileEnc", "Encoding der Datei", 
                                                     list('UTF-8 (linux)'="UTF-8", 'ISO-8859-1 (windows)'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("csvFilePNS", "CSV-Datei mit Probenahmestellen", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,                                           
                                         checkboxInput("owPNS", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("PNSValidationOut"),
                                         uiOutput("PNSDBConsistencyOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tablePNS'))
                                     )),
                            
                            ## Parameter (PAR)
                            tabPanel("Parameter anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("parFileEnc", "Encoding der Datei",
                                                     list('UTF-8 (linux)'="UTF-8", 'ISO-8859-1 (windows)'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("csvFilePAR", "CSV-Datei mit Parametern", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,                     
                                         checkboxInput("owPAR", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("PARValidationOut"),
                                         uiOutput("PARDBConsistencyOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tablePAR'))
                                     )),
                            
                            ## Probe
                            tabPanel("Probe anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("probeFileEnc", "Encoding der Datei", 
                                                     list('UTF-8 (linux)'="UTF-8", 'ISO-8859-1 (windows)'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("csvFileProbe", "CSV-Datei mit Proben", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,  
                                         checkboxInput("owProbe", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("ProbeValidationOut"),
                                         uiOutput("ProbeDBConsistencyOut"), # DBConsistencyActionOut
                                         width = 2),
                                       mainPanel(dataTableOutput('tableProbe'))
                                     )),
                            
                            ##  Messungen hochladen
                            tabPanel("Messungen hochladen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("dataFileEnc", "Encoding der Datei", 
                                                     list('UTF-8 (linux)'="UTF-8", 'ISO-8859-1 (windows)'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("dataCsvFile", "CSV-Datei mit Messungen", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         #   selectInput("dataBG", "Zeile mit Bestimmungsgrenze:", choices = c(1:20), selected = "1"),
                                         #   selectInput("dataUoM", "Zeile mit Maßeinheit:", choices = c(1:20), selected = "2"),
                                         #   selectInput("dataStgr", "Zeile mit Elementgruppe:", choices = c(1:20), selected = "3"),
                                         # textInput("dataBGchar", "Unter Bestimmungsgrenze:", value = BGchar),
                                         csvInfo, br(),
                                         paste0("Werte unterhalb der Bestimmungsgrenze müssen als \"", BGchar, "\" und unterhalb der Nachweisgrenze als \"", NGchar, "\" in der CSV-Datei abgelegt sein."),
                                         checkboxInput("dataOW", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("dataValidationOut"),
                                         uiOutput("dataDBConsistencyOut"),
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
                                         fileInput("csvFileReferenz", "CSV-Datei mit Referenzen", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owReferenz", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("ReferenzValidationOut"),
                                         uiOutput("ReferenzDBConsistencyOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tableReferenz'))
                                     )),
                            
                            tabPanel("Literaturdaten hochladen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput("csvFileLiteratur", "CSV-Datei mit Literaturwerten", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owLiteratur", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("LiteraturValidationOut"),
                                         uiOutput("LiteraturDBConsistencyOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tableLiteratur'))
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
  
  source("server_1_Ort.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_2_Probenahmestelle.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_3_Parameter.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_4_Probe.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_5_Messungen.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_2_1_Referenz.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_2_2_Literatur.R", local = TRUE, encoding = "UTF-8")$value
  
  # Datei upload
}

pool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = dbName,
  host = dbHost,
  user = dbUser,
  password = dbPassword,
  port = dbPort,
  minSize = 3,
  maxSize = 10,
  idleTimeout = 30000 # 30sec
)

connectToDB <- function() {
  db <- poolCheckout(pool)
  if (Sys.info()["sysname"] == "Windows") {
    dbSendQuery(db, "set client_encoding='windows-1252'")
  }
  db
}

modalErrorHandler <- function(e) {
  if (!is.null(e)) {
    print(e)
  }
  showModalMessage(title="Fehler", e["message"])
}

showModalMessage <- function(..., title="Title") {
  showModal(modalDialog(..., title = title, footer = modalButton("Ok")))
}

shinyApp(ui, server, onStart = function() {
  cat("Starting Application ...")
  # Ensure the DB pool closes all connections
  onStop(function() {
    cat("Closing DB connection pool")
    poolClose(pool)
  })
})

