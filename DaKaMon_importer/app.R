# UI DaKaMon import
library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(rjson)
library(RPostgreSQL)
library(pool)
library(readr)

source("conf.R")

## UI

ui <- navbarPage("Datenimport",
                 navbarMenu("Messdaten",
                            ## Ort
                            tabPanel(dataStep1$title,
                                     useShinyjs(),
                                     div(class="row",div(class="col-sm-12",h3(style=tabTitleStyle,class="tab-title", dataStep1$title))),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("ortFileEnc", "Encoding der Datei",
                                                     list('UTF-8'="UTF-8", 'ISO-8859-1'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("csvFileOrt", "CSV-Datei mit Orten",
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt",
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owOrt", "Alle Daten überschreiben?", FALSE),
                                         uiOutput("OrtValidationOut"),
                                         uiOutput("OrtDBConsistencyOut"),
                                         width = sideBarWidth),
                                       mainPanel(
                                         dataTableOutput('tableOrt'),
                                         width = mainPanelWidth)
                                     )),

                            ## Probenahmestelle (PNS)
                            tabPanel(dataStep2$title,
                                     useShinyjs(),
                                     h3(style=tabTitleStyle,class="tab-title", dataStep2$title),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("pnsFileEnc", "Encoding der Datei",
                                                     list('UTF-8'="UTF-8", 'ISO-8859-1'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("csvFilePNS", "CSV-Datei mit Probenahmestellen",
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt",
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owPNS", "Alle Daten überschreiben?", FALSE),
                                         uiOutput("PNSValidationOut"),
                                         uiOutput("PNSDBConsistencyOut"),
                                         width = sideBarWidth),
                                       mainPanel(
                                         dataTableOutput('tablePNS'),
                                         width = mainPanelWidth)
                                     )),

                            ## Parameter (PAR)
                            tabPanel(dataStep3$title,
                                     useShinyjs(),
                                     h3(style=tabTitleStyle,class="tab-title", dataStep3$title),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("parFileEnc", "Encoding der Datei",
                                                     list('UTF-8'="UTF-8", 'ISO-8859-1'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("csvFilePAR", "CSV-Datei mit Parametern",
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt",
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owPAR", "Alle Daten überschreiben?", FALSE),
                                         uiOutput("PARValidationOut"),
                                         uiOutput("PARDBConsistencyOut"),
                                         width = sideBarWidth),
                                       mainPanel(
                                         dataTableOutput('tablePAR'),
                                         width = mainPanelWidth)
                                     )),

                            ## Probe
                            tabPanel(dataStep4$title,
                                     useShinyjs(),
                                     h3(style=tabTitleStyle,class="tab-title", dataStep4$title),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("probeFileEnc", "Encoding der Datei",
                                                     list('UTF-8'="UTF-8", 'ISO-8859-1'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("csvFileProbe", "CSV-Datei mit Proben",
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt",
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owProbe", "Alle Daten überschreiben?", FALSE),
                                         uiOutput("ProbeValidationOut"),
                                         uiOutput("ProbeDBConsistencyOut"), # DBConsistencyActionOut
                                         width = sideBarWidth),
                                       mainPanel(
                                         dataTableOutput('tableProbe'),
                                         width = mainPanelWidth)
                                     )),

                            ##  Messungen hochladen
                            tabPanel(dataStep5$title,
                                     useShinyjs(),
                                     h3(style=tabTitleStyle,class="tab-title", dataStep5$title),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("dataFileEnc", "Encoding der Datei",
                                                     list('UTF-8'="UTF-8", 'ISO-8859-1'="ISO-8859-1", 'Windows-1252'="windows-1252")),
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
                                         width = sideBarWidth),
                                       mainPanel(
                                         dataTableOutput('tableData'),
                                         width = mainPanelWidth)
                                     ))),

                 ######################
                 ### Literaturdaten ###
                 ######################

                 navbarMenu("Literaturdaten",
                            ## Referenz (Ref)
                            tabPanel(litStep1$title,
                                     useShinyjs(),
                                     h3(style=tabTitleStyle,class="tab-title", litStep1$title),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("refFileEnc", "Encoding der Datei",
                                                     list('UTF-8'="UTF-8", 'ISO-8859-1'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("csvFileReferenz", "CSV-Datei mit Referenzen",
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt",
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owReferenz", "Alle Daten überschreiben?", FALSE),
                                         uiOutput("ReferenzValidationOut"),
                                         uiOutput("ReferenzDBConsistencyOut"),
                                         width = sideBarWidth),
                                       mainPanel(
                                         dataTableOutput('tableReferenz'),
                                         width = mainPanelWidth)
                                     )),

                            tabPanel(litStep2$title,
                                     useShinyjs(),
                                     h3(style=tabTitleStyle,class="tab-title", litStep2$title),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("litFileEnc", "Encoding der Datei",
                                                     list('UTF-8'="UTF-8", 'ISO-8859-1'="ISO-8859-1", 'Windows-1252'="windows-1252")),
                                         fileInput("csvFileLiteratur", "CSV-Datei mit Literaturwerten",
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt",
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owLiteratur", "Alle Daten überschreiben?", FALSE),
                                         uiOutput("LiteraturValidationOut"),
                                         uiOutput("LiteraturDBConsistencyOut"),
                                         width = sideBarWidth),
                                       mainPanel(dataTableOutput('tableLiteratur'),
                                                 width = mainPanelWidth)
                                     ))),

                 ## Dateien
                 tabPanel(filesTab$title,
                          useShinyjs(),
                          h3(style=tabTitleStyle,class="tab-title", filesTab$title),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("FileUploadCategory", "Wähle eine Referenz-Kategorie",
                                          choices = list("Ort", "Literatur")),
                              fileInput("FileUpload", "Hochzuladende Datei",
                                        buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt"),
                              checkboxInput("overrideFile", "Datei überschreiben?", FALSE),
                              uiOutput("FileUploadValidationOut"),
                              uiOutput("FileUploadDBConsistencyOut"),
                              width = sideBarWidth),
                            mainPanel(DTOutput('tableFileUploadReferenz'),
                                      width = mainPanelWidth)
                          ))

)


server <- function(input, output) {

  source("server_1_Ort.R", local = TRUE, encoding = "UTF-8")$value

  source("server_2_Probenahmestelle.R", local = TRUE, encoding = "UTF-8")$value

  source("server_3_Parameter.R", local = TRUE, encoding = "UTF-8")$value

  source("server_4_Probe.R", local = TRUE, encoding = "UTF-8")$value

  source("server_5_Messungen.R", local = TRUE, encoding = "UTF-8")$value

  source("server_Referenz.R", local = TRUE, encoding = "UTF-8")$value

  source("server_Literatur.R", local = TRUE, encoding = "UTF-8")$value

  source("server_Upload.R", local = TRUE, encoding = "UTF-8")$value
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

