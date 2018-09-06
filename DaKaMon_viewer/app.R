# ui - DaKaMon viewer
library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(rjson)
library(RPostgreSQL)
library(stringi)
library(pool)

source("conf.R", local = TRUE, encoding = "UTF-8")$value

## tools
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

## /tools

ui <-  navbarPage("Datenansicht", id="inNavbarpage",
                  navbarMenu("Messdaten", 
                             ## Ort
                             tabPanel("Ort(e) auswählen",
                                      uiOutput("ewsSelInput"),
                                      DTOutput('tableOrt'),
                                      conditionalPanel(
                                        "$('#tableOrt').hasClass('recalculating')",
                                        tags$div('Lade ... ')
                                      ),
                                      textOutput("selTextOrt"),
                                      downloadButton("exportKaCSV", "Export als csv-Datei."),
                                      downloadButton("exportKaRData", "Export als RData-Datei."),
                                      br(),
                                      actionButton("fromOrtToPNS", "Weiter...")),
                             ## PNS
                             tabPanel(
                               "Probenahmestelle(n) auswählen",
                               DTOutput('tablePNS'),
                               conditionalPanel(
                                 "$('#tablePNS').hasClass('recalculating')",
                                 tags$div('Lade ... ')
                               ),
                               textOutput("selTextPNS"),
                               downloadButton("exportKaVsCSV", "Export als csv-Datei."),
                               downloadButton("exportKaVsRData", "Export als RData-Datei."),
                               br(),
                               actionButton("fromPNStoMessdaten", "Weiter...")),
                             ## Messdaten
                             tabPanel("Messdaten anzeigen",
                                      sidebarLayout(
                                        sidebarPanel(
                                          uiOutput("elemGroup"),
                                          uiOutput("obsPhen"),
                                          checkboxInput("showBG",
                                                        label = "Lade BG-Wert",
                                                        value = FALSE),
                                          checkboxInput("showNG",
                                                        label = "Lade NG-Wert",
                                                        value = FALSE),
                                          checkboxInput("showSG",
                                                        label = "Lade Stoffgruppe",
                                                        value = FALSE),
                                          radioButtons("repBG",
                                                       label = "Ersetze Werte unterhalb der Bestimmungsgrenze durch:",
                                                       choices = c("'BG'", "0", "BG/2", "BG"), 
                                                       selected = "'BG'", inline=TRUE),
                                          checkboxInput("randomId",
                                                        label = "Anonymisiere IDs",
                                                        value = FALSE),
                                          actionButton("refreshData", "Lade Daten aus der DB."),
                                          checkboxInput("computeStat",
                                                        label = "Berechne Statistiken",
                                                        value = FALSE),
                                          textOutput("warnUnit"),
                                          downloadButton("exportDataCSV", "Export als csv-Datei."),
                                          downloadButton("exportDataRData", "Export als RData-Datei."),
                                          width = 2),
                                        mainPanel(
                                          DTOutput('tableDaten'),
                                          conditionalPanel(
                                            "$('#tableDaten').hasClass('recalculating')",
                                            tags$div('Lade ... ')
                                          ),
                                          textOutput("selTextDaten"),
                                          br(),
                                          DTOutput('tableStatistik'),
                                          conditionalPanel(
                                            "$('#tableStat').hasClass('recalculating')",
                                            tags$div('Berechne ... ')
                                          ))
                                        
                                      ))),
                  tabPanel("Parameter anzeigen",
                           uiOutput("paramElemGroupInput"),
                           DTOutput("tableParameter")),
                  tabPanel("Proben anzeigen",
                           uiOutput("probenPNSInput"),
                           DTOutput("tableProben")),
                  navbarMenu("Literaturdaten",
                             tabPanel("Parameter",
                                      uiOutput("litParamInput"),
                                      DTOutput("tableLitParam"),
                                      br(),
                                      actionButton("fromParamtoThematik", "Weiter...")),
                             tabPanel("Thematik", # Thematik (vorherEntwässerungssystem) (Gewässer, Kläranlagen, etc.)
                                      uiOutput("litThematikInput"),
                                      DTOutput("tableLitThematik"),
                                      br(),
                                      actionButton("fromThematikToPub", "Weiter...")),
                             tabPanel("Publikation", # pubId
                                      uiOutput("litPubIdInput"),
                                      DTOutput("tableLitPubId"),
                                      actionButton("fromPubToLit", "Weiter...")),
                             tabPanel("Literatur", # literatur
                                      uiOutput("litInput"),
                                      DTOutput("tableLit")))
)

server <- function(input, output, session) {
  
  source("server_Messdaten.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_Parameter.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_Proben.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_Literaturdaten.R", local = TRUE, encoding = "UTF-8")$value
}



shinyApp(ui, server, 
         onStart = function() {
           cat("Starting Application ...")
           # Ensure the DB pool closes all connections
           onStop(function() {
             cat("Closing DB connection pool")
             poolClose(pool)
           })
         })