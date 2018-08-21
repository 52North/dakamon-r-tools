# ui - DaKaMon viewer
library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(rjson)
library(RPostgreSQL)

source("conf.R", local = TRUE, encoding = "UTF-8")$value


## tools

connectToDB <- function() {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbName, user=dbUser, password=dbPassword, port=dbPort)
  
  if (Sys.info()["sysname"] == "Windows")
    dbSendQuery(db, "set client_encoding='windows-1252'")
  
  db
}

SOSgetObs <- function(obsProp, foiURI) {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
         <sos:GetObservation
         xmlns:sos=\"http://www.opengis.net/sos/2.0\"
         xmlns:fes=\"http://www.opengis.net/fes/2.0\"
         xmlns:gml=\"http://www.opengis.net/gml/3.2\"
         xmlns:swe=\"http://www.opengis.net/swe/2.0\"
         xmlns:xlink=\"http://www.w3.org/1999/xlink\"
         xmlns:swes=\"http://www.opengis.net/swes/2.0\"
         xmlns:sosrf=\"http://www.opengis.net/sosrf/1.0\"
         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" service=\"SOS\" version=\"2.0.0\" xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd\">
         <swes:extension>
         <swe:Boolean definition=\"MergeObservationsIntoDataArray\">
         <swe:value>true</swe:value>
         </swe:Boolean>
         </swes:extension>
         <sos:observedProperty>", obsProp, "</sos:observedProperty>
         <sos:featureOfInterest>", foiURI, "</sos:featureOfInterest>
         </sos:GetObservation>")
}

SOSgetObsByProc <- function(proc) {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <sos:GetObservation
    xmlns:sos=\"http://www.opengis.net/sos/2.0\"
    xmlns:fes=\"http://www.opengis.net/fes/2.0\"
    xmlns:gml=\"http://www.opengis.net/gml/3.2\"
    xmlns:swe=\"http://www.opengis.net/swe/2.0\"
    xmlns:xlink=\"http://www.w3.org/1999/xlink\"
    xmlns:swes=\"http://www.opengis.net/swes/2.0\"
    xmlns:sosrf=\"http://www.opengis.net/sosrf/1.0\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" service=\"SOS\" version=\"2.0.0\" xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd\">
    <swes:extension>
      <swe:Boolean definition=\"MergeObservationsIntoDataArray\">
        <swe:value>true</swe:value>
      </swe:Boolean>
    </swes:extension>
    <sos:procedure>", proc, "</sos:procedure>
  </sos:GetObservation>")
}

SOSgetObsByFoITime <- function(obsProp, time, foiURI) {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <sos:GetObservation
    xmlns:sos=\"http://www.opengis.net/sos/2.0\"
    xmlns:fes=\"http://www.opengis.net/fes/2.0\"
    xmlns:gml=\"http://www.opengis.net/gml/3.2\"
    xmlns:swe=\"http://www.opengis.net/swe/2.0\"
    xmlns:xlink=\"http://www.w3.org/1999/xlink\"
    xmlns:swes=\"http://www.opengis.net/swes/2.0\"
    xmlns:sosrf=\"http://www.opengis.net/sosrf/1.0\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" service=\"SOS\" version=\"2.0.0\" xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd\">
    <swes:extension>
      <swe:Boolean definition=\"MergeObservationsIntoDataArray\">
        <swe:value>true</swe:value>
      </swe:Boolean>
    </swes:extension>",
         paste("<sos:observedProperty>", obsProp, "</sos:observedProperty>", collapse = " \n "),
         "<sos:temporalFilter>
      <fes:TEquals>
        <fes:ValueReference>phenomenonTime</fes:ValueReference>
        <gml:TimeInstant gml:id=\"ti_1\">
          <gml:timePosition>", time, "</gml:timePosition>
        </gml:TimeInstant>
      </fes:TEquals>
    </sos:temporalFilter>
    <sos:featureOfInterest>", foiURI, "</sos:featureOfInterest>
  </sos:GetObservation>")
}
## /tools

ui <-  navbarPage("Datenansicht",
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
                                      downloadButton("exportKaRData", "Export als RData-Datei.")),
                             ## PNS
                             tabPanel(
                               "Probenahemstelle(n) auswählen",
                               column(12, DTOutput('tablePNS'),
                                      conditionalPanel(
                                        "$('#tablePNS').hasClass('recalculating')",
                                        tags$div('Lade ... ')
                                      ),
                                      textOutput("selTextPNS"),
                                      downloadButton("exportKaVsCSV", "Export als csv-Datei."),
                                      downloadButton("exportKaVsRData", "Export als RData-Datei."))),
                             ## Messdaten
                             tabPanel("Messdaten anzeigen",
                                      sidebarLayout(
                                        sidebarPanel(
                                          uiOutput("elemGroup"),
                                          uiOutput("obsPhen"),
                                          radioButtons("repBG",
                                                       label = "Ersetzung der Bestimmungsgrenze durch:",
                                                       choices = c("'BG'", "BG/2", "BG"), 
                                                       selected = "'BG'", inline=TRUE),
                                          radioButtons("repNG",
                                                       label = "Ersetzung der Nachweisgrenze durch:",
                                                       choices = c("'NG'", "NG/2", "NG"), 
                                                       selected = "'NG'", inline=TRUE),
                                          checkboxInput("randomId",
                                                        label = "Sollen IDs anonymisiert ausgegeben werden?",
                                                        value = FALSE),
                                          actionButton("refreshData", "Lade Daten aus der DB."),
                                          checkboxInput("computeStat",
                                                        label = "Sollen Statistiken berechnet werden?",
                                                        value = FALSE),
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
                                          DTOutput('tableStat'),
                                          conditionalPanel(
                                            "$('#tableStat').hasClass('recalculating')",
                                            tags$div('Berechne ... ')
                                          ))
                                        
                                      ))),
                  tabPanel("Literaturdaten",
                           sidebarLayout(
                             sidebarPanel(),
                             mainPanel()))
)

server <- function(input, output) {
  
  source("server_Messdaten.R", local = TRUE, encoding = "UTF-8")$value
  
  source("server_Literaturdaten.R", local = TRUE, encoding = "UTF-8")$value
  
}



shinyApp(ui, server)