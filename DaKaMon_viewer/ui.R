# ui - DaKaMon viewer
library(shiny)
library(DT)
library(shinyjs)

BGchar <- "< BG"
# BGlabel <- "Bestimmungsgrenze" # label in DB

ui <- fluidPage(tabsetPanel(
  tabPanel(
    "Kläranlagen",
    column(12, DTOutput('tableFoi'),
           conditionalPanel(
             "$('#tableFoi').hasClass('recalculating')",
             tags$div('Lade ... ')
           ),
           textOutput("selText"),
           downloadButton("exportKaCSV", "Export als csv-Datei."),
           downloadButton("exportKaRData", "Export als RData-Datei."))
  ),
  tabPanel(
    "Verfahrensschritte",
    column(12, DTOutput('table2'),
           conditionalPanel(
             "$('#table2').hasClass('recalculating')",
             tags$div('Lade ... ')
           ),
           textOutput("selText2"),
           downloadButton("exportKaVsCSV", "Export als csv-Datei."),
           downloadButton("exportKaVsRData", "Export als RData-Datei."))
  ),
  tabPanel("Daten",
           sidebarLayout(
             sidebarPanel(
               uiOutput("elemGroup"),
               uiOutput("obsPhen"),
               checkboxInput(
                 "repBG",
                 label = paste0("Sollen Werte unterhalb der Bestimmungsgrenze durch '", BGchar, "' ersetzt werden?"),
                 value = FALSE
               ),
               checkboxInput("randomId",
                             label = "Sollen IDs anonymisiert ausgegeben werden?",
                             value = FALSE),
               actionButton("refreshData", "Lade Daten aus der DB."),
               checkboxInput("computeStat",
                             label = "Sollen Statistiken berechnet werden?",
                             value = FALSE),
               checkboxInput("includeMetaHead",
                             label = "Sollen die Metadaten mit exportiert werden?",
                             value = FALSE),
               downloadButton("exportDataCSV", "Export als csv-Datei."),
               downloadButton("exportDataRData", "Export als RData-Datei."),
               width = 2),
             mainPanel(
               DTOutput('table3'),
               conditionalPanel(
                 "$('#table3').hasClass('recalculating')",
                 tags$div('Lade ... ')
               ),
               textOutput("selText3"),
               br(),
               DTOutput('table3stat'),
               conditionalPanel(
                 "$('#table3stat').hasClass('recalculating')",
                 tags$div('Berechne ... ')
               ))
             
           ))
))
