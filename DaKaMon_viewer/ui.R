# ui - DaKaMon viewer
library(shiny)

BGchar <- "< BG"
# BGlabel <- "Bestimmungsgrenze" # label in DB

ui <- fluidPage(tabsetPanel(
  tabPanel(
    "Stammanalgen selection",
    column(12, dataTableOutput('table'),
           conditionalPanel(
             "$('#table').hasClass('recalculating')",
             tags$div('Loading ... ')
           ),
           textOutput("selText"),
           downloadButton("exportKaCSV", "Export as csv-file."),
           downloadButton("exportKaRData", "Export as RData-file."))
  ),
  tabPanel(
    "Kläranlagen feature selection",
    column(12, dataTableOutput('table2'),
           conditionalPanel(
             "$('#table2').hasClass('recalculating')",
             tags$div('Loading ... ')
           ),
           textOutput("selText2"),
           downloadButton("exportKaFCSV", "Export as csv-file."),
           downloadButton("exportKaFRData", "Export as RData-file."))
  ),
  tabPanel("Daten",
           sidebarLayout(
             sidebarPanel(
               uiOutput("obsPhen"),
               checkboxInput(
                 "repBG",
                 label = paste0("Shall values below the detection limit be replaced by ", BGchar, "?"),
                 value = FALSE
               ),
               checkboxInput("computeStat",
                             label = "Compute summary statistics?",
                             value = FALSE),
               actionButton("refreshData", "Load data from DB."),
               checkboxInput("includeMetaHead",
                            label = "Include metadata header in export?",
                            value = FALSE),
               downloadButton("exportCSV", "Export as csv-file."),
               downloadButton("exportRData", "Export as RData-file."),
               width = 2
             ),
             mainPanel(
               dataTableOutput('table3stat'),
               conditionalPanel(
                 "$('#table3stat').hasClass('recalculating')",
                 tags$div('Calculating ... ')
               ),
               br(),
               dataTableOutput('table3'),
               conditionalPanel(
                 "$('#table3').hasClass('recalculating')",
                 tags$div('Loading ... ')
               )
             )
             
           ))
))
