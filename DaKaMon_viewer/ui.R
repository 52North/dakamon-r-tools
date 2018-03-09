# ui - DaKaMon viewer
library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Stammanalgen selection", 
             column(12, dataTableOutput('table'),
                    textOutput("selText"))
    ), 
    tabPanel("Kläranlagen feature selection",
             column(12, dataTableOutput('table2'),
                    textOutput("selText2"))
    ),
    tabPanel("Daten",
             sidebarLayout(
               sidebarPanel(uiOutput("obsPhen"), width = 2),
               mainPanel(column(12, dataTableOutput('tabSummary')),
                         column(12, br()),
                         column(12, dataTableOutput('table3'))))
             
    )
  )
)
