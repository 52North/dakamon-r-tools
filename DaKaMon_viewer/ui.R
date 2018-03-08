# ui - DaKaMon viewer
library(shiny)
library("rpostgis")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Ka selection", 
             column(12, dataTableOutput('table'),
                    textOutput("selText"))
    ), 
    tabPanel("Sub FOI",
             column(12, dataTableOutput('table2'),
                    textOutput("selText2"))
    ),
    tabPanel("Data",
             column(12, dataTableOutput('tabSummary')),
             column(12, br()),
             column(12, dataTableOutput('table3'))
    )
  )
)
