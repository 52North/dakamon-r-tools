library(shiny)
library(DT)

library("rpostgis")
library("httr")

# db <- dbConnect("PostgreSQL", host="localhost", dbname="sos", user="postgres", password="postgres", port="5433")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("DaKaMon Importer"),
   
  tabsetPanel(
    tabPanel("FoI generation",
             sidebarLayout(
               sidebarPanel(fileInput("csvFileFoI", "Select a FoI file for upload."),
                            fluidRow(column(6, textInput("sepFoI", "Column separator:", value = ";", width = "80%")),
                                     column(6, textInput("decFoI", "Decimal separator:", value = ".", width = "80%"))),
                            selectInput("UoMFoI", "UoM row:", choices = c(NA, 1:10), selected = "1"),
                            textInput("exclRowFoI", "Exclude rows:"),
                            textInput("exclColFoI", "Exclude columns:"),
                            checkboxInput("owFoI", "Overwrite FoI?", FALSE), 
                            uiOutput("foiValidationOut"),
                            uiOutput("DBConsistencyTxtOut"),
                            uiOutput("DBConsistencyActionOut"),
                            width = 2),
               mainPanel(dataTableOutput('tableHeadFoI'),
                         dataTableOutput('tableFoI')))),
  
    tabPanel("Data upload",
             sidebarLayout(
               sidebarPanel(fileInput("csvFileData", "Select a data file for upload."),
                            checkboxInput("headerData", "Header", TRUE),
                            fluidRow(column(6, textInput("sepData", "Column separator:", value = ";", width = "80%")),
                                     column(6, textInput("decData", "Decimal separator:", value = ".", width = "80%"))),
                            selectInput("UoMData", "UoM row:", choices = c(NA, 1:10), selected = "1"),
                            selectInput("BgData", "Detection threshold row:", choices = c(NA, 1:10), selected = "2"),
                            selectInput("StgrData", "Element group row:", choices = c(NA, 1:10), selected = "3"),
                            textInput("exclRowData", "Exclude rows:"),
                            textInput("exclColData", "Exclude columns:"),
                            checkboxInput("owData", "Overwrite Data?", FALSE), 
                            actionButton("storeData", "Store in DB!"),
                            width=2),
               mainPanel(dataTableOutput('tableData'))
   )))
)

server <- function(input, output) {
  
  ## FoI logic
  # toAdd: validate FoIs IDs?
  # toAdd: restrict hierarchie?
  # toAdd: validate UoM?
  # add Button "check DB consistency" -> list known vs new variables; matching vs non-matching uom ( -> hottable to fix?) -> manually update csv file
  inCSVFoI <- reactive({
    inFile <- input$csvFileFoI
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = TRUE, sep = input$sepFoI, dec = input$decFoI, stringsAsFactors = FALSE)
  })
  
  inclRowFoI <- reactive({
    if (is.null(inCSVFoI())) return(numeric())
    exclText <- input$exclRowFoI 
    if (is.null(exclText))
      return(1:ncol(inCSVFoI()))
    exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
    !c(1:nrow(inCSVFoI())) %in% c(exclNum[!is.na(exclNum)], input$UoMFoI)
  })
  
  inclColFoI <- reactive({
    if (is.null(inCSVFoI())) return(numeric())
    exclText <- input$exclColFoI
    if (is.null(exclText))
      return(1:ncol(inCSVFoI()))
    exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
    !c(1:ncol(inCSVFoI())) %in% exclNum[!is.na(exclNum)]
  })
  
  vali <- reactiveValues(csv=FALSE)
  
  ## validation of csv
  # look for Name, ID, lat, lon and super_FoI,
  # check whether columns have unique names
  output$foiValidationOut <- renderUI({
    txt <- NULL
    foi_header <- colnames(inCSVFoI()[, inclColFoI(), drop=F])
    if (!("ID" %in% foi_header) || length(unique(foi_header)) != length(foi_header))
      txt <- paste(txt, "<li>An unique identifier is mandatory for each feature of interest; please supply a non-empty and unique column 'ID'.</li>", sep="")
    if (!("Name" %in% foi_header))
      txt <- paste(txt, "<li>A name is mandatory for each feature of interest; please supply a non-empty column 'Name'.</li>", sep="")
    if (!("lat" %in% foi_header))
      txt <- paste(txt, "<li>Latitude is mandatory for each feature of interest; please supply a non-empty column 'lat'.</li>", sep="")
    if (!("lon" %in% foi_header))
      txt <- paste(txt, "<li>Longitude is mandatory for each feature of interest; please supply a non-empty column 'lon'.</li>", sep="")
    if (!("super_FoI" %in% foi_header))
      txt <- paste(txt, "<li>A superior feature of interest is mandatory for each feature of interest (yet, it might be empty); please supply a column 'super_FoI'.</li>", sep="")
    comp_header <- outer(foi_header, foi_header, "==")
    if(any(comp_header[upper.tri(comp_header)]))
      txt <- paste(txt, "<li>Column names must be unique.</li>", sep="")
    if (is.null(txt)) {
      vali$csv <- TRUE
      return(actionButton("checkDB", "Check DB consistency!"))
    }
    
    vali$csv <- FALSE
    return(HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", txt, "</ul></div></html")))
  })
  
  observeEvent(input$csvFileFoI, {
    if(!vali$csv) {
               output$DBConsistencyTxtOut <- renderUI(HTML(""))
               output$DBConsistencyActionOut  <- renderUI(HTML(""))}})
  
  ## check DB consistency:
  # find existing FoIs
  # check Parameter and their UoMs
  observeEvent(input$checkDB, {
    txtInfo <- NULL
    txtErr <- NULL
    FoIinDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('", 
                                    paste(inCSVFoI()$ID[inclRowFoI()], collapse="', '"),"')"))
    txtInfo <- paste("The following features are already in the DB: <ul><li>",
                     paste0(FoIinDB$identifier, collapse="</li><li>"))
    vali$foiInDB <- FoIinDB$identifier
  
    foi_header <- colnames(inCSVFoI()[, inclColFoI(), drop=F])
    colinDB <- dbGetQuery(db, paste0("SELECT columnid, dede, unit.unit FROM foidatametadata left outer join unit on (unit.unitid = uom) WHERE dede IN ('", 
                                     paste(foi_header, collapse="', '"),"')"))
    colinDB$unit[is.na(colinDB$unit)] <- ""
    
    foi_uom <- as.character(inCSVFoI()[c(input$UoMFoI), inclColFoI()])
    foi_uom[foi_uom == "NA"] <- ""
    compUoM <- foi_uom[match(colinDB$dede, foi_header)] == colinDB$unit
    
    vali$uomMissMatchCols <- colinDB$dede[which(compUoM)]
    
    # uoms <- colinDB$uom[!is.na(colinDB$uom)]
    # if (length(uoms) > 0) {
    #   colinDB_UoMs <- dbGetQuery(db, paste0("SELECT unitid, unit FROM unit WHERE unitid IN ('", 
    #                                         paste(uoms, collapse="', '"),"')"))
    # }
    # colinDB_UoMs
    
    if (is.null(txtErr) && vali$csv) {
      output$DBConsistencyTxtOut <- renderUI(HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", txtInfo, "</li></ul></div></html")))
      output$DBConsistencyActionOut <- renderUI(actionButton("storeDB", "Store in DB!"))
    } else {
      output$DBConsistencyTxtOut <- renderUI(HTML(paste0("<html>", txtInfo, "</html")))
      output$DBConsistencyActionOut <- renderUI(actionButton("storeDB", "Store in DB!"))
    }
    }, ignoreInit=TRUE)
  
  
  
  # foi_data <- read.csv("Daten/FoI_sample.csv", sep = ";", header = FALSE, skip = 2, stringsAsFactors = FALSE)
  # 
  # foi_empty_cols <- apply(foi_data, 2, function(x) all(is.na(x)))
  # foi_header <- as.character(foi_header[,!foi_empty_cols])
  # 
  # foi_uom <- as.character(foi_uom[,!foi_empty_cols])
  # foi_data <- foi_data[,!foi_empty_cols]
  # 
  # colnames(foi_data) <- foi_header

  # output$tableHeadFoI <- DT::renderDataTable(inCSVFoI()[c(input$UoMFoI), inclColFoI(), drop=F],
  #                                            options = list(paging=FALSE, bFilter=FALSE))
  
  output$tableFoI <- DT::renderDataTable({
    print(vali$foiInDB)
    if (!is.null(inCSVFoI())) {
    showTab <- inCSVFoI()[inclRowFoI(), inclColFoI(), drop=F]
    if (!is.na(input$UoMFoI)  && ! is.null(input$UoMFoI)) {
      showUoM <- as.character(inCSVFoI()[c(input$UoMFoI), inclColFoI()])
      showUoM <- sapply(showUoM, function(x) {
        if (nchar(x) > 0 && x != "NA") {
          paste0(" [",x,"]")
        } else {
          ""
        }})
      if (!is.null(inCSVFoI()))              
        colnames(showTab) <- paste0(colnames(showTab), showUoM)
    }
    if(!is.null(vali$foiInDB)) {
      datatable(showTab,  options = list(paging=FALSE, bFilter=FALSE))  %>% formatStyle("ID",
                                                                                        target="row",
                                                                                        backgroundColor=styleEqual(vali$foiInDB, rep("yellow", length(vali$foiInDB))))
      } else {
        datatable(showTab,  options = list(paging=FALSE, bFilter=FALSE))
      }  }} )
    
  

  

  

  # data tab logic:
  # toAdd: validate UoM?
  # add Button "check DB consistency" -> list known vs new variables; matching vs non-matching uom ( -> hottable to fix?) -> manually update csv file
  inCSVData <- reactive({
    inFile <- input$csvFileData
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = input$headerData, sep=input$sepData, dec=input$decData)
  })
  
  inclRowData <- reactive({
    if (is.null(inCSVData())) return(numeric())
    exclText <- input$exclRowData
    if (is.null(exclText))
      return(1:ncol(inCSVData()))
    exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
    !c(1:nrow(inCSVData())) %in% exclNum[!is.na(exclNum)]
  })
  
  inclColData <- reactive({
    if (is.null(inCSVData())) return(numeric())
    exclText <- input$exclColData
    if (is.null(exclText))
      return(1:ncol(inCSVData()))
    exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
    !c(1:ncol(inCSVData())) %in% exclNum[!is.na(exclNum)]
  })
  
  output$tableData <- DT::renderDataTable(inCSVData()[inclRowData(), inclColData(), drop=F], 
                                      filter="top",
                                      options = list(paging=FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)

