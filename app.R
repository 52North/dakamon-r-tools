library(shiny)
library(DT)

library("rpostgis")
library("httr")

# db <- dbConnect("PostgreSQL", host="localhost", dbname="sos", user="postgres", password="postgres", port="5432")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("DaKaMon Importer"),
   
  tabsetPanel(
    tabPanel("FoI generation",
             sidebarLayout(
               sidebarPanel(fluidRow(column(6, textInput("sepFoI", "Column separator:", value = ";", width = "80%")),
                                     column(6, textInput("decFoI", "Decimal separator:", value = ".", width = "80%"))),
                            fileInput("csvFileFoI", "Select a FoI file for upload."),
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

  ###############################################################################
  ###################################         ###################################
  ###################################   FoI   ###################################
  ###################################         ###################################
  ###############################################################################
  
  inCSVFoI <- reactiveValues()
  vali <- reactiveValues(validated = FALSE)
  checkDB <- reactiveValues(checked = FALSE)
  
  ## FoI logic
  # toAdd: validate FoIs IDs?
  # toAdd: restrict hierarchie?
  # toAdd: validate UoM?
  # add Button "check DB consistency" -> list known vs new variables; matching vs non-matching uom ( -> hottable to fix?) -> manually update csv file
  
  inclRowFoI <- reactive({
    if (is.null(inCSVFoI$df)) 
      return(numeric())
    exclText <- input$exclRowFoI 
    if (is.null(exclText))
      return(1:ncol(inCSVFoI$df))
    exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
    !c(1:nrow(inCSVFoI$df)) %in% exclNum[!is.na(exclNum)]
  })
  
  inclColFoI <- reactive({
    if (is.null(inCSVFoI$df))
      return(numeric())
    exclText <- input$exclColFoI
    if (is.null(exclText))
      return(1:ncol(inCSVFoI$df))
    exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
    !c(1:ncol(inCSVFoI$df)) %in% exclNum[!is.na(exclNum)]
  })
  
  observeEvent(input$csvFileFoI, {
    vali$validated <- FALSE
    checkDB$checked <- FALSE
    
    inCSVFoI$headAsChar <- as.character(read.csv(input$csvFileFoI$datapath,
                                                 header = FALSE,
                                                 sep = input$sepFoI, dec = input$decFoi,
                                                 nrows = 1, stringsAsFactors = FALSE))

    inCSVFoI$UoMs <- read.csv(input$csvFileFoI$datapath, header = FALSE,
                              sep = input$sepFoI, dec = input$decFoi,
                              skip = as.numeric(input$UoMFoI), nrows = 1,
                              stringsAsFactors = FALSE)
    inCSVFoI$UoMs[is.na(inCSVFoI$UoMs)] <- ""
    inCSVFoI$UoMs <- as.character(inCSVFoI$UoMs)

    inCSVFoI$df <- read.csv(input$csvFileFoI$datapath, header = FALSE,
                            sep = input$sepFoI, dec = input$decFoi,
                            skip = as.numeric(input$UoMFoI)+1,
                            stringsAsFactors = FALSE)
    colnames(inCSVFoI$df) <- inCSVFoI$headAsChar
  
    ############################
    ## validation of csv-file ##
    ############################
    
    txt <- NULL
    if (!("ID" %in% inCSVFoI$headAsChar) || length(unique(inCSVFoI$headAsChar)) != length(inCSVFoI$headAsChar))
      txt <- paste(txt, "<li>An unique identifier is mandatory for each feature of interest; please supply a non-empty and unique column 'ID'.</li>", sep="")
    if (!("Name" %in% inCSVFoI$headAsChar))
      txt <- paste(txt, "<li>A name is mandatory for each feature of interest; please supply a non-empty column 'Name'.</li>", sep="")
    if (!("lat" %in% inCSVFoI$headAsChar))
      txt <- paste(txt, "<li>Latitude is mandatory for each feature of interest; please supply a non-empty column 'lat'.</li>", sep="")
    if (!("lon" %in% inCSVFoI$headAsChar))
      txt <- paste(txt, "<li>Longitude is mandatory for each feature of interest; please supply a non-empty column 'lon'.</li>", sep="")
    if (!("super_FoI" %in% inCSVFoI$headAsChar))
      txt <- paste(txt, "<li>A superior feature of interest is mandatory for each feature of interest (yet, it might be empty); please supply a column 'super_FoI'.</li>", sep="")
    
    vali$txt <- txt
    
    comp_header <- outer(inCSVFoI$headAsChar, inCSVFoI$headAsChar, "==")
    if(any(comp_header[upper.tri(comp_header)]))
      vali$txt <- paste(vali$txt, "<li>Column names must be unique.</li>", sep="")
    
    vali$validated <- TRUE
  })

  
  # look for Name, ID, lat, lon and super_FoI,
  # check whether columns have unique names
  
  output$foiValidationOut <- renderUI({
    if (vali$validated) {
      if (is.null(vali$txt)) {
        actionButton("checkDB", "Check DB consistency!")
      } else {
        HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", vali$txt, "</ul></div></html"))
      }
    } else {
      return()
    }
  })
  
  ##########################
  ## check DB consistency ##
  ##########################
  
  # find existing FoIs
  # check Parameter and their UoMs
    
  observeEvent(input$checkDB, {
    FoIinDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('", 
                                     paste(inCSVFoI$df$ID[inclRowFoI()], collapse="', '"),"')"))
    checkDB$txtInfo <- paste("The following features are already in the DB: <ul><li>",
                             paste0(FoIinDB$identifier, collapse="</li><li>"))
    
    checkDB$foiInDB <- FoIinDB$identifier

    # find columns already in DB: colInDB with columnid, dede and its unit from the unit table
    checkDB$colInDB <- dbGetQuery(db, paste0("SELECT columnid, dede, unit.unit FROM foidatametadata left outer join unit on (unit.unitid = uom) WHERE dede IN ('", 
                                     paste(inCSVFoI$headAsChar, collapse="', '"),"')"))

    # replace NA UoM with ""
    checkDB$colInDB$unit[is.na(checkDB$colInDB$unit)] <- ""
    
    # compare UoMs from the csv with the DB for overlapping columns
    compUoM <- inCSVFoI$UoMs[match(checkDB$colInDB$dede, inCSVFoI$headAsChar)] == checkDB$colInDB$unit
    checkDB$uomMissMatchCols <-which(!compUoM)
    
    checkDB$txtErr <- NULL
    if (length(checkDB$uomMissMatchCols) > 0) {
      checkDB$txtErr <-
        paste(
          "The following columns have non-matching units of measurement: <ul><li>",
          paste0(
            paste0(checkDB$colInDB$dede[checkDB$uomMissMatchCols], ": [",
                  checkDB$colInDB$unit[checkDB$uomMissMatchCols], "] "),
            collapse = "</li><li>"
          )
        )
    }
    
    checkDB$checked <- TRUE
  }, ignoreInit=TRUE)
  
  output$DBConsistencyTxtOut <- renderUI({
    if (checkDB$checked) {
      if (is.null(checkDB$txtErr)) {
        HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDB$txtInfo, "</li></ul></div></html"))
      } else {
        HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDB$txtErr, "</li></ul></div></html"))
      }
    } else {
      HTML("")
    }
  })
  
  output$DBConsistencyActionOut <- renderUI({
    if (checkDB$checked) {
      if (is.null(checkDB$txtErr)) {
        if (is.null(checkDB$txtInfo) || input$owFoI) {
          actionButton("storeDB", "Store in DB!")
        } 
      } 
    } else {
      HTML("")
    }
  })
  
  output$tableFoI <- DT::renderDataTable({
    if (!is.null(inCSVFoI$df)) {
      showTab <- inCSVFoI$df[inclRowFoI(), inclColFoI(), drop=F]

      if (!is.na(input$UoMFoI) && !is.null(input$UoMFoI)) {
        showUoM <- sapply(inCSVFoI$UoMs, function(x) {
          if (!is.na(x) & nchar(x) > 0) {
            paste0(" [",x,"]")
          } else {
            ""
          }
        })
        if (!is.null(inCSVFoI$df))            
          colnames(showTab) <- paste0(colnames(showTab), showUoM)
      }
      
      showDT <- datatable(showTab,  options = list(paging=FALSE, bFilter=FALSE))
      
      # if DB consistency has been checked, apply colors 
      if (checkDB$checked) {
        rowClrs <- rep("white", nrow(showTab))

        if (!is.null(checkDB$txtInfo)) {
          rowClrs[which(showTab$ID %in% checkDB$foiInDB)] <- "yellow"
          for (col in which(inCSVFoI$headAsChar %in% checkDB$colInDB$dede)) {
            if (col %in% checkDB$uomMissMatchCols) next;
            showDT <- formatStyle(showDT, col, "ID", 
                                  backgroundColor = styleEqual(showTab$ID, rowClrs))
          }
        }
        
        if (!is.null(checkDB$txtErr)) {
          rowClrs <- rep("red", nrow(showTab))
          for (col in checkDB$uomMissMatchCols) {
            showDT <- formatStyle(showDT, col, "ID", 
                                  backgroundColor = styleEqual(showTab$ID, rowClrs))
          }
        }
      }
      showDT
    }
  })
  
  ################################################################################
  ###################################          ###################################
  ###################################   DATA   ###################################
  ###################################          ###################################
  ################################################################################

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

