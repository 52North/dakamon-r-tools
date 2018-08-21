################################################################################
########################   Upload der Parameter (PAR)   ########################
################################################################################

# storage of variables that might change through the GUI
inCSVPAR <- reactiveValues()
valiPAR <- reactiveValues(validated = FALSE)
checkDBPAR <- reactiveValues(checked = FALSE)

sepPAR <- colSep
decPAR <- decSep

observeEvent(input$csvFilePAR, {
  valiPAR$validated <- FALSE
  checkDBPAR$checked <- FALSE
  
  # check whether an encoding has been set; fallback: guess the eoncoding using readr
  if (is.null(csvEncode)) {
    csvEncode <- readr::guess_encoding(input$csvFilePAR$datapath)
    csvEncode <- csvEncode$encoding[which.max(csvEncode$confidence)]
  }
  
  inCSVPAR$csvEncode <- csvEncode
  
  inCSVPAR$df <- read.csv(input$csvFilePAR$datapath,
                          header = TRUE,
                          sep = sepPAR, dec = decPAR,
                          stringsAsFactors = FALSE, 
                          fileEncoding = inCSVPAR$csvEncode)
  
  inCSVPAR$headAsChar <- colnames(inCSVPAR$df)
  
  ## validation of PAR csv-file 
  # look for required column names
  # check whether columns have unique names
  
  txt <- NULL
  if (!(reqColPAR$id %in% inCSVPAR$headAsChar) || length(unique(inCSVPAR$df[,reqColPAR$id])) != length(inCSVPAR$df[,reqColPAR$id]))
    txt <- paste0(txt, "<li>Jeder Parameter benötigt eine persistente und eindeutige ID in der Spalte'", reqColPAR$id, "'.</li>")
  for (reqColName in reqColPAR[-1]) {
    if (!(reqColName %in% inCSVPAR$headAsChar))
      txt <- paste0(txt, "<li>Bitte die Spalte '", reqColName, "' ergänzen.</li>", sep="")
  }
  
  if(length(unique(inCSVPAR$headAsChar)) != length(inCSVPAR$headAsChar))
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")
  
  valiPAR$txt <- txt
  valiPAR$validated <- TRUE
})

# write txt feedback as html list - or action button

output$PARValidationOut <- renderUI({
  if (valiPAR$validated) {
    if (is.null(valiPAR$txt)) {
      actionButton("checkDB", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiPAR$txt, "</ul></div></html"))
    }
  } else {
    return()
  }
})

##########################
## check DB consistency ##
##########################

# find existing Parameters

observeEvent(input$checkDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbName, user=dbUser, password=dbPassword, port=dbPort)
  on.exit(dbDisconnect(db), add=T)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add = T)
  
  progress$set(message = "Prüfe bereits registrierte Parameter.", value = 0)
  
  # get all PARs from the DB that have any of the identifiers in the CSV
  if (length(inCSVPAR$df) > 0) {
    PARInDB <- dbGetQuery(db, paste0("SELECT observablepropertyid, identifier FROM observableproperty WHERE identifier IN ('",
                                   paste(inCSVPAR$df[,reqColPAR$id], collapse="', '"),"')"))
  
    if (!is.null(PARInDB) && length(PARInDB) > 0 && nrow(PARInDB) > 0) {
      checkDBPAR$txt <- paste("Folgende Parameter sind bereits in der DB: <ul><li>",
                              paste0(PARInDB$identifier, collapse="</li><li>"))
    } else {
      checkDBPAR$txt <- NULL
  }
  
    checkDBPAR$PARInDB <- PARInDB
  }
  
  checkDBPAR$checked <- TRUE
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button
# TODO check name in app.R
output$PARDBConsistencyOut <- renderUI({
  if (checkDBPAR$checked) {
    if (is.null(checkDBPAR$txt)) {
      actionButton("storeDB", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBPAR$txt, "</li></ul></div></html"))
    }
  } else {
    return()
  }
})

# plot table with CSV
output$tablePAR <- renderDataTable({
  if (!is.null(inCSVPAR$df)) {
    showTab <- inCSVPAR$df
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showHead <- paste0(showHead, "</span>")
    
    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sPAR=FALSE, dom="t",
                                       language=list(url = lngJSON)),
                        escape=FALSE)
    
    # if DB consistency has been checked, apply colors
    if (checkDBPAR$checked) {
      rowColors <- rep("white", nrow(showTab))
      
      if (nrow(checkDBPAR$PARInDB) > 0) {
        rowColors[showTab$ID %in% checkDBPAR$PARInDB] <- "red"
        showDT <- formatStyle(showDT, "ID", target="row",
                              backgroundColor = styleEqual(showTab$ID, rowColors))
      }
    }
    showDT
  }
})

###########################################
## Insert Parameter/ObservableProperties ##
###########################################

observeEvent(input$storeDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbName, user=dbUser, password=dbPassword, port=dbPort)
  on.exit(dbDisconnect(db), add=T)
  
  PAR_data <- inCSVPAR$df
  PAR_header <- inCSVPAR$headAsChar
  
  PAR_empty_cols <- apply(PAR_data, 2, function(x) all(is.na(x)))
  
  PAR_header <- PAR_header[!PAR_empty_cols]
  PAR_data <- PAR_data[,!PAR_empty_cols]
  
  nRowDf <- nrow(PAR_data)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add=T)
  
  progress$set(message = "Füge Parameter in DB ein.", value = 0)
  
  ## add missign columns
  regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata"))[,1]
  misCols <- which(sapply(paste0("param_", PAR_header), # TODO drop ID, parent identifier
                          function(x) is.na(match(x, regCols))))
  
  if (length(misCols > 0)) {
    for (i in 1:length(misCols)) {# i <- 1
      colId <- paste0("param_", sprintf("col%03d", i + length(regCols)))
      coltype = switch(class(PAR_data[,misCols[i]]),
                       integer = "numeric",
                       numeric = "numeric",
                       character = "character varying(255)")
      
      # TODO adopt to new FoI table
      dbSendQuery(db, paste0("ALTER TABLE parameter_data ADD COLUMN ", colId, " ", coltype, ";"))
      
      dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, dede)
                               VALUES ('", paste(colId, PAR_header[misCols[i]], sep="', '"),"')"))
    }
  }
  
  # if there are already PARe in the DB that are again in the CSV
  if (nrow(checkDBPAR$PARInDB) > 0) {
    ## UPDATE PAR via SQL, returns the id (pkid) of the updated parameter ##
    dbSendQuery(db, paste0("with update_param as (
        UPDATE observableproperty 
        SET
        name = name_var
        WHERE identifier = var
        RETURNING observablepropertyid
      )
      UPDATE parameter_data 
      SET
      param_col003 = param_col003_var,
      param_col004 = param_col004_var,
      param_col005 = param_col005_var,
      param_col006 = param_col006_var,
      param_col007 = param_col007_var
      WHERE observablepropertyid = (SELECT observablepropertyid FROM update_param)
      RETURNING observablepropertyid;"))
  } else {
    ## INSERT PAR via SQL, returns the id (pkid) of the inserted parameter ##
    dbSendQuery(db, paste0("with insert_param as (
        INSERT INTO observableproperty 
        (observablepropertyid, identifier, name) 
        VALUES 
        (nextval('observablepropertyid_seq'),
          'identifier_var',
          'name_var')
        RETURNING observablepropertyid
      )
      INSERT INTO parameter_data 
      SELECT  observablepropertyid,
      'param_col003_var',
      'param_col004_var',
      'param_col005_var',
      'param_col006_var',
      'param_col007_var'
      FROM insert_param
      RETURNING observablepropertyid;"))	
  }
  
  showModal(modalDialog(
    title = "Vorgang abgeschlossen.",
    "Parameter erfolgreich angelegt.",
    easyClose = TRUE,
    footer = NULL
  ))
})