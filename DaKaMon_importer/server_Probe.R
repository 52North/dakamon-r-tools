################################################################################
############################   Upload der Probe   ##############################
################################################################################

# storage of variables that might change through the GUI
inCSVProbe <- reactiveValues()
valiProbe <- reactiveValues(validated = FALSE)
checkDBProbe <- reactiveValues(checked = FALSE)

sepProbe <- colSep
decProbe <- decSep

observeEvent(input$csvFileProbe, {
  valiProbe$validated <- FALSE
  checkDBProbe$checked <- FALSE
  
  # check whether an encoding has been set; fallback: guess the eoncoding using readr
  if (is.null(csvEncode)) {
    csvEncode <- readr::guess_encoding(input$csvFileProbe$datapath)
    csvEncode <- csvEncode$encoding[which.max(csvEncode$confidence)]
  }
  
  inCSVProbe$csvEncode <- csvEncode
  
  inCSVProbe$df <- read.csv(input$csvFileProbe$datapath,
                            header = TRUE,
                            sep = sepProbe, dec = decProbe,
                            stringsAsFactors = FALSE, 
                            fileEncoding = inCSVProbe$csvEncode)
  
  inCSVProbe$headAsChar <- colnames(inCSVProbe$df)
  
  ## validation of Probe csv-file 
  # look for required column names
  # check whether columns have unique names
  
  txt <- NULL
  if (!(reqColProbe$id %in% inCSVProbe$headAsChar) || length(unique(inCSVProbe$df[,reqColProbe$id])) != length(inCSVProbe$df[,reqColProbe$id]))
    txt <- paste0(txt, "<li>Jede Probe benötigt eine persistente und eindeutige ID in der Spalte'", reqColProbe$id, "'.</li>")
  for (reqColName in reqColProbe[-1]) {
    if (!(reqColName %in% inCSVProbe$headAsChar))
      txt <- paste0(txt, "<li>Bitte die Spalte '", reqColName, "' ergänzen.</li>", sep="")
  }
  
  if(length(unique(inCSVProbe$headAsChar)) != length(inCSVProbe$headAsChar))
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")
  
  valiProbe$txt <- txt
  valiProbe$validated <- TRUE
})

# write txt feedback as html list - or action button

output$ProbeValidationOut <- renderUI({
  if (valiProbe$validated) {
    if (is.null(valiProbe$txt)) {
      actionButton("checkDB", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiProbe$txt, "</ul></div></html"))
    }
  } else {
    return()
  }
})

##########################
## check DB consistency ##
##########################

# find existing Proben; check whether the PNSID exists

observeEvent(input$checkDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname="sos", user="postgres", password="postgres", port="5432")
  on.exit(dbDisconnect(db), add=T)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add = T)
  
  progress$set(message = "Prüfe bereits registrierte Proben.", value = 0)
  
  # get all Probes from the DB that have any of the identifiers in the CSV
  ProbeInDB <- dbGetQuery(db, paste0("SELECT probeid, identifier FROM probe WHERE identifier IN ('",
                                     paste(inCSVProbe$df[,reqColProbe$id], collapse="', '"),"')"))
  if (nrow(ProbeInDB) > 0) {
    checkDBProbe$txt <- paste("Folgende Proben sind bereits in der DB: <ul><li>",
                              paste0(ProbeInDB$identifier, collapse="</li><li>"))
  } else {
    checkDBProbe$txt <- NULL
  }
  
  checkDB$ProbeInDB <- ProbeInDB
  
  # check PNS ids
  PNSInDB <- dbGetQuery(db, paste0("SELECT identifier FROM featureofinterest WHERE identifier IN ('",
                                   paste(inCSVProbe$df[,reqColProbe$geoSub], collapse="', '"),"')"))
  misPNSIDs <- which(!(inCSVProbe$df[,reqColProbe$geoSub] %in% PNSInDB$identifier))
  
  if(length(misPNSIDs) > 0) # error state: no upload
    checkDBProbe$txt <- paste("Folgende Probenahestellen fehlen in der DB: <ul><li>",
                              paste0(inCSVProbe$df[misPNSIDs, reqColProbe$geoSub], collapse="</li><li>"))
  
  checkDBProbe$checked <- TRUE
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button
output$ProbeDBConsistencyOut <- renderUI({
  if (checkDBProbe$checked) {
    if (is.null(checkDBProbe$txt)) {
      actionButton("storeDB", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBProbe$txt, "</li></ul></div></html"))
    }
  } else {
    return()
  }
})

# plot table with CSV
output$tableProbe <- renderDataTable({
  if (!is.null(inCSVProbe$df)) {
    showTab <- inCSVProbe$df
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showHead <- paste0(showHead, "</span>")
    
    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sProbe=FALSE, dom="t",
                                       language=list(url = lngJSON)),
                        escape=FALSE)
    
    # if DB consistency has been checked, apply colors
    if (checkDBProbe$checked) {
      rowColors <- rep("white", nrow(showTab))
      
      if (nrow(checkDBProbe$ProbeInDB) > 0) {
        rowColors[showTab$ID %in% checkDBProbe$ProbeInDB] <- "red"
        showDT <- formatStyle(showDT, "ID", target="row",
                              backgroundColor = styleEqual(showTab$ID, rowColors))
      }
    }
    showDT
  }
})

#####################
## Insert Probe ##
#####################

observeEvent(input$storeDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbname, user="postgres", password="postgres", port="5432")
  on.exit(dbDisconnect(db), add=T)
  
  Probe_data <- inCSVProbe$df
  Probe_header <- inCSVProbe$headAsChar
  
  Probe_empty_cols <- apply(Probe_data, 2, function(x) all(is.na(x)))
  
  Probe_header <- Probe_header[!Probe_empty_cols]
  Probe_data <- Probe_data[,!Probe_empty_cols]
  
  nRowDf <- nrow(Probe_data)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add=T)
  
  progress$set(message = "Füge Proben in DB ein.", value = 0)
  
  ## add missign columns
  regCols <- dbGetQuery(db, paste0("SELECT dede FROM columnmetadata"))[,1]
  misCols <- which(sapply(paste0("Probe_", Probe_header), # TODO drop ID, Probeent identifier
                          function(x) is.na(match(x, regCols))))
  
  if (length(misCols > 0)) {
    for (i in 1:length(misCols)) {# i <- 1
      colId <- paste0("Probe_", sprintf("col%03d", i + length(regCols)))
      coltype = switch(class(Probe_data[,misCols[i]]),
                       integer = "numeric",
                       numeric = "numeric",
                       character = "character varying(255)")
      
      # TODO adopt to new FoI table
      dbSendQuery(db, paste0("ALTER TABLE Probedata ADD COLUMN ", colId, " ", coltype, ";"))
      
      dbSendQuery(db, paste0("INSERT INTO Probedatametadata (columnid, dede)
                               VALUES ('", paste(colId, Probe_header[misCols[i]], sep="', '"),"')"))
    }
  }
  
  # if there are already Probee in the DB that are again in the CSV
  if (nrow(checkDBProbe$ProbeInDB) > 0) {
    ## UPDATE Probe via SQL ##
  } else {
    ## INSERT Probe via SQL ##
  }
  
  showModal(modalDialog(
    title = "Vorgang abgeschlossen.",
    "Proben erfolgreich angelegt.",
    easyClose = TRUE,
    footer = NULL
  ))
})