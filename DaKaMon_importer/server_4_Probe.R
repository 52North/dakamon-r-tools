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
      actionButton("checkDBProbe", "Prüfe Datenkonsistenz!")
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

observeEvent(input$checkDBProbe, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbName, user=dbUser, password=dbPassword, port=dbPort)
  on.exit(dbDisconnect(db), add=T)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add = T)
  
  progress$set(message = "Prüfe bereits registrierte Proben.", value = 0)
  
  # get all Probes from the DB that have any of the identifiers in the CSV
  if (length(inCSVProbe$df) > 0) {
    ProbeInDB <- dbGetQuery(db, paste0("SELECT id, identifier FROM probe WHERE identifier IN ('",
                                       paste(inCSVProbe$df[,reqColProbe$id], collapse="', '"),"')"))
    
    if (!is.null(ProbeInDB) && length(ProbeInDB) > 0 && nrow(ProbeInDB) > 0) {
      checkDBProbe$txt <- paste("Folgende Proben sind bereits in der DB: <ul><li>",
                                paste0(ProbeInDB$identifier, collapse="</li><li>"))
    } else {
      checkDBProbe$txt <- NULL
    }
    
    checkDBProbe$ProbeInDB <- ProbeInDB
    
    # check PNS ids
    PNSInDB <- dbGetQuery(db, paste0("SELECT identifier FROM featureofinterest WHERE identifier IN ('",
                                     paste(inCSVProbe$df[,reqColProbe$geoSub], collapse="', '"),"')"))
    misPNSIDs <- which(!(inCSVProbe$df[,reqColProbe$geoSub] %in% PNSInDB$identifier))
    
    if(length(misPNSIDs) > 0) # error state: no upload
      checkDBProbe$txt <- paste("Folgende Probenahestellen fehlen in der DB: <ul><li>",
                                paste0(inCSVProbe$df[misPNSIDs, reqColProbe$geoSub], collapse="</li><li>"))
  }
  checkDBProbe$checked <- TRUE
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button
output$ProbeDBConsistencyOut <- renderUI({
  if (checkDBProbe$checked) {
    if (is.null(checkDBProbe$txt)  || input$owProbe) {
      actionButton("storeDBProbe", "Einfügen in DB!")
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
        rowColors[showTab$ID %in% checkDBProbe$ProbeInDB$identifier] <- "red"
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

observeEvent(input$storeDBProbe, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbName, user=dbUser, password=dbPassword, port=dbPort)
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
  
  ## add missing columns
  regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata"))[,1]
  probeDataCols <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata WHERE prefixid IN ('probe')"))
  misCols <- which(sapply(Probe_header, # TODO drop ID, Probeent identifier
                          function(x) is.na(match(x, regCols))))
  
  if (length(misCols) > 0) {
    for (i in 1:length(misCols)) {# i <- 1
      colId <- paste0(sprintf("col%03d", i + length(regCols)))
      coltype = switch(class(Probe_data[,misCols[i]]),
                       integer = "numeric",
                       numeric = "numeric",
                       character = "character varying(255)")
      
      # TODO adopt to new FoI table
      dbSendQuery(db, paste0("ALTER TABLE probe ADD COLUMN ", colId, " ", coltype, ";"))
      
      dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, prefixid, dede)
                               VALUES ('", paste(colId, 'probe', Probe_header[misCols[i]], sep="', '"),"')"))
    }
  }
  
  for (probe in 1:nrow(Probe_data)) { # probe <- 1
    # if there are already Probee in the DB that are again in the CSV
    if (nrow(checkDBProbe$ProbeInDB) > 0) {
      ## UPDATE Probe via SQL, returns the id (pkid) of the updated probe ##
      # TODO implement workflow with dynamic columns
      dbSendQuery(db, paste0("UPDATE probe
      	SET
      		probe_col003 = probe_col003_var,
      		probe_col004 = probe_col004_var,
      		probe_col005 = probe_col005_var,
      		probe_col006 = probe_col006_var,
      		probe_col007 = probe_col007_var,
      		probe_col008 = probe_col008_var,
      		probe_col009 = probe_col009_var,
      		probe_col010 = probe_col010_var,
      		probe_col011 = probe_col011_var,
      		probe_col012 = probe_col012_var,
      		probe_col013 = probe_col013_var,
      		probe_col014 = probe_col014_var,
      		probe_col015 = probe_col015_var
      RETURNING id;	"))
    } else {
      ## INSERT Probe via SQL ##
      # FIXME handling of dynamic values, e.g. to_date('18-07-2018 12:00', 'DD-MM-YYYY HH:MM') and pns_id
      dynamicColumns = paste0(probeDataCols[, 1], collapse = ", ")
      dynamicValues = ""
      for (col in probeDataCols[["dede"]]) { # col <- "PNS_ID"
        value = Probe_data[probe, col]
        cat(value, "\n")
        if (col == reqColProbe$geoSub) {
          dynamicValues = paste(dynamicValues, "(SELECT pns_id FROM query_pns)", sep = ", ")
        } else {
          if (is.null(value) || is.na(value)) {
            if (class(value) == "character") {
              dynamicValues = paste(dynamicValues, "", sep = ", ")
            } else {
              dynamicValues = paste(dynamicValues, -1, sep = ", ")
            }
          } else {
            if (class(value) == "character") {
              # if (!(col %in% unlist(reqColProbe[c("eventTimeEnd", "eventTiemBegin", "colDate")])))
                value = paste0("'", value, "'")
            }
            dynamicValues = paste(dynamicValues, value, sep = ", ")
          }
        }
      }
      get_pns_id = paste0("WITH query_pns AS (
                          SELECT featureofinterestid AS pns_id
                          FROM featureofinterest
                          WHERE identifier = '",
                          Probe_data[probe, reqColProbe$geoSub],
                          "')")
      query = paste0(get_pns_id,
                     " INSERT INTO probe
        (id, identifier, ", dynamicColumns, ")
        VALUES (nextval('probeid_seq'), '",
                     Probe_data[probe, reqColProbe$id], "'",
                     dynamicValues,
                     ");")
      cat(query)
      dbSendQuery(db, query)
    }
  }
  
  showModal(modalDialog(
    title = "Vorgang abgeschlossen",
    paste0(nrow(Probe_data), " Proben wurden erfolgreich in der Datenbank angelegt."),
    footer = modalButton("Ok")
  ))
})
