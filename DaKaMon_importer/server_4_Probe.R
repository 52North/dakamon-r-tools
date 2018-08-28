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
  db <- connectToDB()
  tryCatch({
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
  }, error = modalErrorHandler, finally = poolReturn(db))
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
  db <- connectToDB()
  tryCatch({
    Probe_data <- inCSVProbe$df
    Probe_header <- inCSVProbe$headAsChar
  
    Probe_empty_cols <- apply(Probe_data, 2, function(x) all(is.na(x)))
  
    Probe_header <- Probe_header[!Probe_empty_cols]
    Probe_data <- Probe_data[,!Probe_empty_cols]
  
    nRowDf <- nrow(Probe_data)
  
    progress <- shiny::Progress$new(min = 0, max = nrow(Probe_data))
    on.exit(progress$close(), add=T)
  
    progress$set(message = "Füge Proben in DB ein.", value = 0)
  
    ## add missing columns
    registeredColumns <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata WHERE prefixid IN ('probe', 'global')"))[,1]
    probeColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata WHERE prefixid IN ('probe')"))
    missingColumns <- which(sapply(Probe_header, # TODO drop ID, Probeent identifier
                            function(x) is.na(match(x, registeredColumns))))
  
    if (length(missingColumns > 0)) {
      for (i in 1:length(missingColumns)) {# i <- 1
        colId <- paste0(sprintf("col%03d", i + length(registeredColumns)))
        coltype = switch(class(Probe_data[, missingColumns[i]]),
                         integer = "numeric",
                         numeric = "numeric",
                         character = "character varying(255)")
  
        # TODO adopt to new FoI table
        dbSendQuery(db, paste0("ALTER TABLE probe ADD COLUMN ", colId, " ", coltype, ";"))
  
        dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, prefixid, dede)
                                 VALUES ('", paste(colId, 'probe', Probe_header[missingColumns[i]], sep="', '"),"')"))
      }
    }
  
    for (probe in 1:nrow(Probe_data)) { # probe <- 1
      dynamicDf <- NULL
      dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(probeColumnMappings)))
      colnames(dynamicDfRow) <- c("columnid", "dede", "value")
      for (col in 1:nrow(probeColumnMappings)) {
        dynamicDfRow$columnid <- probeColumnMappings[col, "columnid"]
        dynamicDfRow$dede <- probeColumnMappings[col, "dede"]
        value = Probe_data[probe, probeColumnMappings[col, "dede"]]
        if (is.null(value) || is.na(value)) {
          dynamicDfRow$value = "EMPTY"
        } else {
          if (class(value) == "character") {
            value = paste0("'", value, "'")
          }
          dynamicDfRow$value = value
        }
        dynamicDf <- rbind(dynamicDf, dynamicDfRow)
      }
      # if there are already Probee in the DB that are again in the CSV
      if (Probe_data[probe,"ID"] %in% checkDBProbe$ProbeInDB$identifier) {
        ## UPDATE Probe via SQL, returns the id (pkid) of the updated probe ##
        # TODO implement workflow with dynamic columns
        query <- paste0("UPDATE probe
      	        SET ",
                paste0(paste0(dynamicDf[["columnid"]], " = ", gsub("EMPTY", "NULL", dynamicDf[["value"]])), collapse = ", "),
                ";")
        dbSendQuery(db, query)
      } else {
        ## INSERT Probe via SQL ##
        dynamicColumns = paste0(probeColumnMappings[, 1], collapse = ", ")
        dynamicValues = paste0(gsub("EMPTY", "NULL", dynamicDf[["value"]]), collapse = ", ")
        get_pns_id = paste0("WITH query_pns AS (
                            SELECT featureofinterestid AS pns_id
                            FROM featureofinterest
                            WHERE identifier = '",
                            Probe_data[probe, reqColProbe$geoSub],
                            "')")
        query = paste(get_pns_id,
                      "INSERT INTO probe
                       (id, identifier, pns_id,", dynamicColumns, ")
                       VALUES (nextval('probeid_seq'),'", "', (SELECT pns_id FROM query_pns),",
                       dynamicValues,
                       ");")
        dbSendQuery(db, query)
      }
    }
  
    message = paste0(nrow(Probe_data), " Proben wurden erfolgreich in der Datenbank angelegt.")
    showModalMessage("Vorgang abgeschlossen", message)
  }, error = modalErrorHandler, finally = poolReturn(db))
})
