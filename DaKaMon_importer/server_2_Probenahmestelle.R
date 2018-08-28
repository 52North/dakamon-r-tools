################################################################################
#####################   Upload der Probenahmestelle (PNS)   ####################
################################################################################

# storage of variables that might change through the GUI
inCSVPNS <- reactiveValues()
valiPNS <- reactiveValues(validated = FALSE)
checkDBPNS <- reactiveValues(checked = FALSE)

sepPNS <- colSep
decPNS <- decSep

observeEvent(input$csvFilePNS, {
  valiPNS$validated <- FALSE
  checkDBPNS$checked <- FALSE

  # check whether an encoding has been set; fallback: guess the eoncoding using readr
  if (is.null(csvEncode)) {
    csvEncode <- readr::guess_encoding(input$csvFilePNS$datapath)
    csvEncode <- csvEncode$encoding[which.max(csvEncode$confidence)]
  }

  inCSVPNS$csvEncode <- csvEncode

  inCSVPNS$df <- read.csv(input$csvFilePNS$datapath,
                          header = TRUE,
                          sep = sepPNS, dec = decPNS,
                          stringsAsFactors = FALSE,
                          fileEncoding = inCSVPNS$csvEncode)

  inCSVPNS$headAsChar <- colnames(inCSVPNS$df)

  ## validation of PNS csv-file
  # look for required column names
  # check whether columns have unique names

  txt <- NULL
  if (!(reqColPNS$id %in% inCSVPNS$headAsChar) || length(unique(inCSVPNS$df[,reqColPNS$id])) != length(inCSVPNS$df[,reqColPNS$id]))
    txt <- paste0(txt, "<li>Jede Probenahmestelle benötigt eine persistente und eindeutige ID in der Spalte'", reqColPNS$id, "'.</li>")
  for (reqColName in reqColPNS[-1]) {
    if (!(reqColName %in% inCSVPNS$headAsChar))
      txt <- paste0(txt, "<li>Bitte die Spalte '", reqColName, "' ergänzen.</li>", sep="")
  }

  if(length(unique(inCSVPNS$headAsChar)) != length(inCSVPNS$headAsChar))
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")

  valiPNS$txt <- txt
  valiPNS$validated <- TRUE
})

# write txt feedback as html list - or action button

output$PNSValidationOut <- renderUI({
  if (valiPNS$validated) {
    if (is.null(valiPNS$txt)) {
      actionButton("checkDBPNS", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiPNS$txt, "</ul></div></html"))
    }
  } else {
    return()
  }
})

##########################
## check DB consistency ##
##########################

# find existing PNSe

observeEvent(input$checkDBPNS, {
  db <- connectToDB()
  tryCatch({
    progress <- shiny::Progress$new()
    on.exit(progress$close(), add = T)
  
    progress$set(message = "Prüfe bereits registrierte PNSe.", value = 0)
  
    # get all PNSe from the DB that have any of the identifiers in the CSV
    PNSInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('",
                                     paste(inCSVPNS$df[,reqColPNS$id], collapse="', '"),"')"))
    if (nrow(PNSInDB) > 0) {
      checkDBPNS$txt <- paste("Folgende PNSe sind bereits in der DB: <ul><li>",
                              paste0(PNSInDB$identifier, collapse="</li><li>"))
    } else {
      checkDBPNS$txt <- NULL
    }
  
    # check whether referenced super FoIs exist; if not -> error state: no upload
    OrtInDB <- dbGetQuery(db, paste0("SELECT identifier FROM featureofinterest WHERE identifier IN ('",
                                     paste(inCSVPNS$df[,reqColPNS$geo], collapse="', '"),"')"))
  
    if (nrow(OrtInDB) == 0) {
      checkDBPNS$txt <- paste("Folgende Orte sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                              paste0(inCSVPNS$df[,reqColPNS$geo], collapse="</li><li>"))
    } else {
      misingOrte <- which(sapply(inCSVPNS$df[,reqColPNS$geo], # TODO drop ID, parent identifier
                              function(x) is.na(match(x, OrtInDB$identifier))))
      if (length(misingOrte > 0)) {
        checkDBPNS$txt <- paste("Folgende Orte sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                                paste0(inCSVPNS$df[,reqColPNS$geo][misingOrte], collapse="</li><li>"))  
      }
    }
    
    checkDBPNS$PNSInDB <- PNSInDB
  
    checkDBPNS$checked <- TRUE
  }, finally = poolReturn(db))
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button
output$PNSDBConsistencyOut <- renderUI({
  if (checkDBPNS$checked) {
    if (is.null(checkDBPNS$txt) || input$owPNS) {
      actionButton("storeDBPNS", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBPNS$txt, "</li></ul></div></html"))
    }
  } else {
    return()
  }
})

# plot table with CSV
output$tablePNS <- renderDataTable({
  if (!is.null(inCSVPNS$df)) {
    showTab <- inCSVPNS$df

    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

    showHead <- paste0(showHead, "</span>")

    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sPNS=FALSE, dom="t",
                                       language=list(url = lngJSON)),
                        escape=FALSE)

    # if DB consistency has been checked, apply colors
    if (checkDBPNS$checked) {
      rowColors <- rep("white", nrow(showTab))

      if (nrow(checkDBPNS$PNSInDB) > 0) {
        cat(is.null(inCSVPNS$df))
        rowColors[showTab$ID %in% checkDBPNS$PNSInDB$identifier] <- "red"
        showDT <- formatStyle(showDT, "ID", target="row",
                              backgroundColor = styleEqual(showTab$ID, rowColors))
      }
    }
    showDT
  }
})


#############################
## Insert Probenamestellen ##
#############################

observeEvent(input$storeDBPNS, {
  db <- connectToDB()
  tryCatch({
    PNS_data <- inCSVPNS$df
    PNS_header <- inCSVPNS$headAsChar
  
    PNS_empty_cols <- apply(PNS_data, 2, function(x) all(is.na(x)))
  
    PNS_header <- PNS_header[!PNS_empty_cols]
    PNS_data <- PNS_data[,!PNS_empty_cols]
  
    nRowDf <- nrow(PNS_data)
  
    progress <- shiny::Progress$new()
    on.exit(progress$close(), add=T)
  
    progress$set(message = "Füge Probenahmestellen in DB ein.", value = 0)
  
    ## add missing columns
    regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata WHERE prefixid IN ('pns', 'global')"))[,1]
    pnsColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata WHERE prefixid IN ('pns')"))
    misCols <- which(sapply(PNS_header, # TODO drop ID, parent identifier
                            function(x) is.na(match(x, regCols))))
  
    if (length(misCols > 0)) {
      for (i in 1:length(misCols)) {# i <- 1
        colId <- paste0(sprintf("col%03d", i + length(regCols)))
        coltype = switch(class(PNS_data[,misCols[i]]),
                         integer = "numeric",
                         numeric = "numeric",
                         character = "character varying(255)")
  
        # TODO adopt to new FoI table
        dbSendQuery(db, paste0("ALTER TABLE pns_data ADD COLUMN ", colId, " ", coltype, ";"))
  
        dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, prefixid, dede)
                                 VALUES ('", paste(colId, 'pns', PNS_header[misCols[i]], sep="', '"),"')"))
      }
      pnsColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata WHERE prefixid IN ('pns')"))
    }
  
    # if there are already PNSn in the DB that are again in the CSV
    for (pns in 1:nrow(PNS_data)) {
      dynamicDf <- NULL
      for (col in 1:nrow(pnsColumnMappings)) {
        dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(pnsColumnMappings)))
        colnames(dynamicDfRow) <- c("columnid", "dede", "value")
        dynamicDfRow$columnid <- pnsColumnMappings[col, "columnid"]
        dynamicDfRow$dede <- pnsColumnMappings[col, "dede"]
        value = PNS_data[pns, pnsColumnMappings[col, "dede"]]
        if (is.null(value) || is.na(value)) {
          #if (class(value) == "character") {
          #  dynamicDfRow$value =  "''"
          #} else {
          #  dynamicDfRow$value = -1
          #}
          dynamicDfRow$value = "EMPTY"
        } else {
          if (class(value) == "character") {
            value = paste0("'", value, "'")
          }
          dynamicDfRow$value = value
        }
        dynamicDf <- rbind(dynamicDf, dynamicDfRow)
      }
      if (PNS_data[pns,"ID"] %in% checkDBPNS$PNSInDB$identifier) { # -> UPDATE
        # TODO switch to workflow with dynamic columns
        query = paste0("with update_pns as (
        UPDATE featureofinterest
        	SET
        		 name = '", PNS_data[pns,reqColPNS$name],
             "', geom = ST_GeomFromText('POINT (",
                       PNS_data[pns,reqColPNS$lat],
                       " ",
                       PNS_data[pns,reqColPNS$lon],
                       ")', 4326)
            WHERE identifier = '", PNS_data[pns,reqColPNS$id],
            "' RETURNING featureofinterestid
        )
        UPDATE pns_data
        	SET ",
            paste0(paste0(dynamicDf[["columnid"]], " = ", gsub("EMPTY", "NULL", dynamicDf[["value"]])), collapse = ", "),
        " WHERE featureofinterestid = (SELECT featureofinterestid FROM update_pns)
        RETURNING featureofinterestid as pns_id;")
        
        updatedId <- dbGetQuery(db, query)
        ## if pns - foi relation does not exist, insert relation ##
        query = paste("INSERT INTO featurerelation
        	(SELECT featureofinterestid, ", updatedId$pns_id," FROM featureofinterest 
                      WHERE identifier = '", PNS_data[pns,reqColPNS$geo],"');")
        dbSendQuery(db, query)
      } else { # -> INSERT
        ## INSERT FoI and data via SQL, mind the parental FoI ##
        dynamicColumns = paste0(pnsColumnMappings[, 1], collapse = ", ")
        dynamicValues = paste0(gsub("EMPTY", "NULL", dynamicDf[["value"]]), collapse = ", ")
        query = paste0("WITH
                      insert_pns AS (
                      INSERT INTO featureofinterest (featureofinterestid, featureofinteresttypeid, identifier, name, geom)
                      VALUES (nextval('featureofinterestid_seq'), 1,'",
                      PNS_data[pns,reqColPNS$id], "',",
                      "'", PNS_data[pns,reqColPNS$name], "',",
                      " ST_GeomFromText('POINT (",
                      PNS_data[pns,reqColPNS$lat],
                      " ",
                      PNS_data[pns,reqColPNS$lon],
                      ")', 4326))
                      RETURNING featureofinterestid AS pns_id
                      ),
                      query_ort AS (
                          SELECT featureofinterestid AS ort_id FROM featureofinterest
                          WHERE identifier = '", PNS_data[pns,reqColPNS$geo],
                      "'),
                      insert_pns_rel AS (
                       INSERT INTO featurerelation
                       SELECT query_ort.ort_id, insert_pns.pns_id
                       FROM insert_pns, query_ort
                       RETURNING childfeatureid
                      )
                      INSERT INTO pns_data (featureofinterestid, rndid, ", dynamicColumns, ")
                      SELECT pns_id, pseudo_encrypt(nextval('rndIdSeq')::int), ",
                        dynamicValues,
                       " FROM insert_pns;")
        dbSendQuery(db, query)
      }
    }
  
    showModal(modalDialog(
      title = "Vorgang abgeschlossen",
      paste0(nrow(PNS_data), " Probenahmestellen wurden erfolgreich in der Datenbank angelegt."),
      footer = modalButton("Ok")
    ))
  }, finally = poolReturn(db))
})
