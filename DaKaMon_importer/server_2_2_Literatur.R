###################################################################################
#############################   Upload der Literatur   #############################
###################################################################################


# storage of variables that might change through the GUI
inCSVLiteratur <- reactiveValues()
valiLiteratur <- reactiveValues(validated = FALSE)
checkDBLiteratur <- reactiveValues(checked = FALSE)

sepLiteratur <- colSep
decLiteratur <- decSep

observeEvent(input$csvFileLiteratur, {
  valiLiteratur$validated <- FALSE
  checkDBLiteratur$checked <- FALSE
  
  # check whether an encoding has been set; fallback: guess the eoncoding using readr
  if (is.null(csvEncode)) {
    csvEncode <- readr::guess_encoding(input$csvFileLiteratur$datapath)
    csvEncode <- csvEncode$encoding[which.max(csvEncode$confidence)]
  }
  
  inCSVLiteratur$csvEncode <- csvEncode
  
  inCSVLiteratur$df <- read.csv(input$csvFileLiteratur$datapath,
                               header = TRUE,
                               sep = sepLiteratur, dec = decLiteratur,
                               stringsAsFactors = FALSE,
                               fileEncoding = inCSVLiteratur$csvEncode)
  
  inCSVLiteratur$headAsChar <- colnames(inCSVLiteratur$df)
  
  ## validation of Literatur csv-file
  # look for required column names
  # check whether columns have unique names
  
  txt <- NULL
  # if (!(reqColLiteratur$id %in% inCSVLiteratur$headAsChar) || length(unique(inCSVLiteratur$df[,reqColLiteratur$id])) != length(inCSVLiteratur$df[,reqColLiteratur$id]))
  #   txt <- paste0(txt, "<li>Jeder Literatur benötigt eine persistente und eindeutige ID in der Spalte '", reqColLiteratur$id, "'.</li>")
  # for (reqColName in reqColLiteratur[-1]) {
  #   if (!(reqColName %in% inCSVLiteratur$headAsChar))
  #     txt <- paste0(txt, "<li>Bitte die Spalte '", reqColName, "' ergänzen.</li>", sep="")
  # }
  
  if(length(unique(inCSVLiteratur$headAsChar)) != length(inCSVLiteratur$headAsChar))
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")
  
  valiLiteratur$txt <- txt
  
  valiLiteratur$validated <- TRUE
})

# write txt feedback as html list - or action button
output$LiteraturValidationOut <- renderUI({
  if (valiLiteratur$validated) {
    if (is.null(valiLiteratur$txt)) {
      actionButton("checkDBLiteratur", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiLiteratur$txt, "</ul></div></html"))
    }
  } else {
    return()
  }
})

##########################
## check DB consistency ##
##########################

# find existing Literatur

observeEvent(input$checkDBLiteratur, {
  db <- connectToDB()
  tryCatch({
    progress <- shiny::Progress$new()
    on.exit(progress$close(), add = T)
    
    progress$set(message = "Prüfe bereits registrierte Literaturn.", value = 0)
    
    checkDBLiteratur$txt <- NULL
    
    # get all Literatur from the DB that have any of the identifiers in the CSV
    LiteraturInDB <- dbGetQuery(db, paste0("SELECT DISTINCT lit.thematik, pns.identifier as pns, op.identifier as parameter, lit.untersuchungsbeginn, lit.untersuchungsende
                                      from literatur lit
                                           LEFT OUTER JOIN observableproperty op ON (lit.param_id = op.observablepropertyid)
                                           LEFT OUTER JOIN featureofinterest pns ON (pns.featureofinterestid = lit.pns_id) WHERE ",
                                      "lit.thematik IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$thematik], collapse="', '"),"')
                                      AND pns.identifier IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$pnsId], collapse="', '"),"')
                                      AND op.identifier IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$paramId], collapse="', '"),"')
                                      AND lit.untersuchungsbeginn IN (", paste0("to_timestamp('", inCSVLiteratur$df[,reqColLiteratur$uBegin], "', '", dbTimestampPattern, "')::timestamptz at time zone 'UTC'"),") 
                                      AND lit.untersuchungsende IN (", paste0("to_timestamp('", inCSVLiteratur$df[,reqColLiteratur$uEnde], "', '", dbTimestampPattern, "')::timestamptz at time zone 'UTC'"),")"))
    if (nrow(LiteraturInDB) > 0) {
      checkDBLiteratur$txt <- paste("Folgende Literatur sind bereits in der DB: <ul><li>",
                                   paste0(LiteraturInDB$identifier, collapse="</li><li>"))
    }
    
    # check whether referenced Referenz exist; if not -> error state: no upload
    RefInDB <- dbGetQuery(db, paste0("SELECT identifier FROM referenz WHERE identifier IN ('",
                                     paste(inCSVLiteratur$df[,reqColLiteratur$refId], collapse="', '"),"')"))
    
    if (nrow(RefInDB) == 0) {
      checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende Referenzen sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                              paste0(inCSVLiteratur$df[,reqColLiteratur$refId], collapse="</li><li>")))
    } else {
      misingReferenz <- which(sapply(inCSVLiteratur$df[,reqColLiteratur$refId], # TODO drop ID, parent identifier
                                 function(x) is.na(match(x, RefInDB$identifier))))
      if (length(misingReferenz > 0)) {
        checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende Referenzen sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                                paste0(inCSVLiteratur$df[,reqColLiteratur$refId][misingReferenz], collapse="</li><li>")))
      }
    }
    
    # check whether referenced Parameter exist; if not -> error state: no upload
    ParamInDB <- dbGetQuery(db, paste0("SELECT identifier FROM observableproperty WHERE identifier IN ('",
                                     paste(inCSVLiteratur$df[,reqColLiteratur$paramId], collapse="', '"),"')"))
    
    if (nrow(ParamInDB) == 0) {
      checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende Parameter sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                              paste0(inCSVLiteratur$df[,reqColLiteratur$paramId], collapse="</li><li>")))
    } else {
      misingParam <- which(sapply(inCSVLiteratur$df[,reqColLiteratur$paramId], # TODO drop ID, parent identifier
                                 function(x) is.na(match(x, ParamInDB$identifier))))
      if (length(misingParam > 0)) {
        checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende PArameter sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                                paste0(inCSVLiteratur$df[,reqColLiteratur$paramId][misingParam], collapse="</li><li>")))
      }
    }
    
    # check whether referenced PNS exist; if not -> error state: no upload
    PnsInDB <- dbGetQuery(db, paste0("SELECT identifier FROM featureofinterest WHERE identifier IN ('",
                                     paste(inCSVLiteratur$df[,reqColLiteratur$pnsId], collapse="', '"),"')"))
    
    if (nrow(PnsInDB) == 0) {
      checkDBPNS$txt <- paste(checkDBLiteratur$txt, paste("Folgende Orte sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                              paste0(inCSVLiteratur$df[,reqColLiteratur$pnsId], collapse="</li><li>")))
    } else {
      misingPns <- which(sapply(inCSVLiteratur$df[,reqColLiteratur$pnsId], # TODO drop ID, parent identifier
                                 function(x) is.na(match(x, PnsInDB$identifier))))
      if (length(misingPns > 0)) {
        checkDBPNS$txt <- paste(checkDBLiteratur$txt, paste("Folgende Orte sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                                paste0(inCSVLiteratur$df[,reqColLiteratur$pnsId][misingPns], collapse="</li><li>")))
      }
    }
    
    # check whether referenced combination of Thematik, PNS and Parameter exist; if not -> error state: no upload
    CombinationInDB <- dbGetQuery(db, paste0("SELECT DISTINCT od.thematik, pns.identifier as pnsid, op.identifier as paramid
                                      FROM probe_parameter pp
                                      LEFT OUTER JOIN probe pro ON (pro.id = pp.probe_id)
                                      LEFT OUTER JOIN observableproperty op ON (pp.parameter_id = op.observablepropertyid)
                                      LEFT OUTER JOIN pns_data pd ON (pro.pns_id = pd.featureofinterestid)
                                      LEFT OUTER JOIN featureofinterest pns ON (pns.featureofinterestid = pd.featureofinterestid)
                                      LEFT OUTER JOIN featurerelation fr ON (fr.childfeatureid = pd.featureofinterestid)
                                      LEFT OUTER JOIN featureofinterest parent ON (parent.featureofinterestid = fr.parentfeatureid)
                                      LEFT OUTER JOIN ort_data od ON (parent.featureofinterestid = od.featureofinterestid) WHERE ",
                                      "od.thematik IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$thematik], collapse="', '"),"')
                                      AND pns.identifier IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$pnsId], collapse="', '"),"')
                                      AND op.identifier IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$paramId], collapse="', '"),"')"))
    
    if (nrow(CombinationInDB) == 0) {
      checkDBPNS$txt <- paste(checkDBLiteratur$txt, paste("Folgende Kombination von Thematik, PNS and Parameter sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                                                          paste0(inCSVLiteratur$df[,reqColLiteratur$thematik], ", ", inCSVLiteratur$df[,reqColLiteratur$pnsId], ", ", inCSVLiteratur$df[,reqColLiteratur$paramid], collapse="</li><li>")))
    } else {
      misingCombination <- which(sapply(inCSVLiteratur$df[,reqColLiteratur$pnsId],
                                function(x) is.na(match(x, CombinationInDB$identifier))))
      if (length(misingCombination > 0)) {
        checkDBPNS$txt <- paste(checkDBLiteratur$txt, paste("Folgende Kombination von Thematik, PNS and Parameter sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                                                            paste0(inCSVLiteratur$df[,reqColLiteratur$pnsId][misingCombination], collapse="</li><li>")))
      }
    }
    
    checkDBLiteratur$LiteraturInDB <- LiteraturInDB
    
    checkDBLiteratur$checked <- TRUE
  }, error = modalErrorHandler, finally = poolReturn(db))
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button

output$LiteraturDBConsistencyOut <- renderUI({
  if (checkDBLiteratur$checked) {
    if (is.null(checkDBLiteratur$txt) || input$owLiteratur) {
      actionButton("storeDBLiteratur", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBLiteratur$txt, "</li></ul></div></html"))
    }
  } else {
    return()
  }
})

# plot table with CSV

output$tableLiteratur <- renderDataTable({
  if (!is.null(inCSVLiteratur$df)) {
    showTab <- inCSVLiteratur$df
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showHead <- paste0(showHead, "</span>")
    
    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sLiteratur=FALSE, dom="t",
                                       language=list(url = lngJSON)),
                        escape=FALSE)
    
    # if DB consistency has been checked, apply colors
    if (checkDBLiteratur$checked) {
      rowColors <- rep("white", nrow(showTab))
      
      if (nrow(checkDBLiteratur$LiteraturInDB) > 0) {
        rowColors[showTab$ID %in% checkDBLiteratur$LiteraturInDB$identifier] <- "red"
        showDT <- formatStyle(showDT, "ID", target="row",
                              backgroundColor = styleEqual(showTab$ID, rowColors))
      }
    }
    showDT
  }
})

############################
## Insert/Update REferenz ##
############################

observeEvent(input$storeDBLiteratur, {
  db <- connectToDB()
  tryCatch({
    
    Literatur_data <- inCSVLiteratur$df
    Literatur_header <- inCSVLiteratur$headAsChar
    
    Literatur_empty_cols <- apply(Literatur_data, 2, function(x) all(is.na(x)))
    
    Literatur_header <- Literatur_header[!Literatur_empty_cols]
    Literatur_data <- Literatur_data[,!Literatur_empty_cols]
    
    nRowDf <- nrow(Literatur_data)
    
    progress <- shiny::Progress$new()
    on.exit(progress$close(), add=T)
    
    progress$set(message = "Füge Literaturn in DB ein.", value = 0)
    
    ## add missign columns
    regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata WHERE plitixid IN ('lit', 'global')"))[,1]
    literaturColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, plitixid, dede FROM column_metadata 
                                                    WHERE plitixid IN ('lit') AND columnid LIKE 'col%'"))
    misCols <- which(sapply(Literatur_header, # TODO drop ID and Name
                            function(x) is.na(match(x, regCols))))
    
    if (length(misCols > 0)) {
      for (i in 1:length(misCols)) {# i <- 1
        colId <- sprintf("col%03d", i + length(regCols))
        coltype = switch(class(Literatur_data[,misCols[i]]),
                         integer = "numeric",
                         numeric = "numeric",
                         character = "character varying(255)")
        
        colMetadataExists = dbGetQuery(db, paste0("SELECT count(columnid) > 0 
                                                  FROM column_metadata 
                                                  WHERE columnid='", colId, "'
                                                  AND plitixid='lit' ;"))
        if (!colMetadataExists) {
          dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, plitixid, dede)
                                 VALUES ('", paste(colId, 'lit', Literatur_header[misCols[i]], sep="', '"),"');"))
        }
        
        colExists = dbGetQuery(db, paste0("SELECT count(column_name) > 0
                                          FROM information_schema.columns 
                                          WHERE table_name='literatur' 
                                          AND column_name='", colId, "';"))
        if (!colExists) {
          dbSendQuery(db, paste0("ALTER TABLE literatur ADD COLUMN ", colId, " ", coltype, ";"))
        }
        
        }
      literaturColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, plitixid, dede FROM column_metadata 
                                                  WHERE plitixid IN ('lit') AND columnid LIKE 'col%'"))
  }
    
    # if there are already Literatur in the DB that are again in the CSV
    for (lit in 1:nrow(Literatur_data)) {
      dynamicDf <- NULL
      dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(literaturColumnMappings)))
      colnames(dynamicDfRow) <- c("columnid", "dede", "value")
      for (col in 1:nrow(literaturColumnMappings)) {
        dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(literaturColumnMappings)))
        colnames(dynamicDfRow) <- c("columnid", "dede", "value")
        dynamicDfRow$columnid <- literaturColumnMappings[col, "columnid"]
        dynamicDfRow$dede <- literaturColumnMappings[col, "dede"]
        value = Literatur_data[lit, literaturColumnMappings[col, "dede"]]
        if (is.null(value) || is.na(value) || value == '') {
          dynamicDfRow$value = "EMPTY"
        } else {
          if (class(value) == "character") {
            value = paste0("'", value, "'")
          }
          dynamicDfRow$value = value
        }
        dynamicDf <- rbind(dynamicDf, dynamicDfRow)
      }
      if (Literatur_data[lit,"ID"] %in% checkDBLiteratur$LiteraturInDB$identifier) {
        ## UPDATE literatur and data via SQL ##
        update = paste0("UPDATE literatur SET ", 
                        paste0(paste0(dynamicDf[["columnid"]], " = ", gsub("EMPTY", "NULL", dynamicDf[["value"]])), collapse = ", "),
                        " WHERE identifier = '", Literatur_data[lit, reqColLiteratur$id], "';")
        dbSendQuery(db, update)
      } else {
        ## INSERT literatur and data via SQL ##
        dynamicColumns = paste0(dynamicDf[["columnid"]], collapse = ", ")
        dynamicValues = paste0(gsub("EMPTY", "NULL", dynamicDf[["value"]]), collapse = ", ")
        
        insert = paste0("INSERT INTO literatur (id, identifier, ", dynamicColumns, ")
                        SELECT nextval('literaturid_seq')::int, '", 
                        Literatur_data[lit, reqColLiteratur$id], "', ",
                        dynamicValues, ";")
        dbSendQuery(db, insert)
      }
    }
    
    message = paste0(nrow(Literatur_data) , " Literaturn wurden erfolgreich in der Datenbank angelegt.")
    showModalMessage("Vorgang abgeschlossen", message)
    }, error = modalErrorHandler, finally = poolReturn(db))
  })