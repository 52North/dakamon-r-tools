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
    LiteraturInDB <- dbGetQuery(db, paste0("SELECT DISTINCT ref.identifier as ref, lit.thematik, pns.identifier as pns, op.identifier as parameter, 
                                      to_char(lit.untersuchungsbeginn::timestamp, '",  dbTimestampPattern, "') AS untersuchungsbeginn, 
                                      to_char(lit.untersuchungsende::timestamp, '",  dbTimestampPattern, "') AS untersuchungsende 
                                      from literatur lit
                                           LEFT OUTER JOIN observableproperty op ON (lit.param_id = op.observablepropertyid)
                                           LEFT OUTER JOIN featureofinterest pns ON (pns.featureofinterestid = lit.pns_id) 
                                           LEFT OUTER JOIN referenz ref ON (ref.id = lit.referenz_id) WHERE 
                                      ref.identifier IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$refId], collapse="', '"),"')
                                      AND lit.thematik IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$thematik], collapse="', '"),"')
                                      AND pns.identifier IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$pnsId], collapse="', '"),"')
                                      AND op.identifier IN ('", paste(inCSVLiteratur$df[,reqColLiteratur$paramId], collapse="', '"),"')
                                      AND lit.untersuchungsbeginn IN (", paste0("to_timestamp('", inCSVLiteratur$df[,reqColLiteratur$uBegin], "', '", dbTimestampPattern, "')::timestamptz at time zone '", feederTimeZoneIdentifier, "'"),") 
                                      AND lit.untersuchungsende IN (", paste0("to_timestamp('", inCSVLiteratur$df[,reqColLiteratur$uEnde], "', '", dbTimestampPattern, "')::timestamptz at time zone '", feederTimeZoneIdentifier, "'"),")"))
    if (nrow(LiteraturInDB) > 0) {
      checkDBLiteratur$txt <- paste("Folgende Literatur ist bereits in der DB: <ul><li>",
                                   paste0(paste(LiteraturInDB$ref, 
                                                 LiteraturInDB$thematik, 
                                                 LiteraturInDB$pns,
                                                 LiteraturInDB$parameter,
                                                 LiteraturInDB$untersuchungsbeginn,
                                                 LiteraturInDB$untersuchungsende, sep = ", "), collapse="</li><li>"))
    }
    
    # check whether referenced Referenz exist; if not -> error state: no upload
    RefInDB <- dbGetQuery(db, paste0("SELECT identifier FROM referenz WHERE identifier IN ('",
                                     paste(inCSVLiteratur$df[,reqColLiteratur$refId], collapse="', '"),"')"))
    
    if (nrow(RefInDB) == 0) {
      checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende Referenzen sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                              paste0(inCSVLiteratur$df[,reqColLiteratur$refId], collapse="</li><li>")))
    } else {
      misingReferenz <- which(sapply(inCSVLiteratur$df[,reqColLiteratur$refId],
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
      misingParam <- which(sapply(inCSVLiteratur$df[,reqColLiteratur$paramId],
                                 function(x) is.na(match(x, ParamInDB$identifier))))
      if (length(misingParam > 0)) {
        checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende Parameter sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                                paste0(inCSVLiteratur$df[,reqColLiteratur$paramId][misingParam], collapse="</li><li>")))
      }
    }
    
    # check whether referenced PNS exist; if not -> error state: no upload
    PnsInDB <- dbGetQuery(db, paste0("SELECT identifier FROM featureofinterest WHERE identifier IN ('",
                                     paste(inCSVLiteratur$df[,reqColLiteratur$pnsId], collapse="', '"),"')"))
    
    if (nrow(PnsInDB) == 0) {
      checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende Orte sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                              paste0(inCSVLiteratur$df[,reqColLiteratur$pnsId], collapse="</li><li>")))
    } else {
      misingPns <- which(sapply(inCSVLiteratur$df[,reqColLiteratur$pnsId],
                                 function(x) is.na(match(x, PnsInDB$identifier))))
      if (length(misingPns > 0)) {
        checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende Orte sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                                paste0(inCSVLiteratur$df[,reqColLiteratur$pnsId][misingPns], collapse="</li><li>")))
      }
    }
    
    # check whether referenced combination of Thematik, PNS and Parameter exist; if not -> error state: no upload
    CombinationInDB <- dbGetQuery(db, paste0("SELECT DISTINCT od.thematik, pns.identifier as pnsId, op.identifier as paramId
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
      checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende Kombination von Thematik, PNS and Parameter sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
                                                          paste0(inCSVLiteratur$df[,reqColLiteratur$thematik], ", ", inCSVLiteratur$df[,reqColLiteratur$pnsId], ", ", inCSVLiteratur$df[,reqColLiteratur$paramId], collapse="</li><li>")))
    } else {
      misingCombination <- NULL
      for (combiCSV in 1:nrow(inCSVLiteratur$df[,c(reqColLiteratur$thematik, reqColLiteratur$pnsId, reqColLiteratur$paramId)])) {
        missing <- TRUE
        for (combiDB in 1:nrow(CombinationInDB)) {
          if (inCSVLiteratur$df[combiCSV, reqColLiteratur$pnsId] == CombinationInDB[combiDB, "pnsid"]
              && inCSVLiteratur$df[combiCSV, reqColLiteratur$thematik] == CombinationInDB[combiDB, "thematik"]
              && inCSVLiteratur$df[combiCSV, reqColLiteratur$paramId] == CombinationInDB[combiDB, "paramid"]) {
              missing <- FALSE
          }
        }
        if (missing == TRUE) {
          misingCombination <- rbind(misingCombination, inCSVLiteratur$df[combiCSV])
        }
      }
      
      if (length(misingCombination > 0)) {
        checkDBLiteratur$txt <- paste(checkDBLiteratur$txt, paste("Folgende Kombination von Thematik, PNS and Parameter sind nicht in der DB vorhanden und müssen zuvor eingefügt werden: <ul><li>",
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
        rowColors[showTab$Referenz_ID %in% checkDBLiteratur$LiteraturInDB$ref] <- "red"
                  # && showTab$Thematik %in% checkDBLiteratur$LiteraturInDB$thematik
                  # && showTab$Parameter %in% checkDBLiteratur$LiteraturInDB$parameter
                  # && showTab$PNS_ID %in% checkDBLiteratur$LiteraturInDB$pns
                  # && showTab$Untersuchungsbeginn %in% checkDBLiteratur$LiteraturInDB$untersuchungsbeginn
                  # && showTab$Untersuchungsende %in% checkDBLiteratur$LiteraturInDB$untersuchungsende
        showDT <- formatStyle(showDT, "Referenz_ID", target="row",
                              backgroundColor = styleEqual(showTab$Referenz_ID, rowColors))
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
    regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata WHERE prefixid IN ('lit', 'global')"))[,1]
    literaturColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata 
                                                    WHERE prefixid IN ('lit') AND columnid LIKE 'col%'"))
    misCols <- which(sapply(Literatur_header,
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
                                                  AND prefixid='lit' ;"))
        if (!colMetadataExists) {
          dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, prefixid, dede)
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
      literaturColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata 
                                                  WHERE prefixid IN ('lit') AND columnid LIKE 'col%'"))
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
      litExists <- FALSE
      for (litEx in 1:nrow(checkDBLiteratur$LiteraturInDB)) {
        if (Literatur_data[lit,reqColLiteratur$refId] %in% checkDBLiteratur$LiteraturInDB[litEx, "ref"]
            && Literatur_data[lit,reqColLiteratur$thematik] %in% checkDBLiteratur$LiteraturInDB[litEx, "thematik"]
            && Literatur_data[lit,reqColLiteratur$paramId] %in% checkDBLiteratur$LiteraturInDB[litEx, "parameter"]
            && Literatur_data[lit,reqColLiteratur$pnsId] %in% checkDBLiteratur$LiteraturInDB[litEx, "pns"]
            && Literatur_data[lit,reqColLiteratur$uBegin] %in% checkDBLiteratur$LiteraturInDB[litEx, "untersuchungsbeginn"]
            && Literatur_data[lit,reqColLiteratur$uEnde] %in% checkDBLiteratur$LiteraturInDB[litEx, "untersuchungsende"]) {
            litExists <- TRUE
        }
     }
     if (litExists) {
       ## UPDATE literatur and data via SQL ##
       update = paste0("WITH ref as (
                       SELECT id FROM referenz WHERE identifier IN ('", Literatur_data[lit, reqColLiteratur$refId] ,"')),
                       param as (
                       SELECT observablepropertyid  AS id  FROM observableproperty WHERE identifier IN ('", Literatur_data[lit, reqColLiteratur$paramId] ,"')),
                       pns as (
                       SELECT featureofinterestid as id FROM featureofinterest WHERE identifier IN ('", Literatur_data[lit, reqColLiteratur$pnsId] ,"'))
                       UPDATE literatur SET ", 
                       paste0(paste0(dynamicDf[["columnid"]], " = ", gsub("EMPTY", "NULL", dynamicDf[["value"]])), collapse = ", "),
                       " WHERE referenz_id = (SELECT id FROM ref)
                       AND thematik = '", Literatur_data[lit, reqColLiteratur$thematik], "'
                       AND param_id = (SELECT id FROM param) 
                       AND pns_id = (SELECT id FROM pns) 
                       AND untersuchungsbeginn IN (", paste0("to_timestamp('", inCSVLiteratur$df[,reqColLiteratur$uBegin], "', '", dbTimestampPattern, "')::timestamptz at time zone '", feederTimeZoneIdentifier, "'"),") 
                       AND untersuchungsende IN (", paste0("to_timestamp('", inCSVLiteratur$df[,reqColLiteratur$uEnde], "', '", dbTimestampPattern, "')::timestamptz at time zone '", feederTimeZoneIdentifier, "')"),
                       ";")
       dbSendQuery(db, update)
      } else {
        ## INSERT literatur and data via SQL ##
        dynamicColumns = paste0(dynamicDf[["columnid"]], collapse = ", ")
        dynamicValues = paste0(gsub("EMPTY", "NULL", dynamicDf[["value"]]), collapse = ", ")
        
        insert = paste0("WITH ref as (
                          SELECT id FROM referenz WHERE identifier IN ('", Literatur_data[lit, reqColLiteratur$refId] ,"')),
                          param as (
                            SELECT observablepropertyid  AS id  FROM observableproperty WHERE identifier IN ('", Literatur_data[lit, reqColLiteratur$paramId] ,"')),
                          pns as (
                            SELECT featureofinterestid as id FROM featureofinterest WHERE identifier IN ('", Literatur_data[lit, reqColLiteratur$pnsId] ,"'))
                        INSERT INTO literatur (id, referenz_id, thematik, param_id, pns_id, untersuchungsbeginn, untersuchungsende, ", dynamicColumns, ")
                          SELECT nextval('literaturid_seq')::int, ref.id, '",
                        Literatur_data[lit, reqColLiteratur$thematik], "', param.id, pns.id, "
                        , paste0("to_timestamp('", Literatur_data[lit, reqColLiteratur$uBegin], "', '", dbTimestampPattern, "')::timestamptz at time zone '", feederTimeZoneIdentifier, "'"),
                        ", " , paste0("to_timestamp('", Literatur_data[lit, reqColLiteratur$uEnde], "', '", dbTimestampPattern, "')::timestamptz at time zone '", feederTimeZoneIdentifier, "', "),
                        dynamicValues, "
                          FROM ref, param, pns;")
        dbSendQuery(db, insert)
      }
    }
    
    message = paste0(nrow(Literatur_data) , " Literaturn wurden erfolgreich in der Datenbank angelegt.")
    showModalMessage("Vorgang abgeschlossen", message)
    }, error = modalErrorHandler, finally = poolReturn(db))
  })