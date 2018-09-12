###################################################################################
#############################   Upload der Referenz   #############################
###################################################################################

# storage of variables that might change through the GUI
inCSVReferenz <- reactiveValues()
valiReferenz <- reactiveValues(validated = FALSE)
checkDBReferenz <- reactiveValues(checked = FALSE)

sepReferenz <- colSep
decReferenz <- decSep

observeEvent(input$csvFileReferenz, {
  valiReferenz$validated <- FALSE
  checkDBReferenz$checked <- FALSE

  inCSVReferenz$df <- read.csv(input$csvFileReferenz$datapath,
                          header = TRUE,
                          sep = sepReferenz, dec = decReferenz,
                          stringsAsFactors = FALSE,
                          fileEncoding = input$refFileEnc)

  inCSVReferenz$headAsChar <- colnames(inCSVReferenz$df)

  ## validation of Referenz csv-file
  # look for required column names
  # check whether columns have unique names

  txt <- NULL
  if (!(reqColReferenz$id %in% inCSVReferenz$headAsChar) || length(unique(inCSVReferenz$df[,reqColReferenz$id])) != length(inCSVReferenz$df[,reqColReferenz$id]))
    txt <- paste0(txt, "<li>Jede Referenz benötigt eine persistente und eindeutige ID in der Spalte '", reqColReferenz$id, "'.</li>")
  for (reqColName in reqColReferenz[-1]) {
    if (!(reqColName %in% inCSVReferenz$headAsChar))
      txt <- paste0(txt, "<li>Bitte die Spalte '", reqColName, "' ergänzen.</li>", sep="")
  }

  if(length(unique(inCSVReferenz$headAsChar)) != length(inCSVReferenz$headAsChar))
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")

  valiReferenz$txt <- txt

  valiReferenz$validated <- TRUE
})

# write txt feedback as html list - or action button
output$ReferenzValidationOut <- renderUI({
  if (valiReferenz$validated) {
    if (is.null(valiReferenz$txt)) {
      actionButton("checkDBReferenz", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiReferenz$txt, "</ul></div></html"))
    }
  } else {
    return()
  }
})

##########################
## check DB consistency ##
##########################

# find existing Referenze

observeEvent(input$checkDBReferenz, {
  db <- connectToDB()
  tryCatch({
    progress <- shiny::Progress$new()
    on.exit(progress$close(), add = T)

    progress$set(message = "Prüfe bereits registrierte Referenzen.", value = 0)

    # get all Referenze from the DB that have any of the identifiers in the CSV
    ReferenzInDB <- dbGetQuery(db, paste0("SELECT id, identifier FROM referenz WHERE identifier IN ('",
                                     paste(inCSVReferenz$df[,reqColReferenz$id], collapse="', '"),"')"))
    if (nrow(ReferenzInDB) > 0) {
      checkDBReferenz$txt <- paste("Folgende Referenzen sind bereits in der DB: <ul><li>",
                              paste0(ReferenzInDB$identifier, collapse="</li><li>"))
    } else {
      checkDBReferenz$txt <- NULL
    }

    checkDBReferenz$ReferenzInDB <- ReferenzInDB

    checkDBReferenz$checked <- TRUE
  }, error = modalErrorHandler, finally = poolReturn(db))
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button

output$ReferenzDBConsistencyOut <- renderUI({
  if (checkDBReferenz$checked) {
    if (is.null(checkDBReferenz$txt) || input$owReferenz) {
      actionButton("storeDBReferenz", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBReferenz$txt, "</li></ul></div></html"))
    }
  } else {
    return()
  }
})

# plot table with CSV

output$tableReferenz <- renderDataTable({
  if (!is.null(inCSVReferenz$df)) {
    showTab <- inCSVReferenz$df

    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

    showHead <- paste0(showHead, "</span>")

    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sReferenz=FALSE, dom="t",
                                       language=list(url = lngJSON)),
                        escape=FALSE)

    # if DB consistency has been checked, apply colors
    if (checkDBReferenz$checked) {
      rowColors <- rep("white", nrow(showTab))

      if (nrow(checkDBReferenz$ReferenzInDB) > 0) {
        rowColors[showTab$ID %in% checkDBReferenz$ReferenzInDB$identifier] <- "red"
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

observeEvent(input$storeDBReferenz, {
  db <- connectToDB()
  tryCatch({

    Referenz_data <- inCSVReferenz$df
    Referenz_header <- inCSVReferenz$headAsChar

    Referenz_empty_cols <- apply(Referenz_data, 2, function(x) all(is.na(x)))

    Referenz_header <- Referenz_header[!Referenz_empty_cols]
    Referenz_data <- Referenz_data[,!Referenz_empty_cols]

    nRowDf <- nrow(Referenz_data)

    progress <- shiny::Progress$new()
    on.exit(progress$close(), add=T)

    progress$set(message = "Füge Referenzen in DB ein.", value = 0)

    ## add missign columns
    regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata WHERE prefixid IN ('ref', 'global')"))[,1]
    referenzColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata 
                                                  WHERE prefixid IN ('ref') AND columnid LIKE 'col%'"))
    misCols <- which(sapply(Referenz_header,
                            function(x) is.na(match(x, regCols))))

    if (length(misCols > 0)) {
      for (i in 1:length(misCols)) {# i <- 1
        colId <- sprintf("col%03d", i + length(regCols))
        coltype = switch(class(Referenz_data[,misCols[i]]),
                         integer = "numeric",
                         numeric = "numeric",
                         character = "character varying(255)")

        colMetadataExists = dbGetQuery(db, paste0("SELECT count(columnid) > 0
                                                  FROM column_metadata
                                                  WHERE columnid='", colId, "'
                                                  AND prefixid='ref' ;"))
        if (!colMetadataExists) {
          dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, prefixid, dede)
                                 VALUES ('", paste(colId, 'ref', Referenz_header[misCols[i]], sep="', '"),"');"))
        }

        colExists = dbGetQuery(db, paste0("SELECT count(column_name) > 0
                                          FROM information_schema.columns 
                                          WHERE table_name='referenz' 
                                          AND column_name='", colId, "';"))
        if (!colExists) {
          dbSendQuery(db, paste0("ALTER TABLE referenz ADD COLUMN ", colId, " ", coltype, ";"))
        }

      }
      referenzColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata 
                                                  WHERE prefixid IN ('ref') AND columnid LIKE 'col%'"))
    }

    # if there are already Referenze in the DB that are again in the CSV
    for (ref in 1:nrow(Referenz_data)) {
      dynamicDf <- NULL
      dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(referenzColumnMappings)))
      colnames(dynamicDfRow) <- c("columnid", "dede", "value")
      for (col in 1:nrow(referenzColumnMappings)) {
        dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(referenzColumnMappings)))
        colnames(dynamicDfRow) <- c("columnid", "dede", "value")
        dynamicDfRow$columnid <- referenzColumnMappings[col, "columnid"]
        dynamicDfRow$dede <- referenzColumnMappings[col, "dede"]
        value = Referenz_data[ref, referenzColumnMappings[col, "dede"]]
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
      if (Referenz_data[ref,"ID"] %in% checkDBReferenz$ReferenzInDB$identifier) {
        ## UPDATE referenz and data via SQL ##
        update = paste0("UPDATE referenz SET ", 
                           paste0(paste0(dynamicDf[["columnid"]], " = ", gsub("EMPTY", "NULL", dynamicDf[["value"]])), collapse = ", "),
                           " WHERE identifier = '", Referenz_data[ref, reqColReferenz$id], "';")
        dbSendQuery(db, update)
      } else {
        ## INSERT referenz and data via SQL ##
        dynamicColumns = paste0(dynamicDf[["columnid"]], collapse = ", ")
        dynamicValues = paste0(gsub("EMPTY", "NULL", dynamicDf[["value"]]), collapse = ", ")

        insert = paste0("INSERT INTO referenz (id, identifier, ", dynamicColumns, ")
                        SELECT nextval('referenzid_seq')::int, '", 
                           Referenz_data[ref, reqColReferenz$id], "', ",
                           dynamicValues, ";")
        dbSendQuery(db, insert)
      }
    }

    message = paste0(nrow(Referenz_data) , " Referenzen wurden erfolgreich in der Datenbank angelegt.")
    showModalMessage("Vorgang abgeschlossen", message)
    }, error = modalErrorHandler, finally = poolReturn(db))
})
