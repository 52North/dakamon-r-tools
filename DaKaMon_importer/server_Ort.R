################################################################################
#############################   Upload des Ortes   #############################
################################################################################

inCSVOrt <- reactiveValues()
vali <- reactiveValues(validated = FALSE)
checkDB <- reactiveValues(checked = FALSE)

observeEvent(input$csvFileOrt, {
  vali$validated <- FALSE
  checkDB$checked <- FALSE

  if (is.null(csvEncode))
    csvEncode <- readr::guess_encoding(input$csvFileOrt$datapath)
  
  inCSVOrt$csvEncode <- csvEncode$encoding[which.max(csvEncode$confidence)]
  
  inCSVOrt$headAsChar <- as.character(read.csv(input$csvFileOrt$datapath,
                                               header = FALSE,
                                               sep = sepOrt, dec = decOrt,
                                               nrows = 1, stringsAsFactors = FALSE, 
                                               fileEncoding = inCSVOrt$csvEncode))

  inCSVOrt$UoMs <- read.csv(input$csvFileOrt$datapath, header = FALSE,
                            sep = input$sepOrt, dec = input$decOrt,
                            skip = 1, nrows = 1,
                            stringsAsFactors = FALSE, 
                            fileEncoding = inCSVOrt$csvEncode)
  inCSVOrt$UoMs[is.na(inCSVOrt$UoMs)] <- ""
  inCSVOrt$UoMs <- as.character(inCSVOrt$UoMs)

  inCSVOrt$df <- read.csv(input$csvFileOrt$datapath, header = FALSE,
                          sep = input$sepOrt, dec = input$decOrt,
                          skip = 2,
                          stringsAsFactors = FALSE, 
                          fileEncoding = inCSVOrt$csvEncode)
  colnames(inCSVOrt$df) <- inCSVOrt$headAsChar
  
  ################################
  ## validation of Ort csv-file ##
  ################################
  # look for Name, ID, lat, lon and Stammanlage,
  # check whether columns have unique names

  txt <- NULL
  if (!(reqColOrt$id %in% inCSVOrt$headAsChar) || length(unique(inCSVOrt$df[,reqColOrt$id])) != length(inCSVOrt$df[,reqColOrt$id]))
    txt <- paste0(txt, "<li>Jede Kläranlage und jeder Verfahrensschritt benötigt eine persistente und eindeutige ID in der Spalte'", reqColOrt[1], "'.</li>")
  for (reqColName in reqColOrt[-1]) {
    if (!(reqColName %in% inCSVOrt$headAsChar))
      txt <- paste0(txt, "<li>Bitte ergänze die Spalte '", reqColName, "'.</li>", sep="")
  }

  if(length(unique(inCSVOrt$headAsChar)) != length(inCSVOrt$headAsChar))
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")

  vali$txt <- txt
  vali$validated <- TRUE
})

output$OrtValidationOut <- renderUI({
  if (vali$validated) {
    if (is.null(vali$txt)) {
      actionButton("checkDB", "Prüfe Datenkonsistenz!")
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

# find existing Orts
# check Parameter and their UoMs

observeEvent(input$checkDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname="sos", user="postgres", password="postgres", port="5432")
  on.exit(dbDisconnect(db), add=T)

  progress <- shiny::Progress$new()
  on.exit(progress$close(), add = T)

  progress$set(message = "Prüfe Datenkonsistenz.", value = 0)
  OrtinDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('",
                                   paste(inCSVOrt$df[,reqColOrt$id], collapse="', '"),"')")) ## [inclRowOrt()]
  if (nrow(OrtinDB) > 0) {
    checkDB$txtInfo <- paste("Folgende Kläranlagen/Verfahrensschritte sind bereits in der DB: <ul><li>",
                             paste0(OrtinDB$identifier, collapse="</li><li>"))
  } else {
    checkDB$txtInfo <- NULL
  }

  progress$inc(1/2, "Features")

  checkDB$OrtInDB <- OrtinDB$identifier

  # find columns already in DB: colInDB with columnid, dede and its unit from the unit table
  checkDB$colInDB <- dbGetQuery(db, paste0("SELECT columnid, dede, unit.unit FROM Ortdatametadata left outer join unit on (unit.unitid = uom) WHERE dede IN ('",
                                           paste(inCSVOrt$headAsChar, collapse="', '"),"')"))

  # replace NA UoM with ""
  if (nrow(checkDB$colInDB) > 0 ) {
    checkDB$colInDB$unit[is.na(checkDB$colInDB$unit)] <- ""

    # compare UoMs from the csv with the DB for overlapping columns
    compUoM <- inCSVOrt$UoMs[match(checkDB$colInDB$dede, inCSVOrt$headAsChar)] == checkDB$colInDB$unit
    checkDB$uomMissMatchCols <-which(!compUoM)

    checkDB$txtErr <- NULL
    if (length(checkDB$uomMissMatchCols) > 0) {
      checkDB$txtErr <-
        paste(
          "Folgende Spalten haben unterschiedliche Maßeinheiten: <ul><li>",
          paste0(
            paste0(checkDB$colInDB$dede[checkDB$uomMissMatchCols], ": [",
                   checkDB$colInDB$unit[checkDB$uomMissMatchCols], "] "),
            collapse = "</li><li>"
          )
        )
    }
  }

  progress$inc(1, "Maßeinheiten")

  checkDB$checked <- TRUE
}, ignoreInit=TRUE)

output$DBConsistencyTxtOut <- renderUI({
  if (checkDB$checked) {
    if (!is.null(checkDB$txtInfo) | !is.null(checkDB$txtErr)) {
      if (!is.null(checkDB$txtErr)) {
        HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDB$txtErr, "</li></ul></div></html"))
      } else {
        HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDB$txtInfo, "</li></ul></div></html"))
      }
    }
  } else {
    HTML("")
  }
})

output$DBConsistencyActionOut <- renderUI({
  if (checkDB$checked) {
    if (is.null(checkDB$txtErr)) {
      if (is.null(checkDB$txtInfo) || input$owOrt) {
        actionButton("storeDB", "Einfügen in DB!")
      }
    }
  } else {
    HTML("")
  }
})

output$tableOrt <- renderDataTable({
  if (!is.null(inCSVOrt$df)) {
    showTab <- inCSVOrt$df

    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

    if (!is.null(inCSVOrt$UoMs)) {
      showUoM <- sapply(inCSVOrt$UoMs, function(x) {
        if (!is.na(x) & nchar(x) > 0) {
          paste0(" [",x,"]")
        } else {
          ""
        }
      })
      if (!is.null(inCSVOrt$df))
        showHead <- paste0(showHead, showUoM)
    }

    showHead <- paste0(showHead, "</span>")

    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sort=FALSE, dom="t"),
                        escape=FALSE)

    # if DB consistency has been checked, apply colors
    if (checkDB$checked) {
      rowClrs <- rep("white", nrow(showTab))

      if (!is.null(checkDB$txtInfo)) {
        rowClrs[which(showTab[[reqColOrt$id]] %in% checkDB$OrtInDB)] <- "yellow"
        for (col in c(1, which(inCSVOrt$headAsChar %in% checkDB$colInDB$dede))) {
          if (col %in% checkDB$uomMissMatchCols) next;
          showDT <- formatStyle(showDT, col, "ID",
                                backgroundColor = styleEqual(showTab[[reqColOrt$id]], rowClrs))
        }
      }

      if (!is.null(checkDB$txtErr)) {
        rowClrs <- rep("red", nrow(showTab))
        for (col in checkDB$uomMissMatchCols) {
          showDT <- formatStyle(showDT, col, "ID",
                                backgroundColor = styleEqual(showTab[[reqColOrt$id]], rowClrs))
        }
      }
    }
    showDT
  }
})

###############################################
##############                   ##############
##############  Insert Feautres  ##############
##############                   ##############
###############################################

observeEvent(input$storeDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname="sos", user="postgres", password="postgres", port="5432")
  on.exit(dbDisconnect(db), add=T)

  Ort_data <- inCSVOrt$df
  Ort_header <- inCSVOrt$headAsChar ## [inclColOrt()]
  Ort_uom <- inCSVOrt$UoMs ## [inclColOrt()]

  Ort_empty_cols <- apply(Ort_data, 2, function(x) all(is.na(x)))
  
  Ort_empty_cols[which(names(Ort_empty_cols) == reqColOrt$super_Ort)] <- FALSE

  Ort_header <- Ort_header[!Ort_empty_cols]
  Ort_uom <- Ort_uom[!Ort_empty_cols]
  Ort_data <- Ort_data[,!Ort_empty_cols]

  # any parent features that need to be inserted before? Those with empty or missing Stammanlage column
  par_Ort <- is.na(Ort_data[,reqColOrt$super_Ort]) | nchar(Ort_data[,reqColOrt$super_Ort]) == 0

  nRowDf <- nrow(Ort_data)

  progress <- shiny::Progress$new()
  on.exit(progress$close(), add=T)

  progress$set(message = "Füge Daten in DB ein.", value = 0)

  # parent feature first
  if (any(par_Ort)) {
    for (sOrt in which(par_Ort)) {# sOrt <- 1
      progress$inc(1/nRowDf)
      # check whether the feature is already in the DB
      OrtInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '",
                                       Ort_data[sOrt,reqColOrt$id],"'"))
      if (nrow(OrtInDB) > 0) {
        if (!input$owOrt) {
          next;
        } else {
          curId <- OrtInDB$featureofinterestid

          # retrive relevant part of featurerelation table
          relationTab <- dbGetQuery(db, paste0("SELECT parentfeatureid, childfeatureid FROM featurerelation WHERE parentfeatureid = ",
                                               curId, "OR childfeatureid =",curId))

          # remove tmp feature if still present (previous crash)
          odlTmpId <- dbGetQuery(db, "SELECT featureofinterestid FROM featureofinterest WHERE identifier = 'tmp'")
          if (nrow(odlTmpId)>0) {
            # remove tmp feature
            dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", odlTmpId$featureofinterestid))
            dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", odlTmpId$featureofinterestid))

            # cache-update!
            SOScacheUpdate("tmp", verbose=verbose)
          }

          # insert tmp feature
          insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"),
                                         body = SOSinsOrt("tmp", "tmp", 0, 0),
                                         content_type_xml(), accept_xml())$content)
          if (verbose)
            message(insMsg)

          tmpId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = 'tmp'"))$featureofinterestid

          # map curID in table series to tmp id
          dbSendQuery(db, paste0("UPDATE series SET featureofinterestid = ", tmpId, " WHERE featureofinterestid = ", curId))

          # map curID in table Ortdata to tmp id
          dbSendQuery(db, paste0("UPDATE Ortdata SET featureofinterestid = ", tmpId, " WHERE featureofinterestid = ", curId))

          # delete relevant part of featurerelation table and from feature of interest table
          dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE parentfeatureid = ",
                                 curId, " OR childfeatureid = ", curId))
          dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", curId))

          # cache-update!
          SOScacheUpdate(Ort_data[sOrt,reqColOrt$id])

          # re-insert the Ort
          insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"),
                                         body = SOSinsOrt(Ort_data[sOrt,reqColOrt$id], Ort_data[sOrt,reqColOrt$name],
                                                          Ort_data[sOrt,reqColOrt$lat], Ort_data[sOrt,reqColOrt$lon]),
                                         content_type_xml(), accept_xml())$content)
          if (verbose)
            message(insMsg)

          # find the new featureofinterest id
          newOrtId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '",
                                            Ort_data[sOrt,reqColOrt$id],"'"))

          # clear featurerelation table first
          dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", newOrtId))

          # re-assign the featureofinterest id to the old one
          dbSendQuery(db, paste0("UPDATE featureofinterest SET featureofinterestid = ", curId, " WHERE identifier = '",
                                 Ort_data[sOrt,reqColOrt$id],"'"))

          # restore the featurerelation table
          dbWriteTable(db, "featurerelation", relationTab, append = TRUE, row.names=FALSE)

          # re-assign the right Ort id in series table replacing the temp ID
          dbSendQuery(db, paste0("UPDATE series SET featureofinterestid = ", curId, " WHERE featureofinterestid = ", tmpId))

          # re-assign the right Ort id in Ortdata table replacing the temp ID
          dbSendQuery(db, paste0("UPDATE Ortdata SET featureofinterestid = ", curId, " WHERE featureofinterestid = ", tmpId))

          # remove tmp feature
          dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", tmpId))
          dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", tmpId))

          # cache-update!
          SOScacheUpdate("tmp")
        }
      } else {
        insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"),
                                       body = SOSinsOrt(Ort_data[sOrt,reqColOrt$id], Ort_data[sOrt,reqColOrt$name],
                                                        Ort_data[sOrt,reqColOrt$lat], Ort_data[sOrt,reqColOrt$lon]),
                                       content_type_xml(), accept_xml())$content)
        if (verbose)
          message(insMsg)
      }
    }
  }

  # insert remaining Ort
  for (Ort in which(!par_Ort)) { # Ort <- 2
    progress$inc(1/nRowDf)

    # check whether the feature is already in the DB
    OrtInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '",
                                     Ort_data[Ort,reqColOrt$id],"'"))
    if (nrow(OrtInDB) > 0) {
      if (!input$owOrt) {
        next;
      } else {
        curId <- OrtInDB$featureofinterestid

        # retrive relevant part of featurerelation table
        relationTab <- dbGetQuery(db, paste0("SELECT parentfeatureid, childfeatureid FROM featurerelation WHERE parentfeatureid = ",
                                             curId, "OR childfeatureid =",curId))

        # insert tmp feature
        insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"),
                                       body = SOSinsOrt("tmp", "tmp", 0, 0),
                                       content_type_xml(), accept_xml())$content)
        if (verbose)
          message(insMsg)

        tmpId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = 'tmp'"))$featureofinterestid

        # map curID from series on tmp feature
        dbSendQuery(db, paste0("UPDATE series SET featureofinterestid = ", tmpId, " WHERE featureofinterestid = ", curId))

        # map curID in table Ortdata to tmp id
        dbSendQuery(db, paste0("UPDATE Ortdata SET featureofinterestid = ", tmpId, " WHERE featureofinterestid = ", curId))

        # delete relevant part of featurerelation table and from feature of interest table
        dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE parentfeatureid = ",
                               curId, " OR childfeatureid = ", curId))
        dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", curId))

        # cache-update!
        SOScacheUpdate(Ort_data[Ort,reqColOrt$id])

        # re-insert the Ort
        insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"),
                                       body = SOSinsOrt(Ort_data[Ort,reqColOrt$id], Ort_data[Ort,reqColOrt$idname],
                                                        Ort_data[Ort,reqColOrt$lat], Ort_data[Ort,reqColOrt$lon],
                                                        Ort_data[Ort,reqColOrt$super_Ort]),
                                       content_type_xml(), accept_xml())$content)
        if (verbose)
          message(insMsg)

        # find the new featureofinterest id
        newOrtId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '",
                                          Ort_data[Ort,reqColOrt$id],"'"))

        # clear featurerelation table first
        dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", newOrtId))

        # re-assign the featureofinterest id to the old one
        dbSendQuery(db, paste0("UPDATE featureofinterest SET featureofinterestid = ", curId, " WHERE identifier = '",
                               Ort_data[Ort, reqColOrt$id],"'"))

        # restore the featurerelation table
        if (nrow(relationTab) > 0)
          dbWriteTable(db, "featurerelation", relationTab, append = TRUE, row.names=FALSE)

        # re-assign the right Ort id in series table replacing the temp ID
        dbSendQuery(db, paste0("UPDATE series SET featureofinterestid = ", curId, " WHERE featureofinterestid = ", tmpId))

        # re-assign the right Ort id in Ortdata table replacing the temp ID
        dbSendQuery(db, paste0("UPDATE Ortdata SET featureofinterestid = ", curId, " WHERE featureofinterestid = ", tmpId))

        # remove tmp feature
        dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", tmpId))
        dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", tmpId))

        # cache-update!
        SOScacheUpdate("tmp")
      }

    } else {
      insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"),
                                     body = SOSinsOrt(Ort_data[Ort,reqColOrt$id], Ort_data[Ort,reqColOrt$name],
                                                      Ort_data[Ort,reqColOrt$lat], Ort_data[Ort,reqColOrt$lon],
                                                      Ort_data[Ort,reqColOrt$super_Ort]),
                                     content_type_xml(), accept_xml())$content)
      if (verbose)
        message(insMsg)
    }
  }

  # pre-process: add new units
  regUoMs <- dbGetQuery(db, paste0("SELECT unit FROM unit"))
  unqUoMs <- unique(Ort_uom[-1])
  if (nrow(regUoMs) > 0) {
    regUoMs <- regUoMs[,1]
    misUoMs <- unqUoMs[which(sapply(unqUoMs, function(x) is.na(match(x, regUoMs))) & nchar(unqUoMs) > 0)]
  } else {
    misUoMs <- unqUoMs
  }

  for (uom in misUoMs) {
    dbSendQuery(db, paste0("INSERT INTO unit (unitid, unit) VALUES (nextval('unitid_seq'), '", uom, "');"))
  }

  ### add new columns
  regCols <- dbGetQuery(db, paste0("SELECT dede FROM Ortdatametadata"))[,1]
  misCols <- which(sapply(Ort_header, function(x) is.na(match(x, regCols))))

  if (length(misCols > 0)) {
    for (i in 1:length(misCols)) {# i <- 1
      colId <- sprintf("col%03d", i + length(regCols))
      coltype = switch(class(Ort_data[,misCols[i]]),
                       integer = "numeric",
                       numeric = "numeric",
                       character = "character varying(255)")

      dbSendQuery(db, paste0("ALTER TABLE Ortdata ADD COLUMN ", colId, " ", coltype, ";"))

      # look-up UoM id
      unitId <- NULL
      if (nchar(Ort_uom[misCols[i]]) > 0) {
        unitId <- dbGetQuery(db, paste0("SELECT unitid FROM unit WHERE unit = '", Ort_uom[misCols[i]], "'"))[1,1]
      }

      if (is.null(unitId)) {
        dbSendQuery(db, paste0("INSERT INTO Ortdatametadata (columnid, dede)
                               VALUES ('", paste(colId, Ort_header[misCols[i]], sep="', '"),"')"))
      } else {
        dbSendQuery(db, paste0("INSERT INTO Ortdatametadata (columnid, dede, uom)
                               VALUES ('", paste(colId, Ort_header[misCols[i]], unitId, sep="', '"),"')"))
      }
    }
  }

  # feed data row-wise
  for (i in 1:nrow(Ort_data)) {
    nonEmpty <- which(!is.na(Ort_data[i,]))
    if (all(!nonEmpty)) next;

    # map csv-header to DB header via Ortdatametadata
    Ort_db_col_ids <- dbGetQuery(db, paste0("SELECT columnid, dede FROM Ortdatametadata WHERE dede IN ('",
                                            paste(Ort_header[nonEmpty], collapse="', '"),"')"))

    # mind the ordering
    Ort_db_col_ids <- Ort_db_col_ids[match(Ort_header[nonEmpty], Ort_db_col_ids$dede), "columnid"]

    # find the Ort idntifier
    Ort_db_id <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier ='",
                                       Ort_data[[reqColOrt$id]][i],"'"))

    # check whether the Ort has already some data
    if (nrow(dbGetQuery(db, paste0("SELECT id FROM Ortdata WHERE featureofinterestid = ", Ort_db_id))) > 0) {
      if(input$owOrt) {
        dbSendQuery(db, paste0("UPDATE Ortdata SET ",
                               paste(Ort_db_col_ids, Ort_data[i, nonEmpty], sep = " = '", collapse = "', "),
                               "' WHERE featureofinterestid = ", Ort_db_id, ";"))
      } else {
        next()
      }
    } else {
      dbSendQuery(db, paste0("INSERT INTO Ortdata ( featureofinterestid, ", paste(Ort_db_col_ids, collapse=", "), ") ",
                             "VALUES ('", Ort_db_id, "', '", paste(Ort_data[i, nonEmpty], collapse="', '"), "')"))
    }
  }

  showModal(modalDialog(
    title = "Vorgang abgeschlossen.",
    "Kläranlagen/Verfahrensschritte erfolgreich angelegt.",
    easyClose = TRUE,
    footer = NULL
  ))
})
