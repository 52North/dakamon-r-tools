# Copyright (C) 2017-2018 52°North Initiative for
# Geospatial Open Source Software GmbH
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 as published
# by the Free Software Foundation.
#
# If the program is linked with libraries which are licensed under one of
# the following licenses, the combination of the program with the linked
# library is not considered a "derivative work" of the program:
#
#     - Apache License, version 2.0
#     - Apache Software License, version 1.0
#     - GNU Lesser General Public License, version 3
#     - Mozilla Public License, versions 1.0, 1.1 and 2.0
#     - Common Development and Distribution License (CDDL), version 1.0
#
# Therefore the distribution of the program linked with libraries licensed
# under the aforementioned licenses, is permitted by the copyright holders
# if the distribution is compliant with both the GNU General Public
# License version 2 and the aforementioned licenses.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
############################################################################## #
#############################   Upload des Ortes   #############################
############################################################################## #

# storage of variables that might change through the GUI
inCSVOrt <- reactiveValues()
valiOrt <- reactiveValues(validated = FALSE)
checkDBOrt <- reactiveValues(checked = FALSE)

sepOrt <- colSep
decOrt <- decSep

observeEvent(input$csvFileOrt, {
  valiOrt$validated <- FALSE
  checkDBOrt$checked <- FALSE

  inCSVOrt$df <- read.csv(input$csvFileOrt$datapath,
                          header = TRUE,
                          sep = sepOrt, dec = decOrt,
                          stringsAsFactors = FALSE,
                          fileEncoding = input$ortFileEnc)

  inCSVOrt$headAsChar <- colnames(inCSVOrt$df)

  ## validation of Ort csv-file
  # look for required column names
  # check whether columns have unique names

  txt <- NULL
  if (!(reqColOrt$id %in% inCSVOrt$headAsChar) || length(unique(inCSVOrt$df[,reqColOrt$id])) != length(inCSVOrt$df[,reqColOrt$id]))
    txt <- paste0(txt, "<li>Jeder Ort benötigt eine persistente und eindeutige ID in der Spalte '", reqColOrt$id, "'.</li>")
  for (reqColName in reqColOrt[-1]) {
    if (!(reqColName %in% inCSVOrt$headAsChar))
      txt <- paste0(txt, "<li>Bitte die Spalte '", reqColName, "' ergänzen.</li>", sep="")
  }

  if(length(unique(inCSVOrt$headAsChar)) != length(inCSVOrt$headAsChar)) {
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")
  }

  #
  # validate identifier
  #
  if (length(grep(identifierRegex, inCSVOrt$df[,reqColOrt$id], perl=TRUE)) != length(unique(inCSVOrt$df[,reqColOrt$id]))) {
    txt <- paste0(txt, "<li>Die ID darf nur aus folgenden Zeichen bestehen: a-z, A-Z, 0-9, -, _ und muss mit einem Buchstaben oder einer Zahl beginnen:<ul>", sep="")
    for (id in inCSVOrt$df[,reqColOrt$id]) {
      if (length(grep(identifierRegex, id, perl=TRUE)) == 0) {
        txt <- paste0(txt, "<li>", id, "</li>", sep = "")
      }
    }
    txt <- paste0(txt, "</ul></li>")
  }

  valiOrt$txt <- txt

  valiOrt$validated <- TRUE
})

# write txt feedback as html list - or action button
output$OrtValidationOut <- renderUI({
  if (valiOrt$validated) {
    if (is.null(valiOrt$txt)) {
      actionButton("checkDBOrt", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiOrt$txt, "</ul></div></html>"))
    }
  } else {
    return()
  }
})

############################ #
#### Check DB consistency ####
############################ #

# find existing Orte

observeEvent(input$checkDBOrt, {
  db <- connectToDB()
  tryCatch({
    progress <- shiny::Progress$new()
    on.exit(progress$close(), add = T)

    progress$set(message = "Prüfe bereits registrierte Orte.", value = 0)

    # get all Orte from the DB that have any of the identifiers in the CSV
    OrtInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('",
                                     paste(inCSVOrt$df[,reqColOrt$id], collapse="', '"),"')"))
    if (nrow(OrtInDB) > 0) {
      checkDBOrt$txt <- paste("Folgende Orte sind bereits in der DB: <ul><li>",
                              paste0(OrtInDB$identifier, collapse="</li><li>"))
    } else {
      checkDBOrt$txt <- NULL
    }

    checkDBOrt$OrtInDB <- OrtInDB

    checkDBOrt$checked <- TRUE
  }, error = modalErrorHandler, finally = poolReturn(db))
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button

output$OrtDBConsistencyOut <- renderUI({
  if (checkDBOrt$checked) {
    if (is.null(checkDBOrt$txt) || input$owOrt) {
      actionButton("storeDBOrt", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBOrt$txt, "</li></ul></div></html>"))
    }
  } else {
    return()
  }
})

########################### #
#### Plot table with CSV ####
########################### #
output$tableOrt <- renderDataTable({
  if (!is.null(inCSVOrt$df)) {
    showTab <- inCSVOrt$df

    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

    showHead <- paste0(showHead, "</span>")

    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sort=FALSE, dom="t",
                                       language=list(url = lngJSON)),
                        escape=FALSE)

    # if DB consistency has been checked, apply colors
    if (checkDBOrt$checked) {
      rowColors <- rep("white", nrow(showTab))

      if (nrow(checkDBOrt$OrtInDB) > 0) {
        rowColors[showTab$ID %in% checkDBOrt$OrtInDB$identifier] <- "red"
        showDT <- formatStyle(showDT, "ID", target="row",
                              backgroundColor = styleEqual(showTab$ID, rowColors))
      }
    }
    showDT
  }
})

############################## #
#### Insert/Update Features ####
############################## #
observeEvent(input$storeDBOrt, {
  checkDBOrt$checked <- FALSE
  db <- connectToDB()
  tryCatch({
    dbWithTransaction(db, {

      Ort_data <- inCSVOrt$df
      Ort_header <- inCSVOrt$headAsChar

      Ort_empty_cols <- apply(Ort_data, 2, function(x) all(is.na(x)))

      Ort_header <- Ort_header[!Ort_empty_cols]
      Ort_data <- Ort_data[,!Ort_empty_cols]

      nRowDf <- nrow(Ort_data)

      progress <- shiny::Progress$new()
      on.exit(progress$close(), add=T)

      progress$set(message = "Füge Orte in DB ein.", value = 0)

      ## add missing columns
      regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata WHERE prefixid IN ('ort', 'global')"))[,1]
      ortColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata
                                                  WHERE prefixid IN ('ort') AND columnid LIKE 'col%'"))
      misCols <- which(sapply(Ort_header,
                              function(x) is.na(match(x, regCols))))

      if (length(misCols > 0)) {
        for (i in 1:length(misCols)) {# i <- 1
          colId <- sprintf("col%03d", i + length(regCols))
          coltype <- switch(class(Ort_data[,misCols[i]]),
                            integer = "numeric",
                            numeric = "numeric",
                            character = "character varying(255)")
          colHeader <- Ort_header[misCols[i]]
          colMetadataExists = dbGetQuery(db, paste0("SELECT count(columnid) > 0
                                                    FROM column_metadata
                                                    WHERE columnid='", colId, "'
                                                    AND prefixid='ort' ;"))
          if (!colMetadataExists) {
            # print(paste0("add to column_metadata: id='", colId, "', dede='", colHeader, "', prefix='ort'"))
            dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, prefixid, dede)
                                    VALUES ('", paste(colId, 'ort', Ort_header[misCols[i]], sep="', '"),"');"))
          }

          colExists = dbGetQuery(db, paste0("SELECT count(column_name) > 0
                                             FROM information_schema.columns
                                             WHERE table_name='ort_data'
                                             AND column_name='", colId, "';"))
          if (!colExists) {
            # print(paste0("add column '", colId, "' ('", colHeader, "') to ort_data"))
            dbSendQuery(db, paste0("ALTER TABLE ort_data ADD COLUMN ", colId, " ", coltype, ";"))
          }
        }
        ortColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata
                                                    WHERE prefixid IN ('ort') AND columnid LIKE 'col%'"))
      }

      for (ort in 1:nrow(Ort_data)) {
        dynamicDf <- NULL
        dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(ortColumnMappings)))
        colnames(dynamicDfRow) <- c("columnid", "dede", "value")
        for (col in 1:nrow(ortColumnMappings)) {
          dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(ortColumnMappings)))
          colnames(dynamicDfRow) <- c("columnid", "dede", "value")
          dynamicDfRow$columnid <- ortColumnMappings[col, "columnid"]
          dynamicDfRow$dede <- ortColumnMappings[col, "dede"]
          value = Ort_data[ort, ortColumnMappings[col, "dede"]]
          if (is.null(value) || is.na(value) || value == '') {
            dynamicDfRow$value <- "EMPTY"
          } else {
            if (class(value) == "character") {
              value <- paste0("'", value, "'")
            }
            dynamicDfRow$value = value
          }
          dynamicDf <- rbind(dynamicDf, dynamicDfRow)
        }

        # if there are already Orte in the DB that are again in the CSV
        if (Ort_data[ort,"ID"] %in% checkDBOrt$OrtInDB$identifier) {
          query <- paste0("with update_ort as (
          UPDATE featureofinterest SET
          name = ", paste0("'", Ort_data[ort, reqColOrt$name], "'"), ",
          geom = ", paste0("ST_GeomFromText('POINT (", paste(Ort_data[ort, reqColOrt$lon],
                                                             Ort_data[ort, reqColOrt$lat],
                                                    ")', 4326) ")),
                           "WHERE identifier = ", paste0("'", Ort_data[ort, reqColOrt$id], "'"),
                          " RETURNING featureofinterestid
          )
          UPDATE ort_data SET ",
                          paste0(paste0(dynamicDf[["columnid"]],
                                        " = ",
                                        gsub("EMPTY", "NULL", dynamicDf[["value"]])),
                                 collapse = ", "),
                          " WHERE featureofinterestid = (SELECT featureofinterestid FROM update_ort);"
          )
          dbSendQuery(db, query)
        } else {
          ## INSERT FoI and data via SQL ##
          if (length(ortColumnMappings) > 0) {
            #dynamicColumns = paste0(dynamicDf[["columnid"]], collapse = ", ")
            dynamicColumns <- paste0(ortColumnMappings[, 1], collapse = ", ")
            dynamicValues <- paste0(gsub("EMPTY", "NULL", dynamicDf[["value"]]), collapse=", ")
          } else {
            dynamicColumns <- NULL
            dynamicValues <- NULL
          }

          # insertFeature = paste("INSERT INTO featureofinterest (featureofinterestid, featureofinteresttypeid, identifier, name, geom)
          #              VALUES (nextval('featureofinterestid_seq'), 1",
          #   paste0("'", Ort_data[ort, reqColOrt$id], "'"),
          #   paste0("'", Ort_data[ort, reqColOrt$name], "'"),
          #   paste0("ST_GeomFromText('POINT (", Ort_data[ort, reqColOrt$lat], " ",
          #           Ort_data[ort, reqColOrt$lon], ")', 4326)) "),
          #           sep = ", ")
          #
          # insertOrt = paste0("INSERT INTO ort_data (featureofinterestid, rndid, ",
          #   dynamicColumns, ")
          #           SELECT ort_id, pseudo_encrypt(nextval('rndIdSeq')::int),",
          #   dynamicValues, " FROM insert_ort")
          # query = paste("WITH insert_ort as (", insertFeature, " RETURNING featureofinterestid as ort_id)",
          #   insertOrt, ";")


          query <- paste0("WITH select_type as (
              SELECT featureofinteresttypeid as id FROM featureofinteresttype WHERE featureofinteresttype = '", foiType,"'
              ), insert_ort as (
              INSERT INTO featureofinterest
              (featureofinterestid, featureofinteresttypeid, identifier, name, geom)
              VALUES (nextval('featureofinterestid_seq'), (SELECT id FROM select_type)", ", ",
                          paste0("'", Ort_data[ort, reqColOrt$id], "'"), ", ",
                          paste0("'", Ort_data[ort, reqColOrt$name], "'"), ", ",
                          paste("ST_GeomFromText('POINT (", paste(Ort_data[ort, reqColOrt$lon],
                                                                  Ort_data[ort, reqColOrt$lat], ")', 4326)")),
                          ") RETURNING featureofinterestid as ort_id)",
                          "INSERT INTO ort_data
              (featureofinterestid, rndid, lat, lon, thematik", ifelse(is.null(dynamicColumns), "", ","), dynamicColumns, ")
              SELECT ort_id, pseudo_encrypt(nextval('rndIdSeq')::int), ", Ort_data[ort, reqColOrt$lat],", ",
                          Ort_data[ort, reqColOrt$lon], ", '", Ort_data[ort, reqColOrt$thematik], "'",
                          ifelse(is.null(dynamicColumns), "", ","), dynamicValues,
                          " FROM insert_ort")

          dbSendQuery(db, query)
        }
      }

      message <- paste0(nrow(Ort_data) , " Orte wurden erfolgreich in der Datenbank angelegt.")
      showModalMessage(title="Vorgang abgeschlossen", message)
    })
  }, error = modalErrorHandler, finally = poolReturn(db))
})
