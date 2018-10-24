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

  inCSVPAR$df <- read.csv(input$csvFilePAR$datapath,
                          header = TRUE,
                          sep = sepPAR, dec = decPAR,
                          stringsAsFactors = FALSE,
                          fileEncoding = input$parFileEnc)

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
      actionButton("checkDBParameter", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiPAR$txt, "</ul></div></html>"))
    }
  } else {
    return()
  }
})


##########################
## check DB consistency ##
##########################

# find existing Parameters

observeEvent(input$checkDBParameter, {
  db <- connectToDB()
  tryCatch({

    progress <- shiny::Progress$new()
    on.exit(progress$close(), add = T)

    progress$set(message = "Prüfe bereits registrierte Parameter.", value = 0)

    # get all PARs from the DB that have any of the identifiers in the CSV
    if (length(inCSVPAR$df) > 0) {
      PARInDB <- dbGetQuery(db, paste0("SELECT observablepropertyid, identifier, name FROM observableproperty WHERE identifier IN ('",
                                     paste(inCSVPAR$df[,reqColPAR$id], collapse="', '"), "') OR name IN ('",
                                     paste(inCSVPAR$df[,reqColPAR$name], collapse="', '"), "')"))

      if (!is.null(PARInDB) && length(PARInDB) > 0 && nrow(PARInDB) > 0) {
        checkDBPAR$txt <- paste("Folgende Parameter sind bereits in der DB: <ul><li>",
                                paste0(PARInDB$identifier, collapse="</li><li>"))
      } else {
        checkDBPAR$txt <- NULL
    }

      checkDBPAR$PARInDB <- PARInDB
    }

    checkDBPAR$checked <- TRUE
  }, error = modalErrorHandler, finally = poolReturn(db))
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button
output$PARDBConsistencyOut <- renderUI({
  if (checkDBPAR$checked) {
    if (is.null(checkDBPAR$txt) || input$owPAR) {
      actionButton("storeDBParameter", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBPAR$txt, "</li></ul></div></html>"))
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
        rowColors[showTab$ID %in% checkDBPAR$PARInDB$identifier] <- "red"
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

observeEvent(input$storeDBParameter, {
  checkDBPAR$checked <- FALSE
  db <- connectToDB()
  tryCatch({
    dbWithTransaction(db, {
      PAR_data <- inCSVPAR$df
      PAR_header <- inCSVPAR$headAsChar

      PAR_empty_cols <- apply(PAR_data, 2, function(x) all(is.na(x)))

      PAR_header <- PAR_header[!PAR_empty_cols]
      PAR_data <- PAR_data[,!PAR_empty_cols]

      nRowDf <- nrow(PAR_data)

      progress <- shiny::Progress$new()
      on.exit(progress$close(), add=T)

      progress$set(message = "Füge Parameter in DB ein.", value = 0)

      ## add missing columns
      regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata WHERE prefixid IN ('param', 'global')"))[,1]
      paramColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata
                                                    WHERE prefixid IN ('param') AND columnid LIKE 'col%'"))
      misCols <- which(sapply(PAR_header,
                              function(x) is.na(match(x, regCols))))

      if (length(misCols > 0)) {
        for (i in 1:length(misCols)) {# i <- 1
          colId <- paste0(sprintf("col%03d", i + length(regCols)))
          coltype = switch(class(PAR_data[,misCols[i]]),
                           integer = "numeric",
                           numeric = "numeric",
                           character = "character varying(255)")

          dbSendQuery(db, paste0("ALTER TABLE parameter_data ADD COLUMN ", colId, " ", coltype, ";"))

          dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, prefixid, dede)
                                  VALUES ('", paste(colId, 'param', PAR_header[misCols[i]], sep="', '"),"')"))
        }
        paramColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata
                                                      WHERE prefixid IN ('param') AND columnid LIKE 'col%'"))
      }

      for (param in 1:nrow(PAR_data)) {
        dynamicDf <- NULL
        dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(paramColumnMappings)))
        colnames(dynamicDfRow) <- c("columnid", "dede", "value")
        for (col in 1:nrow(paramColumnMappings)) {
          dynamicDfRow$columnid <- paramColumnMappings[col, "columnid"]
          dynamicDfRow$dede <- paramColumnMappings[col, "dede"]
          value = PAR_data[param, paramColumnMappings[col, "dede"]]
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
        # if there are already PARe in the DB that are again in the CSV
        if (PAR_data[param,reqColPAR$id] %in% checkDBPAR$PARInDB$identifier
            || PAR_data[param,reqColPAR$name] %in% checkDBPAR$PARInDB$name) {
          ## UPDATE PAR via SQL, returns the id (pkid) of the updated parameter ##
          query <- paste0("with update_param as (
            UPDATE observableproperty
            SET
            identifier = '", PAR_data[param, reqColPAR$id], "', ",
            "name = '", PAR_data[param,reqColPAR$name], "' ",
            "WHERE identifier = '", PAR_data[param,reqColPAR$id], "' ",
            "OR name = '",  PAR_data[param,reqColPAR$name], "' ",
            "RETURNING observablepropertyid
          )
          UPDATE parameter_data
          SET ",
          paste0(paste0(dynamicDf[["columnid"]],
                        " = ",
                        gsub("EMPTY", "NULL", dynamicDf[["value"]])),
                 collapse = ", "),
          " WHERE observablepropertyid = (SELECT observablepropertyid FROM update_param);")
          dbSendQuery(db, query)
        } else {
          ## INSERT PAR and data via SQL ##
          dynamicColumns = paste0(paramColumnMappings[, 1], collapse = ", ")
          dynamicValues = paste0(gsub("EMPTY", "NULL", dynamicDf[["value"]]), collapse = ", ")
          insertParams = paste0("(
            INSERT INTO observableproperty
            (observablepropertyid, identifier, name)
            VALUES
            (nextval('observablepropertyid_seq'), '",
                PAR_data[param, reqColPAR$id],
                "', '",
                PAR_data[param, reqColPAR$name],
                "')
            RETURNING observablepropertyid
          )")
          insertParameterValues = paste("INSERT INTO parameter_data
                          (observablepropertyid", ifelse(is.null(dynamicColumns), "", ","), dynamicColumns, ")
                          (SELECT observablepropertyid", ifelse(is.null(dynamicValues), "", ","), dynamicValues,
                          "FROM insert_param)")
          query = paste("WITH insert_param AS", insertParams, insertParameterValues, ";")
          dbSendQuery(db, query)
        }
      }

      message = paste0(nrow(PAR_data), " Parameter wurden erfolgreich in der Datenbank angelegt.")
      showModalMessage(title="Vorgang abgeschlossen", message)
    })
  }, error = modalErrorHandler, finally = poolReturn(db))
})
