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
############################   Upload der Probe   ##############################
############################################################################## #

# storage of variables that might change through the GUI
inCSVProbe <- reactiveValues()
valiProbe <- reactiveValues(validated = FALSE)
checkDBProbe <- reactiveValues(checked = FALSE)

sepProbe <- colSep
decProbe <- decSep

################################## #
## validation of Probe csv-file ####
################################## #
observeEvent(input$csvFileProbe, {
  valiProbe$validated <- FALSE
  checkDBProbe$checked <- FALSE

  inCSVProbe$df <- read.csv(input$csvFileProbe$datapath,
                            header = TRUE,
                            sep = sepProbe, dec = decProbe,
                            stringsAsFactors = FALSE,
                            fileEncoding = input$probeFileEnc)

  inCSVProbe$headAsChar <- colnames(inCSVProbe$df)

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
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiProbe$txt, "</ul></div></html>"))
    }
  } else {
    return()
  }
})


########################## #
## check DB consistency ####
########################## #

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
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBProbe$txt, "</li></ul></div></html>"))
    }
  } else {
    return()
  }
})

######################### #
## plot table with CSV ####
######################### #
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


################## #
## Insert Probe ####
################## #

observeEvent(input$storeDBProbe, {
  checkDBProbe$checked <- FALSE
  db <- connectToDB()
  tryCatch({
    dbWithTransaction(db, {
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
      probeColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata
                                                    WHERE prefixid IN ('probe') AND columnid LIKE 'col%'"))
      missingColumns <- which(sapply(Probe_header,
                              function(x) is.na(match(x, registeredColumns))))

      if (length(missingColumns > 0)) {
        for (i in 1:length(missingColumns)) {# i <- 1
          colId <- paste0(sprintf("col%03d", i + length(registeredColumns)))
          coltype = switch(class(Probe_data[, missingColumns[i]]),
                           integer = "numeric",
                           numeric = "numeric",
                           character = "character varying(255)")

          dbSendQuery(db, paste0("ALTER TABLE probe ADD COLUMN ", colId, " ", coltype, ";"))

          dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, prefixid, dede)
                                   VALUES ('", paste(colId, 'probe', Probe_header[missingColumns[i]], sep="', '"),"')"))
        }
        probeColumnMappings <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata
                                                      WHERE prefixid IN ('probe') AND columnid LIKE 'col%'"))
      }

      for (probe in 1:nrow(Probe_data)) { # probe <- 1
        dynamicDf <- NULL
        dynamicDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(probeColumnMappings)))
        colnames(dynamicDfRow) <- c("columnid", "dede", "value")
        for (col in 1:nrow(probeColumnMappings)) {
          dynamicDfRow$columnid <- probeColumnMappings[col, "columnid"]
          dynamicDfRow$dede <- probeColumnMappings[col, "dede"]
          value = Probe_data[probe, probeColumnMappings[col, "dede"]]
          if (is.null(value) || is.na(value) || value == '') {
            dynamicDfRow$value = "EMPTY"
          } else {
            if (class(value) == "character") {
              if (length(grep(timestampRegExPattern, value, value = TRUE)) > 0) {
                value = paste0("to_timestamp('", value, "', '", dbTimestampPattern, "')::timestamp at time zone '", dbTimeZoneIdentifier, "'")
              } else {
                value = paste0("'", value, "'")
              }
            }
            dynamicDfRow$value = value
          }
          dynamicDf <- rbind(dynamicDf, dynamicDfRow)
        }

        # if there are already Probee in the DB that are again in the CSV
        if (Probe_data[probe, reqColProbe$id] %in% checkDBProbe$ProbeInDB$identifier) {
          ## UPDATE Probe via SQL ##
          query <- paste0("UPDATE probe SET ",
                          "abfluss_situation = '", Probe_data[probe, reqColProbe$abfluss_situation], "', ",
                          "resulttime = to_timestamp('", Probe_data[probe, reqColProbe$colDate], "', '", dbTimestampPattern, "')::timestamp at time zone '", dbTimeZoneIdentifier, "', ",
                          "phenomenontimestart = to_timestamp('", Probe_data[probe, reqColProbe$eventTimeBegin], "', '", dbTimestampPattern, "')::timestamp at time zone '", dbTimeZoneIdentifier, "', ",
                          "phenomenontimeend = to_timestamp('", Probe_data[probe, reqColProbe$eventTimeEnd], "', '", dbTimestampPattern, "')::timestamp at time zone '", dbTimeZoneIdentifier, "', ",
                          "pns_id = (SELECT pns_id FROM query_pns), ",
                          "lab = '", Probe_data[probe, reqColProbe$labName], "', ",
                          "lab_id = '", Probe_data[probe, reqColProbe$labId], "', ",
                          "subprobe = '", gsub("EMPTY", "NULL", Probe_data[probe, reqColProbe$subprobe]), "', ",
                          paste0(paste0(dynamicDf[["columnid"]], " = ", gsub("EMPTY", "NULL", dynamicDf[["value"]])), collapse = ", "),
                          " WHERE identifier = '", Probe_data[probe, reqColProbe$id], "';")
          dbSendQuery(db, query)
        } else {
          ## INSERT Probe via SQL ##
          if (length(probeColumnMappings) > 0) {
            dynamicColumns <- paste0(probeColumnMappings[, 1], collapse = ", ")
            dynamicValues <- paste0(gsub("EMPTY", "NULL", dynamicDf[["value"]]), collapse = ", ")
          } else {
            dynamicColumns <- NULL
            dynamicValues <- NULL
          }

          get_pns_id <- paste0("WITH query_pns AS (
                              SELECT featureofinterestid AS pns_id
                              FROM featureofinterest
                              WHERE identifier = '",
                              Probe_data[probe, reqColProbe$geoSub],
                              "')")
          query <- paste(get_pns_id,
                        "INSERT INTO probe
                         (id, identifier, resulttime, phenomenontimestart, phenomenontimeend, pns_id, lab, lab_id, abfluss_situation, subprobe",
                            ifelse(is.null(dynamicColumns), "", ","), dynamicColumns, ")",
                         paste("VALUES (nextval('probeid_seq')",
                           paste0("'", Probe_data[probe, reqColProbe$id], "'"),
                           paste0("to_timestamp('", Probe_data[probe, reqColProbe$colDate], "', '", dbTimestampPattern, "')::timestamp at time zone '", dbTimeZoneIdentifier, "'"),
                           paste0("to_timestamp('", Probe_data[probe, reqColProbe$eventTimeBegin], "', '", dbTimestampPattern, "')::timestamp at time zone '", dbTimeZoneIdentifier, "'"),
                           paste0("to_timestamp('", Probe_data[probe, reqColProbe$eventTimeEnd], "', '", dbTimestampPattern, "')::timestamp at time zone '", dbTimeZoneIdentifier, "'"),
                           "(SELECT pns_id FROM query_pns)",
                           paste0("'", Probe_data[probe, reqColProbe$labName], "'"),
                           paste0("'", Probe_data[probe, reqColProbe$labId], "'"),
                           paste0("'", Probe_data[probe, reqColProbe$abfluss_situation], "'"),
                           paste0("'", gsub("EMPTY", "NULL", Probe_data[probe, reqColProbe$subprobe]), "'"),
                           sep=", "
                        ),
                        ifelse(is.null(dynamicValues), "", ","),
                        dynamicValues,
                        ");")
          dbSendQuery(db, query)
        }
      }

      message <- paste0(nrow(Probe_data), " Proben wurden erfolgreich in der Datenbank angelegt.")
      showModalMessage(title="Vorgang abgeschlossen", message)
    })
  }, error = modalErrorHandler, finally = poolReturn(db))
})
