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
## viewer server Messdaten

#########
# Tab 1 #
#########
ortData <- NULL
db <- connectToDB()
tryCatch({
  # load all Entwässerungssysteme from DB
  ews <- dbGetQuery(db, paste0("SELECT DISTINCT thematik FROM ort_data"))
  output$ewsSelInput <- renderUI(selectInput("ews", "Thematik", ews[,"thematik"]))

  # load all super FoI from DB
  ort <- dbGetQuery(db, "SELECT DISTINCT foi.featureofinterestid, foi.name, foi.identifier
                  FROM featureofinterest AS foi
                  RIGHT OUTER JOIN ort_data od ON foi.featureofinterestid = od.featureofinterestid")
  # RIGHT OUTER JOIN featurerelation fr ON foi.featureofinterestid = fr.parentfeatureid
  # RIGHT OUTER JOIN probe pro ON pro.pns_id = fr.childfeatureid WHERE foi.featureofinterestid = od.featureofinterestid")
}, error = modalErrorHandler, finally = poolReturn(db))

# if any
if (nrow(ort) > 0) {
  ortData <- reactive({
    db <- connectToDB()
    tryCatch({
      ortDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('ort', 'global')"))
      ortDataOrtMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('ort')"))
      ortColColumns <- paste0("od.", grep("col*", ortDataOrtMetaData$columnid, value = TRUE))
      selectedThematik <- input$ews
      if (Sys.info()["sysname"] == "Windows") {
        selectedThematik <- stri_enc_tonative(input$ews)
      }
      query <- paste0("SELECT foi.featureofinterestid, foi.identifier, foi.name, od.Thematik, lat, lon, ", paste0(ortColColumns, collapse=", "),
                      " FROM featureofinterest foi
                                       RIGHT OUTER JOIN ort_data od ON foi.featureofinterestid = od.featureofinterestid
                                       WHERE foi.featureofinterestid IN (",
                      paste0(ort$featureofinterestid, collapse=", "), ")
                                       AND od.thematik IN (", paste0("'", selectedThematik, "'" ,collapse=", ") ,")")
      ortData <- dbGetQuery(db, query)


      if (nrow(ortData) > 0)
        colnames(ortData) <- ortDataMetaData$dede[match(colnames(ortData), ortDataMetaData$columnid)]

      ortData
    }, error = modalErrorHandler, finally = poolReturn(db))
  })

  output$tableOrt  <- renderDT({
    if (nrow(ortData()) > 0) {
      showTab <- ortData()[,-1]

      showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

      showHead <- paste0(showHead, "</span>")

      if ("PLZ" %in% colnames(showTab)) {
        showTab$PLZ <- sprintf("%05i", showTab$PLZ)
      }

      dt <- datatable(showTab, colnames = showHead,
                      filter="top",
                      options = list(paging=FALSE, dom = 'Brt',
                                     language=list(decimal=",",
                                                   url = lngJSON)),
                      escape=FALSE)
      colNoneLatLon <- colnames(showTab)[!colnames(showTab)  %in% c("lat", "lon")]
      numCol <- colNoneLatLon[which(as.logical(sapply(showTab[,colNoneLatLon],is.numeric)))]
      numCol <- numCol[apply(matrix(showTab[,numCol] > floor(showTab[,numCol])), 2, any)]
      numCol <- numCol[!is.na(numCol)]
      dt <- formatRound(dt, c("lat", "lon"), digits=6, dec.mark=",", mark=".")
      if (length(numCol) > 0)
        dt <- formatRound(dt, numCol, digits=3, dec.mark=",", mark=".")
      dt
    }
  })

  sOrt <- reactive({
    sr <- input$tableOrt_rows_selected
    if(is.null(sr)) {
      input$tableOrt_rows_all
    } else {
      sort(sr)
    }
  })

  output$selTextOrt <- renderText({
    if (length(sOrt()) == 1) {
      paste("Zeile", sOrt(), "ist ausgewählt.")
    } else {
      paste("Zeilen", paste(sOrt(), collapse=", "), "sind ausgewählt.")
    }
  })

  output$exportOrtCSVLatin1 <- downloadHandler(
    filename = function() paste("Ort-", Sys.Date(), ".csv", sep=""),
    content = function(file) {
      df <- isolate(ortData()[sOrt(), -1])
      if ("PLZ" %in% colnames(df)) {
        df$PLZ <- sprintf("%05i", df$PLZ)
      }
      write.table(df, file, sep = ";", dec = ",", na = "",
                  fileEncoding = "Latin1", row.names = FALSE)
    }
  )

  output$exportOrtCSVUtf8 <- downloadHandler(
    filename = function() paste("Ort-", Sys.Date(), ".csv", sep=""),
    content = function(file) {
      df <- isolate(ortData()[sOrt(), -1])
      if ("PLZ" %in% colnames(df)) {
        df$PLZ <- sprintf("%05i", df$PLZ)
      }
      write.table(df, file, sep = ";", dec = ",", na = "",
                  fileEncoding = "UTF-8", row.names = FALSE)
    }
  )

  output$exportOrtRData <- downloadHandler(
    filename = function() {
      paste("Ort-", Sys.Date(), ".RData", sep="")
    },
    content = function(file) {
      df <- isolate(ortData()[sOrt(),-1])
      if ("PLZ" %in% colnames(df)) {
        df$PLZ <- sprintf("%05i", df$PLZ)
      }
      save(df, file = file)
    }
  )
}

observeEvent(input$fromOrtToPNS, {
  updateTabsetPanel(session, "inNavbarpage",selected = "Probenahmestelle(n) auswählen")
})


#####################
######  TAB 2  ######
#####################
# load all PNS for all selected Orte from DB

if(!is.null(ortData)) {
  pnsData <- reactive({
    db <- connectToDB()
    tryCatch({

      pnsDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns', 'global')"))
      pnsDataPnsMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns')"))

      pnsColColumns <- NULL
      if (nrow(pnsDataPnsMetaData) > 0 && length(grep("col*", pnsDataPnsMetaData$columnid, value = TRUE)) > 0) {
        pnsColColumns <- paste0(", pns.", grep("col*", pnsDataPnsMetaData$columnid, value = TRUE))
      }

      pns <- dbGetQuery(db, paste0("SELECT DISTINCT  foi.featureofinterestid, foi.identifier, foi.name, pfoi.identifier as orts_id, lat, lon",
                                   paste0(pnsColColumns),
                                   " FROM featureofinterest foi
                                 RIGHT OUTER JOIN pns_data pns ON foi.featureofinterestid = pns.featureofinterestid
                                 RIGHT OUTER JOIN featurerelation fr ON foi.featureofinterestid = fr.childfeatureid
                                 LEFT OUTER JOIN featureofinterest pfoi ON pfoi.featureofinterestid = fr.parentfeatureid
                                 WHERE fr.parentfeatureid in (",
                                   paste(ortData()[sOrt(),1], collapse=", "), ")"))
      if (nrow(pns) > 0)
        colnames(pns) <- pnsDataMetaData$dede[match(colnames(pns), pnsDataMetaData$columnid)]

      pns
    }, error = modalErrorHandler, finally = poolReturn(db))
  })


  output$tablePNS <- renderDT({

    showTab <- pnsData()[,-1]

    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

    showHead <- paste0(showHead, "</span>")

    dt <- datatable(showTab, colnames = showHead, filter="top",
                    options = list(paging=FALSE, dom = 'Brt', ordering=FALSE,
                                   language=list(url = lngJSON)),
                    escape=FALSE)
    colNoneLatLon <- colnames(showTab)[!colnames(showTab)  %in% c("lat", "lon")]
    numCol <- colNoneLatLon[which(as.logical(sapply(showTab[,colNoneLatLon],is.numeric)))]
    numCol <- numCol[apply(showTab[,numCol] > floor(showTab[,numCol]), 2, any)]
    numCol <- numCol[!is.na(numCol)]
    dt <- formatRound(dt, c("lat", "lon"), digits=6, dec.mark=",", mark=".")
    if (length(numCol) > 0)
      dt <- formatRound(dt, numCol, digits=3, dec.mark=",", mark=".")
    dt
  })

  sPNS <- reactive({
    sr <- input$tablePNS_rows_selected
    if(length(sr) == 0) {
      input$tablePNS_rows_all
    } else {
      sort(sr)
    }
  })

  output$selTextPNS <- renderText({
    if (length(sPNS()) == 0) {
      "Keine zugehörigen Probenahmestellen vorhanden."
    } else {
      if (length(sPNS()) == 1) {
        paste("Zeile", sPNS(), "ist ausgewählt.")
      } else {
        paste("Zeilen", paste(sPNS(), collapse=", "), "sind ausgewählt.")
      }
    }
  })

  output$exportPNSCSVLatin1 <- downloadHandler(
    filename = function() {
      paste("Probenahmestelle-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(isolate(pnsData()[sPNS(), -1]), file, sep = ";", dec=",",
                  fileEncoding = "Latin1", row.names = FALSE)
    }
  )

  output$exportPNSCSVUtf8 <- downloadHandler(
    filename = function() {
      paste("Probenahmestelle-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(isolate(pnsData()[sPNS(), -1]), file, sep = ";", dec=",",
                  fileEncoding = "UTF-8", row.names = FALSE)
    }
  )

  output$exportPNSRData <- downloadHandler(
    filename = function() {
      paste("Probenahmestelle-", Sys.Date(), ".RData", sep="")
    },
    content = function(file) {
      df <- isolate(pnsData()[sPNS(), -1])
      save(df, file = file)
    }
  )
} else {
  output$selTextPNS <- renderText("Bitte zunächst mindestens einen Ort auswählen.")
}

observeEvent(input$fromPNStoMessdaten, {
  updateTabsetPanel(session, "inNavbarpage", selected = "Messdaten anzeigen")
})

#####################
######  TAB 3  ######
#####################
# load all avaialble elemGroup for the selected PNS

elemGroup <- reactive({
  db <- connectToDB()

  tryCatch({
    res <- NULL

    col <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
    if (nrow(col) != 0) {
      res <- dbGetQuery(db, paste0("SELECT DISTINCT ", col, " as name FROM parameter_data
                                    WHERE ", col, " IS NOT NULL"))
    }
    res
  }, error = modalErrorHandler, finally = poolReturn(db))
})

output$elemGroup <- renderUI(selectInput("selElemGroup", "Stoffgruppenauswahl:",
                                         elemGroup()$name, multiple = TRUE,
                                         selected = elemGroup()$name[1]))

# load all avaialble obsProp for the selected FoI

obsProp <- reactive({
  if (is.null(input$selElemGroup))
    return(NULL)

  db <- connectToDB()
  tryCatch({
    colStoffgruppe <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
    op <- NULL
    for (i in 1:length(sPNS())) { # i <- 1
      query <- paste0("SELECT DISTINCT op.observablepropertyid, op.identifier, op.name, foi.identifier AS foiid,
                        foi.featureofinterestid, s.seriesid
                       FROM observableproperty AS op
                        LEFT OUTER JOIN series AS s ON (op.observablepropertyid = s.observablepropertyid)
                        LEFT OUTER JOIN featureofinterest AS foi ON (s.featureofinterestid = foi.featureofinterestid)
                        LEFT OUTER JOIN procedure AS p ON (s.procedureid = p.procedureid)
                        LEFT OUTER JOIN parameter_data AS pd ON (op.observablepropertyid = pd.observablepropertyid)
                        LEFT OUTER JOIN probe_parameter AS pp ON (op.observablepropertyid = pp.parameter_id) AND (pd.observablepropertyid = pp.parameter_id)
                        RIGHT OUTER JOIN probe AS pro ON (pp.probe_id = pro.id AND foi.featureofinterestid = pro.pns_id)
                       WHERE foi.identifier = '", pnsData()[sPNS()[i],]$ID, "' AND s.firsttimestamp != '1970-01-01 00:00' AND pd.", colStoffgruppe, " IN ('", paste(elemGroup()$name[elemGroup()$name %in% input$selElemGroup], collapse="', '"), "')")

      op <- rbind(op, dbGetQuery(db, query))
    }
    op
  }, error = modalErrorHandler, finally = poolReturn(db))
})

output$obsPhen <- renderUI(selectInput("selObsPhen", "Parameterauswahl:",
                                       obsProp()$name, multiple = TRUE,
                                       selected = obsProp()$name[1]))

data <- reactive({
  if (!is.null(input$selObsPhen)) {

    resDf <- NULL

    phenValueCount <- 2
    postfix <- c("Wert", "Einheit")
    if (input$showBG) {
      postfix <- append(postfix, "BG")
      phenValueCount <-  phenValueCount + 1
    }
    if (input$showNG) {
      postfix <- append(postfix, "NG")
      phenValueCount <-  phenValueCount + 1
    }
    if (input$showSG) {
      postfix <- append(postfix, "Stoffgruppe")
      phenValueCount <-  phenValueCount + 1
    }
    columnCount <- (length(input$selObsPhen) * phenValueCount) + 2

    db <- connectToDB()
    tryCatch({
      colStoffgruppe <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")

      for (foi in pnsData()[sPNS(), "ID"]) {

        selObsPropFoi <- obsProp()[obsProp()$name %in% input$selObsPhen & obsProp()$foiid == foi,]

        if(length(selObsPropFoi$seriesid) == 0) next;

        obsPropSel <- obsProp()$name %in% input$selObsPhen

        uObsPropSelId <- unique(obsProp()[obsPropSel, "identifier"])

        query <- paste0("SELECT DISTINCT o.observationid,
                                         o.seriesid,
                                         u.unit,
                                         nv.value,
                                         op.identifier AS observableProperty,
                                         to_char(timezone('",  dbTimeZoneIdentifier,"', pro.resulttime::timestamptz), '", dbTimestampPattern, "') AS resulttime,
                                         pro.abfluss_situation,
                                         pp.bg,
                                         pp.ng,
                                         pd.", colStoffgruppe, " AS stgrname,
					 pro.id AS proid
                    FROM observation o
                        LEFT OUTER JOIN numericvalue nv ON (o.observationid = nv.observationid)
                        LEFT OUTER JOIN series AS s ON (o.seriesid = s.seriesid)
                        LEFT OUTER JOIN featureofinterest AS foi ON (s.featureofinterestid = foi.featureofinterestid)
                        LEFT OUTER JOIN observableproperty AS op ON (s.observablepropertyid = op.observablepropertyid)
                        LEFT OUTER JOIN parameter_data AS pd ON (op.observablepropertyid = pd.observablepropertyid)
                        LEFT OUTER JOIN probe_parameter AS pp ON (op.observablepropertyid = pp.parameter_id)
                        LEFT OUTER JOIN unit AS u ON (o.unitid = u.unitid)
                        RIGHT OUTER JOIN probe AS pro ON (pp.probe_id = pro.id AND foi.featureofinterestid = pro.pns_id AND timezone('UTC', pro.resulttime::timestamptz) = o.resulttime)
                        WHERE s.featureofinterestid IN (", paste0("'", selObsPropFoi$featureofinterestid, "'" , collapse=", "), ")" )

        if (!is.null(input$selObsPhen))
          query <- paste0(query, " AND op.name IN (", paste0("'", input$selObsPhen, "'" ,collapse=", ") ,")")

        res <- dbGetQuery(db, query)

        resDf <- NULL

        if(length(res) != 0) {
          for (obs in 1:nrow(res)) {

            oldRow <- 0
            if (res[obs, "proid"] %in% resDf$proId) {
              oldRow <- which(resDf$proId == res[obs, "proid"])
              resDfRow <- resDf[oldRow, , drop=F]
            } else {
              resDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = columnCount + 1))
              colnames(resDfRow) <- c("PNS_ID", "Probenahmedatum", as.vector(t(outer(uObsPropSelId, postfix, paste, sep="_"))), "proId")
              resDfRow$PNS_ID <- foi
              resDfRow$Probenahmedatum <- res[obs, "resulttime"]
              resDfRow$Abflusssituation <- res[obs, "abfluss_situation"]
              resDfRow$proId <- res[obs, "proid"]
            }

            valueRow <- paste(res[obs, "observableproperty"], "Wert", sep="_")
            if (res[obs, "value"] == noDataEncode) {
              resDfRow[valueRow] <- NA_character_
            } else if (res[obs, "value"] < ifelse(is.na(res[obs, "bg"]),-Inf,res[obs, "bg"])) {
              if (input$repBG == 'BG') {
                resDfRow[valueRow] <- res[obs, "bg"]
              } else if (input$repBG == 'BG/2') {
                resDfRow[valueRow] <- res[obs, "bg"]/2
              } else {
                resDfRow[valueRow] <- input$repBG
              }
            } else {
              value <- res[obs, "value"]
              resDfRow[valueRow] <- ifelse(value == noDataEncode, NA_character_, value)
            }

            resDfRow[paste(res[obs, "observableproperty"], "Einheit", sep="_")] <- ifelse(is.na(res[obs, "unit"]),"NA",res[obs, "unit"])
            if (tolower(resDfRow[paste(res[obs, "observableproperty"], "Einheit", sep="_")]) == "percent" ||
                tolower(resDfRow[paste(res[obs, "observableproperty"], "Einheit", sep="_")]) == "prozent") {
              resDfRow[paste(res[obs, "observableproperty"], "Einheit", sep="_")] <- "%"
            }
            if (input$showBG) {
              resDfRow[paste(res[obs, "observableproperty"], "BG", sep="_")] <- ifelse(is.na(res[obs, "bg"]),"NA",res[obs, "bg"])
            }
            if (input$showNG) {
              resDfRow[paste(res[obs, "observableproperty"], "NG", sep="_")] <- ifelse(is.na(res[obs, "ng"]),"NA",res[obs, "ng"])
            }
            if (input$showSG) {
              resDfRow[paste(res[obs, "observableproperty"], "Stoffgruppe", sep="_")] <- ifelse(is.na(res[obs, "stgrname"]),"NA",res[obs, "stgrname"])
            }

            if (oldRow > 0) {
              resDf[oldRow, ] <- resDfRow
            } else {
              resDf <- rbind(resDf, resDfRow)
            }
          }

        }

      }

      if (input$randomId) {
        dbIdMap <- dbGetQuery(db, paste0("SELECT f.identifier, od.rndid FROM ort_data od
                                         LEFT OUTER JOIN featureofinterest f ON od.featureofinterestid = f.featureofinterestid
                                         WHERE f.identifier IN ('", paste(resDf$PNS_ID, collapse="', '"), "')
                                         UNION
                                         SELECT f.identifier, pnsd.rndid FROM pns_data pnsd
                                         LEFT OUTER JOIN featureofinterest f ON pnsd.featureofinterestid = f.featureofinterestid
                                         WHERE f.identifier IN ('", paste(resDf$PNS_ID, collapse="', '"), "')"))
        resDf$PNS_ID <- dbIdMap[match(resDf$PNS_ID, dbIdMap$id), "rndid"]
      }

      list(resDf=resDf[,-(columnCount+1)])
    }, error = modalErrorHandler, finally = poolReturn(db))
  }
})

output$tableDaten <- renderDT({
  input$refreshData

  showTab <- isolate(data()[["resDf"]])

  if (!is.null(showTab)) {
    dt <- datatable(showTab,
                    filter="top",
                    rownames=FALSE,
                    options=list(paging=FALSE,dom = 'Brt',
                                 language=list(url = lngJSON)),
                    escape=FALSE)

    numCol <- colnames(showTab)
    numCol <- numCol[!(numCol %in% c('PNS_ID'))]
    numCol <- numCol[which(as.logical(sapply(showTab[,numCol],is.numeric)))]
    numCol <- numCol[apply(matrix(showTab[,numCol] > floor(showTab[,numCol])), 2, any)]
    numCol <- numCol[!is.na(numCol)]
    if (length(numCol) > 0) {
      dt <- formatRound(dt, numCol, digits=3, dec.mark=",", mark=".")
    }

    dt
  }
})

selData <- reactive({
  sr <- input$tableDaten_rows_selected
  if(is.null(sr)) {
    input$tableDaten_rows_all
  } else {
    sort(sr)
  }
})

output$tableStatistik <- renderDataTable({
  if (!is.null(selData()) & input$computeStat) {
    input$refreshData

    output$warnUnit <- renderText("Achtung: Messwerte eines Parameters können verschiedene Einheiten haben und werden nicht konvertiert.")

    filterData <- isolate(data()$resDf[selData(),])

    stat <- apply(filterData[,-c(1:2), drop=FALSE], 2, function(x) {
      xSum <- suppressWarnings(as.numeric(x))
      if (all(is.na(xSum))) {
        xSum <- c(rep(NA,7), length(levels(as.factor(x))))
      } else {
        xSum <- summary(xSum)
      }
      if (length(xSum) == 6)
        xSum <- c(xSum, 0, NA)
      if (length(xSum) == 7)
        xSum <- c(xSum, NA)

      xSum
    })

    rownames(stat) <- c("Min.","1. Qu.","Median","Mittelw.", "3. Qu.","Max.", "NA", "Anz. Fakt.")

    dt <- datatable(stat,
                    options=list(paging=FALSE, dom = 'Brt',
                                 language=list(url = lngJSON)))

    numCol <- colnames(stat)
    numCol <- numCol[which(apply(stat[1:6,numCol], 2, function(x) all(is.numeric(x)) ))]
    numCol <- numCol[apply(matrix(stat[1:6, numCol] > floor(stat[1:6, numCol])), 2, any)]
    numCol <- numCol[!is.na(numCol)]
    if (length(numCol) > 0)
      dt <- formatRound(dt, numCol, digits=3, dec.mark=",", mark=".")

    dt
  }
})

output$selTextDaten <- renderText({
  if (is.null(selData()))
    return(NULL)
  if (length(selData()) == 1) {
    paste("Zeile", selData(), "ist ausgewählt.")
  } else {
    paste("Zeilen", paste(selData(), collapse=", "), "sind ausgewählt.")
  }
})

output$exportDataCSVLatin1 <- downloadHandler(
  filename = function() paste("Daten-", Sys.Date(), ".csv", sep=""),
  content = function(file) {
    df <- isolate(data()$resDf[selData(),])

    write.table(df, file, sep = ";", dec = ",", na = "",
                fileEncoding = "Latin1", row.names = FALSE)
  }
)

output$exportDataCSVUtf8 <- downloadHandler(
  filename = function() paste("Daten-", Sys.Date(), ".csv", sep=""),
  content = function(file) {
    df <- isolate(data()$resDf[selData(),])

    write.table(df, file, sep = ";", dec = ",", na = "",
                fileEncoding = "UTF-8", row.names = FALSE)
  }
)

output$exportDataRData <- downloadHandler(
  filename = function() paste("Daten-", Sys.Date(), ".RData", sep=""),
  content = function(file) {
    df <- isolate(data()$resDf[selData(),])

    save(df, file = file)
  }
)
