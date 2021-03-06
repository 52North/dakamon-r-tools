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
## server Parameteransicht


db <- connectToDB()
tryCatch({
  res <- NULL
  col <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
  if (nrow(col != 0)) {
    res <- dbGetQuery(db, paste0("SELECT DISTINCT ", col, " as name FROM parameter_data
                                 WHERE ", col, " IS NOT NULL ORDER BY name"))
  }
}, error = modalErrorHandler, finally = poolReturn(db))

output$paramElemGroupInput <- renderUI(selectInput("paramElemGroup", "Stoffgruppenauswahl:",
                                                   res$name, multiple = TRUE,
                                                   selected = res$name[1]))


allParameter<- reactive({
    db <- connectToDB()
    tryCatch({
      # Alle Parameter/Global Spaltennamen
      paramDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('param', 'global')"))

      # Nur Parameter Spaltename
      paramDataParamMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('param')"))

      # Add Prefix zu Spaltennamen
      paramColColumns <- paste0("param.", grep("col*", paramDataParamMetaData$columnid, value = TRUE))


      # Query alle Parameter:
      query <- paste0("SELECT op.observablepropertyid, op.identifier, op.name, ", paste0(paramColColumns, collapse=", "),
                         " FROM observableproperty op
                                              RIGHT OUTER JOIN parameter_data param ON op.observablepropertyid = param.observablepropertyid")

      if (!is.null(input$paramElemGroup))
        query <- paste0(query, " WHERE param.", col, " IN (", paste0("'", input$paramElemGroup, "'" ,collapse=", ") ,")")

      allParameter <- dbGetQuery(db, query)

      if (nrow(allParameter) > 0)
        colnames(allParameter) <- paramDataMetaData$dede[match(colnames(allParameter), paramDataMetaData$columnid)]

      allParameter
    }, error = modalErrorHandler, finally = poolReturn(db))
})

output$tableParameter  <- renderDT({
  showTab <- allParameter()[,-1]

  showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

  showHead <- paste0(showHead, "</span>")

  dt <- datatable(showTab,
                  filter="top",
                  options = list(paging=FALSE, dom = 'Brt',
                                 language=list(url = lngJSON)),
                  escape=FALSE)

  numCol <- colnames(showTab)
  numCol <- numCol[which(as.logical(sapply(showTab[,numCol],is.numeric)))]
  numCol <- numCol[apply(showTab[,numCol] > floor(showTab[,numCol]), 2, any)]
  numCol <- numCol[!is.na(numCol)]
  if (length(numCol) > 0)
    dt <- formatRound(dt, numCol, digits=3, dec.mark=",", mark=".")
  dt
})

sParam <- reactive({
  sr <- input$tableParameter_rows_selected
  if(is.null(sr)) {
    input$tableParameter_rows_all
  } else {
    sort(sr)
  }
})

output$exportParCSVLatin1 <- downloadHandler(
  filename = function() paste("Parameter-", Sys.Date(), ".csv", sep=""),
  content = function(file) {
    df <- isolate(allParameter()[sParam(), -1])
    write.table(df, file, sep = ";", dec=",", na = "",
                fileEncoding = "Latin1", row.names = FALSE)
  }
)

output$exportParCSVUtf8 <- downloadHandler(
  filename = function() paste("Parameter-", Sys.Date(), ".csv", sep=""),
  content = function(file) {
    df <- isolate(allParameter()[sParam(), -1])
    write.table(df, file, sep = ";", dec=",", na = "",
                fileEncoding = "UTF-8", row.names = FALSE)
  }
)

output$exportParRData <- downloadHandler(
  filename = function() paste("Parameter-", Sys.Date(), ".RData", sep=""),
  content = function(file) {
    df <- isolate(allParameter()[sParam(), -1])
    save(df, file = file)
  }
)
