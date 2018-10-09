## server Proben-Ansicht


db <- connectToDB()
tryCatch({

  # Alle PNS/Global Spaltennamen
  # pnsDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns', 'global')"))

  # Nur PNS Spaltename
  pnsDataPnsMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns')"))

  # Add Prefix zu Spaltennamen
  pnsColColumns <- paste0("pns.", grep("col*", pnsDataPnsMetaData$columnid, value = TRUE))

  # Query alle PNS
  probenPNS <- dbGetQuery(db, paste0("SELECT DISTINCT foi.name
                                      FROM featureofinterest foi
                      RIGHT OUTER JOIN pns_data pns ON foi.featureofinterestid = pns.featureofinterestid
                      RIGHT OUTER JOIN featurerelation fr ON foi.featureofinterestid = fr.childfeatureid
                      LEFT OUTER JOIN featureofinterest pfoi ON pfoi.featureofinterestid = fr.parentfeatureid
                      ORDER BY name"))
}, error = modalErrorHandler, finally = poolReturn(db))

output$probenPNSInput <- renderUI(selectInput("probenPNS", "Probenahmestelle:",
                                              probenPNS$name, multiple = TRUE,
                                              selected = probenPNS$name[1]))


# Alle (Misch-)Proben/Global Spaltennamen
allProben <- reactive({

  db <- connectToDB()
  tryCatch({
    # Alle Probe/Global Spaltennamen
    probeDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('probe', 'global')"))

    # qualify selectable columnids of table probe
    probeDataProbeMetaData <- dbGetQuery(db, paste0("SELECT columnId FROM column_metadata WHERE prefixid IN ('probe')"))
    excludeColumns <- c("resulttime", "phenomenontimestart", "phenomenontimeend")
    includeColumns <- probeDataProbeMetaData$columnid[!(probeDataProbeMetaData$columnid %in% excludeColumns)]
    probeDataProbeMetaData <- paste0("pro.", includeColumns)

    probeDataProbeMetaData <- c(paste0("to_char(",
                                       "timezone('", dbTimeZoneIdentifier, "', pro.resulttime::timestamptz) ",
                                       ", '", dbTimestampPattern, "'",
                                       ") AS resulttime"),
                                paste0("to_char(",
                                       "timezone('", dbTimeZoneIdentifier, "', pro.phenomenontimestart::timestamptz) ",
                                       ", '", dbTimestampPattern, "'",
                                       ") AS phenomenontimestart"),
                                paste0("to_char(",
                                       "timezone('", dbTimeZoneIdentifier, "', pro.phenomenontimeend::timestamptz) ",
                                       ", '", dbTimestampPattern, "'",
                                       ") AS phenomenontimeend"),
                                probeDataProbeMetaData)

    # Query alle Proben mit PNS-Identifier
    query <-paste0("SELECT pro.id, pro.identifier, pns.identifier as pns_id, ",
                   paste0(probeDataProbeMetaData, collapse=", "),
                   " FROM probe pro
                     LEFT OUTER JOIN featureofinterest pns ON pns.featureofinterestid = pro.pns_id
                     WHERE (pro.subprobe IS NULL OR pro.subprobe IN (''))")

    if (!is.null(input$probenPNS)) {
      slectedPNS <- input$probenPNS
      if (Sys.info()["sysname"] == "Windows") {
        slectedPNS <- stri_enc_tonative(input$probenPNS)
      }
      query <- paste0(query, " AND pns.name IN (", paste0("'", slectedPNS, "'" ,collapse=", ") ,")")
    }

    allPro <- dbGetQuery(db, query)

    if (nrow(allPro) > 0)
      colnames(allPro) <- probeDataMetaData$dede[match(colnames(allPro), probeDataMetaData$columnid)]

    allPro
  }, error = modalErrorHandler, finally = poolReturn(db))
})

output$tableProben  <- renderDT({
  showTab <- allProben()[,-1]

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

sProben <- reactive({
  sr <- input$tableProben_rows_selected
  if(is.null(sr)) {
    input$tableProben_rows_all
  } else {
    sort(sr)
  }
})

if(!is.null(allProben)) {
  output$exportProbeCSVLatin1 <- downloadHandler(
    filename = function() paste("Probe-", Sys.Date(), ".csv", sep=""),
    content = function(file) {
      df <- isolate(allProben()[sProben(), -1])
      write.table(df, file, sep = ";", dec=",", na = "",
                  fileEncoding = "Latin1", row.names = FALSE)
    }
  )

  output$exportProbeCSVUtf8 <- downloadHandler(
    filename = function() paste("Probe-", Sys.Date(), ".csv", sep=""),
    content = function(file) {
      df <- isolate(allProben()[sProben(), -1])
      write.table(df, file, sep = ";", dec=",", na = "",
                  fileEncoding = "UTF-8", row.names = FALSE)
    }
  )

  output$exportProbeRData <- downloadHandler(
    filename = function() paste("Probe-", Sys.Date(), ".RData", sep=""),
    content = function(file) {
      df <- isolate(allProben()[sProben(), -1])
      save(df, file = file)
    }
  )
}

observeEvent(input$fromProbenToTeilproben, {
  updateTabsetPanel(session, "inNavbarpage", selected = "Teilproben")
})

# Details zu Teilproben
allTeilproben <- reactive({

  db <- connectToDB()
  tryCatch({
    # Alle Probe/Global Spaltennamen
    probeDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('probe', 'global')"))

    # Nur Probe Spaltename
    probeDataProbeMetaData <- dbGetQuery(db, paste0("SELECT columnId FROM column_metadata WHERE prefixid IN ('probe')"))
    excludeColumns <- c("resulttime", "phenomenontimestart", "phenomenontimeend")
    includeColumns <- probeDataProbeMetaData$columnid[!(probeDataProbeMetaData$columnid %in% excludeColumns)]
    probeDataProbeMetaData <- paste0("pro.", includeColumns)
    
    probeDataProbeMetaData <- c(paste0("to_char(",
                                       "timezone('", dbTimeZoneIdentifier, "', pro.resulttime::timestamptz) ",
                                       ", '", dbTimestampPattern, "'",
                                       ") AS resulttime"),
                                paste0("to_char(",
                                       "timezone('", dbTimeZoneIdentifier, "', pro.phenomenontimestart::timestamptz) ",
                                       ", '", dbTimestampPattern, "'",
                                       ") AS phenomenontimestart"),
                                paste0("to_char(",
                                       "timezone('", dbTimeZoneIdentifier, "', pro.phenomenontimeend::timestamptz) ",
                                       ", '", dbTimestampPattern, "'",
                                       ") AS phenomenontimeend"),
                                probeDataProbeMetaData)
    
    # Query alle Teilproben mit PNS-Identifier und zugehöroger Mischprobe
    query <-paste0("SELECT pro.id, pro.identifier, pns.identifier as pns_id, pro.subprobe, ",
                   paste0(probeDataProbeMetaData, collapse=", "),
                   " FROM probe pro
                   LEFT OUTER JOIN featureofinterest pns ON pns.featureofinterestid = pro.pns_id
                   WHERE pro.subprobe in ('",
                   paste0(allProben()[sProben(),"ID"], collapse="', '"),
                   "')")

    if (!is.null(input$probenPNS)) {
      slectedPNS <- input$probenPNS
      if (Sys.info()["sysname"] == "Windows") {
        slectedPNS <- stri_enc_tonative(input$probenPNS)
      }
      query <- paste0(query, " AND pns.name IN (", paste0("'", slectedPNS, "'" ,collapse=", ") ,")")
    }

    cat(query)

    allPro <- dbGetQuery(db, query)

    if (nrow(allPro) > 0)
      colnames(allPro) <- probeDataMetaData$dede[match(colnames(allPro), probeDataMetaData$columnid)]

    allPro
  }, error = modalErrorHandler, finally = poolReturn(db))
})

output$tableTeilproben  <- renderDT({
  showTab <- allTeilproben()[,-1]

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


sTeilproben <- reactive({
  sr <- input$tableTeilproben_rows_selected
  if(is.null(sr)) {
    input$tableTeilproben_rows_all
  } else {
    sort(sr)
  }
})

observeEvent(input$fromTeilprobenToProben, {
  updateTabsetPanel(session, "inNavbarpage", selected = "Proben")
})

if(!is.null(allProben)) {
  output$exportTeilprobeCSVLatin1 <- downloadHandler(
    filename = function() paste("Teilprobe-", Sys.Date(), ".csv", sep=""),
    content = function(file) {
      df <- isolate(allTeilproben()[sTeilproben(), -1])
      write.table(df, file, sep = ";", dec=",", na = "",
                  fileEncoding = "Latin1", row.names = FALSE)
    }
  )

  output$exportTeilprobeCSVUtf8 <- downloadHandler(
    filename = function() paste("Teilprobe-", Sys.Date(), ".csv", sep=""),
    content = function(file) {
      df <- isolate(allTeilproben()[sTeilproben(), -1])
      write.table(df, file, sep = ";", dec=",", na = "",
                  fileEncoding = "UTF-8", row.names = FALSE)
    }
  )

  output$exportTeilprobeRData <- downloadHandler(
    filename = function() paste("Teilprobe-", Sys.Date(), ".RData", sep=""),
    content = function(file) {
      df <- isolate(allTeilproben()[sTeilproben(), -1])
      save(df, file = file)
    }
  )
}
