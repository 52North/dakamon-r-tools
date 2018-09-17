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


# Alle Proben/Global Spaltennamen
allProben <- reactive({
  
  db <- connectToDB()
  tryCatch({
    # Alle Probe/Global Spaltennamen
    probeDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('probe', 'global')"))
    
    # Nur Probe Spaltename
    probeDataProbeMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('probe')"))
    
    # Add Prefix zu Spaltennamen
    colNumId <- grep("col*", probeDataProbeMetaData$columnid, value = F)
    probeDataProbeMetaData$columnid[colNumId] <- paste0("pro.", probeDataProbeMetaData$columnid[colNumId])
    
    
    # Query alle Proben mit PNS-Identifier
    query <-paste0("SELECT pro.id, pro.identifier, pns.identifier as pns_id, ",
                   paste0(probeDataProbeMetaData$columnid, collapse=", "),
                   " FROM probe pro
                     LEFT OUTER JOIN featureofinterest pns ON pns.featureofinterestid = pro.pns_id")
    
    if (!is.null(input$probenPNS)) {
      slectedPNS <- input$probenPNS
      if (Sys.info()["sysname"] == "Windows") {
        slectedPNS <- stri_enc_tonative(input$probenPNS)
      }
      query <- paste0(query, " WHERE pns.name IN (", paste0("'", slectedPNS, "'" ,collapse=", ") ,")")
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

if(!is.null(allProben)) {
  output$exportProbeCSV <- downloadHandler(
    filename = function() {
      paste("Probe-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(isolate(allProben()), file, sep = ";", dec=",",
                  fileEncoding = "UTF-8", row.names = FALSE)
    }
  )
  
  output$exportProbeRData <- downloadHandler(
    filename = function() {
      paste("Probe-", Sys.Date(), ".RData", sep="")
    },
    content = function(file) {
      df <- isolate(allProben())
      save(df, file = file)
    }
  )
}

