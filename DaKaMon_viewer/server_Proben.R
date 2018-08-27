## server Proben-Ansicht 


db <- connectToDB()

# Alle PNS/Global Spaltennamen
# pnsDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns', 'global')"))

# Nur PNS Spaltename
pnsDataPnsMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns')"))

# Add Prefix zu Spaltennamen
pnsColColumns <- paste0("pns.", grep("col*", pnsDataPnsMetaData$columnid, value = TRUE))

# Query alle PNS TODO: Namen genügen
probenPNS <- dbGetQuery(db, paste0("SELECT foi.featureofinterestid, foi.identifier, foi.name, pfoi.identifier as orts_id, ", paste0(pnsColColumns, collapse=", "),
                                   " FROM featureofinterest foi
                    RIGHT OUTER JOIN pns_data pns ON foi.featureofinterestid = pns.featureofinterestid
                    RIGHT OUTER JOIN featurerelation fr ON foi.featureofinterestid = fr.childfeatureid
                    LEFT OUTER JOIN featureofinterest pfoi ON pfoi.featureofinterestid = fr.parentfeatureid"))
dbDisconnect(db)

output$probenPNSInput <- renderUI(selectInput("probenPNS", "Probenahmestelle:", 
                                              probenPNS$name, multiple = TRUE, 
                                              selected = probenPNS$name[1]))


# Alle Proben/Global Spaltennamen
allProben <- reactive({
  
    db <- connectToDB()

    # Alle Probe/Global Spaltennamen
    probeDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('probe', 'global')"))
    
    # Nur Probe Spaltename
    probeDataProbeMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('probe')"))
    
    # Add Prefix zu Spaltennamen
    probeColColumns <- paste0("pro.", grep("col*", probeDataProbeMetaData$columnid, value = TRUE))
    
    # Query alle Proben mit PNS-Identifier TODO: Filter PNS aus input$probenPNS
    query <-paste0("SELECT pro.id, pro.identifier, pns.identifier as pns_id, ", paste0(probeColColumns, collapse=", "), 
                   " FROM probe pro 
                   LEFT OUTER JOIN featureofinterest pns ON pns.featureofinterestid = pro.pns_id") # FIXME: in meiner tAbelle taucht nur col047 für pns_id auf ..?
    
    if (!is.null(input$probenPNS)) {
      slectedPNS <- input$probenPNS
      if (Sys.info()["sysname"] == "Windows") {
        slectedPNS <- stri_enc_tonative(input$probenPNS)
      }
      query <- paste0(query, " WHERE pns.name IN (", paste0("'", slectedPNS, "'" ,collapse=", ") ,")")
    }

    allPro <- dbGetQuery(db, query)
 
    dbDisconnect(db)
    
    if (nrow(allPro) > 0)
      colnames(allPro) <- probeDataMetaData$dede[match(colnames(allPro), probeDataMetaData$columnid)]
    
    allPro 
})

output$tableProben  <- renderDT({
  showTab <- allProben()[,-1]
  
  # showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
  # 
  # showHead <- paste0(showHead, "</span>")
  
  datatable(showTab,
            filter="top",
            options = list(paging=FALSE, dom = 'Brt',
                           language=list(url = lngJSON)),
            escape=FALSE)})
