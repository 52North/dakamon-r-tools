## server Proben-Ansicht 

probenPNS <- reactive({
  db <- connectToDB()
  
  # Alle PNS/Global Spaltennamen
  # pnsDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns', 'global')"))
  
  # Nur PNS Spaltename
  pnsDataPnsMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns')"))
  
  # Add Prefix zu Spaltennamen
  pnsColColumns <- paste0("pns.", grep("col*", pnsDataPnsMetaData$columnid, value = TRUE))
  
  # Query alle PNS TODO: Namen genügen
  res <- dbGetQuery(db, paste0("SELECT foi.featureofinterestid, foi.identifier, foi.name, pfoi.identifier as orts_id, ", paste0(pnsColColumns, collapse=", "),
                        " FROM featureofinterest foi
                      RIGHT OUTER JOIN pns_data pns ON foi.featureofinterestid = pns.featureofinterestid
                      RIGHT OUTER JOIN featurerelation fr ON foi.featureofinterestid = fr.childfeatureid
                      LEFT OUTER JOIN featureofinterest pfoi ON pfoi.featureofinterestid = fr.parentfeatureid"))
  dbDisconnect(db)
  
  res
})

output$probenPNSInput <- renderUI(selectInput("probenPNS", "Probenahmestelle:", 
                                              probenPNS()$name, multiple = TRUE, 
                                              selected = probenPNS()$name[1]))


# Alle Proben/Global Spaltennamen
# paramDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('param', 'global')"))

db <- connectToDB()

# Alle Probe/Global Spaltennamen
# probeDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('probe', 'global')"))

# Nur Probe Spaltename
probeDataProbeMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('probe')"))

# Add Prefix zu Spaltennamen
probeColColumns <- paste0("pro.", grep("col*", probeDataProbeMetaData$columnid, value = TRUE))

# Query alle Proben mit PNS-Identifier TODO: Filter PNS aus input$probenPNS
allProben <- dbGetQuery(db, paste0("SELECT pro.id, pro.identifier, pns.identifier as pns_id, ", paste0(probeColColumns, collapse=", "), 
                                   "FROM probe pro
                                    LEFT OUTER JOIN featureofinterest pns ON pns.featureofinterestid = pro.pns_id"))

output$tableProben  <- renderDT(datatable(allProben,
                                          filter="top",
                                          options = list(paging=FALSE, dom = 'Brt',
                                                         language=list(url = lngJSON)),
                                          escape=FALSE))
