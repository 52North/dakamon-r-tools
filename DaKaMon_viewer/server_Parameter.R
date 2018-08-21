## server Parameteransicht
paramElemGroup <- reactive({
  db <- connectToDB()
  
  col <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
  res <- dbGetQuery(db, paste0("SELECT DISTINCT ", col, " as name FROM parameter_data
                               WHERE ", col, " IS NOT NULL"))
  dbDisconnect(db)
  
  res
})

output$paramElemGroupInput <- renderUI(selectInput("paramElemGroup", "Stoffgruppenauswahl:", 
                                                   paramElemGroup()$name, multiple = TRUE, 
                                                   selected = paramElemGroup()$name[1]))

# Alle Parameter/Global Spaltennamen
# paramDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('param', 'global')"))

db <- connectToDB()

# Nur Parameter Spaltename
  paramDataParamMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('param')"))

# Add Prefix zu Spaltennamen
  paramColColumns <- paste0("param.", grep("col*", paramDataParamMetaData$columnid, value = TRUE))


# Query alle Parameter: TODO: filter auf Stoffgruppe aus input$paramElemGroup
  allParameter <- dbGetQuery(db, paste0("SELECT op.observablepropertyid, op.identifier, op.name, ", paste0(paramColColumns, collapse=", "),
                                        " FROM observableproperty op
                                    RIGHT OUTER JOIN parameter_data param ON op.observablepropertyid = param.observablepropertyid"))


output$tableParameter  <- renderDT(datatable(allParameter,
                                             filter="top",
                                             options = list(paging=FALSE, dom = 'Brt',
                                                            language=list(url = lngJSON)),
                                             escape=FALSE))

