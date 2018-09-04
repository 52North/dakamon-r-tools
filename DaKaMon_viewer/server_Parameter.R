## server Parameteransicht


db <- connectToDB()

res <- NULL
col <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
if (nrow(col != 0)) {
  res <- dbGetQuery(db, paste0("SELECT DISTINCT ", col, " as name FROM parameter_data
                               WHERE ", col, " IS NOT NULL ORDER BY name"))
}
dbDisconnect(db)


output$paramElemGroupInput <- renderUI(selectInput("paramElemGroup", "Stoffgruppenauswahl:", 
                                                   res$name, multiple = TRUE, 
                                                   selected = res$name[1]))


allParameter<- reactive({
    db <- connectToDB()
    
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
    
    dbDisconnect(db)
    allParameter
})



output$tableParameter  <- renderDT({
    showTab <- allParameter()[,-1]
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showHead <- paste0(showHead, "</span>")
    
    datatable(showTab,
                                   filter="top",
                                   options = list(paging=FALSE, dom = 'Brt',
                                                  language=list(url = lngJSON)),
                                   escape=FALSE)})
