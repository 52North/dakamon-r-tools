## server Literaturdaten

## Parameter
db <- connectToDB()

# load all Literatur Parameter from DB
litParamDf <- dbGetQuery(db, "SELECT DISTINCT parameter FROM literatur")
output$litParamInput <- renderUI(selectInput("litPar", "Parameter",
                                             litParamDf$parameter, 
                                             multiple = TRUE))

dbDisconnect(db)

# TODO: load all Literaturwerte containgn the parameter selection input$litPar

## Thematik
db <- connectToDB()

# load all Literatur Thematiken from DB
litThematikDf <- dbGetQuery(db, "SELECT DISTINCT thematik FROM literatur")
output$litThematikInput <- renderUI(selectInput("litThematik", "Thematik",
                                             litParamDf$theme, 
                                             multiple = TRUE))

dbDisconnect(db)

# TODO: load all Literaturwerte containgn the parameter selection input$litThematik

## Publikation # z.B. alle Werte, die in Kaiser2012a ermittelt wurden, pubId wird vom KIT erstellt und mit csv hoch geladen
db <- connectToDB()

# load all Literatur PubId from DB
litPubIdDf <- dbGetQuery(db, "SELECT DISTINCT publikation_id FROM literatur")
output$litPubIdInput <- renderUI(selectInput("litPubId", "Publikation",
                                             litPubIdDf$pubId, 
                                             multiple = TRUE))

dbDisconnect(db)

# TODO: load all Literaturwerte containing the pubId selection input$litPubId
