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
## server Literaturdaten

## Parameter
db <- connectToDB()
tryCatch({
  # load all Literatur Parameter from DB
  litParamDf <- dbGetQuery(db, "SELECT DISTINCT op.identifier as param_id
                                FROM literatur AS lit
                                LEFT OUTER JOIN observableproperty AS op ON (op.observablepropertyid = lit.param_id)")
  output$litParamInput <- renderUI(selectInput("litPar", "Parameter",
                                               litParamDf$param_id,
                                               multiple = TRUE))
}, error = modalErrorHandler, finally = poolReturn(db))

observeEvent(input$fromParamtoThematik, {
  updateTabsetPanel(session, "inNavbarpage",selected = "Thematik")
})

#####################
######  TAB 2  ######
#####################
litThematikDf<- reactive({

  ## Thematik
  db <- connectToDB()
  tryCatch({
    # load all Literatur Thematiken from DB
    query <-  paste0("SELECT DISTINCT lit.thematik
                                FROM literatur AS lit
                                LEFT OUTER JOIN observableproperty AS op ON (op.observablepropertyid = lit.param_id)")

    if (!is.null(input$litPar))
      query <- paste0(query," WHERE op.identifier in (", paste0("'", input$litPar, "'" , collapse=", ") ,")")

    litThematikDf <- dbGetQuery(db, query)
    litThematikDf
  }, error = modalErrorHandler, finally = poolReturn(db))
})

output$litThematikInput <- renderUI(selectInput("litThematik", "Thematik",
                                            litThematikDf()$thematik,
                                             multiple = TRUE))

observeEvent(input$fromThematikToPub, {
  updateTabsetPanel(session, "inNavbarpage",selected = "Publikation")
})

#####################
######  TAB 3  ######
#####################

litPubIdDf<- reactive({
  ## Publikation # z.B. alle Werte, die in Kaiser2012a ermittelt wurden, pubId wird vom KIT erstellt und mit csv hoch geladen
  db <- connectToDB()
  tryCatch({
    query <-  paste0("SELECT DISTINCT pub.id
                                FROM literatur AS lit
                     LEFT OUTER JOIN observableproperty AS op ON (op.observablepropertyid = lit.param_id)
                     LEFT OUTER JOIN referenz AS pub ON (pub.id = lit.referenz_id) ")

    if (!is.null(input$litPar))
      query <- paste0(query," WHERE op.identifier IN (", paste0("'", input$litPar, "'" , collapse=", ") ,")")

    if (!is.null(input$litThematik)) {
      if (is.null(input$litPar)) {
        query <- paste0(query," WHERE ")
      } else {
        query <- paste0(query," AND ")
      }
      query <- paste0(query," lit.thematik IN (", paste0("'", input$litThematik, "'" , collapse=", ") ,")")
    }

    # load all Literatur PubId from DB
    litPubIdDf <- dbGetQuery(db, query)
    litPubIdDf

  }, error = modalErrorHandler, finally = poolReturn(db))
})

output$litPubIdInput <- renderUI(selectInput("litPubId", "Publikation",
                                             litPubIdDf()$pub_id,
                                             multiple = TRUE))
observeEvent(input$fromPubToLit, {
  updateTabsetPanel(session, "inNavbarpage",selected = "Literatur")
})

#####################
######  TAB 4  ######
#####################
litDf<- reactive({
  db <- connectToDB()
  tryCatch({

    # Alle Pub/lit/Global Spaltennamen
    pubLitMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pub', 'lit', 'global')"))

    # Nur Pub and Lit Spaltename
    pubPubMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pub')"))
    LitLitMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('lit')"))

    # Add Prefix zu Spaltennamen
    litColColumns <- grep("col*", LitLitMetaData$columnid, value = TRUE)
    pubColColumns <- grep("col*", pubPubMetaData$columnid, value = TRUE)
    qLitColColumns <- ifelse(length(litColColumns) > 0, paste0("lit.", litColColumns), NA_character_)
    qPubColColumns <- ifelse(length(pubColColumns) > 0, paste0("pub.", pubColColumns), NA_character_)

    query <-  paste0("SELECT DISTINCT lit.id, pub.id, lit.thematik, op.identifier As param_id, foi.identifier As pns_id",
                     ifelse(is.na(qLitColColumns), "", paste0(",", paste0(qLitColColumns, collapse=", "))),
                     ifelse(is.na(qPubColColumns), "", paste0(",", paste0(qPubColColumns, collapse=", "))),
                                " FROM literatur AS lit
                     LEFT OUTER JOIN observableproperty AS op ON (op.observablepropertyid = lit.param_id)
                     LEFT OUTER JOIN referenz AS pub ON (pub.id = lit.referenz_id)
                     LEFT OUTER JOIN featureofinterest AS foi ON (foi.featureofinterestid = lit.pns_id)")

    if (!is.null(input$litPar))
      query <- paste0(query," WHERE op.identifier IN (", paste0("'", input$litPar, "'" , collapse=", ") ,")")

    if (!is.null(input$litThematik)) {
      if (is.null(input$litPar)) {
        query <- paste0(query," WHERE ")
      } else {
        query <- paste0(query," AND ")
      }
      query <- paste0(query," lit.thematik IN (", paste0("'", input$litThematik, "'" , collapse=", ") ,")")
    }

    if (!is.null(input$litPubId)) {
      if (is.null(input$litPar) & !is.null(input$litThematik)) {
        query <- paste0(query," WHERE ")
      } else {
        query <- paste0(query," AND ")
      }
      query <- paste0(query," pub.id IN (", paste0("'", input$litPubId, "'" , collapse=", ") ,")")
    }

    # load all Literatur PubId from DB
    litDf <- dbGetQuery(db, query)

    if (nrow(litDf) > 0)
      colnames(litDf) <- pubLitMetaData$dede[match(colnames(litDf), pubLitMetaData$columnid)]

    litDf
  }, error = modalErrorHandler, finally = poolReturn(db))
})

output$tableLit  <- renderDT({
  showTab <- litDf()[,-1]

  showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

  showHead <- paste0(showHead, "</span>")

  datatable(showTab,
            filter="top",
            options = list(paging=FALSE, dom = 'Brt',
                           language=list(url = lngJSON)),
            escape=FALSE)})
