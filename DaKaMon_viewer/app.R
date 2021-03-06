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
# ui - DaKaMon viewer
library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(rjson)
library(RPostgreSQL)
library(stringi)
library(pool)

source("conf.R", local = TRUE, encoding = "UTF-8")$value

## tools
pool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = dbName,
  host = dbHost,
  user = dbUser,
  password = dbPassword,
  port = dbPort,
  minSize = 3,
  maxSize = 10,
  idleTimeout = 30000 # 30sec
)

connectToDB <- function() {
  db <- poolCheckout(pool)
  if (Sys.info()["sysname"] == "Windows") {
    dbSendQuery(db, "set client_encoding='windows-1252'")
  }
  db
}

## /tools

ui <-  navbarPage("Datenansicht", id="inNavbarpage",
                  ################# #
                  #### Messdaten ####
                  ################# #
                  navbarMenu("Messdaten",
                             ########### #
                             #### Ort ####
                             ########### #
                             tabPanel("Ort(e) auswählen",
                                      uiOutput("ewsSelInput"),
                                      DTOutput('tableOrt'),
                                      conditionalPanel(
                                        "$('#tableOrt').hasClass('recalculating')",
                                        tags$div('Lade ... ')
                                      ),
                                      textOutput("selTextOrt"),
                                      downloadButton("exportOrtCSVLatin1", "Export als csv-Datei (Latin1)."),
                                      downloadButton("exportOrtCSVUtf8", "Export als csv-Datei (UTF-8)."),
                                      downloadButton("exportOrtRData", "Export als RData-Datei."),
                                      br(),
                                      actionButton("fromOrtToPNS", "Weiter...")),
                             ########### #
                             #### PNS ####
                             ########### #
                             tabPanel(
                               "Probenahmestelle(n) auswählen",
                               DTOutput('tablePNS'),
                               conditionalPanel(
                                 "$('#tablePNS').hasClass('recalculating')",
                                 tags$div('Lade ... ')
                               ),
                               textOutput("selTextPNS"),
                               downloadButton("exportPNSCSVLatin1", "Export als csv-Datei (Latin1)."),
                               downloadButton("exportPNSCSVUtf8", "Export als csv-Datei (UTF-8)."),
                               downloadButton("exportPNSRData", "Export als RData-Datei."),
                               br(),
                               actionButton("fromPNStoMessdaten", "Weiter...")),
                             ########################## #
                             #### Messdaten anzeigen ####
                             ########################## #
                             tabPanel("Messdaten anzeigen",
                                      sidebarLayout(
                                        sidebarPanel(
                                          uiOutput("elemGroup"),
                                          uiOutput("obsPhen"),
                                          checkboxInput("showBG",
                                                        label = "Lade BG-Wert",
                                                        value = FALSE),
                                          checkboxInput("showNG",
                                                        label = "Lade NG-Wert",
                                                        value = FALSE),
                                          checkboxInput("showSG",
                                                        label = "Lade Stoffgruppe",
                                                        value = FALSE),
                                          radioButtons("repBG",
                                                       label = "Ersetze Werte unterhalb der Bestimmungsgrenze durch:",
                                                       choices = c("'BG'", "0", "BG/2", "BG"),
                                                       selected = "'BG'", inline=TRUE),
                                          checkboxInput("randomId",
                                                        label = "Anonymisiere IDs",
                                                        value = FALSE),
                                          actionButton("refreshData", "Lade Daten aus der DB."),
                                          checkboxInput("computeStat",
                                                        label = "Berechne Statistiken",
                                                        value = FALSE),
                                          textOutput("warnUnit"),
                                          downloadButton("exportDataCSVLatin1", "Export als csv-Datei (Latin1)."),
                                          downloadButton("exportDataCSVUtf8", "Export als csv-Datei (UTF-8)."),
                                          downloadButton("exportDataRData", "Export als RData-Datei."),
                                          width = 2),
                                        mainPanel(
                                          DTOutput('tableDaten'),
                                          conditionalPanel(
                                            "$('#tableDaten').hasClass('recalculating')",
                                            tags$div('Lade ... ')
                                          ),
                                          textOutput("selTextDaten"),
                                          br(),
                                          DTOutput('tableStatistik'),
                                          conditionalPanel(
                                            "$('#tableStat').hasClass('recalculating')",
                                            tags$div('Berechne ... ')
                                          ))

                                      ))),
                  ########################## #
                  #### Parameter anzeigen ####
                  ########################## #
                  tabPanel("Parameter anzeigen",
                           uiOutput("paramElemGroupInput"),
                           DTOutput("tableParameter"),
                           downloadButton("exportParCSVLatin1", "Export als csv-Datei (Latin1)."),
                           downloadButton("exportParCSVUtf8", "Export als csv-Datei (UTF-8)."),
                           downloadButton("exportParRData", "Export als RData-Datei.")),
                  ####################### #
                  #### Proben anzeigen ####
                  ####################### #
                  navbarMenu("Proben anzeigen",
                             tabPanel("Proben",
                                      uiOutput("probenPNSInput"),
                                      DTOutput("tableProben"),
                                      downloadButton("exportProbeCSVLatin1", "Export als csv-Datei (Latin1)."),
                                      downloadButton("exportProbeCSVUtf8", "Export als csv-Datei (UTF-8)."),
                                      downloadButton("exportProbeRData", "Export als RData-Datei."),
                                      br(),
                                      actionButton("fromProbenToTeilproben", "Details zu Mischproben ...")),
                             tabPanel("Teilproben",
                                      DTOutput("tableTeilproben"),
                                      downloadButton("exportTeilprobeCSVLatin1", "Export als csv-Datei (Latin1)."),
                                      downloadButton("exportTeilprobeCSVUtf8", "Export als csv-Datei (UTF-8)."),
                                      downloadButton("exportTeilprobeRData", "Export als RData-Datei."),
                                      br(),
                                      actionButton("fromTeilprobenToProben", "Zurück zur Probenauswahl ..."))),
                  ###################### #
                  #### Literaturdaten ####
                  ###################### #
                  navbarMenu("Literaturdaten",
                             tabPanel("Parameter",
                                      uiOutput("litParamInput"),
                                      DTOutput("tableLitParam"),
                                      br(),
                                      actionButton("fromParamtoThematik", "Weiter...")),
                             tabPanel("Thematik", # Thematik (vorherEntwässerungssystem) (Gewässer, Kläranlagen, etc.)
                                      uiOutput("litThematikInput"),
                                      DTOutput("tableLitThematik"),
                                      br(),
                                      actionButton("fromThematikToPub", "Weiter...")),
                             tabPanel("Publikation", # pubId
                                      uiOutput("litPubIdInput"),
                                      DTOutput("tableLitPubId"),
                                      actionButton("fromPubToLit", "Weiter...")),
                             tabPanel("Literatur", # literatur
                                      uiOutput("litInput"),
                                      DTOutput("tableLit"),
                                      downloadButton("exportLitCSVLatin1", "Export als csv-Datei (Latin1)."),
                                      downloadButton("exportLitCSVUtf8", "Export als csv-Datei (UTF-8)."),
                                      downloadButton("exportLitRData", "Export als RData-Datei."))),
                  ################# #
                  #### Dokumente ####
                  ################# #
                  navbarMenu("Dokumente",
                             tabPanel("Ort",
                                      DTOutput("documentTableOrt"),
                                      br()),
                             tabPanel("Literatur",
                                      DTOutput("documentTableLiteratur"),
                                      br()))
)

server <- function(input, output, session) {

  source("server_Messdaten.R", local = TRUE, encoding = "UTF-8")$value

  source("server_Parameter.R", local = TRUE, encoding = "UTF-8")$value

  source("server_Proben.R", local = TRUE, encoding = "UTF-8")$value

  source("server_Literaturdaten.R", local = TRUE, encoding = "UTF-8")$value

  source("server_Dokumente.R", local = TRUE, encoding = "UTF-8")$value
}

modalErrorHandler <- function(e) {
  if (!is.null(e)) {
    print(e)
  }
  showModalMessage(title="Fehler", e["message"])
}

showModalMessage <- function(..., title="Title") {
  showModal(modalDialog(..., title = title, footer = modalButton("Ok")))
}

shinyApp(ui, server,
         onStart = function() {
           cat("Starting Application ...")
           # Ensure the DB pool closes all connections
           onStop(function() {
             cat("Closing DB connection pool")
             poolClose(pool)
           })
         })
