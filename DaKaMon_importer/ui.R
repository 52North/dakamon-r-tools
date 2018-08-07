# UI DaKaMon import
library(shiny)
library(DT)
library(shinyjs)
library(httr)

if(!require(readr)) {
  install.packages("readr", quiet=TRUE)
  library(readr)
}


# common constants:
# column Separator
colSep <- ";"
# decimal separator
decSep <- ","
# encoding
csvEncode <- "UTF-8"

SOSWebApp <- "http://localhost:8080/52n-sos-webapp/"
verbose <- TRUE
BGencode <- 0
BGchar <- "BG"
NGencode <- -1
NGchar <- "NG"

csvInfo <- paste0("Die CSV-Datei muss \"", colSep, "\" als Spaltentrennzeichen und \"", decSep, "\" als Dezimaltrennzeichen verwenden und in \"", csvEncode, "\" enkodiert sein.")

ui <- navbarPage("Datenimport",
                 # useShinyjs(),
                 navbarMenu("Messdaten",
                            ## Ort
                            tabPanel("Ort anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # textInput("sepOrt", "Spaltentrennzeichen:", value = ";", width = "80%"),
                                         # textInput("decOrt", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                                         fileInput("csvFileOrt", "CSV-Datei mit Orten", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owOrt", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("OrtValidationOut"),
                                         uiOutput("OrtDBConsistencyTxtOut"),
                                         uiOutput("OrtDBConsistencyActionOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tableOrt'))
                                     )),
                            
                            ## Probenahmestelle (PNS)
                            tabPanel("Probenahmestelle anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # textInput("sepPNS", "Spaltentrennzeichen:", value = ";", width = "80%"),
                                         # textInput("decPNS", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                                         fileInput("csvFilePNS", "CSV-Datei mit Probenahmestellen", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,                                           
                                         checkboxInput("owPNS", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("PNSValidationOut"),
                                         uiOutput("PNSDBConsistencyTxtOut"),
                                         uiOutput("PNSDBConsistencyActionOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tablePNS'))
                                     )),
                            
                            ## Parameter (PRM)
                            tabPanel("Parameter anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # textInput("sepPRM", "Spaltentrennzeichen:", value = ";", width = "80%"),
                                         # textInput("decPRM", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                                         fileInput("csvFilePRM", "CSV-Datei mit Parametern", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,                     
                                         checkboxInput("owPRM", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("PRMValidationOut"),
                                         uiOutput("PRMDBConsistencyTxtOut"),
                                         uiOutput("PRMDBConsistencyActionOut"),
                                         width = 2),
                                       mainPanel(dataTableOutput('tablePRM'))
                                     )),
                            
                            ## Probe
                            tabPanel("Probe anlegen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # textInput("decProbe", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                                         fileInput("csvFileProbe", "CSV-Datei mit Proben", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,  
                                         checkboxInput("owProbe", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("ProbeValidationOut"),
                                         uiOutput("ProbeDBConsistencyTxtOut"),
                                         uiOutput("ProbeDBConsistencyActionOut"), # DBConsistencyActionOut
                                         width = 2),
                                       mainPanel(dataTableOutput('tableProbe'))
                                     )),
                            
                            ##  Messungen hochladen
                            tabPanel("Messungen hochladen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # textInput("dataSep", "Spaltentrennzeichen:", value = ";", width = "80%"),
                                         # textInput("dataDec", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                                         fileInput("dataCsvFile", "CSV-Datei mit Messungen", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         #   selectInput("dataBG", "Zeile mit Bestimmungsgrenze:", choices = c(1:20), selected = "1"),
                                         #   selectInput("dataUoM", "Zeile mit Maßeinheit:", choices = c(1:20), selected = "2"),
                                         #   selectInput("dataStgr", "Zeile mit Elementgruppe:", choices = c(1:20), selected = "3"),
                                         # textInput("dataBGchar", "Unter Bestimmungsgrenze:", value = BGchar),
                                         csvInfo, br(),
                                         paste0("Werte unterhalb der Bestimmungsgrenze müssel als \"", BGchar, "\" und unterhalb der Nachweisgrenze als \"", NGchar, "\" in der CSV-Datei abgelegt sein."),
                                         checkboxInput("dataOW", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("dataValidationOut"),
                                         uiOutput("dataDBConsistencyTxtOut"),
                                         uiOutput("dataDBConsistencyActionOut"),
                                         width=2),
                                       mainPanel(dataTableOutput('tableData'))
                                     ))),
                 
                 ######################
                 ### Literaturdaten ###
                 ######################
                 
                 navbarMenu("Literaturdaten",
                            ## Referenz
                            tabPanel("Referenz anlegen",
                                     sidebarLayout(
                                       sidebarPanel(# textInput("sepProbe", "Spaltentrennzeichen:", value = ";", width = "80%"),
                                         # textInput("decProbe", "Dezimaltrennzeichen:", value = ".", width = "80%"),
                                         fileInput("csvFileProbe", "CSV-Datei mit Referenzen", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owProbe", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("ProbeValidationOut"),
                                         uiOutput("ProbeDBConsistencyTxtOut"),
                                         uiOutput("ProbeDBConsistencyActionOut"), # DBConsistencyActionOut
                                         width = 2),
                                       mainPanel(dataTableOutput('tableProbe'))
                                     )),
                            
                            tabPanel("Literaturdaten hochladen",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput("csvFileProbe", "CSV-Datei mit Literaturwerten", 
                                                   buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         csvInfo,
                                         checkboxInput("owProbe", "Alle Daten überschreiben?", FALSE), 
                                         uiOutput("ProbeValidationOut"),
                                         uiOutput("ProbeDBConsistencyTxtOut"),
                                         uiOutput("ProbeDBConsistencyActionOut"), 
                                         width = 2),
                                       mainPanel(dataTableOutput('tableProbe'))
                                     ))),
                 
                 ## Dateien
                 tabPanel("Dateien hochladen",
                          selectInput("DateiSelType", "Wähle eine Kategorie", 
                                      choices = list(Messdaten=list("Ort", "Probenahmestelle", "Parameter", "Probe"), 
                                                     Literaturdaten=list("Literatur"))),
                          uiOutput("DateiKategorieElementeOut"),
                          fileInput("DateiFile", "Hochzuladende Datei", 
                                    buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt"),
                          checkboxInput("owDatei", "Datei überschreiben?", FALSE), 
                          uiOutput("DateiValidationOut"),
                          uiOutput("DateiDBConsistencyTxtOut"),
                          uiOutput("DateiDBConsistencyActionOut"))
)
