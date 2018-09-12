## DaKaMon conf
# common constants

#
# CSV encoding
#
# column Separator
colSep <- ";"
# decimal separator
decSep <- ","
# encoding
csvEncode <- "UTF-8"
localEncoding <- strsplit(Sys.getlocale("LC_COLLATE"), ".", fixed=TRUE)[[1]][2]
tabTitleStyle <- "text-align: center;margin-top: 0px; margin-bottom: 5px; color: #777;"

# width of the panels (the sum MUST be 12)
sideBarWidth <- 3
mainPanelWidth <- 9


#
# I18N
#
# language
lngJSON <- "http://cdn.datatables.net/plug-ins/1.10.11/i18n/German.json"
dataStep1 <- list(title = "Ort anlegen")
dataStep2 <- list(title = "Probenahmestelle anlegen")
dataStep3 <- list(title = "Parameter anlegen")
dataStep4 <- list(title = "Probe anlegen")
dataStep5 <- list(title = "Messungen hochladen")
litStep1  <- list(title = "Referenz anlegen")
litStep2  <- list(title = "Literaturdaten hochladen")
filesTab  <- list(title = "Dateien hochladen")


csvInfo <- paste0("Die CSV-Datei muss \"", colSep, "\" als Spaltentrennzeichen und \"", decSep, "\" als Dezimaltrennzeichen verwenden und in \"", csvEncode, "\" enkodiert sein.")

#
# DETECTION LIMIT
#
BGencode <- 0
BGchar <- "BG"
# label in DB
BGlabel <- "Bestimmungsgrenze"
#
# LOWER DETECTION LIMIT
#
NGencode <- -1
NGchar <- "NG"
# label in DB
NGlabel <- "Nachweisgrenze"

local <- interactive()
# SOSWebApp MUST end with "/"
SOSWebApp <- ifelse(local, "http://localhost:8080/52n-sos-webapp/", "http://sos:8080/52n-sos-webapp/")

#
# DATABASE
#
dbHost <- ifelse(local, "localhost", "db")
dbPort <- "5432"
dbUser <- "postgres"
dbPassword <- "postgres"
dbName <- "sos"
# Documentation: https://www.postgresql.org/docs/9.6/static/functions-formatting.html#FUNCTIONS-FORMATTING-DATETIME-TABLE
# sync with feederTimestampPattern and feederTimestampPattern
dbTimestampPattern <- "DD-MM-YYYY HH24:MI"
# R time samp pattern as returned by the DB after to_char ^
RtimestampPattern <- "%d-%m-%Y %H:%M"

#
# TIMESTAMPS
#
# Documentation:
# - https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
# - http://www.jdatalab.com/data_science_and_data_mining/2017/03/20/regular-expression-R.html
# sync with dbTimestampPattern and feederTimestampPattern
timestampRegExPattern <- "^[[:digit:]]{2}-[[:digit:]]{2}-[[:digit:]]{4} [[:digit:]]{2}:[[:digit:]]{2}$"

# verbose <- TRUE
verbose <- local

foiType <- "http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint"

#
# FEEDER
#
feederPath <- ifelse(local, "ADJUST_ME/52n-sos-importer-feeder-bin.jar", "/usr/local/52n/52n-sos-importer-feeder-bin.jar")
#
# specifiies the number of parallel performed imports during measurement upload
#
feedNumberOfParallelImports <- 1
#
# WGS84 2D Lat Lon with degree unit
feederEpsgCode <- "4326"
# sync with dbTimestampPattern and timestampRegExPattern
# R reverses the order of the date information and adds seconds (if absent: ":00")
feederTimestampPattern <- "dd-MM-yyyy HH:mm"
feederTimeZoneIdentifier <- "Europe/Berlin"
dbTimeZoneIdentifier <- "Europe/Berlin"
#
feederImporterClass <- "org.n52.sos.importer.feeder.importer.SingleThreadSingleObservationImporter"
# the next two are used, when feederImporterClass is switched to
# feederImporterClass <- "org.n52.sos.importer.feeder.importer.SweArrayObservationWithSplitExtensionImporter"

stndTime <- "T12:00:00+00:00"
adminPwd <- "p"
adminConf <- authenticate("dakamon-administrator", adminPwd)

## DB="GUI/CSV"
reqColOrt <- list(id="ID", # KAM-EPP
                  name="Name",
                  lat="lat",
                  lon="lon",
                  thematik="Thematik")

reqColPNS <- list(id="ID",
                  name="Name",
                  geo="OrtsID",
                  lat="lat",
                  lon="lon")

reqColPAR <- list(id="ID",
                  name="Name",
                  stfgr="Stoffgruppe")


reqColProbe <- list(id="ID",
                    geoSub="PNS_ID",
                    colDate="Probenahmedatum",
                    eventTimeBegin="Ereignisbeginn",
                    eventTimeEnd="Ereignisende",
                    probeTechnic="Probenahmetechnik",
                    probeType="Probenahmeart",
                    labName="Labor",
                    labId="Labor_Nr")

reqColData <- list(probeId = "ProbenID",
                   obsProp = "Parameter",
                   value = "Wert",
                   uom = "Einheit",
                   bg = "BG",
                   ng = "NG")

reqColReferenz<- list(id = "ID")

reqColLiteratur<- list(refId= "Referenz_ID",
                       thematik = "Thematik",
                       paramId = "Parameter",
                       pnsId = "PNS_ID",
                       uBegin = "Untersuchungsbeginn",
                       uEnde = "Untersuchungsende")
