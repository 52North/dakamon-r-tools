## DaKaMon conf

# common constants:
# column Separator
colSep <- ";"
# decimal separator
decSep <- ","
# encoding
csvEncode <- "UTF-8"

# langiage
lngJSON <- "http://cdn.datatables.net/plug-ins/1.10.11/i18n/German.json"

csvInfo <- paste0("Die CSV-Datei muss \"", colSep, "\" als Spaltentrennzeichen und \"", decSep, "\" als Dezimaltrennzeichen verwenden und in \"", csvEncode, "\" enkodiert sein.")

SOSWebApp <- "http://localhost:8080/52n-sos-webapp/"
verbose <- TRUE
BGencode <- 0
BGchar <- "BG"
BGlabel <- "Bestimmungsgrenze" # label in DB

NGencode <- -1
NGchar <- "NG"
NGlabel <- "Nachweisgrenze" # label in DB

local <- interactive()
SOSWebApp <- ifelse(local, "http://localhost:8080/52n-sos-webapp/", "http://sos:8080/52n-sos-webapp/")
dbHost <- ifelse(local, "localhost", "db") 
dbPort <- "5432"
dbUser <- "postgres"
dbPassword <- "postgres"
dbName <- "sos"
verbose <- local

feederPath <- ifelse(local, "~/GitRepos/sos-importer/feeder/target/52n-sos-importer-feeder-bin.jar", "/usr/local/52n/52n-sos-importer-feeder-bin.jar")
#
# specifiies the number of parallel performed imports during measurement upload
#
feedNumberOfParallelImports <- 1
#
stndTime <- "T12:00:00+00:00"
adminPwd <- "p"
ifelse(local, 
       adminConf <- authenticate("a", adminPwd),
       adminConf <-   authenticate("dakamon-administrator", adminPwd))

## DB="GUI/CSV"

reqColOrt <- list(id="ID", # KAM-EPP
                  name="Name",
                  lat="lat",
                  lon="lon")

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
                    eventTiemBegin="Ereignisbeginn",
                    eventTimeEnd="Ereignisende",
                    probeTechnic="Probenahmetechnik",
                    probeType="Probenahmeart")

reqColData <- list(probeId = "ProbenID",
                   obsProp = "Parameter",
                   value = "Wert",
                   uom = "Einheit",
                   bg = "BG",
                   ng = "NG") 



