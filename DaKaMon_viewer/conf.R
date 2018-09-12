## DaKaMon conf
# common constants

# width of the panels (the sum MUST be 12)
sideBarWidth <- 3
mainPanelWidth <- 9


local <- interactive()

#
# DATABASE
#
dbHost <- ifelse(local, "localhost", "db")
# map to arbitrary port locally, use 5432 in non-local setup
dbPort <- ifelse(local, "5433", "5432")
dbUser <- "postgres"
dbPassword <- "postgres"
dbName <- "sos"

# verbose <- TRUE
verbose <- local

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
