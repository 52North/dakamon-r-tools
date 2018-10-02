################################################################################
###########################   Upload der Messungen   ###########################
################################################################################
dataSeparator <- colSep
dataDecimalSeparator <- decSep
## tools

isLoadingCacheUpdate <- function(conf=adminConf) {
  loadingUrl <- paste0(SOSWebApp, "admin/cache/loading")
  response <- GET(url=loadingUrl, config=conf)
  fromJSON(rawToChar(response$content))[["loading"]]
}

sosCacheUpdate <- function(wait=0.5, conf=adminConf, verbose=FALSE) {
  reloadUrl <- paste0(SOSWebApp, "admin/cache/reload")
  POST(url = reloadUrl, config=conf, body="a")
  cat(file=catFile, "Wait until SOS cache update finishes")
  while(isLoadingCacheUpdate(conf)) {
    cat(file=catFile, ".")
    Sys.sleep(wait)
  }
  cat(file=catFile, "\n")
}

sosDeleteDeletedObservations <- function(wait=0.5, conf=adminConf, verbose=FALSE) {
  POST(url = paste0(SOSWebApp, "admin/datasource/deleteDeletedObservations"),
       config=conf, body="a")
  Sys.sleep(wait)
}

sosDeleteObservationsByIdentifier <- function(observationIdentifiers, wait = 0.5) {
  POST(paste0(SOSWebApp, "service"),
       body =  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
           <sosdo:DeleteObservation
         xmlns:sosdo=\"http://www.opengis.net/sosdo/1.0\" version=\"2.0.0\" service=\"SOS\"><sosdo:observation>",
           paste(observationIdentifiers, collapse = "</sosdo:observation><sosdo:observation>"),
           "</sosdo:observation></sosdo:DeleteObservation>"),
       content_type_xml(), accept_json())
  Sys.sleep(wait)
}

createFeederConfiguration <- function(csvPath,
                                      url=SOSWebApp, timestampPattern = feederTimestampPattern,
                                      timeZone = feederTimeZoneIdentifier, epsgCode = feederEpsgCode,
                                      decimalSeparator = decSep, columnSeparator = colSep,
                                      importerClass = feederImporterClass, hunkSize = feederHunkSize,
                                      timeoutBuffer = feederTimeoutBuffer) {
  paste0(
  "<SosImportConfiguration xmlns=\"http://52north.org/sensorweb/sos/importer/0.5/\">
    <DataFile referenceIsARegularExpression=\"false\">
      <LocalFile>
        <Path>", csvPath, "</Path>
        <Encoding>UTF-8</Encoding>
      </LocalFile>
    </DataFile>
    <SosMetadata>
      <URL>", url, "service</URL>
      <Offering generate=\"true\" />
      <Version>2.0.0</Version>
      <Binding>POX</Binding>
      <Importer importerThreads=\"1\">", importerClass, "</Importer>
    </SosMetadata>
    <CsvMetadata>
      <ColumnAssignments>
        <Column>
          <Number>0</Number>
          <Type>OBSERVED_PROPERTY</Type>
        </Column>
        <Column>
          <Number>1</Number>
          <Type>MEASURED_VALUE</Type>
          <Metadata>
            <Key>TYPE</Key>
            <Value>NUMERIC</Value>
          </Metadata>
        </Column>
        <Column>
          <Number>2</Number>
          <Type>UOM</Type>
        </Column>
        <Column>
          <Number>3</Number>
          <Type>SENSOR</Type>
        </Column>
        <Column>
          <Number>4</Number>
          <Type>DATE_TIME</Type>
          <Metadata>
            <Key>GROUP</Key>
            <Value>1</Value>
          </Metadata>
          <Metadata>
            <Key>PARSE_PATTERN</Key>
            <Value>", timestampPattern, "</Value>
          </Metadata>
          <Metadata>
            <Key>TYPE</Key>
            <Value>COMBINATION</Value>
          </Metadata>
          <Metadata>
            <Key>TIME_ZONE_IDENTIFIER</Key>
            <Value>", timeZone, "</Value>
          </Metadata>
          <Metadata>
            <Key>TIME_TYPE</Key>
            <Value>RESULT</Value>
          </Metadata>
        </Column>
        <Column>
          <Number>5</Number>
          <Type>DATE_TIME</Type>
          <Metadata>
            <Key>GROUP</Key>
            <Value>2</Value>
          </Metadata>
          <Metadata>
            <Key>PARSE_PATTERN</Key>
            <Value>", timestampPattern, "</Value>
          </Metadata>
          <Metadata>
            <Key>TYPE</Key>
            <Value>COMBINATION</Value>
          </Metadata>
          <Metadata>
            <Key>TIME_ZONE_IDENTIFIER</Key>
            <Value>", timeZone, "</Value>
          </Metadata>
          <Metadata>
            <Key>TIME_TYPE</Key>
            <Value>OBSERVATION_START</Value>
          </Metadata>
        </Column>
        <Column>
          <Number>6</Number>
          <Type>DATE_TIME</Type>
          <Metadata>
            <Key>GROUP</Key>
            <Value>3</Value>
          </Metadata>
          <Metadata>
            <Key>PARSE_PATTERN</Key>
            <Value>", timestampPattern, "</Value>
          </Metadata>
          <Metadata>
            <Key>TYPE</Key>
            <Value>COMBINATION</Value>
          </Metadata>
          <Metadata>
            <Key>TIME_ZONE_IDENTIFIER</Key>
            <Value>", timeZone, "</Value>
          </Metadata>
          <Metadata>
            <Key>TIME_TYPE</Key>
            <Value>OBSERVATION_END</Value>
          </Metadata>
        </Column>
        <Column>
          <Number>7</Number>
          <Type>FOI</Type>
        </Column>
        <Column>
          <Number>8</Number>
          <Type>POSITION</Type>
          <Metadata>
            <Key>TYPE</Key>
            <Value>COMBINATION</Value>
          </Metadata>
          <Metadata>
            <Key>PARSE_PATTERN</Key>
            <Value>COORD_0</Value>
          </Metadata>
          <Metadata>
            <Key>GROUP</Key>
            <Value>A</Value>
          </Metadata>
          <Metadata>
            <Key>POSITION_EPSG_CODE</Key>
            <Value>", epsgCode, "</Value>
          </Metadata>
        </Column>
        <Column>
          <Number>9</Number>
          <Type>POSITION</Type>
          <Metadata>
            <Key>TYPE</Key>
            <Value>COMBINATION</Value>
          </Metadata>
          <Metadata>
            <Key>PARSE_PATTERN</Key>
            <Value>COORD_1</Value>
          </Metadata>
          <Metadata>
            <Key>GROUP</Key>
            <Value>A</Value>
          </Metadata>
        </Column>
      </ColumnAssignments>
      <DecimalSeparator>", decimalSeparator, "</DecimalSeparator>
      <FirstLineWithData>1</FirstLineWithData>
      <Parameter>
        <CommentIndicator>#</CommentIndicator>
        <ColumnSeparator>", columnSeparator, "</ColumnSeparator>
        <TextIndicator>\"</TextIndicator>
      </Parameter>
      <ObservationCollector ignoreColumnCountMismatch=\"false\">org.n52.sos.importer.feeder.collector.DefaultCsvCollector</ObservationCollector>
    </CsvMetadata>
    <AdditionalMetadata>
      <Metadata>
        <Key>FEEDER_CLASS</Key>
        <Value>org.n52.sos.importer.feeder.SingleThreadFeeder</Value>
      </Metadata>
      <Metadata>
        <Key>TIMEOUT_BUFFER</Key>
        <Value>", timeoutBuffer, "</Value>
      </Metadata>
    </AdditionalMetadata>
  </SosImportConfiguration>")
}

queryProbenMetadata <- function(Messungen_data, db) {
  connected <- is.null(db)
  if (!connected) {
    db <- connectToDB()
  }
  tryCatch({
    # notes
    # - Sensor: lab (kommen aus der Probe: reqColProbe$LabName, reqColProbe$LabId) + parameter (observableProperty.identifier)
    # - obsProp: parameter (observableProperty.identifier)
    # - feature: PNS + sup: ort (probe->pns_id->feature-identifier&geom) + featurerelation: child featureid =: pns_id, parentfeatureid =: ort_id
    #
    # get sensor information
    # SELECT probe.col015 AS identifier, probe.col016 AS name
    # FROM probe
    # WHERE probe.identifier IN ('PRO-001','PRO-002','PRO-003','PRO-004','PRO-005')
    #
    proben <- Messungen_data[,reqColData$probeI]
    probenQuerySection <- paste0(unique(proben), collapse = "','")
    probenMetadataQuery <- paste0("SELECT DISTINCT
                                        pro.identifier AS probeid,
                                        pro.pns_id,
                                        foi.identifier AS pnsid,
                                        pro.lab AS sensorid,
                                        to_char(timezone('",  dbTimeZoneIdentifier,"', pro.resulttime::timestamptz), '",  dbTimestampPattern, "') AS resultTime,
                                        to_char(timezone('",  dbTimeZoneIdentifier,"', pro.phenomenontimestart::timestamptz), '",  dbTimestampPattern, "') AS phenTimeStart,
                                        to_char(timezone('",  dbTimeZoneIdentifier,"', pro.phenomenontimeend::timestamptz), '",  dbTimestampPattern, "') AS phenTimeEnd
                                      FROM
                                        probe AS pro
                                      LEFT OUTER JOIN featureofinterest AS foi ON (foi.featureofinterestid = pro.pns_id)
                                      WHERE
                                        pro.identifier IN ('",
                                  probenQuerySection,
                                  "')")
    cat(file=catFile, probenMetadataQuery, "\n")
    probenMetadata <- dbGetQuery(db, probenMetadataQuery)
  }, error = modalErrorHandler, finally = if (!connected) poolReturn(db))
}

queryParameter <- function(Messungen_data, db) {
  connected <- is.null(db)
  if (!connected) {
    db <- connectToDB()
  }
  tryCatch({
    # SELECT observableproperty.identifier, observableproperty.name
    # FROM observableproperty
    # WHERE observableproperty.identifier IN ('Blei','Amonium')
    #
    parameter <- Messungen_data[,reqColData$obsProp]
    parameterQuerySection <- paste0(unique(parameter), collapse = "','")
    observedpropertiesQuery <- paste0("SELECT DISTINCT
                                        observablepropertyid,
                                        identifier,
                                        name
                                      FROM
                                        observableproperty
                                      WHERE
                                        name IN ('",
                                      parameterQuerySection,
                                      "')")
    observedproperties <- dbGetQuery(db, observedpropertiesQuery)
  }, error = modalErrorHandler, finally = if (!connected) poolReturn(db))
}

queryObservationCharacteristics <- function(Messungen_data, db) {
  connected <- is.null(db)
  if (!connected) {
    db <- connectToDB()
  }
  tryCatch({
    proben <- Messungen_data[,reqColData$probeId]
    probenQuerySection <- paste0(unique(proben), collapse = "','")
    parameter <- Messungen_data[,reqColData$obsProp]
    parameterQuerySection <- paste0(unique(parameter), collapse = "','")
    probenParameterMetadataQuery <- paste0("SELECT
                                            pro.identifier As probeid,
                                            to_char(timezone('",  dbTimeZoneIdentifier,"', pro.resulttime::timestamptz), '",  dbTimestampPattern, "') AS resultTime,
                                            to_char(timezone('",  dbTimeZoneIdentifier,"', pro.phenomenontimestart::timestamptz), '",  dbTimestampPattern, "') AS phenTimeStart,
                                            to_char(timezone('",  dbTimeZoneIdentifier,"', pro.phenomenontimeend::timestamptz), '",  dbTimestampPattern, "') AS phenTimeEnd,
                                            param.identifier AS paramid,
                                            foi.identifier AS foiid,
                                            pro.lab AS lab
                                            FROM
                                            probe_parameter pp
                                            LEFT OUTER JOIN probe AS pro ON (pro.id = pp.probe_id)
                                            LEFT OUTER JOIN observableproperty AS param ON (param.observablepropertyid = pp.parameter_id)
                                            LEFT OUTER JOIN featureofinterest AS foi ON (foi.featureofinterestid = pro.pns_id)
                                            WHERE
                                            pro.identifier IN ('",
                                           probenQuerySection,
                                           "')
                                            AND
                                            param.name IN ('",
                                           parameterQuerySection,
                                           "')")
    cat(file=catFile, probenParameterMetadataQuery, "\n")
    probenParameterMetadata <- dbGetQuery(db, probenParameterMetadataQuery)
  }, error = modalErrorHandler, finally = if (!connected) poolReturn(db))
}


# queryObservationInDB <- function(identifier, db) {
#   connected <- is.null(db)
#   if (!connected) {
#     db <- connectToDB()
#   }
#   tryCatch({
#     query <- paste0("SELECT identifier FROM observation WHERE identifier IN ('", identifier,"');")
#     length(dbGetQuery(db, probenParameterMetadataQuery)) > 0
#   }, error = modalErrorHandler, finally = if (!connected) poolReturn(db))
# }

## /tools

# reactive variables
inCSVData <- reactiveValues()
valiData <- reactiveValues(validated = FALSE)
checkDBData <- reactiveValues(checked = FALSE)


observeEvent(input$dataCsvFile, {
  valiData$validated <- FALSE
  checkDBData$checked <- FALSE

  if (!is.null(input$dataCsvFile$datapath)) {

    inCSVData$df <- read.csv(input$dataCsvFile$datapath, header = TRUE,
                             sep = dataSeparator, dec = dataDecimalSeparator,
                             stringsAsFactors = FALSE,
                             fileEncoding = input$dataFileEnc)
    inCSVData$headAsChar <- colnames(inCSVData$df)
  }

  #################################
  ## validation of data csv-file ##
  #################################
  # ID
  # Parameter
  # Wert
  # Einheit
  # BG
  # NG

  txt <- NULL
  for (reqColName in reqColData) {
    if (!(reqColName %in% inCSVData$headAsChar))
      txt <- paste0(txt, "<li>Bitte eine Spalte '", reqColName, "' angeben.</li>")
  }

  if(length(unique(inCSVData$headAsChar)) != length(inCSVData$headAsChar))
    txt <- paste0(txt, "<li>Spaltennamen müssen eindeutig sein.</li>")

  # check whether each parameter in the csv is reported only once per ProID
  if (length(unique(paste0(inCSVData$df[[reqColData$probeId]], inCSVData$df[[reqColData$obsProp]]))) != nrow(inCSVData$df))
    txt <- paste0(txt, "<li>Jeder Parameter darf nur einmal je Probe vorkommen.</li>")

  valiData$txt <- txt
  valiData$validated <- TRUE
})


output$dataValidationOut <- renderUI({
  if (valiData$validated) {
    if (is.null(valiData$txt)) {
      actionButton("checkDBData", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiData$txt, "</ul></div></html>"))
    }
  } else {
    return()
  }
})

########################
## DB consistency checks
# check whether the ProbeIDs exist
# check whether the Parameter exist
# check whether the combination of ProbeId and Parameter already corresponds to some time series data
# -> upload/update; handle BG and NG in data column -> replace with 0 or -99, -9999, or alike to have pure numbers

observeEvent(input$checkDBData, {
  db <- connectToDB()
  checkDBData$txt <- NULL
  checkDBData$err <- FALSE
  checkDBData$checked <- FALSE

  tryCatch({

    # check whether the ProbeIDs exist
    probenMetadata <- queryProbenMetadata(inCSVData$df, db)

    uniqueInCSVDataProbeId <- unique(inCSVData$df[[reqColData$probeId]])
    matchProbeId <- match(uniqueInCSVDataProbeId, probenMetadata$probeid)
    if (any(is.na(matchProbeId))) {
      checkDBData$txt <- paste("Folgende Proben fehlen in der DB: <ul><li>",
                               paste0(uniqueInCSVDataProbeId[which(is.na(matchProbeId))], collapse="</li><li>"),
                               "</li></ul>")

      checkDBData$err <- TRUE
    }

    # check whether the Parameter exist
    observedproperties <- queryParameter(inCSVData$df, db)

    uniqueInCSVDataParId <- unique(inCSVData$df[,reqColData$obsProp])
    matchParId <- match(uniqueInCSVDataParId, observedproperties$name)
    if (any(is.na(matchParId))) {
      checkDBData$txt <- paste(checkDBData$txt,
                               paste("Folgende Parameter fehlen in der DB: <ul><li>",
                                     paste0(uniqueInCSVDataParId[which(is.na(matchParId))], collapse="</li><li>"),
                                     "</li></ul>"))
      checkDBData$err <- TRUE
    }

    observationCharacteristics <- queryObservationCharacteristics(inCSVData$df, db)

    if (nrow(observationCharacteristics) > 0) {
      # 15319152001516017600/1516190400BleiKAM_BW_EPP_PSLab1-123_Blei

      observationCharacteristics$resulttime <- as.numeric(strptime(observationCharacteristics$resulttime,
                                                                   format=RtimestampPattern,
                                                                   tz=dbTimeZoneIdentifier))
      observationCharacteristics$phentimestart <- as.numeric(strptime(observationCharacteristics$phentimestart,
                                                                      format=RtimestampPattern,
                                                                      tz=dbTimeZoneIdentifier))
      observationCharacteristics$phentimeend <- as.numeric(strptime(observationCharacteristics$phentimeend,
                                                                    format=RtimestampPattern,
                                                                    tz=dbTimeZoneIdentifier))

      inCSVData$obsIdsInCSV <- paste0(observationCharacteristics$phentimestart,
                                     observationCharacteristics$phentimestart,
                                     "/",
                                     observationCharacteristics$phentimeend,
                                     observationCharacteristics$paramid,
                                     observationCharacteristics$foiid,
                                     observationCharacteristics$lab,
                                     "_",
                                     observationCharacteristics$paramid)

      inCSVData$obsIdsInDB <- dbGetQuery(db, paste0("SELECT observationid AS obsid FROM observation WHERE identifier IN ('",
                 paste(inCSVData$obsIdsInCSV, collapse="', '"),
                 "')"))

      if (nrow(inCSVData$obsIdsInDB) > 0) {
        checkDBData$txt <- paste(checkDBData$txt,
                                 paste("Folgende Messungen sind bereits in der DB: <ul><li>",
                                       paste0(inCSVData$obsIdsInDB, collapse="</li><li>"),
                                       "</li></ul>"))
      }
    }
    checkDBData$checked <- TRUE
  }, error = modalErrorHandler, finally = poolReturn(db))
}, ignoreInit=TRUE)


output$dataDBConsistencyOut <- renderUI({ #
  if (checkDBData$checked) {
    if (checkDBData$err == TRUE) {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBData$txt, "</li></ul></div></html>"))
    } else {
      if (!is.null(checkDBData$txt)) {
        if (input$dataOW) {
          actionButton("storeDBData", "Speichere in DB!")
        } else {
          HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBData$txt, "</li></ul></div></html>"))
        }
      } else {
        actionButton("storeDBData", "Speichere in DB!")
      }
    }
  }
})


#####################
## print datatable ##
#####################

output$tableData <- renderDataTable({
  if (!is.null(inCSVData$df)) {
    showTab <- inCSVData$df
    format(showTab, scientific=FALSE)
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", inCSVData$headAsChar)

    showHead <- paste0(showHead, "</span>")

    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sort=FALSE, dom="t",
                                       language=list(url = lngJSON)),
                        escape=FALSE)

    showDT <- formatSignif(showDT, c('NG', 'BG'), 1, dec.mark=",")

    # if DB consistency has been checked, apply colors
    if (checkDBData$checked) {
      if (any(inCSVData$obsInDB > 0)) {
        cat(file=catFile, inCSVData$obsInDB, "\n")
        for (colDf in 4:ncol(inCSVData$df)) {
          rowClrs <- c("white", "yellow", "red", "blue")[inCSVData$obsInDB[,colDf-3]+1]

          showDT <- formatStyle(showDT, colDf, "ID",
                                backgroundColor = styleEqual(showTab$ID, rowClrs))
        }
      }
    }
    showDT
  }
})


######################
## store data in DB ##
######################
# for each FoI in the uploaded csv
#   - build lab_config.xml from CSV
#   - paste text blocks per column in parallel
#   - clean and write csv-file for SOS importer
#   - run multi feeder: java -jar 52n-sos-importer-feeder-bin.jar -c //FOLDER//

observeEvent(input$storeDBData, {
  if (!is.null(inCSVData$df)) {
    checkDBData$checked <- FALSE
    db <- connectToDB()
    tryCatch({
      dbWithTransaction(db, {

        if (input$dataOW & !is.null(inCSVData$obsIdsInDB)) {

          # delete observations already in the DB
          progress <- Progress$new()
          progress$set(message = "Bereite DB vor.", value = 0)
          sosDeleteObservationsByIdentifier(inCSVData$obsIdsInDB)
          sosDeleteDeletedObservations()
          progress$close()
        }

        Messungen_data <- inCSVData$df

        progress <- Progress$new(min = 0, max = 10 + nrow(Messungen_data))
        progress$set(message = "Lade Daten in DB.", value = 0)

        # convert value column BG, NG values in numbers and the whole column to numeric values
        #if (any(Messungen_data[,reqColData$value] == BGchar)) {
        if (any(grep(BGchar, Messungen_data[,reqColData$value]))) {
          column <- Messungen_data[,reqColData$value]
          column[grep(BGchar, column)] <- BGencode
          Messungen_data[,reqColData$value] <- column
        }
        progress$inc(1)

        if (any(grep(NGchar, Messungen_data[,reqColData$value]))) {
          column <- Messungen_data[,reqColData$value]
          column[grep(NGchar, column)] <- NGencode
          Messungen_data[,reqColData$value] <- column
        }
        progress$inc(1)

        # TODO set NO_DATA value in case value is NA

        if (!is.na(any(Messungen_data[,reqColData$value] == noDataValue))) {
          column <- Messungen_data[,reqColData$value]
          column[column == noDataValue] <- noDataEncode
          Messungen_data[,reqColData$value] <- column
        }
        progress$inc(1)

        #
        # Convert "," decimal separator to system separator
        #
        # In der Spalte stehen z.B. auch BG o.ä. Dadurch ist sie im Dataframe
        # eine Character-Spalte und die Konvertierung der Dezimalzahlen von
        # Local-Format ins R interne Format funktioniert nicht. Daher müssen
        # wir händisch hinterher nach dem Komma suchen und es durch Punkt
        # ersetzen.
        #
        if (any(grepl(",", Messungen_data[,reqColData$value]))) {
          column <- Messungen_data[,reqColData$value]
          for (j in 1:length(column)) {
            column[j] <- gsub(",",".", column[j])
          }
          Messungen_data[,reqColData$value] <- column
        }
        progress$inc(1)

        #convert value column to numeric values
        Messungen_data[,reqColData$value] <- as.numeric(Messungen_data[,reqColData$value])

        # notes
        # - Sensor: lab (kommen aus der Probe: reqColProbe$LabName, reqColProbe$LabId) + parameter (observableProperty.identifier)
        # - obsProp: parameter (observableProperty.identifier)
        # - feature: PNS + sup: ort (probe->pns_id->feature-identifier&geom) + featurerelation: child featureid =: pns_id, parentfeatureid =: ort_id
        #
        # get sensor information
        probenMetadata <- queryProbenMetadata(Messungen_data, db)
        progress$inc(1)

        #
        # get observed property information
        #
        observedproperties <- queryParameter(Messungen_data, db)
        progress$inc(1)

        #
        # get PNS and Ort
        #
        # probeid -> pns_id (=foiId) -> pns_data -> lat, lon; featureofinterest -> identifier, name; feature_relation -> orts_id -> parentfeature-identifier
        # WITH
        # my_pns_id AS (
        #   SELECT DISTINCT probe.pns_id
        #   FROM probe
        #   WHERE probe.identifier IN ('PRO-001','PRO-002','PRO-003','PRO-004','PRO-005')
        # )
        # SELECT
        #   my_pns_id.pns_id,
        #   pns_data.col001 AS pns_lat,
        #   pns_data.col002 AS pns_lon,
        #   featureofinterest.identifier AS pns_identifier,
        #   featureofinterest.name AS pns_name
        # FROM
        #   my_pns_id,
        #   pns_data,
        #   featureofinterest
        # WHERE
        #   my_pns_id.pns_id = pns_data.featureofinterestid AND
        #   my_pns_id.pns_id = featureofinterest.featureofinterestid
        # ORDER BY
        #   my_pns_id.pns_id ASC
        #
        probenQuerySection <- paste0(unique(Messungen_data[,match(reqColData$probeId, reqColData)]), collapse = "','")
        pnsQuery <- paste0("WITH
                         my_pns_id AS (
                           SELECT DISTINCT probe.pns_id
                           FROM probe
                           WHERE probe.identifier IN ('",
                            probenQuerySection,
                            "')
                         )
                         SELECT
                           my_pns_id.pns_id,
                           pns_data.lat AS pns_lat,
                           pns_data.lon AS pns_lon,
                           featureofinterest.identifier AS pns_identifier,
                           featureofinterest.name AS pns_name
                         FROM
                           my_pns_id,
                           pns_data,
                           featureofinterest
                         WHERE
                           my_pns_id.pns_id = pns_data.featureofinterestid AND
                           my_pns_id.pns_id = featureofinterest.featureofinterestid")
        features <- dbGetQuery(db, pnsQuery)
        progress$inc(1)

        feedTmpConfigDirectory <- tempdir()
        #
        # Create global CSV file
        #
        feedCSV <- tempfile(pattern = "feed-csv-", feedTmpConfigDirectory, fileext = ".csv")
        #
        # Create the global configuraton file
        #
        cat(file=catFile, "create feeder configuration ...\n")
        feedConf <- tempfile(pattern = "feed-",  feedTmpConfigDirectory, fileext = "-config.xml")

        writeLines(createFeederConfiguration(csvPath = feedCSV), feedConf)
        progress$inc(1)

        cat(file=catFile, "prepare import data\n")
        feedDataContent <- matrix(c("Parameter", "Wert", "Einheit", "sensor-id", "resultTime", "phenStart", "phenEnd", "foiIdentifier", "lat", "lon"), nrow=1, ncol=10)
        for (i in 1:nrow(Messungen_data)) { # i <- 1
          row <- Messungen_data[i,]

          #
          # Fill probe_parameter table
          # 1        2         3    4       5  6
          # ProbenID;Parameter;Wert;Einheit;BG;NG
          # probe_id (in CSV, col#1), parameter_id, pp_unit (in CSV, col), bg, ng
          #
          # INSERT DATA
          #
          # probe id, parameter id pp unit bg ng
          # -- insert probe_parameter
          # WITH query_pro AS (
          #   SELECT id as probe_id FROM probe WHERE identifier = 'probe_id_var'
          # ),
          # query_para AS (
          #   SELECT observablepropertyid as para_id FROM observableproperty WHERE identifier = 'parameter_id_var'
          # ),
          # insert_unit AS (
          #   INSERT INTO unit
          #   (unitid, unit)
          #   VALUES(nextval('unitid_seq'),
          #          'pro_para_col003_var'
          #   )
          #   ON CONFLICT (unit) DO UPDATE SET unit = 'pro_para_col003_var'
          #   RETURNING unitid as unit_id
          # )
          # INSERT INTO probe_parameter
          # SELECT pro.probe_id, para.para_id, unit.unit_id, 'pro_para_col004_var', 'pro_para_col005_var' FROM pro, para, unit
          #
          insertUnitQuery <- if (is.null(row[reqColData$uom]) || is.na(row[reqColData$uom]) || row[reqColData$uom] == '') {
            "insert_unit AS (SELECT NULL::bigint as unit_id)"
          } else {
            paste0("insert_unit AS (
                                            INSERT INTO unit (unitid, unit)
                                            VALUES(nextval('unitid_seq'),'", row[reqColData$uom], "')
                                            ON CONFLICT (unit) DO UPDATE SET unit = '", row[reqColData$uom], "'
                                            RETURNING unitid as unit_id
                                          )")
          }

          query <- paste0("WITH query_probe_id AS (
                      SELECT id as probe_id
                      FROM probe
                      WHERE identifier = '", row[reqColData$probeId], "'
                    ),
                    query_parameter_id AS (
                      SELECT observablepropertyid as para_id
                      FROM observableproperty
                        WHERE name = '", row[reqColData$obsProp], "' ),",
                          insertUnitQuery,
                          " INSERT INTO probe_parameter
                    SELECT query_probe_id.probe_id, query_parameter_id.para_id, insert_unit.unit_id, ",
                          ifelse (is.null(row[reqColData$bg]) || is.na(row[reqColData$bg]) || row[reqColData$bg] == '', "NULL", row[reqColData$bg]),
                          ", ",
                          ifelse (is.null(row[reqColData$ng]) || is.na(row[reqColData$ng]) || row[reqColData$ng] == '', "NULL", row[reqColData$ng]),
                          " FROM query_probe_id, query_parameter_id, insert_unit
                    ON CONFLICT ON CONSTRAINT probe_parameter_pkey
                        DO UPDATE SET
                            pp_unit = (SELECT unit_id FROM insert_unit),
                              bg = ", ifelse (is.null(row[reqColData$bg]) || is.na(row[reqColData$bg]) || row[reqColData$bg] == '', "NULL", row[reqColData$bg]),
                              ", ng = ", ifelse (is.null(row[reqColData$ng]) || is.na(row[reqColData$ng]) || row[reqColData$ng] == '', "NULL", row[reqColData$ng]),
                          " WHERE probe_parameter.probe_id = (SELECT probe_id FROM query_probe_id)
                        AND probe_parameter.parameter_id = (SELECT para_id FROM query_parameter_id);")
          cat(file=catFile, query, "\n")
          dbExecute(db, query)

          newDataRow <- c(observedproperties[is.element(observedproperties$name, row[reqColData$obsProp]),"identifier"], # Parameter ID
                          row[reqColData$value], # Wert
                          row[reqColData$uom], # Einheit
                          paste0(probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"sensorid"],"_",
                                 observedproperties[min(which(is.element(observedproperties$name, row[reqColData$obsProp]))),"identifier"]), # SensorId
                          probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"resulttime"], # resultTime
                          probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"phentimestart"], # phenTimeStart
                          probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"phentimeend"], # phenTimeEnd
                          features[is.element(features$pns_id, probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"pns_id"]),"pns_identifier"], # foiIdentifier
                          features[is.element(features$pns_id, probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"pns_id"]),"pns_lat"], # Lat
                          features[is.element(features$pns_id, probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"pns_id"]),"pns_lon"])  # Lon
          feedDataContent <- rbind(feedDataContent, newDataRow)

          progress$inc(1)
        }

        sosCacheUpdate(wait=1)
        progress$inc(1)

      })

      #
      # write global csv file
      #
      cat(file=catFile, paste("writing import data to: ", feedCSV), "\n")
      write.table(feedDataContent, file = feedCSV, sep = colSep, dec = decSep, fileEncoding = "UTF-8", row.names = FALSE, col.names = FALSE)
      cat(file=catFile, "Done!\n")
      progress$inc(1)

      if (!file.exists(feederPath)) {
        cat(file=catFile, paste("Feeder path does not exist:", feederPath, "\n"))
        showModalMessage(title="Fehler", "Feeder nicht gefunden!")
      } else {
        tryCatch({
          cat(file=catFile, "Start feeding data values\n")
          cat(file=catFile, paste("Config File: ", feedConf), "\n")
          cat(file=catFile, paste("Feeder Jar : ", feederPath), "\n")
          cat(file=catFile, paste("CSV File   : ", feedCSV), "\n")
          logFile <- tempfile(pattern = "feed-",  feedTmpConfigDirectory, fileext = ".log")
          cat(file=catFile, paste("Log File   : ", logFile), "\n")
          system2("java", args = c(paste0("-DDAKAMON_LOG_FILE=", logFile), "-jar", feederPath, "-c", feedConf), stdout = FALSE, stderr = FALSE, wait = FALSE)
          # cmd <- paste0("/usr/bin/java -DDAKAMON_LOG_FILE=", logFile, " -jar ", feederPath, " -c ", feedConf)
          # system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE, intern = FALSE)
          cat(file=catFile, "\n")
          cat(file=catFile, "Wait until import finishes")
          Sys.sleep(1)
          if (Sys.info()["sysname"] != "Windows") {
            check <- system(paste0("ps aux | grep -v grep | grep ", logFile, " | wc -l"), intern = TRUE)
            while (check == 1) {
              Sys.sleep(2)
              cat(file=catFile, ".")
              check <- system(paste0("ps aux | grep -v grep | grep ", logFile, " | wc -l"), intern = TRUE)
            }
          } else {
            isFeederRunning <- function() {
              processListCmd <- paste0("WMIC PROCESS GET Caption,Processid,Commandline")
              tasks <- system2("powershell", args=c(paste0(processListCmd)), stdout=TRUE, wait=TRUE)
              length(grep(basename(logFile), tasks)) > 0
            }

            importInProgress <- isFeederRunning()
            cat(file=catFile, "Import process running")
            while (importInProgress) {
              Sys.sleep(2)
              importInProgress <- isFeederRunning()
              cat(file=catFile, ".")
            }
            Sys.sleep(2)
          }
          cat(file=catFile, "\n")

          cat(file=catFile, "Done!\n")
          progress$inc(1)
          Sys.sleep(5) # wait for log to be written
          result <- read_lines(logFile, locale = locale())

          if (length(grep("Failed observations: 0.", result, value = TRUE)) == 0) {
            cat(file=catFile, "Errors occured during import! Consult importer logs.\n")
            content <- div("Log-Ausgabe",
              pre(style='overflow-y: scroll; max-height: 200px; font-family: monospace; font-size: 75%', paste0(result, collapse = "\n")))
            showModalMessage(title="Bericht", content, size = "l")
          } else {
            content <- "Die Messdaten wurden erfolgreich in der Datenbank angelegt."
            showModalMessage(title="Vorgang abgeschlossen", content)
            ## TODO delete files after successful run!
            # file.remove(logFile)
            # file.remove(feedConf)
            # file.remove(feedCSV)
          }
        }, error = modalErrorHandler, warning = modalErrorHandler, finally=progress$close())
      }
    }, error = modalErrorHandler, finally = poolReturn(db))
  }
}, ignoreInit=TRUE)
