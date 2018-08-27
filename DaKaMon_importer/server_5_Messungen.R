################################################################################
###########################   Upload der Messungen   ###########################
################################################################################
dataSeparator <- colSep
dataDecimalSeparator <- decSep
## tools
sosCacheUpdate <- function(gmlId="tmp", wait=0.5, conf=adminConf, verbose=FALSE) {
  POST(url = paste0(SOSWebApp, "admin/cache/reload"),
       config=conf, body="a")

  # if (!is.null(gmlId)) {
  #   reqMsg <- rawToChar(POST(paste0(SOSWebApp, "service"),
  #                            body = SOSreqFoI(gmlId),
  #                            content_type_xml(), accept_json())$content)
  #
  #   while (is.null(fromJSON(reqMsg)$exceptions)) { #} && length(fromJSON(reqMsg)$featureOfInterest) > 0) {
  #     if (verbose)
  #       message(reqMsg)
  #
  #     Sys.sleep(wait)
  #     reqMsg <- rawToChar(POST(paste0(SOSWebApp, "service"),
  #                              body = SOSreqFoI(gmlId), # foi_data[sfoi,]$ID
  #                              content_type_xml(), accept_json())$content)
  #   }
  # } else {
    Sys.sleep(wait)
  # }
}

createFeederConfiguration <- function(csvPath,
                                      url=SOSWebApp, timestampPattern = feederTimestampPattern,
                                      timeZone = feederTimeZoneIdentifier, epsgCode = feederEpsgCode,
                                      decimalSeparator = ".", columnSeparator = dataSeparator,
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
      <Importer>", importerClass, "</Importer>
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
        <Key>HUNK_SIZE</Key>
        <Value>", hunkSize, "</Value>
      </Metadata>
      <Metadata>
        <Key>TIMEOUT_BUFFER</Key>
        <Value>", timeoutBuffer, "</Value>
      </Metadata>
    </AdditionalMetadata>
  </SosImportConfiguration>")
}

## /tools

# reactive variables
inCSVData <- reactiveValues()
valiData <- reactiveValues(validated = FALSE)
CheckDBData <- reactiveValues(checked = FALSE)


observeEvent(input$dataCsvFile, {
  valiData$validated <- FALSE
  CheckDBData$checked <- FALSE

  if (!is.null(input$dataCsvFile$datapath)) {

    if (is.null(csvEncode)) {
      csvEncode <- readr::guess_encoding(input$csvFileOrt$datapath)
      csvEncode <- csvEncode$encoding[which.max(csvEncode$confidence)]
    }

    inCSVData$csvEncode <- csvEncode

    inCSVData$df <- read.csv(input$dataCsvFile$datapath, header = TRUE,
                             sep = dataSeparator, dec = dataDecimalSeparator,
                             stringsAsFactors = FALSE,
                             fileEncoding = inCSVData$csvEncode)
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

  valiData$txt <- txt
  valiData$validated <- TRUE
})


output$dataValidationOut <- renderUI({
  if (valiData$validated) {
    if (is.null(valiData$txt)) {
      actionButton("checkDBData", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiData$txt, "</ul></div></html"))
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
  on.exit(dbDisconnect(db), add=T)

  # check whether the ProbeIDs exist
  # check whether the Parameter exist
  # check whether the combination of ProbeId and Parameter already corresponds to some time series data

  # TODO implement messungen check database
  # dbSendQuery(db, paste0("WITH query_pro AS (
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
  # SELECT pro.probe_id, para.para_id, unit.unit_id, 'pro_para_col004_var', 'pro_para_col005_var' FROM pro, para, unit"))

  CheckDBData$checked <- TRUE
}, ignoreInit=TRUE)


output$dataDBConsistencyOut <- renderUI({ #
  if (CheckDBData$checked) {
    if (!is.null(CheckDBData$txt)) {
      return( HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", CheckDBData$txt, "</li></ul></div></html")))
    }
    if (all(inCSVData$obsInDB < 2)) {
      if (!any(inCSVData$obsInDB > 0) || input$dataOW) {
        actionButton("storeDBData", "Speichere in DB!")
      } else {
        HTML("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">Einige Daten sind bereits in der DB (siehe gelbe Zellen).</div></html>")
      }
    } else {
      if (any( inCSVData$obsInDB == 2)) {
        HTML("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">Bestimmungsgrenze und/oder Maßeinheit sind in csv und DB unterschiedlich (siehe rote Zellen).</div></html>")
      } else {
        if (input$dataOW) {
          actionButton("dataStoreDB", "Speichere in DB!")
        } else {
          HTML("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">Elementgruppen sind in csv und DB unterschiedlich (siehe blaue Zellen).</div></html>")
        }
      }
    }
  } else {
    HTML("")
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
    if (CheckDBData$checked) {
      if (any(inCSVData$obsInDB > 0)) {
        cat(inCSVData$obsInDB, "\n")
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
    db <- connectToDB()
    on.exit(dbDisconnect(db), add=T)

    # FIXME where is the value inCSVData$obsIdsInDB set?
    if (input$dataOW & !is.null(inCSVData$obsIdsInDB)) {

      # delete observations already in the DB
      progress <- Progress$new()
      progress$set(message = "Bereite DB vor.", value = 0)

      for (id in inCSVData$obsIdsInDB) {
        progress$inc(1/length(inCSVData$obsIdsInDB))
        POST(paste0(SOSWebApp, "service"),
             body = SOSdelObsByID(id),
             content_type_xml(), accept_json())
      }
      progress$close()
    }

    Messungen_data <- inCSVData$df

    progress <- Progress$new(min = 0, max = 10 + nrow(Messungen_data))
    progress$set(message = "Lade Daten in DB.", value = 0)

    valueIndex <- match(reqColData$value, reqColData)
    # convert value column BG, NG values in numbers and the whole column in numeric values
    if (any(Messungen_data[,valueIndex] == BGchar, na.rm = TRUE)) {
      column <- Messungen_data[,valueIndex]
      column[column == BGchar] <- BGencode
      Messungen_data[,valueIndex] <- column
    }
    progress$inc(1)

    if (any(Messungen_data[,valueIndex] == NGchar, na.rm = TRUE)) {
      column <- Messungen_data[,valueIndex]
      column[column == NGchar] <- NGencode
      Messungen_data[,valueIndex] <- column
    }
    progress$inc(1)

    #
    # FIXME convert "," decimal separator to system separator
    #
    if (any(grepl(",", Messungen_data[,valueIndex]))) {
      column <- Messungen_data[,valueIndex]
      for (j in 1:length(column)) {
        column[j] <- gsub(",",".", column[j])
      }
      Messungen_data[,valueIndex] <- column
    }
    progress$inc(1)

    #convert value column to numeric values
    Messungen_data[,valueIndex] <- as.numeric(Messungen_data[,valueIndex])

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
    probeIdIndex <- match(reqColData$probeId, reqColData)
    proben <- Messungen_data[,probeIdIndex]
    probenQuerySection <- paste0(proben, collapse = "','")
    # FIXME col022, col023, col024, col033 and col034 musst be replaced by reqColProbe$LabId & reqColProbe$LabName
    probenMetadataQuery <- paste0("SELECT
                                      identifier AS probeid,
                                      pns_id,
                                      col033 AS sensorId,
                                      col022 AS resulTime,
                                      col023 AS phenTimeStart,
                                      col024 AS phenTimeEnd
                                    FROM
                                      probe
                                    WHERE
                                      identifier IN ('",
                                      probenQuerySection,
                                      "')")
    probenMetadata <- dbGetQuery(db, probenMetadataQuery)
    progress$inc(1)

    #
    # get observed property information
    # SELECT observableproperty.identifier, observableproperty.name
    # FROM observableproperty
    # WHERE observableproperty.identifier IN ('Blei','Amonium')
    #
    parameterIndex <- match(reqColData$obsProp, reqColData)
    parameter <- Messungen_data[,parameterIndex]
    parameterQuerySection <- paste0(unique(parameter), collapse = "','")
    observedpropertiesQuery <- paste0("SELECT
                                      observablepropertyid,
                                      identifier,
                                      name
                                    FROM
                                      observableproperty
                                    WHERE
                                      identifier IN ('",
                                      parameterQuerySection,
                                      "')")
    observedproperties <- dbGetQuery(db, observedpropertiesQuery)
    progress$inc(1)

    #
    # get PNS and Ort
    #
    # FIXME where to add DISTINCT in the queries
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
    # FIXME col001 and col002 musst be replaced by reqColOrt$lat & reqColOrt$lon
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
                         pns_data.col001 AS pns_lat,
                         pns_data.col002 AS pns_lon,
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

    feedTmpConfigDirectory <- tempdir(check = TRUE)
    #
    # Create global CSV file
    #
    feedCSV <- tempfile(pattern = "feed-csv-", feedTmpConfigDirectory, fileext = ".csv")
    #
    # Create the global configuraton file
    #
    feedConf <- tempfile(pattern = "feed-",  feedTmpConfigDirectory, fileext = "-config.xml")

    writeLines(createFeederConfiguration(csvPath = feedCSV), feedConf)
    progress$inc(1)

    feedDataContent <- "Parameter;Wert;Einheit;sensor-id;resultTime;phenStart;phenEnd;foiIdentifier;lat;lon"
    for (i in 1:nrow(Messungen_data)) {
      row <- Messungen_data[i,]

      #
      # Fill probe_parameter table
      # 1        2         3    4       5  6
      # ProbenID;Parameter;Wert;Einheit;BG;NG
      # probe_id (in CSV, col#1), parameter_id, pp_unit (in CSV, col), bg, ng
      # TODO continue here
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

      query <- paste0("WITH query_probe_id AS (
                    SELECT id as probe_id
                    FROM probe
                    WHERE identifier = '", row[1], "'
                  ),
                  query_parameter_id AS (
                    SELECT observablepropertyid as para_id
                    FROM observableproperty
                    WHERE identifier = '", row[2], "'
                  ),
                  insert_unit AS (
                    INSERT INTO unit (unitid, unit)
                    VALUES(nextval('unitid_seq'),'", row[4], "')
                    ON CONFLICT (unit) DO UPDATE SET unit = '", row[4], "'
                    RETURNING unitid as unit_id
                  )
                  INSERT INTO probe_parameter
                  SELECT query_probe_id.probe_id, query_parameter_id.para_id, insert_unit.unit_id, '", row[5], "', '", row[6], "'
                  FROM query_probe_id, query_parameter_id, insert_unit
                  ON CONFLICT ON CONSTRAINT probe_parameter_pkey
                      DO UPDATE SET 
                          pp_unit = (SELECT unit_id FROM insert_unit),
                          bg = ", row[5],
                          ", ng = ", row[6],
                      " WHERE probe_parameter.probe_id = (SELECT probe_id FROM query_probe_id)
                      AND probe_parameter.parameter_id = (SELECT para_id FROM query_parameter_id);")
      dbExecute(db, query)
      newDataRow <- paste(row[2], # Parameter
                          row[3], # Wert
                          row[4], # Einheit
                          paste0(probenMetadata[is.element(probenMetadata$probeid, row[1]),3],"_", observedproperties[is.element(observedproperties$identifier, row[2]),2]), # SensorId
                          gsub(" CEST", "", probenMetadata[is.element(probenMetadata$probeid, row[1]),4]), # resultTime
                          gsub(" CEST", "", probenMetadata[is.element(probenMetadata$probeid, row[1]),5]), # phenTimeStart
                          gsub(" CEST", "", probenMetadata[is.element(probenMetadata$probeid, row[1]),6]), # phenTimeEnd
                          features[is.element(features$pns_id, probenMetadata[is.element(probenMetadata$probeid, row[1]),2]),4], # foiIdentifier
                          features[is.element(features$pns_id, probenMetadata[is.element(probenMetadata$probeid, row[1]),2]),2], # Lat
                          features[is.element(features$pns_id, probenMetadata[is.element(probenMetadata$probeid, row[1]),2]),3], # Lon
                          sep=";")
      feedDataContent <- paste0(feedDataContent, "\n", newDataRow)

      progress$inc(1)
    }

    #
    # write global csv file
    #
    writeLines(feedDataContent, feedCSV)
    progress$inc(1)

    system2("java",
            args = c("-jar", feederPath, "-c", feedConf),
            timeout = 300)
    progress$inc(1)

    sosCacheUpdate(wait=1)
    progress$inc(1)

    progress$close()
  }

  showModal(modalDialog(
    title = "Vorgang abgeschlossen",
    "Die Messdaten wurden erfolgreich in der Datenbank angelegt.",
    footer = modalButton("Ok")
  ))
}, ignoreInit=TRUE)
