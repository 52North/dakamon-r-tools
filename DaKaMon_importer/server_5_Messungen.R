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

sosCacheUpdate <- function(gmlId="tmp", wait=0.5, conf=adminConf, verbose=FALSE) {
  reloadUrl <- paste0(SOSWebApp, "admin/cache/reload")
  POST(url = reloadUrl, config=conf, body="a")
  while(isLoadingCacheUpdate(conf)) {
    print("Wait until SOS cache update finishes ...")
    Sys.sleep(wait)
  }
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
        <Key>HUNK_SIZE</Key>
        <Value>", hunkSize, "</Value>
      </Metadata>
      <Metadata>
        <Key>TIMEOUT_BUFFER</Key>
        <Value>", timeoutBuffer, "</Value>
      </Metadata>
      <Metadata>
        <Key>FEEDER_CLASS</Key>
        <Value>org.n52.sos.importer.feeder.SingleThreadFeeder</Value>
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
    probeIdIndex <- match(reqColData$probeId, reqColData)
    proben <- Messungen_data[,probeIdIndex]
    probenQuerySection <- paste0(unique(proben), collapse = "','")
    probenMetadataQuery <- paste0("SELECT DISTINCT
                                        pro.identifier AS probeid,
                                        pro.pns_id,
                                        foi.identifier AS pnsid,
                                        pro.lab_id AS sensorid,
                                        to_char(pro.resulttime::timestamp, '",  dbTimestampPattern, "') AS resultTime,
                                        to_char(pro.phenomenontimestart::timestamp, '",  dbTimestampPattern, "') AS phenTimeStart,
                                        to_char(pro.phenomenontimeend::timestamp, '",  dbTimestampPattern, "') AS phenTimeEnd
                                      FROM
                                        probe AS pro
                                      LEFT OUTER JOIN featureofinterest AS foi ON (foi.featureofinterestid = pro.pns_id)
                                      WHERE
                                        pro.identifier IN ('",
                                  probenQuerySection,
                                  "')")
    print(probenMetadataQuery)
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
    parameterIndex <- match(reqColData$obsProp, reqColData)
    parameter <- Messungen_data[,parameterIndex]
    parameterQuerySection <- paste0(unique(parameter), collapse = "','")
    observedpropertiesQuery <- paste0("SELECT DISTINCT
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
  }, error = modalErrorHandler, finally = if (!connected) poolReturn(db))
}

queryProbenParameterMetadata <- function(Messungen_data, db) {
  connected <- is.null(db)
  if (!connected) {
    db <- connectToDB()
  }
  tryCatch({
    probeIdIndex <- match(reqColData$probeId, reqColData)
    proben <- Messungen_data[,probeIdIndex]
    probenQuerySection <- paste0(unique(proben), collapse = "','")
    parameterIndex <- match(reqColData$obsProp, reqColData)
    parameter <- Messungen_data[,parameterIndex]
    parameterQuerySection <- paste0(unique(parameter), collapse = "','")
    probenParameterMetadataQuery <- paste0("SELECT DISTINCT
                                        pro.identifier As probeid,
                                        param.identifier AS paramid,
                                        u.unit,
                                        pp.bg,
                                        pp.ng
                                      FROM
                                        probe_parameter pp
                                      LEFT OUTER JOIN probe AS pro ON (pro.id = pp.probe_id)
                                      LEFT OUTER JOIN observableproperty AS param ON (param.observablepropertyid = pp.parameter_id)
                                      LEFT OUTER JOIN unit AS u ON (u.unitid = pp.pp_unit)
                                      WHERE
                                        pro.identifier IN ('",
                                           probenQuerySection,
                                      "')
                                      AND
                                        param.identifier IN ('",
                                        parameterQuerySection,
                                      "')")
    probenParameterMetadata <- dbGetQuery(db, probenParameterMetadataQuery)
  }, error = modalErrorHandler, finally = if (!connected) poolReturn(db))
}

createObsIdentifier <- function(probenMetadata, parameter) {
  #2018-07-20T00:00:00+02:002018-03-15T00:00:00+01:00/2018-03-17T00:00:00+01:00BleiKAM_BW_EPP_PS
  paste0(probenMetadata$resulttime, probenMetadata$phentimestart, "/", probenMetadata$phentimeend, parameter, probenMetadata$pnsid)
}

queryObservationInDB <- function(identifier, db) {
  connected <- is.null(db)
  if (!connected) {
    db <- connectToDB()
  }
  tryCatch({
    query <- paste0("SELECT identifier FROM observation WHERE identifier IN ('", identifier,"');")
    length(dbGetQuery(db, probenParameterMetadataQuery)) > 0
  }, error = modalErrorHandler, finally = if (!connected) poolReturn(db))
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
  tryCatch({

    # check whether the ProbeIDs exist
    probenMetadata <- queryProbenMetadata(inCSVData$df, db)

    # check whether the Parameter exist
    observedproperties <- queryParameter(inCSVData$df, db)

    # check whether the combination of ProbeId and Parameter already corresponds to some time series data
    # TODO unklar, warume es geprüft werden soll?

    # check for existing observations
    probenParameterMetadata <- queryProbenParameterMetadata(inCSVData$df, db)

    #2018-07-20T00:00:00+02:002018-03-15T00:00:00+01:00/2018-03-17T00:00:00+01:00BleiKAM_BW_EPP_PS

    # TODO continue implementation of consistency check
    # missingProben <- NULL
    # missingParameter <- NULL
    # observationInDB <- NULL
    # for(csv in 1:nrow(inCSVData$df)) {
    #   if (!inCSVData$df[csv, reqColData$probeId] %in% probenMetadata["probeid"]) {
    #     missingProben <- append(missingProben, inCSVData$df[csv, reqColData$probeId])
    #   }
    #   if (!inCSVData$df[csv, reqColData$obsProp] %in% observedproperties["identifier"]) {
    #     missingParameter <- append(missingParameter, inCSVData$df[csv, reqColData$obsProp])
    #   }
    #   if (length(missingProben) == 0 && length(missingParameter) == 0) {
    #     obsIdentifier <- createObsIdentifier(probenMetadata[match(inCSVData$df[csv, reqColData$probeId], probenMetadata["probeid"]),],
    #                                          inCSVData$df[csv, reqColData$obsProp])
    #     if (queryObservationInDB(obsIdentifier, db)) {
    #        observationInDB <- append(observationInDB, obsIdentifier)
    #     }
    #   }
    # }
    #
    # inCSVData$missingProben <- unique(missingProben)
    # inCSVData$missingParameter <- unique(missingParameter)
    CheckDBData$checked <- TRUE
  }, error = modalErrorHandler, finally = poolReturn(db))
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
    tryCatch({
      dbWithTransaction(db, {

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
        feedConf <- tempfile(pattern = "feed-",  feedTmpConfigDirectory, fileext = "-config.xml")

        writeLines(createFeederConfiguration(csvPath = feedCSV), feedConf)
        progress$inc(1)

        feedDataContent <- matrix(c("Parameter", "Wert", "Einheit", "sensor-id", "resultTime", "phenStart", "phenEnd", "foiIdentifier", "lat", "lon"), nrow=1, ncol=10)
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

          # FIXME: unit, bg and ng are optional!!!
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
                        WHERE identifier = '", row[reqColData$obsProp], "' ),",
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
          dbExecute(db, query)
          newDataRow <- c(row[reqColData$obsProp], # Parameter
                              row[reqColData$value], # Wert
                              row[reqColData$uom], # Einheit
                              paste0(probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"sensorid"],"_", observedproperties[is.element(observedproperties$identifier, row[reqColData$obsProp]),"identifier"]), # SensorId
                              probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"resulttime"], # resultTime
                              probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"phentimestart"], # phenTimeStart
                              probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"phentimeend"], # phenTimeEnd
                              features[is.element(features$pns_id, probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"pns_id"]),"pns_identifier"], # foiIdentifier
                              features[is.element(features$pns_id, probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"pns_id"]),"pns_lat"], # Lat
                              features[is.element(features$pns_id, probenMetadata[is.element(probenMetadata$probeid, row[reqColData$probeId]),"pns_id"]),"pns_lon"]  # Lon
                              )
          feedDataContent <- rbind(feedDataContent, newDataRow)

          progress$inc(1)
        }
        
        progress$inc(1)
        sosCacheUpdate(wait=1)
        progress$inc(1)

        #
        # write global csv file
        #
        print(paste("writing import data to: ", feedCSV))
        write.table(feedDataContent, file = feedCSV, sep = colSep, dec = decSep, fileEncoding = "UTF-8", row.names = FALSE, col.names = FALSE)
        print("Done!")
        progress$inc(1)

        if (!file.exists(feederPath)) {
          print(paste("Feeder path does not exist:", feederPath))
          showModalMessage(title="Fehler", "Feeder nicht gefunden!")
        } else {
          tryCatch({
            print(paste("Feeding config: ", feedConf))
            print(paste("Start feeding data values from: ", feedCSV))
            logFile <- tempfile(pattern = "feed-",  feedTmpConfigDirectory, fileext = ".log")
            print(paste("Logfile: ", logFile))
            system2("/usr/bin/java", args = c(paste0("-DDAKAMON_LOG_FILE=", logFile), "-jar", feederPath, "-c", feedConf), stdout = FALSE, stderr = FALSE, wait = FALSE)
            # cmd <- paste0("/usr/bin/java -DDAKAMON_LOG_FILE=", logFile, " -jar ", feederPath, " -c ", feedConf)
            # system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE, intern = FALSE)
            Sys.sleep(1)
            check <- system(paste0("ps aux | grep -v grep | grep ", logFile, " | wc -l"), intern = TRUE)
            while (check == 1) {
              check <- system(paste0("ps aux | grep -v grep | grep ", logFile, " | wc -l"), intern = TRUE)
            }
            print("Done!")
            progress$inc(1)
            result <- read_lines(logFile, locale = locale())

            if (length(grep("Exception", result, value = TRUE)) > 0) {
              print("Errors occured during import! Consult importer logs.")
              content <- div("Some Text hier",
                pre(style='overflow-y: scroll; max-height: 200px; font-family: monospace; font-size: 75%', paste0(result, collapse = "\n")))
              showModalMessage(title="Fehler", content, size = "l")
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
      })
    }, error = modalErrorHandler, finally = poolReturn(db))
  }
}, ignoreInit=TRUE)
