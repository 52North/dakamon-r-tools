################################################################################
###########################   Upload der Messungen   ###########################
################################################################################

## tools
confInit <- function(url=SOSWebApp, csvPath) {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
         <SosImportConfiguration xsi:schemaLocation=\"https://raw.githubusercontent.com/52North/sos-importer/master/bindings/src/main/resources/import-configuration.xsd\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns=\"http://52north.org/sensorweb/sos/importer/0.5/\">
         <DataFile referenceIsARegularExpression=\"false\">
         <LocalFile>
         <Path>", csvPath, "</Path>
         <Encoding>UTF-8</Encoding>
         </LocalFile>
         </DataFile>
         <SosMetadata>
         <URL>", url, "service</URL>
         <Offering generate=\"true\"/>
         <Version>2.0.0</Version>
         <Binding>POX</Binding>
         <Importer>org.n52.sos.importer.feeder.importer.SweArrayObservationWithSplitExtensionImporter</Importer>
         </SosMetadata>")
}

confCsvMetaInit <- function() {
  paste0("<CsvMetadata>
         <ColumnAssignments>
         <Column>
         <Number>0</Number>
         <Type>OM_PARAMETER</Type>
         <Metadata>
         <Key>TYPE</Key>
         <Value>TEXT</Value>
         </Metadata>
         <Metadata>
         <Key>NAME</Key>
         <Value>Proben-Nummer</Value>
         </Metadata>
         </Column>
         <Column>
         <Number>1</Number>
         <Type>DATE_TIME</Type>
         <Metadata>
         <Key>PARSE_PATTERN</Key>
         <Value>M/d/yyyy</Value>
         </Metadata>
         <Metadata>
         <Key>TYPE</Key>
         <Value>COMBINATION</Value>
         </Metadata>
         <Metadata>
         <Key>TIME_ZONE</Key>
         <Value>0</Value>
         </Metadata>
         <Metadata>
         <Key>TIME_HOUR</Key>
         <Value>12</Value>
         </Metadata>
         <Metadata>
         <Key>TIME_MINUTE</Key>
         <Value>0</Value>
         </Metadata>
         <Metadata>
         <Key>TIME_SECOND</Key>
         <Value>0</Value>
         </Metadata>
         <Metadata>
         <Key>GROUP</Key>
         <Value>1</Value>
         </Metadata>
         </Column>")
}

confColumnAssignment <- function(colId=3, colRClass,
                          FoITempRef, obsPropTempRef, sensorTempRef, uomTempRef,
                          BGlabel=BGlabel,
                          BGvalue=BGencode,
                          NGlabel=NGlabel,
                          NGvalue=NGencode) {
  paste0("<Column>
         <Number>", colId, "</Number>
         <Type>MEASURED_VALUE</Type>
         <Metadata>
           <Key>TYPE</Key>
           <Value>", switch(colRClass, numeric="NUMERIC", integer="NUMERIC", character="TEXT", factor="TEXT", message("unknown type in column definition")), "</Value>
         </Metadata>
         <RelatedFOI>
            <IdRef>", FoITempRef, "</IdRef>
         </RelatedFOI>
         <RelatedObservedProperty>
           <IdRef>", obsPropTempRef, "</IdRef>
         </RelatedObservedProperty>
         <RelatedSensor>
           <IdRef>", sensorTempRef, "</IdRef>
         </RelatedSensor>
         <RelatedUnitOfMeasurement>
           <IdRef>", uomTempRef, "</IdRef>
         </RelatedUnitOfMeasurement>
         <RelatedReferenceValue>
           <Label>", BGlabel, "</Label>
           <Value>", BGvalue, "</Value>
         </RelatedReferenceValue>
         </Column>")
}

confCsvMetaClose <- function(decSep, skipRows, comInd="#",
                             colSep, txtInd="\"", header=FALSE,
                             obsCol="org.n52.sos.importer.feeder.collector.DefaultCsvCollector") {
  paste0("</ColumnAssignments>
         <DecimalSeparator>", decSep, "</DecimalSeparator>
         <FirstLineWithData>", skipRows, "</FirstLineWithData>
         <Parameter>
         <CommentIndicator>", comInd, "</CommentIndicator>
         <ColumnSeparator>", colSep, "</ColumnSeparator>
         <TextIndicator>", txtInd, "</TextIndicator>
         </Parameter>
         <ObservationCollector>", obsCol, "</ObservationCollector>
         </CsvMetadata>")
}

confAddMetaInit <- function() {
  paste0("<AdditionalMetadata>
         <Metadata>
         <Key>HUNK_SIZE</Key>
         <Value>5</Value>
         </Metadata>
         <Metadata>
         <Key>TIMEOUT_BUFFER</Key>
         <Value>50000</Value>
         </Metadata>")
}

confSensorManualDef <- function(sensorTempRef,
                                foiURI, foiName,
                                obsPropURI, obsPropName) {
  paste0("<Sensor>
    <ManualResource>
      <ID>", sensorTempRef, "</ID>
      <URI>", paste(foiURI, obsPropURI, sep="-"), "</URI>
      <Name>", paste(obsPropName, foiName, sep=" at "), "</Name>
    </ManualResource>
  </Sensor>
")
}

confSensorDef <- function(sensorTempRef, obsPropName) {
  paste0("<Sensor>
    <GeneratedResource>
      <ID>", sensorTempRef, "</ID>
      <Number>0</Number>
      <URI useAsPrefix=\"true\">", obsPropName, "_</URI>
      <ConcatString>_</ConcatString>
    </GeneratedResource>
  </Sensor>
")
}

confObsPropDef <- function(obsPropTempRef, obsPropURI, obsPropName){
  paste0("<ObservedProperty>
    <ManualResource>
      <ID>", obsPropTempRef, "</ID>
      <URI>", obsPropURI, "</URI>
      <Name>", obsPropName, "</Name>
    </ManualResource>
  </ObservedProperty>")
}

confFoIManualDef <- function(FoITempRef, FoIURI, FoIName){
  paste0("<FeatureOfInterest>
    <ManualResource>
      <ID>", FoITempRef, "</ID>
      <URI>", FoIURI, "</URI>
      <Name>", FoIName, "</Name>
    </ManualResource>
  </FeatureOfInterest>")
}

confUomDef <- function(uomTempRef, uomName, uomURI) {
  paste0("<UnitOfMeasurement>
      <ManualResource>
        <ID>", uomTempRef, "</ID>
        <URI>", uomURI, "</URI>
        <Name>", uomName, "</Name>
      </ManualResource>
    </UnitOfMeasurement>")
}

confAddMetaClose <- function() {
  "</AdditionalMetadata>
</SosImportConfiguration>"
}

## /tools

dataSeparator <- colSep
dataDecimalSeparator <- decSep

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

    confColumnAssignments <- NULL
    confObsPropTxt <- NULL
    confUomTxt <- NULL

    valueIndex <- match(reqColData$value, reqColData)
    # convert value column BG, NG values in numbers and the whole column in numeric values
    if (any(Messungen_data[,valueIndex] == BGchar, na.rm = TRUE)) {
      column <- Messungen_data[,valueIndex]
      column[column == BGchar] <- BGencode
      Messungen_data[,valueIndex] <- column
    }
    if (any(Messungen_data[,valueIndex] == NGchar, na.rm = TRUE)) {
      column <- Messungen_data[,valueIndex]
      column[column == NGchar] <- NGencode
      Messungen_data[,valueIndex] <- column
    }
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
    # FIXME col033 and col034 musst be replaced by reqColProbe$LabId & reqColProbe$LabName
    sensors <- dbGetQuery(db, paste0("SELECT probe.identifier AS probeid, probe.pns_id, probe.col033 AS sensorId, probe.col034 AS sensorName
                                    FROM probe
                                    WHERE probe.identifier IN ('",
                                    probenQuerySection,
                                    "')"))
    #
    # get observed property information
    # SELECT observableproperty.identifier, observableproperty.name
    # FROM observableproperty
    # WHERE observableproperty.identifier IN ('Blei','Amonium')
    #
    parameterIndex <- match(reqColData$obsProp, reqColData)
    parameter <- Messungen_data[,parameterIndex]
    parameterQuerySection <- paste0(unique(parameter), collapse = "','")
    observedpropertiesQuery <- paste0("SELECT observablepropertyid, observableproperty.identifier, observableproperty.name
                                    FROM observableproperty
                                    WHERE observableproperty.identifier IN ('",
              parameterQuerySection,
              "')")
    observedproperties <- dbGetQuery(db, observedpropertiesQuery)
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
    for (i in 1:nrow(Messungen_data)) {
      row <- Messungen_data[i,]

      #
      # Fill probe_parameter table
      #
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


      #
      # CREATE FEEDER CONFIGURATIONS
      #
      # each row requires its own configuration
      #
      cat(confColumnAssignments)
      confColumnAssignments <- paste(confColumnAssignments,
                          confColumnAssignment(colId = i-2,
                                               colRClass = class(columns),
                                               FoITempRef = "thisFoI",
                                               obsPropTempRef = paste0("obsProp",i),
                                               uomTempRef = paste0("uom",i),
                                               sensorTempRef = paste0("sensor",i),
                                               BGlabel = BGlabel,
                                               BGvalue = columns),
                          sep="\n")

      confObsPropTxt <- paste(confObsPropTxt,
                              confObsPropDef(obsPropTempRef = paste0("obsProp",i),
                                             obsPropURI = inCSVData$headAsChar[i], obsPropName = inCSVData$headAsChar[i]),
                              sep="\n")

      confUomTxt <-paste(confUomTxt,
                         confUomDef(uomTempRef = paste0("uom",i),
                                    uomName = inCSVData$UoMs[i],
                                    uomURI = inCSVData$UoMs[i]),
                         sep="\n")


    }

    progress <- Progress$new()
    progress$set(message = "Lade Daten in DB.", value = 0)

    # loop over unique FoI
    uFoIs <- unique(Messungen_data[,reqColData$id])
    nUFoIs <- length(uFoIs)

    for (uFoI in uFoIs) {
      progress$inc(1/nUFoIs)

      confSensorTxt <- NULL
      confFoITxt <- confFoIManualDef("thisFoI", uFoI, uFoI)

      for (i in 4:ncol(Messungen_data)) {
        columns <- Messungen_data[,i]
        confSensorTxt <- paste(confSensorTxt,
                               confSensorManualDef(sensorTempRef = paste0("sensor",i),
                                                   foiURI = uFoI, foiName = uFoI,
                                                   obsPropURI = inCSVData$headAsChar[i], obsPropName = inCSVData$headAsChar[i]),
                               sep="\n")
      }

      feedTmpConfigDirectory <- tempdir(check = TRUE)

      feedCSV <- tempfile(pattern = "feed-csv-", feedTmpConfigDirectory, fileext = ".csv")

      write.table(Messungen_data[Messungen_data[,reqColData$id] == uFoI,-1], feedCSV,
                  sep = dataSeparator, dec = dataDecimalSeparator,
                  row.names = FALSE, col.names=TRUE,
                  fileEncoding="UTF-8")

      feedConf <- tempfile(pattern = "feed-",  feedTmpConfigDirectory, fileext = "-config.xml")

      writeLines(paste(confInit(SOSWebApp, csvPath = feedCSV),
                       confCsvMetaInit(),
                       confColumnAssignments,
                       confCsvMetaClose(decSep = dataDecimalSeparator,
                                        skipRows = 0,
                                        colSep = dataSeparator),
                       confAddMetaInit(),
                       confSensorTxt,
                       confObsPropTxt,
                       confFoITxt,
                       confUomTxt,
                       confAddMetaClose(),
                       sep="\n"), feedConf)

    }

    if (length(uFOIs) > 0) {
      system2("java", args = c("-jar", feederPath, "-m", feedTmpConfigDirectory, "0", feedNumberOfParallelImports))
    }




    ## add Stoffgruppe and link observablepropertyrelation
    # remove missing or "NA"
    inCSVData$stgr[inCSVData$stgr == "" | inCSVData$stgr == "NA"] <- "ohne"

    progress <- Progress$new()
    progress$set(message = "Registriere Elementgruppe in DB.", value = 0)

    nColDf <- ncol(inCSVData$df)

    # fill observablepropertyrelation table
    for (colDf in 4:nColDf) { # colDf <- 4
      progress$inc(1/(nColDf-3))

      # find observablepropertyids
      opIdPhen <- dbGetQuery(db, paste0("SELECT observablepropertyid, name FROM observableproperty WHERE name = '", inCSVData$headAsChar[colDf], "'"))
      if (nrow(opIdPhen) == 0) {
        message(paste0("Folgendes 'ObserveableProperty' fehlt in der DB: ",  inCSVData$headAsChar[colDf]))
        next;
      }
      # check for opIdPhen being mentioned in relation
      opIdsRel <- dbGetQuery(db, paste0("SELECT parentobservablepropertyid, childobservablepropertyid FROM observablepropertyrelation WHERE childobservablepropertyid = ", opIdPhen$observablepropertyid))

      if (is.na(inCSVData$stgr[colDf])) {
        if (input$dataOW & nrow(opIdsRel) == 1) {
          dbSendQuery(db, paste0("DELETE FROM observablepropertyrelation WHERE childobservablepropertyid = '", opIdPhen$observablepropertyid,  "'"))
        }
        next;
      }

      opIdStgr <- dbGetQuery(db, paste0("SELECT observablepropertyid, name FROM observableproperty WHERE name = '", inCSVData$stgr[colDf], "'"))

      if (nrow(opIdsRel) == 0) {
        if (nrow(opIdStgr) == 0) {
          dbSendQuery(db, paste0("INSERT INTO observableproperty (observablepropertyid, identifier, name, description, disabled, hiddenchild)
                              VALUES (nextval('observablepropertyid_seq'), '", inCSVData$stgr[colDf], "Stgr', '", inCSVData$stgr[colDf], "', 'Stoffgruppe', 'F', 'F');"))
          opIdStgr <- dbGetQuery(db, paste0("SELECT observablepropertyid, name FROM observableproperty WHERE name = '", inCSVData$stgr[colDf], "'"))
        }
        dbSendQuery(db, paste0("INSERT INTO observablepropertyrelation (parentobservablepropertyid, childobservablepropertyid) VALUES ('", opIdStgr$observablepropertyid, "', '", opIdPhen$observablepropertyid, "')"))
      } else {
        if (input$dataOW) {
          if (nrow(opIdStgr) == 0) {
            dbSendQuery(db, paste0("INSERT INTO observableproperty (observablepropertyid, identifier, name, description, disabled, hiddenchild)
                              VALUES (nextval('observablepropertyid_seq'), '", inCSVData$stgr[colDf], "Stgr', '", inCSVData$stgr[colDf], "', 'Stoffgruppe', 'F', 'F');"))
            opIdStgr <- dbGetQuery(db, paste0("SELECT observablepropertyid, name FROM observableproperty WHERE name = '", inCSVData$stgr[colDf], "'"))
          }

          if (opIdsRel$parentobservablepropertyid != opIdStgr$observablepropertyid)
            dbSendQuery(db, paste0("UPDATE observablepropertyrelation SET parentobservablepropertyid = ", opIdStgr$observablepropertyid,
                                   " WHERE parentobservablepropertyid = '", opIdsRel$parentobservablepropertyid,"' AND childobservablepropertyid = '", opIdPhen$observablepropertyid,"'"))
        }
      }
    }

    dbSendQuery(db, "UPDATE series SET published = 'T'")
    SOScacheUpdate(wait=1)

    progress$close()
  }

  showModal(modalDialog(
    title = "Vorgang abgeschlossen",
    "Die Messdaten wurden erfolgreich in der Datenbank angelegt.",
    footer = modalButton("Ok")
  ))
}, ignoreInit=TRUE)
