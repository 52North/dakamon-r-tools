################################################################################
###################################          ###################################
###################################   DATA   ###################################
###################################          ###################################
################################################################################

## tools
confInit <- function(url=SOSWebApp, csvPath="~/GitRepos/dakamon_r-tools/Daten/KA2017_03938_BG.csv") {
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

confColumnDef <- function(colId=3, colRClass, 
                          FoITempRef, obsPropTempRef, sensorTempRef, uomTempRef,
                          BGlabel=BGlabel,
                          BGvalue) {
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

# reactive variables
inCSVData <- reactiveValues()
valiData <- reactiveValues(validated = FALSE)
CheckDBData <- reactiveValues(checked = FALSE)

rowSkip <- reactive(max(as.numeric(c(input$dataStgr, input$dataBG, input$dataUoM))))

## data logic
# toAdd: validate UoM?
# add Button "check DB consistency" -> list known vs new variables; matching vs non-matching uom ( -> hottable to fix?) -> manually update csv file
# check Identifier: mark missing identifier!

observeEvent(input$dataUoM, {
  if (!is.null(input$dataCsvFile$datapath)) {
    inCSVData$UoMs <- read.csv(input$dataCsvFile$datapath, header = FALSE,
                               sep = input$dataSep, dec = input$dataDec,
                               skip = as.numeric(input$dataUoM), nrows = 1,
                               stringsAsFactors = FALSE)
    inCSVData$UoMs[is.na(inCSVData$UoMs)] <- ""
    inCSVData$UoMs <- as.character(inCSVData$UoMs)
    
    inCSVData$df <- read.csv(input$dataCsvFile$datapath, header = FALSE,
                             sep = input$dataSep, dec = input$dataDec,
                             skip = rowSkip()+1,
                             stringsAsFactors = FALSE)
    colnames(inCSVData$df) <- inCSVData$headAsChar
  }
})

observeEvent(input$dataBG, {
  if (!is.null(input$dataCsvFile$datapath)) {
    inCSVData$bg <- as.character(read.csv(input$dataCsvFile$datapath,
                                          header = FALSE,
                                          sep = input$dataSep, dec = input$dataDec,
                                          skip = as.numeric(input$dataBG), nrows = 1, 
                                          stringsAsFactors = FALSE))
    
    inCSVData$df <- read.csv(input$dataCsvFile$datapath, header = FALSE,
                             sep = input$dataSep, dec = input$dataDec,
                             skip = rowSkip()+1,
                             stringsAsFactors = FALSE)
    colnames(inCSVData$df) <- inCSVData$headAsChar
  }
})

observeEvent(input$dataStgr, {
  if (!is.null(input$dataCsvFile$datapath)) {
    inCSVData$stgr <- as.character(read.csv(input$dataCsvFile$datapath,
                                            header = FALSE,
                                            sep = input$dataSep, dec = input$dataDec,
                                            skip = as.numeric(input$dataStgr), nrows = 1, 
                                            stringsAsFactors = FALSE))
    
    inCSVData$df <- read.csv(input$dataCsvFile$datapath, header = FALSE,
                             sep = input$dataSep, dec = input$dataDec,
                             skip = rowSkip()+1,
                             stringsAsFactors = FALSE)
    colnames(inCSVData$df) <- inCSVData$headAsChar
  }
})

observeEvent(input$dataCsvFile, {
  valiData$validated <- FALSE
  CheckDBData$checked <- FALSE
  
  inCSVData$headAsChar <- as.character(read.csv(input$dataCsvFile$datapath,
                                                header = FALSE,
                                                sep = input$dataSep, dec = input$dataDec,
                                                nrows = 1, stringsAsFactors = FALSE))
  
  inCSVData$UoMs <- read.csv(input$dataCsvFile$datapath, header = FALSE,
                             sep = input$dataSep, dec = input$dataDec,
                             skip = as.numeric(input$dataUoM), nrows = 1,
                             stringsAsFactors = FALSE)
  inCSVData$UoMs[is.na(inCSVData$UoMs)] <- ""
  inCSVData$UoMs <- as.character(inCSVData$UoMs)
  
  inCSVData$bg <- as.character(format(read.csv(input$dataCsvFile$datapath,
                                        header = FALSE,
                                        sep = input$dataSep, dec = input$dataDec,
                                        skip = as.numeric(input$dataBG), nrows = 1, 
                                        stringsAsFactors = FALSE), scientific=FALSE))
  
  inCSVData$stgr <- as.character(read.csv(input$dataCsvFile$datapath,
                                          header = FALSE,
                                          sep = input$dataSep, dec = input$dataDec,
                                          skip = as.numeric(input$dataStgr), nrows = 1, 
                                          stringsAsFactors = FALSE))
  
  
  inCSVData$df <- read.csv(input$dataCsvFile$datapath, header = FALSE,
                           sep = input$dataSep, dec = input$dataDec,
                           skip = rowSkip()+1,
                           stringsAsFactors = FALSE)
  colnames(inCSVData$df) <- inCSVData$headAsChar
  
  #################################
  ## validation of data csv-file ##
  #################################
  # ID
  # Proben-Nr
  # Datum
  
  txt <- NULL
  if (!(reqColData$id %in% inCSVData$headAsChar))
    txt <- paste0(txt, "<li>A pre-registered identifier is mandatory for each row; please supply a non-empty column 'ID'.</li>")
  if (!(reqColData$probeId %in% inCSVData$headAsChar))
    txt <- paste0(txt, "<li>A probe number is mandatory (yet, it might be empty); please supply a column '", reqColData$probeId, "'.</li>")
  if (!(reqColData$date %in% inCSVData$headAsChar))
    txt <- paste0(txt, "<li>A date is mandatory for each row; please supply a non-empty column '", reqColData$date, "'.</li>")
  if(length(unique(inCSVData$headAsChar)) != length(inCSVData$headAsChar))
    txt <- paste0(txt, "<li>Column names must be unique.</li>")
  
  valiData$txt <- txt
  valiData$validated <- TRUE
})

output$dataValidationOut <- renderUI({
  if (valiData$validated) {
    if (is.null(valiData$txt)) {
      actionButton("dataCheckDB", "Check DB consistency!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiData$txt, "</ul></div></html"))
    }
  } else {
    return()
  }
})

###########################
## DB consistency checks ##
###########################
# check for FoI, phenomenon and date -> to avoid exception in the DB; if input$dataOW -> remove entry in DB and re-insert
# check for identical Stoffgruppe in 'observablepropertyrelation'; Stoffgruppe is an observable property and a parentfeature of the observableproperties
#   a new Stoffgruppe needs to be inserted after the insertion of the data into tables 'observableproperties' and 'observeablepropertyrelation'
#   if the observeableproperty already is in the DB, the Stoffgruppe field is overwritten if input$dataOW if !input$dataOW -> no upload!
# check for identical UoMs: 
#   missmatch -> no upload!
# check for identical BG:
#   missmatch -> no upload!

observeEvent(input$dataCheckDB, {
  db <- dbConnect("PostgreSQL", host="localhost", dbname="sos", user="postgres", password="postgres", port="5432")
  on.exit(dbDisconnect(db), add=T)
  
  # check whether all FoIs are already in the DB
  FoIinDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('", 
                                   paste(inCSVData$df$ID, collapse="', '"),"')"))
  foiInCSV <- inCSVData$df$ID
  
  missFoI <- foiInCSV[!(foiInCSV %in% FoIinDB$identifier)]
  
  if (length(missFoI) > 0) {
    CheckDBData$txtErr <- paste("The following features are not yet in the DB: <ul><li>",
                                paste0(missFoI, collapse="</li><li>"))
  } else {
    checkDB$txtErr <- NULL
  }
  
  # loop over columns, querry data for each FoI and Date, store presence/absence per column and row
  inCSVData$obsInDB <- NULL
  
  progress <- Progress$new()
  on.exit(progress$close())
  
  progress$set(message = "Checking DB!", value = 0)
  
  nRowDf <- nrow(inCSVData$df)
  nColDf <- ncol(inCSVData$df)
  
  obsIdsInDB <- NULL
  
  for (colDf in 4:nColDf) { # colDf <- 5
    colVec <- rep(0, nRowDf)
    
    progress$inc(1/(nColDf-3), paste(detail="Checking column", colDf))
    
    # querry Stoffgruppe
    opIdPhen <- dbGetQuery(db, paste0("SELECT observablepropertyid, name FROM observableproperty WHERE identifier = '", inCSVData$headAsChar[colDf], "'"))
    if (nrow(opIdPhen) == 1) {
      # check for opIdPhen being mentioned in observablepropertyrelation
      opIdsRel <- dbGetQuery(db, paste0("SELECT parentobservablepropertyid, childobservablepropertyid FROM observablepropertyrelation WHERE childobservablepropertyid = ", opIdPhen$observablepropertyid))
      if (nrow(opIdsRel) == 1) {
        stgrInDB <- dbGetQuery(db, paste0("SELECT name FROM observableproperty WHERE observablepropertyid = ", opIdsRel$parentobservablepropertyid))$name
      } else {
        stgrInDB <- ""
      }
    }
    
    for (i in 1:nRowDf) { # i <- 1
      phenTime <- paste0(as.character(as.Date(inCSVData$df[i, "Datum"], format = "%m/%e/%Y")), stndTime)
      
      # request observations from SOS
      
      # dbGetQuery(db, paste0("SELECT observationid FROM observation WHERE observablepropertyid = ", opIdsRel$parentobservablepropertyid))$name
      
      insMsg <- fromJSON(rawToChar(POST(paste0(SOSWebApp, "service"), 
                                        body = SOSreqObs(FoI=inCSVData$df$ID[i],
                                                         obsProp=inCSVData$headAsChar[colDf],
                                                         phenTime=phenTime),
                                        content_type_xml(), accept_json())$content))
      
      # querry UoM
      curDBUoM <- dbGetQuery(db, paste0("SELECT unit 
      FROM unit AS u
      LEFT OUTER JOIN series AS s ON (u.unitid = s.unitid)
      LEFT OUTER JOIN observableproperty AS op ON (s.observablepropertyid = op.observablepropertyid)
      LEFT OUTER JOIN featureofinterest AS foi ON (s.featureofinterestid = foi.featureofinterestid)
      WHERE op.name = '", inCSVData$headAsChar[colDf], "' AND foi.identifier = '", inCSVData$df$ID[i], "'"))
      
      # querry BG
      # TODO
      
      if (is.null(insMsg$exceptions)) {
        if (length(insMsg$observations) > 0) {
          colVec[i] <- 1
          obsIdsInDB <- c(obsIdsInDB, insMsg$observations[[1]]$identifier$value)
          
          if (length(curDBUoM$unit) > 0) {
            if (inCSVData$UoMs[colDf] != curDBUoM$unit)
              colVec[i] <- 2
          }
          
          inCSVData$stgr[colDf][is.na(inCSVData$stgr[colDf]) || inCSVData$stgr[colDf] == "NA"] <- ""
          if (inCSVData$stgr[colDf] != stgrInDB)
            colVec[i] <- 3
        }
      }
    }
    
    inCSVData$obsInDB <- cbind(inCSVData$obsInDB, colVec)
  }
  inCSVData$obsIdsInDB <- obsIdsInDB
  
  CheckDBData$checked <- TRUE
}, ignoreInit=TRUE)

output$dataDBConsistencyActionOut <- renderUI({
  if (CheckDBData$checked) {
    if (!is.null(CheckDBData$txtErr)) {
      return( HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", CheckDBData$txtErr, "</li></ul></div></html")))
    }
    if (all(inCSVData$obsInDB < 2)) {
      if (!any(inCSVData$obsInDB > 0) || input$dataOW) {
        actionButton("dataStoreDB", "Store in DB!")
      } else {
        HTML("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">Some observations are already in the DB (see yellow cells). Check the box above to overwrite the data in the data base.</div></html>")
      }
    } else {
      if (any( inCSVData$obsInDB == 3)) {
        HTML("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">Group of elements differ in csv and data base (see blue cells).</div></html>")
      } else {
        HTML("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">Detection limit and/or unit of measurement differ in csv and data base (see red cells).</div></html>")
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
    
    if (!is.null(inCSVData$UoMs)) {
      showUoM <- sapply(inCSVData$UoMs, function(x) {
        if (!is.na(x) & nchar(x) > 0  & x != "NA") {
          paste0(" [",x,"]")
        } else {
          ""
        }
      })
      if (!is.null(inCSVData$df))
        showHead <- paste0(showHead, showUoM)
    }
    
    if (!is.null(inCSVData$bg)) {
      showBg <- sapply(inCSVData$bg, function(x) {
        if (!is.na(x) & nchar(x) > 0 & x != "NA") {
          paste0("<br> BG: ", x)
        } else {
          "<br>"
        }
      })
      if (!is.null(inCSVData$df))
        showHead <- paste0(showHead, showBg)
    }
    
    if (!is.null(inCSVData$stgr)) {
      showStgr <- sapply(inCSVData$stgr, function(x) {
        if (!is.na(x) & nchar(x) > 0  & x != "NA") {
          paste0("<br>", x)
        } else {
          "<br>"
        }
      })
      if (!is.null(inCSVData$df))
        showHead <- paste0(showHead, showStgr)
    }
    
    showHead <- paste0(showHead, "</span>")
    
    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sort=FALSE),
                        escape=FALSE)
    
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
#   - run: java -jar 52n-sos-importer-feeder-bin.jar -c lab_config.xml
# store Stoffgruppen

observeEvent(input$dataStoreDB, {
  if (!is.null(inCSVData$df)) {
    if (input$dataOW & !is.null(inCSVData$obsIdsInDB)) {
      
      # delete observations already in the DB
      progress <- Progress$new()
      progress$set(message = "Preparing DB.", value = 0)
      
      for (id in inCSVData$obsIdsInDB) {
        progress$inc(1/length(inCSVData$obsIdsInDB))
        POST(paste0(SOSWebApp, "service"), 
             body = SOSdelObsByID(id),
             content_type_xml(), accept_json())
      }
      progress$close()
    } 
    
    feedTab <- inCSVData$df
    
    confColTxt <- NULL
    confObsPropTxt <- NULL
    confUomTxt <- NULL
    
    # prepare common conf part
    for (i in 4:ncol(feedTab)) {
      colVec <- feedTab[,i]
      
      # clean detection limits an
      if (any(colVec == input$dataBGchar, na.rm = TRUE)) 
        colVec[colVec == input$dataBGchar]  <- BGencode
      colVecNum <- as.numeric(colVec)
      if (any(!is.na(colVecNum))) {
        colVec <- colVecNum
        feedTab[,i] <- colVec
      }
      cat(confColTxt)
      confColTxt <- paste(confColTxt, 
                          confColumnDef(colId = i-2, colRClass = class(colVec), 
                                        FoITempRef = "thisFoI",
                                        obsPropTempRef = paste0("obsProp",i), 
                                        uomTempRef = paste0("uom",i), 
                                        sensorTempRef = paste0("sensor",i), 
                                        BGlabel = "Bestimmungsgrenze", BGvalue = inCSVData$bg[i]), 
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
    progress$set(message = "Uploading data into DB.", value = 0)
    
    # loop over unique FoI
    uFoIs <- unique(feedTab[,reqColData$id])
    nUFoIs <- length(uFoIs)
    
    for (uFoI in uFoIs) {
      progress$inc(1/nUFoIs)
      
      confSensorTxt <- NULL
      confFoITxt <- confFoIManualDef("thisFoI", uFoI, uFoI)
      
      for (i in 4:ncol(feedTab)) {
        colVec <- feedTab[,i]
        confSensorTxt <- paste(confSensorTxt,
                               confSensorManualDef(sensorTempRef = paste0("sensor",i), 
                                                   foiURI = uFoI, foiName = uFoI,
                                                   obsPropURI = inCSVData$headAsChar[i], obsPropName = inCSVData$headAsChar[i]), 
                               sep="\n")
      }
      
      feedCSV <- tempfile(pattern = "feedCSV", fileext = ".csv")
      
      write.table(feedTab[feedTab[,reqColData$id] == uFoI,-1], feedCSV, sep = input$dataSep, dec = input$dataDec, row.names = FALSE, col.names=TRUE, fileEncoding="UTF-8")
      
      feedConf <- tempfile(pattern = "feedConf", fileext = ".xml")
      
      writeLines(paste(confInit(SOSWebApp, csvPath = feedCSV),
                       confCsvMetaInit(),
                       confColTxt,
                       confCsvMetaClose(decSep = input$dataDec, 
                                        skipRows = 0,
                                        colSep = input$dataSep),
                       confAddMetaInit(),
                       confSensorTxt,
                       confObsPropTxt,
                       confFoITxt,
                       confUomTxt,
                       confAddMetaClose(),
                       sep="\n"), feedConf)
      
      system(paste0("java -jar ", feederPath, " -c ", feedConf))
    }
    
    progress$close()

    ## add Stoffgruppe and link observablepropertyrelation
    # remove missing or "NA"
    inCSVData$stgr[inCSVData$stgr == "" | inCSVData$stgr == "NA"] <- NA
    
    progress <- Progress$new()
    progress$set(message = "Registering element groups in DB.", value = 0)
    
    nColDf <- ncol(inCSVData$df)
    
    # fill observablepropertyrelation table
    for (colDf in 4:nColDf) { # colDf <- 4
      progress$inc(1/(nColDf-3))
      
      # find observablepropertyids
      opIdPhen <- dbGetQuery(db, paste0("SELECT observablepropertyid, name FROM observableproperty WHERE name = '", inCSVData$headAsChar[colDf], "'"))
      if (nrow(opIdPhen) == 0) {
        message(paste0("The following observable property is missing in the DB: ",  inCSVData$headAsChar[colDf]))
        next;
      }
      # check for opIdOehn being mentioned in relation
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
    progress$close()
  }
  
  showModal(modalDialog(
    title = "Upload completed.",
    "Time series upload completed.",
    easyClose = TRUE,
    footer = NULL
  ))
}, ignoreInit=TRUE)