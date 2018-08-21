## viewer server Messdaten

#########
# Tab 1 #
#########

db <- connectToDB()

# load all Entwässerungssysteme from DB
ews <- dbGetQuery(db, "SELECT DISTINCT col013 FROM ort_data")
output$ewsSelInput <- renderUI(selectInput("ews", "Entwässerungssystem", ifelse(nrow(ews) == 0, "keine Daten", ews$col013)))

# load all super FoI from DB
ort <- dbGetQuery(db, "SELECT foi.featureofinterestid, foi.name, foi.identifier 
                  FROM featureofinterest foi, ort_data AS od
                  WHERE foi.featureofinterestid = od.featureofinterestid")
dbDisconnect(db)

# if any
if (nrow(ort) > 0) {
  db <- connectToDB()
  
  ortDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('ort', 'global')"))
  ortDataOrtMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('ort')"))
  ortColColumns <- paste0("od.", grep("col*", ortDataOrtMetaData$columnid, value = TRUE))
  ortData <- dbGetQuery(db, paste0("SELECT foi.featureofinterestid, foi.identifier, foi.name, ", paste0(ortColColumns, collapse=", "),
                                   " FROM featureofinterest foi
                                   RIGHT OUTER JOIN ort_data od ON foi.featureofinterestid = od.featureofinterestid
                                   WHERE foi.featureofinterestid IN (", 
                                   paste0(ort$featureofinterestid, collapse=", "), ")"))
  
  
  if (nrow(ortData) > 0)
    colnames(ortData) <- ortDataMetaData$dede[match(colnames(ortData), ortDataMetaData$columnid)]
  
  showTab <- ortData[,-1]
  
  showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
  
  showHead <- paste0(showHead, "</span>")
  
  output$tableOrt  <- renderDT(datatable(showTab, colnames = showHead, 
                                         filter="top",
                                         options = list(paging=FALSE, dom = 'Brt',
                                                        language=list(url = lngJSON)),
                                         escape=FALSE))
  
  sOrt <- reactive({
    sr <- input$tableFoi_rows_selected
    if(is.null(sr)) {
      input$tableFoi_rows_all
    } else {
      sort(sr)
    }
  })
  
  output$selTextOrt <- renderText({
    if (length(sOrt()) == 1) {
      paste("Zeile", sOrt(), "ist ausgewählt.")
    } else {
      paste("Zeilen", paste(sOrt(), collapse=", "), "sind ausgewählt.")
    }
  })
  
  output$exportKaCSV <- downloadHandler(
    filename = function() {
      paste("Orte-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(isolate(ortData[sOrt(),-1]), file, sep = ";", 
                  fileEncoding = "UTF-8", row.names = FALSE)
    }
  )
  
  output$exportKaRData <- downloadHandler(
    filename = function() {
      paste("Orte-", Sys.Date(), ".RData", sep="")
    },
    content = function(file) {
      df <- isolate(ortData[sOrt(),-1])
      save(df, file = file)
    }
  )
}



#####################
######  TAB 2  ######
#####################
# load all PNS for all selected Orte from DB

pnsData <- reactive({
  db <- connectToDB()
  
  pnsDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns', 'global')"))
  pnsDataPnsMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns')"))
  
  pnsColColumns <- paste0("pns.", grep("col*", pnsDataPnsMetaData$columnid, value = TRUE))
  
  pns <- dbGetQuery(db, paste0("SELECT foi.featureofinterestid, foi.identifier, foi.name, pfoi.identifier as orts_id, ", 
                               paste0(pnsColColumns, collapse=", "),
                               " FROM featureofinterest foi
                               RIGHT OUTER JOIN pns_data pns ON foi.featureofinterestid = pns.featureofinterestid
                               RIGHT OUTER JOIN featurerelation fr ON foi.featureofinterestid = fr.childfeatureid
                               LEFT OUTER JOIN featureofinterest pfoi ON pfoi.featureofinterestid = fr.parentfeatureid
                               WHERE fr.parentfeatureid in (", 
                               paste(ortData[sOrt(),1], collapse=", "), ")"))
  dbDisconnect(db)
  
  if (nrow(pns) > 0)
    colnames(pns) <- pnsDataMetaData$dede[match(colnames(pns), pnsDataMetaData$columnid)]
  
  pns[,-1]
})

output$tablePNS <- renderDT({
  showTab <- pnsData()
  
  showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
  
  showHead <- paste0(showHead, "</span>")
  
  datatable(showTab, colnames = showHead, filter="top",
            options = list(paging=FALSE, dom = 'Brt', ordering=FALSE,
                           language=list(url = lngJSON)),
            escape=FALSE)
})

sPNS <- reactive({
  sr <- input$table2_rows_selected
  if(length(sr) == 0) {
    input$table2_rows_all
  } else {
    sort(sr)
  }
})

output$selTextPNS <- renderText({
  if (length(sPNS()) == 1) {
    paste("Zeile", sPNS(), "ist ausgewählt.")
  } else {
    paste("Zeilen", paste(sPNS(), collapse=", "), "sind ausgewählt.")
  }
})

output$exportKaVsCSV <- downloadHandler(
  filename = function() {
    paste("Probenahmestellen-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.table(isolate(pnsData()[sPNS(),]), file, sep = ";", 
                fileEncoding = "UTF-8", row.names = FALSE)
  }
)

output$exportKaVsRData <- downloadHandler(
  filename = function() {
    paste("Probenahmestellen-", Sys.Date(), ".RData", sep="")
  },
  content = function(file) {
    df <- isolate(pnsData()[sPNS(),])
    save(df, file = file)
  }
)



#####################
######  TAB 3  ######
#####################
# load all avaialble elemGroup for the selected PNS

elemGroup <- reactive({
  db <- connectToDB()
  
  col <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
  res <- dbGetQuery(db, paste0("SELECT DISTINCT ", col, " as name FROM parameter_data
                               WHERE ", col, " IS NOT NULL"))
  dbDisconnect(db)
  
  res
})

output$elemGroup <- renderUI(selectInput("selElemGroup", "Stoffgruppenauswahl:", 
                                         elemGroup()$name, multiple = TRUE, 
                                         selected = elemGroup()$name[1]))

# load all avaialble obsProp for the selected FoI

obsProp <- reactive({
  if (is.null(input$selElemGroup))
    return(NULL)
  db <- connectToDB()
  col <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
  op <- NULL
  for (i in 1:length(sPNS())) { # i <- 1
    op <- rbind(op, 
                dbGetQuery(db, paste0("SELECT op.observablepropertyid, op.identifier, op.name, 
                                      s.seriesid, s.unitid, s.featureofinterestid,
                                      p.identifier AS procId, foi.identifier AS foiid,
                                      pd.", col, " AS stgrname,  u.unit, pro.identifier As probeid, pro.identifier As probeid, pp.bg, pp.ng
                                      FROM observableproperty AS op
                                      LEFT OUTER JOIN series AS s ON (op.observablepropertyid = s.observablepropertyid)
                                      LEFT OUTER JOIN featureofinterest AS foi ON (s.featureofinterestid = foi.featureofinterestid)
                                      LEFT OUTER JOIN procedure AS p ON (s.procedureid = p.procedureid)
                                      LEFT OUTER JOIN parameter_data AS pd ON (op.observablepropertyid = pd.observablepropertyid)
                                      LEFT OUTER JOIN unit AS u ON (s.unitid = u.unitid)
                                      LEFT OUTER JOIN probe_parameter AS pp ON (op.observablepropertyid = pp.parameter_id)
                                      RIGHT OUTER JOIN probe AS pro ON (pp.probe_id = pro.id AND foi.featureofinterestid = pro.pns_id)
                                      WHERE foi.identifier = '", pnsData()[sPNS()[i],]$ID, "' AND s.firsttimestamp != '1970-01-01 00:00' AND pd.", col, " IN ('", paste(elemGroup()$name[elemGroup()$name %in% input$selElemGroup], collapse="', '"), "')")))
  }
  dbDisconnect(db)
  
  op
})

output$obsPhen <- renderUI(selectInput("selObsPhen", "Phänomenauswahl:", 
                                       obsProp()$name, multiple = TRUE, 
                                       selected = obsProp()$name[1]))

data <- reactive({
  if (!is.null(input$selObsPhen)) {
    db <- connectToDB()
    
    resDf <- NULL
    
    for (foi in pnsData()[sPNS(), "ID"]) {
      selObsPropFoi <- obsProp()[obsProp()$name %in% input$selObsPhen & obsProp()$foiid == foi,]
      
      if(length(selObsPropFoi$seriesid) == 0) next;
      
      # lookup observed time stamps for all series of this FoI
      foiTimes <- unique(dbGetQuery(db, paste0("SELECT DISTINCT phenomenontimestart
                                               FROM observation
                                               WHERE seriesid IN ('", paste( selObsPropFoi$seriesid, collapse="', '"), "')"))$phenomenontimestart)
      
      obsPropSel <- obsProp()$name %in% input$selObsPhen
      
      uObsPropSelId <- unique(obsProp()[obsPropSel, "identifier"])
      
      # for each time stamp, get the corresponding data and store it in resDfRow
      for (ft in as.character(foiTimes)) { # ft <- foiTimes[1] 
        resDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+2))
        colnames(resDfRow) <- c("id", "date", uObsPropSelId)
        
        resDfRow$id <- foi
        resDfRow$date <- ft
        
        res <- fromJSON(rawToChar(POST(paste0(SOSWebApp, "service"), 
                                       body = SOSgetObsByFoITime(input$selObsPhen, gsub(pattern = " ", replacement = "T", paste0(ft, ".000")), foi),
                                       content_type_xml(), accept_json())$content))
        for (obs in res$observations) { 
          if (input$repBG & obs$result$value < obsProp()[obsProp()$identifier == obs$observableProperty & obsProp()$foiid  == foi,]$bg) {
            resDfRow[obs$observableProperty] <- BGchar
          } else {
            resDfRow[obs$observableProperty] <- obs$result$value
          }
        }
        colnames(resDfRow) <- c("ID", "Datum", unique(obsProp()[obsPropSel, "name"]))
        
        resDf <- rbind(resDf, resDfRow)
      }
    }
    dbDisconnect(db)
    
    resDf$Datum <- as.Date(resDf$Datum)
    
    if (input$randomId) {
      db <- connectToDB()
      dbIdMap <- dbGetQuery(db, paste0("SELECT f.identifier, od.rndid FROM ort_data od 
                                       LEFT OUTER JOIN featureofinterest f ON od.featureofinterestid = f.featureofinterestid
                                       WHERE f.identifier IN ('", paste(resDf$ID, collapse="', '"), "')
                                       UNION
                                       SELECT f.identifier, pnsd.rndid FROM pns_data pnsd 
                                       LEFT OUTER JOIN featureofinterest f ON pnsd.featureofinterestid = f.featureofinterestid
                                       WHERE f.identifier IN ('", paste(resDf$ID, collapse="', '"), "')"))
      resDf$ID <- dbIdMap[match(resDf$ID, dbIdMap$id), 2]
      dbDisconnect(db)
    }
    
    resUom <-  as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+2))
    colnames(resUom) <- c("ID", "Datum", uObsPropSelId)
    resBg <-  as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+2))
    colnames(resBg) <- c("ID", "Datum", uObsPropSelId)
    resStgr <-  as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+2))
    colnames(resStgr) <- c("ID", "Datum", uObsPropSelId)
    
    for (obsPropId in uObsPropSelId) { # obsPropId <- uObsPropSelId[1]
      frid <- match(obsPropId, obsProp()$identifier)
      resUom[[obsPropId]] <- obsProp()[frid, "unit"]
      resBg[[obsPropId]] <- obsProp()[frid, "bg"]
      resStgr[[obsPropId]] <- obsProp()[frid, "stgrname"]
    }
    
    list(resDf=resDf,
         stgr=resStgr[1,],
         bg=resBg[1,],
         uom=resUom[1,])
  }
})

output$tableDaten <- renderDT({
  input$refreshData
  
  showTab <- isolate(data()[["resDf"]])
  
  isolate({
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    
    if (!is.null(data()[["uom"]])) {
      showUoM <- sapply(data()[["uom"]], function(x) {
        if (!is.na(x) & nchar(x) > 0  & x != "NA") {
          paste0(" [",x,"]")
        } else {
          ""
        }
      })
      showHead <- paste0(showHead, showUoM)
    }
    
    if (!is.null(data()[["bg"]])) {
      showBg <- sapply(data()[["bg"]], function(x) {
        if (!is.na(x) & nchar(x) > 0 & x != "NA") {
          paste0("<br> BG: ", x)
        } else {
          "<br>"
        }
      })
      showHead <- paste0(showHead, showBg)
    }
    
    if (!is.null(data()[["stgr"]])) {
      showStgr <- sapply(data()[["stgr"]], function(x) {
        if (!is.na(x) & nchar(x) > 0  & x != "NA") {
          paste0("<br>", x)
        } else {
          "<br>"
        }
      })
      showHead <- paste0(showHead, showStgr)
    }
    
    showHead <- paste0(showHead, "</span>")
  })
  
  if (!is.null(showTab)) {
    colnames(showTab) <- showHead
    datatable(showTab, #colnames=showHead,
              filter="top", 
              options=list(paging=FALSE,dom = 'Brt',
                           language=list(url = lngJSON)),
              escape=FALSE)
  }
})

selData <- reactive({
  sr <- input$tableDaten_rows_selected
  if(is.null(sr)) {
    input$tableDaten_rows_all
  } else {
    sort(sr)
  }
})

output$tableStatistik <- renderDataTable({
  if (!is.null(selData()) & input$computeStat) {
    input$refreshData
    
    filterData <- isolate(data()$resDf[selData(),])
    
    stat <- apply(filterData[,-c(1:2), drop=FALSE], 2, function(x) {
      xSum <- round(summary(as.numeric(x)),3)
      if (length(xSum) == 6) 
        xSum <- c(xSum,0)
      xSum
    })
    
    rownames(stat) <- c("Min.","1st Qu.","Median","Mittelw.", "3rd Qu.","Max.", "NA")
    
    datatable(stat,
              options=list(paging=FALSE, dom = 'Brt',
                           language=list(url = lngJSON)))
  }
})

output$selTextDaten <- renderText({
  if (is.null(selData()))
    return(NULL)
  if (length(selData()) == 1) {
    paste("Zeile", selData(), "ist ausgewählt.")
  } else {
    paste("Zeilen", paste(selData(), collapse=", "), "sind ausgewählt.")
  }
})

output$exportDataCSV <- downloadHandler(
  filename = function() paste("Daten-", Sys.Date(), ".csv", sep=""),
  content = function(file) {
    
    df <- data()$resDf[selData(),]
    
    write.table(isolate(df), file, sep = ";", 
                fileEncoding = "UTF-8", row.names = FALSE)
  }
)

output$exportDataRData <- downloadHandler(
  filename = function() paste("Daten-", Sys.Date(), ".RData", sep=""),
  content = function(file) {
    df <- data()$resDf[selData(),]
    
    if (input$includeMetaHead) {
      dfCol <- colnames(df)
      df <- rbind(setNames(data()[["uom"]], dfCol), 
                  setNames(data()[["bg"]], dfCol), 
                  setNames(data()[["stgr"]], dfCol),
                  df)
    }
    
    save(df, file = file)
  }
)