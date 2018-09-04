## viewer server Messdaten

#########
# Tab 1 #
#########
ortData <- NULL
db <- connectToDB()

# load all Entwässerungssysteme from DB
ews <- dbGetQuery(db, paste0("SELECT DISTINCT thematik FROM ort_data"))
output$ewsSelInput <- renderUI(selectInput("ews", "Thematik", ews[,"thematik"]))

# load all super FoI from DB
ort <- dbGetQuery(db, "SELECT DISTINCT foi.featureofinterestid, foi.name, foi.identifier 
                  FROM featureofinterest AS foi
                  RIGHT OUTER JOIN ort_data od ON foi.featureofinterestid = od.featureofinterestid")
                  # RIGHT OUTER JOIN featurerelation fr ON foi.featureofinterestid = fr.parentfeatureid
                  # RIGHT OUTER JOIN probe pro ON pro.pns_id = fr.childfeatureid WHERE foi.featureofinterestid = od.featureofinterestid")
dbDisconnect(db)

cat("foo \n")

# if any
if (nrow(ort) > 0) {
  ortData <- reactive({
    db <- connectToDB()
    
    ortDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('ort', 'global')"))
    ortDataOrtMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('ort')"))
    ortColColumns <- paste0("od.", grep("col*", ortDataOrtMetaData$columnid, value = TRUE))
    selectedThematik <- input$ews
    if (Sys.info()["sysname"] == "Windows") {
      selectedThematik <- stri_enc_tonative(input$ews)
    }
    query <- paste0("SELECT foi.featureofinterestid, foi.identifier, foi.name, od.Thematik, ", paste0(ortColColumns, collapse=", "),
                    " FROM featureofinterest foi
                                     RIGHT OUTER JOIN ort_data od ON foi.featureofinterestid = od.featureofinterestid
                                     WHERE foi.featureofinterestid IN (", 
                    paste0(ort$featureofinterestid, collapse=", "), ")
                                     AND od.thematik IN (", paste0("'", selectedThematik, "'" ,collapse=", ") ,")")
    ortData <- dbGetQuery(db, query)
    
    
    if (nrow(ortData) > 0)
      colnames(ortData) <- ortDataMetaData$dede[match(colnames(ortData), ortDataMetaData$columnid)]
    
    ortData
  })
  
  output$tableOrt  <- renderDT({
    showTab <- ortData()[,-1]
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showHead <- paste0(showHead, "</span>")
    
    datatable(showTab, colnames = showHead, 
              filter="top",
              options = list(paging=FALSE, dom = 'Brt',
                             language=list(url = lngJSON)),
              escape=FALSE)
  })
  
  sOrt <- reactive({
    sr <- input$tableOrt_rows_selected
    if(is.null(sr)) {
      input$tableOrt_rows_all
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
  dbDisconnect(db)
}

observeEvent(input$fromOrtToPNS, {
  updateTabsetPanel(session, "inNavbarpage",selected = "Probenahemstelle(n) auswählen")
})


#####################
######  TAB 2  ######
#####################
# load all PNS for all selected Orte from DB

if(!is.null(ortData)) {
  pnsData <- reactive({
    db <- connectToDB()
    
    pnsDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns', 'global')"))
    pnsDataPnsMetaData <- dbGetQuery(db, paste0("SELECT * FROM column_metadata WHERE prefixid IN ('pns')"))
    
    pnsColColumns <- NULL
    if (nrow(pnsDataPnsMetaData) > 0 && length(grep("col*", pnsDataPnsMetaData$columnid, value = TRUE)) > 0) {
        pnsColColumns <- paste0(", pns.", grep("col*", pnsDataPnsMetaData$columnid, value = TRUE))
    }
    
    pns <- dbGetQuery(db, paste0("SELECT DISTINCT  foi.featureofinterestid, foi.identifier, foi.name, pfoi.identifier as orts_id", 
                                 paste0(pnsColColumns),
                                 " FROM featureofinterest foi
                               RIGHT OUTER JOIN pns_data pns ON foi.featureofinterestid = pns.featureofinterestid
                               RIGHT OUTER JOIN featurerelation fr ON foi.featureofinterestid = fr.childfeatureid
                               RIGHT OUTER JOIN probe pro ON pro.pns_id = fr.childfeatureid   
                               LEFT OUTER JOIN featureofinterest pfoi ON pfoi.featureofinterestid = fr.parentfeatureid
                               WHERE fr.parentfeatureid in (", 
                                 paste(ortData()[sOrt(),1], collapse=", "), ")"))
    dbDisconnect(db)
    
    if (nrow(pns) > 0)
      colnames(pns) <- pnsDataMetaData$dede[match(colnames(pns), pnsDataMetaData$columnid)]
    
    pns
  })
  
  
  output$tablePNS <- renderDT({
    
    showTab <- pnsData()[,-1]
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showHead <- paste0(showHead, "</span>")
    
    datatable(showTab, colnames = showHead, filter="top",
              options = list(paging=FALSE, dom = 'Brt', ordering=FALSE,
                             language=list(url = lngJSON)),
              escape=FALSE)
  })
  
  sPNS <- reactive({
    sr <- input$tablePNS_rows_selected
    if(length(sr) == 0) {
      input$tablePNS_rows_all
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
  
  output$exportPnsCSV <- downloadHandler(
    filename = function() {
      paste("Probenahmestellen-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(isolate(pnsData()[sPNS(),]), file, sep = ";", 
                  fileEncoding = "UTF-8", row.names = FALSE)
    }
  )
  
  output$exportPnsRData <- downloadHandler(
    filename = function() {
      paste("Probenahmestellen-", Sys.Date(), ".RData", sep="")
    },
    content = function(file) {
      df <- isolate(pnsData()[sPNS(),])
      save(df, file = file)
    }
  )
} else {
  output$selTextPNS <- renderText("Bitte zunächst mindestens einen Ort auswählen.")
}

observeEvent(input$fromPNStoMessdaten, {
  updateTabsetPanel(session, "inNavbarpage", selected = "Messdaten anzeigen")
})

#####################
######  TAB 3  ######
#####################
# load all avaialble elemGroup for the selected PNS

elemGroup <- reactive({
  db <- connectToDB()
  res <- NULL  
  print(str(col))
  
  col <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
  if (nrow(col) != 0) {
    res <- dbGetQuery(db, paste0("SELECT DISTINCT ", col, " as name FROM parameter_data
                               WHERE ", col, " IS NOT NULL"))
  }
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
  colStoffgruppe <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
  op <- NULL
  for (i in 1:length(sPNS())) { # i <- 1
    query <- paste0("SELECT DISTINCT op.observablepropertyid, op.identifier, op.name, foi.identifier AS foiid,
                      foi.featureofinterestid, s.seriesid
                     FROM observableproperty AS op
                      LEFT OUTER JOIN series AS s ON (op.observablepropertyid = s.observablepropertyid)
                      LEFT OUTER JOIN featureofinterest AS foi ON (s.featureofinterestid = foi.featureofinterestid)
                      LEFT OUTER JOIN procedure AS p ON (s.procedureid = p.procedureid)
                      LEFT OUTER JOIN parameter_data AS pd ON (op.observablepropertyid = pd.observablepropertyid)
                      LEFT OUTER JOIN probe_parameter AS pp ON (op.observablepropertyid = pp.parameter_id) AND (pd.observablepropertyid = pp.parameter_id)
                      RIGHT OUTER JOIN probe AS pro ON (pp.probe_id = pro.id AND foi.featureofinterestid = pro.pns_id)
                     WHERE foi.identifier = '", pnsData()[sPNS()[i],]$ID, "' AND s.firsttimestamp != '1970-01-01 00:00' AND pd.", colStoffgruppe, " IN ('", paste(elemGroup()$name[elemGroup()$name %in% input$selElemGroup], collapse="', '"), "')")
    cat(query)
    op <- rbind(op, dbGetQuery(db, query))
  }
  dbDisconnect(db)
  
  op
})

output$obsPhen <- renderUI(selectInput("selObsPhen", "Parameterauswahl:", 
                                       obsProp()$name, multiple = TRUE, 
                                       selected = obsProp()$name[1]))

data <- reactive({
  if (!is.null(input$selObsPhen)) {
    db <- connectToDB()
    
    resDf <- NULL
    
    phenValueCount <- 3
    postfix <- c("Wert", "Einheit", "Stoffgruppe")
    if (input$showBG) {
      postfix <- append(postfix, "BG")
      phenValueCount <-  phenValueCount + 1
    }
    if (input$showNG) {
      postfix <- append(postfix, "NG")
      phenValueCount <-  phenValueCount + 1
    }
    
    columnCount <- (length(input$selObsPhen) * phenValueCount) + 4
    
    colStoffgruppe <- dbGetQuery(db, "SELECT columnid FROM column_metadata WHERE prefixid = 'param' AND dede = 'Stoffgruppe' limit 1")
    
    for (foi in pnsData()[sPNS(), "ID"]) {
     
      
      selObsPropFoi <- obsProp()[obsProp()$name %in% input$selObsPhen & obsProp()$foiid == foi,]
      
      if(length(selObsPropFoi$seriesid) == 0) next;
      
      # lookup observed time stamps for all series of this FoI
      foiTimes <- dbGetQuery(db, paste0("SELECT DISTINCT phenomenontimestart, phenomenontimeend, resulttime
                                               FROM observation
                                               WHERE seriesid IN ('", paste( selObsPropFoi$seriesid, collapse="', '"), "')"))
      
      obsPropSel <- obsProp()$name %in% input$selObsPhen
      
      uObsPropSelId <- unique(obsProp()[obsPropSel, "identifier"])
      
      as.vector(t(outer(uObsPropSelId, postfix, paste, sep="_"))) 
      
      
      query <- paste0("SELECT DISTINCT o.observationid, o.seriesid, o.phenomenontimestart, o.phenomenontimeend, o.resulttime,
                                       u.unit, nv.value, op.identifier as observableProperty, pp.bg, pp.ng, pd.", colStoffgruppe, " AS stgrname 
                  FROM observation o
                      LEFT OUTER JOIN numericvalue nv ON (o.observationid = nv.observationid)
                      LEFT OUTER JOIN series AS s ON (o.seriesid = s.seriesid)
                      LEFT OUTER JOIN featureofinterest AS foi ON (s.featureofinterestid = foi.featureofinterestid)
                      LEFT OUTER JOIN observableproperty AS op ON (s.observablepropertyid = op.observablepropertyid)
                      LEFT OUTER JOIN parameter_data AS pd ON (op.observablepropertyid = pd.observablepropertyid)
                      LEFT OUTER JOIN probe_parameter AS pp ON (op.observablepropertyid = pp.parameter_id)
                      LEFT OUTER JOIN unit AS u ON (o.unitid = u.unitid)
                      RIGHT OUTER JOIN probe AS pro ON (pp.probe_id = pro.id AND foi.featureofinterestid = pro.pns_id)
                      WHERE s.featureofinterestid IN (", paste0("'", selObsPropFoi$featureofinterestid, "'" , collapse=", "), ")")
      
      if (!is.null(input$selObsPhen))
        query <- paste0(query, " AND op.identifier IN (", paste0("'", input$selObsPhen, "'" ,collapse=", ") ,")")
      
      minPhenTimeStart <- foiTimes[which.min(as.POSIXct(foiTimes$phenomenontimestart)), "phenomenontimestart"]
      maxPhenTimeStart <- foiTimes[which.max(as.POSIXct(foiTimes$phenomenontimestart)), "phenomenontimestart"]
      maxPhenTimeEnd <- foiTimes[which.max(as.POSIXct(foiTimes$phenomenontimeend)), "phenomenontimeend"]
      
      query <- paste0(query, " AND (o.phenomenontimestart >= to_timestamp('", as.character(minPhenTimeStart), "','YYYY-mm-DD HH24:MI:SS') 
                      AND (o.phenomenontimestart <= to_timestamp('", as.character(maxPhenTimeStart), "','YYYY-mm-DD HH24:MI:SS') 
                      OR o.phenomenontimeend <= to_timestamp('", as.character(maxPhenTimeEnd), "','YYYY-mm-DD HH24:MI:SS'))) " )

      res <- dbGetQuery(db, query)
      
      if(!is.null(res)) {
        for (ft in 1:nrow(foiTimes)) {
        #for (ft in as.character(foiTimes)) { # ft <- foiTimes[1] 
          resDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = columnCount))
          colnames(resDfRow) <- c("PNS_ID", "Probenahmedatum", "Ereignisbeginn", "Ereignisende", as.vector(t(outer(uObsPropSelId, postfix, paste, sep="_"))))
          
          resDfRow$PNS_ID <- foi
          for (obs in 1:nrow(res)) {
            if ( strftime(foiTimes[ft,"phenomenontimestart"], format='%d.%m.%Y %H:%M') == strftime(res[obs, "phenomenontimestart"], format='%d.%m.%Y %H:%M')
                 & strftime(foiTimes[ft,"phenomenontimeend"], format='%d.%m.%Y %H:%M') == strftime(res[obs, "phenomenontimeend"], format='%d.%m.%Y %H:%M')
                 & strftime(foiTimes[ft,"resulttime"], format='%d.%m.%Y %H:%M') == strftime(res[obs, "resulttime"], format='%d.%m.%Y %H:%M')) {
              resDfRow$Probenahmedatum <- strftime(res[obs, "resulttime"], format='%d.%m.%Y %H:%M')
              resDfRow$Ereignisbeginn <- strftime(res[obs, "phenomenontimestart"], format='%d.%m.%Y %H:%M')
              resDfRow$Ereignisende <- strftime(res[obs, "phenomenontimeend"], format='%d.%m.%Y %H:%M')
              valueRow <- paste(res[obs, "observableproperty"], "Wert", sep="_")
              if (res[obs, "value"] < res[obs, "bg"]) {
                if (input$repBG == 'BG') {
                resDfRow[valueRow] <- res[obs, "bg"]
                } else if (input$repBG == 'BG/2') {
                  resDfRow[valueRow] <- res[obs, "bg"]/2
                } else {
                  resDfRow[valueRow] <- input$repBG
                }
              } else {
                resDfRow[valueRow] <- res[obs, "value"]
              }
              resDfRow[paste(res[obs, "observableproperty"], "Einheit", sep="_")] <- res[obs, "unit"]
              resDfRow[paste(res[obs, "observableproperty"], "Stoffgruppe", sep="_")] <- res[obs, "stgrname"]
              if (input$showBG) {
                resDfRow[paste(res[obs, "observableproperty"], "BG", sep="_")] <- res[obs, "bg"]
              }
              if (input$showNG) {
                resDfRow[paste(res[obs, "observableproperty"], "NG", sep="_")] <- res[obs, "ng"]
              }
            }
          }
          
          resDf <- rbind(resDf, resDfRow)
        }
      }
      
    }
    dbDisconnect(db)
    
    if (input$randomId) {
      db <- connectToDB()
      dbIdMap <- dbGetQuery(db, paste0("SELECT f.identifier, od.rndid FROM ort_data od 
                                       LEFT OUTER JOIN featureofinterest f ON od.featureofinterestid = f.featureofinterestid
                                       WHERE f.identifier IN ('", paste(resDf$PNS_ID, collapse="', '"), "')
                                       UNION
                                       SELECT f.identifier, pnsd.rndid FROM pns_data pnsd 
                                       LEFT OUTER JOIN featureofinterest f ON pnsd.featureofinterestid = f.featureofinterestid
                                       WHERE f.identifier IN ('", paste(resDf$PNS_ID, collapse="', '"), "')"))
      resDf$PNS_ID <- dbIdMap[match(resDf$PNS_ID, dbIdMap$id), "rndid"]
      dbDisconnect(db)
    }
    
    #resUom <-  as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+columnCount))
    #colnames(resUom) <- c("PNS_ID", "Probenahmedatum", "Ereignisbeginn", "Ereignisende", uObsPropSelId)
    #resBg <-  as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+columnCount))
    #colnames(resBg) <- c("PNS_ID", "Probenahmedatum", "Ereignisbeginn", "Ereignisende", uObsPropSelId)
    #resStgr <-  as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+columnCount))
    #colnames(resStgr) <- c("PNS_ID", "Probenahmedatum", "Ereignisbeginn", "Ereignisende", uObsPropSelId)
    
    #for (obsPropId in uObsPropSelId) { # obsPropId <- uObsPropSelId[1]
    #  frid <- match(obsPropId, obsProp()$identifier)
    #  resUom[[obsPropId]] <- obsProp()[frid, "unit"]
    #  resBg[[obsPropId]] <- obsProp()[frid, "bg"]
    #  resStgr[[obsPropId]] <- obsProp()[frid, "stgrname"]
    #}
    
    list(resDf=resDf)
  }
})

output$tableDaten <- renderDT({
  input$refreshData
  
  showTab <- isolate(data()[["resDf"]])
  
  isolate({
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    
    # if (!is.null(data()[["uom"]])) {
    #   showUoM <- sapply(data()[["uom"]], function(x) {
    #     if (!is.na(x) & nchar(x) > 0  & x != "NA") {
    #       paste0(" [",x,"]")
    #     } else {
    #       ""
    #     }
    #   })
    #   showHead <- paste0(showHead, showUoM)
    # }
    # 
    # if (!is.null(data()[["bg"]])) {
    #   showBg <- sapply(data()[["bg"]], function(x) {
    #     if (!is.na(x) & nchar(x) > 0 & x != "NA") {
    #       paste0("<br> BG: ", x)
    #     } else {
    #       "<br>"
    #     }
    #   })
    #   showHead <- paste0(showHead, showBg)
    # }
    # 
    # if (!is.null(data()[["stgr"]])) {
    #   showStgr <- sapply(data()[["stgr"]], function(x) {
    #     if (!is.na(x) & nchar(x) > 0  & x != "NA") {
    #       paste0("<br>", x)
    #     } else {
    #       "<br>"
    #     }
    #   })
    #   showHead <- paste0(showHead, showStgr)
    # }
    
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