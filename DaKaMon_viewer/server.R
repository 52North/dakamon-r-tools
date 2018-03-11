# server  DaKaMon viewer
library(DT)
library(shiny)
library(httr)
library(rjson)
library(RPostgreSQL)

local <- FALSE
SOSWebApp <- ifelse(local, "http://localhost:8080/52n-sos-webapp/", "http://sos:8080/52n-sos-webapp/")
dbHost <- ifelse(local, "localhost", "db") 
verbose <- local
BGencode <- 0
BGchar <- "< BG"
BGlabel <- "Bestimmungsgrenze" # label in DB

## tools
SOSgetObs <- function(obsProp, foiURI) {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <sos:GetObservation
    xmlns:sos=\"http://www.opengis.net/sos/2.0\"
    xmlns:fes=\"http://www.opengis.net/fes/2.0\"
    xmlns:gml=\"http://www.opengis.net/gml/3.2\"
    xmlns:swe=\"http://www.opengis.net/swe/2.0\"
    xmlns:xlink=\"http://www.w3.org/1999/xlink\"
    xmlns:swes=\"http://www.opengis.net/swes/2.0\"
    xmlns:sosrf=\"http://www.opengis.net/sosrf/1.0\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" service=\"SOS\" version=\"2.0.0\" xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd\">
    <swes:extension>
      <swe:Boolean definition=\"MergeObservationsIntoDataArray\">
        <swe:value>true</swe:value>
      </swe:Boolean>
    </swes:extension>
    <sos:observedProperty>", obsProp, "</sos:observedProperty>
    <sos:featureOfInterest>", foiURI, "</sos:featureOfInterest>
  </sos:GetObservation>")
}

SOSgetObsByProc <- function(proc) {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <sos:GetObservation
    xmlns:sos=\"http://www.opengis.net/sos/2.0\"
    xmlns:fes=\"http://www.opengis.net/fes/2.0\"
    xmlns:gml=\"http://www.opengis.net/gml/3.2\"
    xmlns:swe=\"http://www.opengis.net/swe/2.0\"
    xmlns:xlink=\"http://www.w3.org/1999/xlink\"
    xmlns:swes=\"http://www.opengis.net/swes/2.0\"
    xmlns:sosrf=\"http://www.opengis.net/sosrf/1.0\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" service=\"SOS\" version=\"2.0.0\" xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd\">
    <swes:extension>
      <swe:Boolean definition=\"MergeObservationsIntoDataArray\">
        <swe:value>true</swe:value>
      </swe:Boolean>
    </swes:extension>
    <sos:procedure>", proc, "</sos:procedure>
  </sos:GetObservation>")
}

SOSgetObsByFoITime <- function(obsProp, time, foiURI) {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <sos:GetObservation
    xmlns:sos=\"http://www.opengis.net/sos/2.0\"
    xmlns:fes=\"http://www.opengis.net/fes/2.0\"
    xmlns:gml=\"http://www.opengis.net/gml/3.2\"
    xmlns:swe=\"http://www.opengis.net/swe/2.0\"
    xmlns:xlink=\"http://www.w3.org/1999/xlink\"
    xmlns:swes=\"http://www.opengis.net/swes/2.0\"
    xmlns:sosrf=\"http://www.opengis.net/sosrf/1.0\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" service=\"SOS\" version=\"2.0.0\" xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd\">
    <swes:extension>
      <swe:Boolean definition=\"MergeObservationsIntoDataArray\">
        <swe:value>true</swe:value>
      </swe:Boolean>
    </swes:extension>",
         paste("<sos:observedProperty>", obsProp, "</sos:observedProperty>", collapse = " \n "),
         "<sos:temporalFilter>
      <fes:TEquals>
        <fes:ValueReference>phenomenonTime</fes:ValueReference>
        <gml:TimeInstant gml:id=\"ti_1\">
          <gml:timePosition>", time, "</gml:timePosition>
        </gml:TimeInstant>
      </fes:TEquals>
    </sos:temporalFilter>
    <sos:featureOfInterest>", foiURI, "</sos:featureOfInterest>
  </sos:GetObservation>")
}
## /tools

server <- function(input, output) {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname="sos", user="postgres", password="postgres", port="5432")
  on.exit(dbDisconnect(db), add=T)
  
  # load all super FoI from DB
  superFoi <- dbGetQuery(db, paste0("SELECT featureofinterestid, name, identifier FROM featureofinterest WHERE identifier != 'unknown' AND featureofinterestid IN (SELECT parentfeatureid FROM featurerelation)"))
  superFoiData <- dbGetQuery(db, paste0("SELECT * FROM foidata WHERE featureofinterestid IN (SELECT featureofinterestid FROM featureofinterest WHERE identifier != 'unknown' AND featureofinterestid IN (SELECT parentfeatureid FROM featurerelation))"))
  foiDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM foidatametadata"))
  colnames(superFoiData) <- foiDataMetaData$dede[match(colnames(superFoiData), foiDataMetaData$columnid)]
  
  output$table  <- renderDataTable({
    showTab <- superFoiData[,-1]
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showUoM <- sapply(foiDataMetaData$uom, function(x) {
      if (!is.na(x) & nchar(x) > 0) {
        paste0(" [",x,"]")
      } else {
        ""
      }
    })
    showHead <- paste0(showHead, showUoM)
    showHead <- paste0(showHead, "</span>")
    
    datatable(showTab, colnames = showHead, filter="top",
              options = list(paging=FALSE, dom = 'Brti'),
              escape=FALSE)
  })
  
  s <- reactive({
    sr <- input$table_rows_selected
    if(is.null(sr)) {
      input$table_rows_all
    } else {
      sort(sr)
    }
  })
  
  output$selText <- renderText({
    if (length(s()) == 1) {
      paste("Row", s(), "is selected.")
    } else {
      paste("Rows", paste(s(), collapse=", "), "are selected.")
    }
  })
  
  output$exportKaCSV <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(isolate(superFoiData[s(),-1]), file, sep = ";", fileEncoding = "UTF-8")
    }
  )
  
  output$exportKaRData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".RData", sep="")
    },
    content = function(file) {
      df <- isolate(superFoiData[s(),-1])
      save(df, file = file)
    }
  )
  #####################
  ######  TAB 2  ######
  #####################
  # load all selcted sub FoI from DB
  # subFoi <- dbGetQuery(db, paste0("SELECT featureofinterestid, name, identifier FROM featureofinterest WHERE identifier != 'unknown' AND featureofinterestid IN (SELECT parentfeatureid FROM featurerelation)"))
  
  subFoiData <- reactive({
    db <- dbConnect("PostgreSQL", host=dbHost, dbname="sos", user="postgres", password="postgres", port="5432")
    on.exit(dbDisconnect(db), add=T)
    
    sfd <- dbGetQuery(db, paste0("SELECT * FROM foidata WHERE featureofinterestid IN (SELECT childfeatureid FROM featurerelation WHERE parentfeatureid IN ('", paste(superFoi[s(),1], collapse="', '") , "'))"))
    colnames(sfd) <- foiDataMetaData$dede[match(colnames(sfd), foiDataMetaData$columnid)]
    sfd[,-1]
  })
  
  output$table2 <- renderDataTable({
    showTab <- subFoiData()
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showUoM <- sapply(foiDataMetaData$uom, function(x) {
      if (!is.na(x) & nchar(x) > 0) {
        paste0(" [",x,"]")
      } else {
        ""
      }
    })
    
    showHead <- paste0(showHead, showUoM)
    
    showHead <- paste0(showHead, "</span>")
    
    datatable(showTab, colnames = showHead, filter="top",
              options = list(paging=FALSE, dom = 'Brti'),
              escape=FALSE)
  })
  
  sp <- reactive({
    sr <- input$table2_rows_selected
    if(length(sr) == 0) {
      input$table2_rows_all
    } else {
      sort(sr)
    }})
  
  output$selText2 <- renderText({
    if (length(sp()) == 1) {
      paste("Row", sp(), "is selected.")
    } else {
      paste("Rows", paste(sp(), collapse=", "), "are selected.")
    }
  })
  
  output$exportKaFCSV <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(isolate(subFoiData()[sp(),]), file, sep = ";", fileEncoding = "UTF-8")
    }
  )
  
  output$exportKaFRData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".RData", sep="")
    },
    content = function(file) {
      df <- isolate(subFoiData()[sp(),])
      save(df, file = file)
    }
  )
  
  #####################
  ######  TAB 3  ######
  #####################
  # load all avaialble obsProp for the selected FoI
  
  obsProp <- reactive({
    db <- dbConnect("PostgreSQL", host=dbHost, dbname="sos", user="postgres", password="postgres", port="5432")
    on.exit(dbDisconnect(db), add=T)
    
    op <- NULL
    for (i in 1:length(sp())) { # i <- 1
      op <- rbind(op, 
                  dbGetQuery(db, paste0("SELECT op.observablepropertyid, op.identifier, op.name, 
                                         s.seriesid, s.unitid, s.featureofinterestid,
                                         p.identifier AS procId, foi.identifier AS foiid,
                                         sr.referenceseriesid, srv.lastnumericvalue, 
                                         opr.parentobservablepropertyid, oprv.identifier AS stgrid, oprv.name AS stgrname,
                                         u.unit
      FROM observableproperty AS op
      LEFT OUTER JOIN series AS s ON (op.observablepropertyid = s.observablepropertyid)
      LEFT OUTER JOIN featureofinterest AS foi ON (s.featureofinterestid = foi.featureofinterestid)
      LEFT OUTER JOIN procedure AS p ON (s.procedureid = p.procedureid)
      LEFT OUTER JOIN seriesreference AS sr ON (s.seriesid = sr.seriesid)
      LEFT OUTER JOIN series AS srv ON (sr.referenceseriesid = srv.seriesid)
      LEFT OUTER JOIN observablepropertyrelation AS opr ON (op.observablepropertyid = opr.childobservablepropertyid)
      LEFT OUTER JOIN observableproperty AS oprv ON (opr.parentobservablepropertyid = oprv.observablepropertyid)
      LEFT OUTER JOIN unit AS u ON (s.unitid = u.unitid)
      WHERE foi.identifier = '", subFoiData()[sp()[i],]$ID, "' AND s.firsttimestamp != '1970-01-01 00:00'")))
    }
    
    op
  })
  
  output$obsPhen <- renderUI(selectInput("selObsPhen", "Select phenomenon:", 
                                         obsProp()$name, multiple = TRUE, 
                                         selected = obsProp()$name[1]))
  
  data <- reactive({
    if (!is.null(input$selObsPhen)) {
      db <- dbConnect("PostgreSQL", host=dbHost, dbname="sos", user="postgres", password="postgres", port="5432")
      on.exit(dbDisconnect(db), add=T)
      
      # uObsPropIds <- unique(obsProp()$identifier)
      resDf <- NULL
      
      for (foi in subFoiData()[sp(), "ID"]) {
        selObsPropFoi <- obsProp()[obsProp()$name %in% input$selObsPhen & obsProp()$foiid == foi,]
        
        # lookup observed time stamps for all series of this FoI
        foiTimes <- unique(dbGetQuery(db, paste0("SELECT resulttime
          FROM observation
          WHERE seriesid IN ('", paste( selObsPropFoi$seriesid, collapse="', '"), "')"))$resulttime)
        
        obsPropSel <- obsProp()$name %in% input$selObsPhen
        
        uObsPropSelId <- unique(obsProp()[obsPropSel, "identifier"])
        
        # for each time stamp, get the corresponding data and store it in resDfRow
        for (ft in as.character(foiTimes)) { # ft <- foiTimes[1] 
          resDfRow <- as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+2))
          colnames(resDfRow) <- c("id", "date", uObsPropSelId)
          
          resDfRow$id <- foi
          resDfRow$date <- ft
          
          res <- fromJSON(rawToChar(POST(paste0(SOSWebApp, "service"), 
                                         body = SOSgetObsByFoITime(input$selObsPhen, gsub(pattern = " ", replacement = "T", ft), foi),
                                         content_type_xml(), accept_json())$content))
          for (obs in res$observations) { 
            if (input$repBG & obs$result$value < obsProp()[obsProp()$identifier == obs$observableProperty & obsProp()$foiid  == foi,]$lastnumericvalue) {
              resDfRow[obs$observableProperty] <- BGchar
            } else {
              resDfRow[obs$observableProperty] <- obs$result$value
            }
          }
          colnames(resDfRow) <- c("id", "date", unique(obsProp()[obsPropSel, "name"]))
          
          resDf <- rbind(resDf, resDfRow)
        }
      }
      
      
      resUom <-  as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+2))
      colnames(resUom) <- c("id", "date", uObsPropSelId)
      resBg <-  as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+2))
      colnames(resBg) <- c("id", "date", uObsPropSelId)
      resStgr <-  as.data.frame(matrix(NA, nrow = 1, ncol = length(input$selObsPhen)+2))
      colnames(resStgr) <- c("id", "date", uObsPropSelId)
      
      for (obsPropId in uObsPropSelId) { # obsPropId <- uObsPropSelId[1]
        frid <- match(obsPropId, obsProp()$identifier)
        resUom[[obsPropId]] <- obsProp()[frid, "unit"]
        resBg[[obsPropId]] <- obsProp()[frid, "lastnumericvalue"]
        resStgr[[obsPropId]] <- obsProp()[frid, "stgrname"]
      }
      
      list(resDf=resDf,
           stgr=resStgr[1,],
           bg=resBg[1,],
           uom=resUom[1,])
    }
  })
  
  output$table3 <- renderDataTable({
    input$refreshData
    
    showTab <- isolate(data()[["resDf"]])
    # on.exit(showTab <- NULL, add=TRUE)
    
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
                options=list(paging=FALSE,dom = 'Brti'),
                escape=FALSE)
    }
  })
  
  output$table3stat <- renderDataTable({
    if (!is.null(input$table3_rows_all) & input$computeStat) {
      input$refreshData
      
      filterData <- isolate(data()$resDf[input$table3_rows_all,])
      
      stat <- apply(filterData[,-c(1:2), drop=FALSE], 2, function(x) {
        xSum <- round(summary(as.numeric(x)),3)
        if (length(xSum) == 6) 
          xSum <- c(xSum,0)
        xSum
      })
      
      rownames(stat) <- c("Min.","1st Qu.","Median","Mean", "3rd Qu.","Max.", "NA")
      
      datatable(stat, # colnames=showHead,
                options=list(paging=FALSE, dom = 'Brti'))
    }
  })
  
  output$exportCSV <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if (input$includeMetaHead) {
        dfCol <- colnames(df)
        metaHead <- rbind(data()[["uom"]], 
                          data()[["bg"]], 
                          data()[["stgr"]])
        colnames(metaHead) <- dfCol 
        df <- rbind(metaHead, df)
      }
      write.table(df, file, sep = ";", fileEncoding = "UTF-8")
    }
  )
  
  output$exportRData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".RData", sep="")
    },
    content = function(file) {
      df <- isolate(data()$resDf[input$table3_rows_all,])
      if (input$includeMetaHead) {
        dfCol <- colnames(df)
        metaHead <- rbind(data()[["uom"]], 
                          data()[["bg"]], 
                          data()[["stgr"]])
        colnames(metaHead) <- dfCol 
        df <- rbind(metaHead, df)
      }
      save(df, file = file)
    }
  )
}