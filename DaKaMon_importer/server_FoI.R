###############################################################################
###################################         ###################################
###################################   FoI   ###################################
###################################         ###################################
###############################################################################

inCSVFoI <- reactiveValues()
vali <- reactiveValues(validated = FALSE)
checkDB <- reactiveValues(checked = FALSE)

observeEvent(input$csvFileFoI, {
  vali$validated <- FALSE
  checkDB$checked <- FALSE
  
  inCSVFoI$headAsChar <- as.character(read.csv(input$csvFileFoI$datapath,
                                               header = FALSE,
                                               sep = input$sepFoI, dec = input$decFoi,
                                               nrows = 1, stringsAsFactors = FALSE))
  
  inCSVFoI$UoMs <- read.csv(input$csvFileFoI$datapath, header = FALSE,
                            sep = input$sepFoI, dec = input$decFoi,
                            skip = 1, nrows = 1,
                            stringsAsFactors = FALSE)
  inCSVFoI$UoMs[is.na(inCSVFoI$UoMs)] <- ""
  inCSVFoI$UoMs <- as.character(inCSVFoI$UoMs)
  
  inCSVFoI$df <- read.csv(input$csvFileFoI$datapath, header = FALSE,
                          sep = input$sepFoI, dec = input$decFoi,
                          skip = 2,
                          stringsAsFactors = FALSE)
  colnames(inCSVFoI$df) <- inCSVFoI$headAsChar
  
  ################################
  ## validation of FoI csv-file ##
  ################################
  # look for Name, ID, lat, lon and Stammanlage,
  # check whether columns have unique names
  
  txt <- NULL
  if (!(reqColFoI$id %in% inCSVFoI$headAsChar) || length(unique(inCSVFoI$df[,reqColFoI$id])) != length(inCSVFoI$df[,reqColFoI$id]))
    txt <- paste0(txt, "<li>Jede Kläranlage und jeder Verfahrensschritt benötigt eine presistent und eindeutige ID in der Spalte'", reqColFoI[1], "'.</li>")
  for (reqColName in reqColFoI[-1]) {
    if (!(reqColName %in% inCSVFoI$headAsChar))
      txt <- paste0(txt, "<li>Bitte ergänze die Spalte '", reqColName, "'.</li>", sep="")
  }

  if(length(unique(inCSVFoI$headAsChar)) != length(inCSVFoI$headAsChar))
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")
  
  vali$txt <- txt
  vali$validated <- TRUE
})

output$foiValidationOut <- renderUI({
  if (vali$validated) {
    if (is.null(vali$txt)) {
      actionButton("checkDB", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", vali$txt, "</ul></div></html"))
    }
  } else {
    return()
  }
})

##########################
## check DB consistency ##
##########################

# find existing FoIs
# check Parameter and their UoMs

observeEvent(input$checkDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname="sos", user="postgres", password="postgres", port="5432")
  on.exit(dbDisconnect(db), add=T)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add = T)
  
  progress$set(message = "Prüfe Datenkonsistenz.", value = 0)
  FoIinDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('", 
                                   paste(inCSVFoI$df[,reqColFoI$id], collapse="', '"),"')")) ## [inclRowFoI()]
  if (nrow(FoIinDB) > 0) {
    checkDB$txtInfo <- paste("Folgende Kläranlagen/Verfahrensschritte sind bereits in der DB: <ul><li>",
                             paste0(FoIinDB$identifier, collapse="</li><li>"))
  } else {
    checkDB$txtInfo <- NULL
  }
  
  progress$inc(1/2, "Features")
  
  checkDB$foiInDB <- FoIinDB$identifier
  
  # find columns already in DB: colInDB with columnid, dede and its unit from the unit table
  checkDB$colInDB <- dbGetQuery(db, paste0("SELECT columnid, dede, unit.unit FROM foidatametadata left outer join unit on (unit.unitid = uom) WHERE dede IN ('", 
                                           paste(inCSVFoI$headAsChar, collapse="', '"),"')"))
  
  # replace NA UoM with ""
  if (nrow(checkDB$colInDB) > 0 ) {
    checkDB$colInDB$unit[is.na(checkDB$colInDB$unit)] <- ""
    
    # compare UoMs from the csv with the DB for overlapping columns
    compUoM <- inCSVFoI$UoMs[match(checkDB$colInDB$dede, inCSVFoI$headAsChar)] == checkDB$colInDB$unit
    checkDB$uomMissMatchCols <-which(!compUoM)
    
    checkDB$txtErr <- NULL
    if (length(checkDB$uomMissMatchCols) > 0) {
      checkDB$txtErr <-
        paste(
          "Folgende Spalten haben unterschiedliche Maßeinheiten: <ul><li>",
          paste0(
            paste0(checkDB$colInDB$dede[checkDB$uomMissMatchCols], ": [",
                   checkDB$colInDB$unit[checkDB$uomMissMatchCols], "] "),
            collapse = "</li><li>"
          )
        )
    }
  }
  
  progress$inc(1, "Maßeinheiten")
  
  checkDB$checked <- TRUE
}, ignoreInit=TRUE)

output$DBConsistencyTxtOut <- renderUI({
  if (checkDB$checked) {
    if (!is.null(checkDB$txtInfo) | !is.null(checkDB$txtErr)) {
      if (!is.null(checkDB$txtErr)) {
        HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDB$txtErr, "</li></ul></div></html"))
      } else {
        HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDB$txtInfo, "</li></ul></div></html"))
      }
    } 
  } else {
    HTML("")
  }
})

output$DBConsistencyActionOut <- renderUI({
  if (checkDB$checked) {
    if (is.null(checkDB$txtErr)) {
      if (is.null(checkDB$txtInfo) || input$owFoI) {
        actionButton("storeDB", "Einfügen in DB!")
      } 
    } 
  } else {
    HTML("")
  }
})

output$tableFoI <- renderDataTable({
  if (!is.null(inCSVFoI$df)) {
    showTab <- inCSVFoI$df 
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    if (!is.null(inCSVFoI$UoMs)) {
      showUoM <- sapply(inCSVFoI$UoMs, function(x) {
        if (!is.na(x) & nchar(x) > 0) {
          paste0(" [",x,"]")
        } else {
          ""
        }
      })
      if (!is.null(inCSVFoI$df))            
        showHead <- paste0(showHead, showUoM)
    }
    
    showHead <- paste0(showHead, "</span>")
    
    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sort=FALSE, dom="t"),
                        escape=FALSE)
    
    # if DB consistency has been checked, apply colors 
    if (checkDB$checked) {
      rowClrs <- rep("white", nrow(showTab))
      
      if (!is.null(checkDB$txtInfo)) {
        rowClrs[which(showTab[[reqColFoI$id]] %in% checkDB$foiInDB)] <- "yellow"
        for (col in c(1, which(inCSVFoI$headAsChar %in% checkDB$colInDB$dede))) {
          if (col %in% checkDB$uomMissMatchCols) next;
          showDT <- formatStyle(showDT, col, "ID", 
                                backgroundColor = styleEqual(showTab[[reqColFoI$id]], rowClrs))
        }
      }
      
      if (!is.null(checkDB$txtErr)) {
        rowClrs <- rep("red", nrow(showTab))
        for (col in checkDB$uomMissMatchCols) {
          showDT <- formatStyle(showDT, col, "ID", 
                                backgroundColor = styleEqual(showTab[[reqColFoI$id]], rowClrs))
        }
      }
    }
    showDT
  }
})

###############################################
##############                   ##############
##############  Insert Feautres  ##############
##############                   ##############
###############################################

observeEvent(input$storeDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname="sos", user="postgres", password="postgres", port="5432")
  on.exit(dbDisconnect(db), add=T)
  
  foi_data <- inCSVFoI$df
  foi_header <- inCSVFoI$headAsChar ## [inclColFoI()]
  foi_uom <- inCSVFoI$UoMs ## [inclColFoI()]
  
  foi_empty_cols <- apply(foi_data, 2, function(x) all(is.na(x)))
  
  foi_header <- foi_header[!foi_empty_cols]
  foi_uom <- foi_uom[!foi_empty_cols]
  foi_data <- foi_data[,!foi_empty_cols]
  
  # any parent features that need to be inserted before? Those with empty or missing Stammanalge column
  par_foi <- is.na(foi_data[,reqColFoI$super_FoI]) | nchar(foi_data[,reqColFoI$super_FoI]) == 0
  
  nRowDf <- nrow(foi_data)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add=T)
  
  progress$set(message = "Füge Daten in DB ein.", value = 0)
  
  # parent feature first
  if (any(par_foi)) {
    for (sfoi in which(par_foi)) {# sfoi <- 1
      progress$inc(1/nRowDf)
      # check whether the feature is already in the DB
      foiInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                       foi_data[sfoi,reqColFoI$id],"'"))
      if (nrow(foiInDB) > 0) {
        if (!input$owFoI) {
          next;
        } else {
          curId <- foiInDB$featureofinterestid

          # retrive relevant part of featurerelation table
          relationTab <- dbGetQuery(db, paste0("SELECT parentfeatureid, childfeatureid FROM featurerelation WHERE parentfeatureid = ",
                                               curId, "OR childfeatureid =",curId))
          
          # remove tmp feature if still present (previous crash)
          odlTmpId <- dbGetQuery(db, "SELECT featureofinterestid FROM featureofinterest WHERE identifier = 'tmp'")
          if (nrow(odlTmpId)>0) {
            # remove tmp feature
            dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", odlTmpId$featureofinterestid))
            dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", odlTmpId$featureofinterestid))
            
            # cache-update!
            SOScacheUpdate("tmp", verbose=verbose)
          }
          
          # insert tmp feature
          insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"), 
                                         body = SOSinsFoI("tmp", "tmp", 0, 0),
                                         content_type_xml(), accept_xml())$content)
          if (verbose)
            message(insMsg)
          
          tmpId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = 'tmp'"))$featureofinterestid
          
          # map curID in table series to tmp id
          dbSendQuery(db, paste0("UPDATE series SET featureofinterestid = ", tmpId, " WHERE featureofinterestid = ", curId))
          
          # map curID in table foidata to tmp id
          dbSendQuery(db, paste0("UPDATE foidata SET featureofinterestid = ", tmpId, " WHERE featureofinterestid = ", curId))
          
          # delete relevant part of featurerelation table and from feature of interest table
          dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE parentfeatureid = ",
                                 curId, " OR childfeatureid = ", curId))
          dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", curId))
          
          # cache-update!
          SOScacheUpdate(foi_data[sfoi,reqColFoI$id])
          
          # re-insert the FoI
          insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"), 
                                         body = SOSinsFoI(foi_data[sfoi,reqColFoI$id], foi_data[sfoi,reqColFoI$name],
                                                          foi_data[sfoi,reqColFoI$lat], foi_data[sfoi,reqColFoI$lon]),
                                         content_type_xml(), accept_xml())$content)
          if (verbose)
            message(insMsg)
          
          # find the new featureofinterest id
          newFoIId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                            foi_data[sfoi,reqColFoI$id],"'"))
          
          # clear featurerelation table first
          dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", newFoIId))
          
          # re-assign the featureofinterest id to the old one
          dbSendQuery(db, paste0("UPDATE featureofinterest SET featureofinterestid = ", curId, " WHERE identifier = '", 
                                 foi_data[sfoi,reqColFoI$id],"'"))
          
          # restore the featurerelation table
          dbWriteTable(db, "featurerelation", relationTab, append = TRUE, row.names=FALSE)
          
          # re-assign the right FoI id in series table replacing the temp ID
          dbSendQuery(db, paste0("UPDATE series SET featureofinterestid = ", curId, " WHERE featureofinterestid = ", tmpId))
          
          # re-assign the right FoI id in foidata table replacing the temp ID
          dbSendQuery(db, paste0("UPDATE foidata SET featureofinterestid = ", curId, " WHERE featureofinterestid = ", tmpId))
          
          # remove tmp feature
          dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", tmpId))
          dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", tmpId))
          
          # cache-update!
          SOScacheUpdate("tmp")
        }
      } else {
        insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"), 
                                       body = SOSinsFoI(foi_data[sfoi,reqColFoI$id], foi_data[sfoi,reqColFoI$name], 
                                                        foi_data[sfoi,reqColFoI$lat], foi_data[sfoi,reqColFoI$lon]),
                                       content_type_xml(), accept_xml())$content)
        if (verbose)
          message(insMsg)
      }
    }
  }
  
  # insert remaining FoI
  for (foi in which(!par_foi)) { # foi <- 2
    progress$inc(1/nRowDf)
    
    # check whether the feature is already in the DB
    foiInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                     foi_data[foi,reqColFoI$id],"'"))
    if (nrow(foiInDB) > 0) {
      if (!input$owFoI) {
        next;
      } else {
        curId <- foiInDB$featureofinterestid
        
        # retrive relevant part of featurerelation table
        relationTab <- dbGetQuery(db, paste0("SELECT parentfeatureid, childfeatureid FROM featurerelation WHERE parentfeatureid = ",
                                             curId, "OR childfeatureid =",curId))
        
        # insert tmp feature
        insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"), 
                                       body = SOSinsFoI("tmp", "tmp", 0, 0),
                                       content_type_xml(), accept_xml())$content)
        if (verbose)
          message(insMsg)
        
        tmpId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = 'tmp'"))$featureofinterestid
        
        # map curID from series on tmp feature
        dbSendQuery(db, paste0("UPDATE series SET featureofinterestid = ", tmpId, " WHERE featureofinterestid = ", curId))
        
        # map curID in table foidata to tmp id
        dbSendQuery(db, paste0("UPDATE foidata SET featureofinterestid = ", tmpId, " WHERE featureofinterestid = ", curId))
        
        # delete relevant part of featurerelation table and from feature of interest table
        dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE parentfeatureid = ",
                               curId, " OR childfeatureid = ", curId))
        dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", curId))
        
        # cache-update!
        SOScacheUpdate(foi_data[foi,reqColFoI$id])
        
        # re-insert the FoI
        insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"), 
                                       body = SOSinsFoI(foi_data[foi,reqColFoI$id], foi_data[foi,reqColFoI$idname],
                                                        foi_data[foi,reqColFoI$lat], foi_data[foi,reqColFoI$lon], 
                                                        foi_data[foi,reqColFoI$super_FoI]),
                                       content_type_xml(), accept_xml())$content)
        if (verbose)
          message(insMsg)
        
        # find the new featureofinterest id
        newFoIId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                          foi_data[foi,reqColFoI$id],"'"))
        
        # clear featurerelation table first
        dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", newFoIId))
        
        # re-assign the featureofinterest id to the old one
        dbSendQuery(db, paste0("UPDATE featureofinterest SET featureofinterestid = ", curId, " WHERE identifier = '", 
                               foi_data[foi, reqColFoI$id],"'"))
        
        # restore the featurerelation table
        if (nrow(relationTab) > 0)
          dbWriteTable(db, "featurerelation", relationTab, append = TRUE, row.names=FALSE)
        
        # re-assign the right FoI id in series table replacing the temp ID
        dbSendQuery(db, paste0("UPDATE series SET featureofinterestid = ", curId, " WHERE featureofinterestid = ", tmpId))
        
        # re-assign the right FoI id in foidata table replacing the temp ID
        dbSendQuery(db, paste0("UPDATE foidata SET featureofinterestid = ", curId, " WHERE featureofinterestid = ", tmpId))
        
        # remove tmp feature
        dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", tmpId))
        dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", tmpId))
        
        # cache-update!
        SOScacheUpdate("tmp")
      }
      
    } else {
      insMsg <- rawToChar(POST(paste0(SOSWebApp, "service"), 
                                     body = SOSinsFoI(foi_data[foi,reqColFoI$id], foi_data[foi,reqColFoI$name],
                                                      foi_data[foi,reqColFoI$lat], foi_data[foi,reqColFoI$lon], 
                                                      foi_data[foi,reqColFoI$super_FoI]),
                                     content_type_xml(), accept_xml())$content)
      if (verbose)
        message(insMsg)
    }
  }
  
  # pre-process: add new units
  regUoMs <- dbGetQuery(db, paste0("SELECT unit FROM unit"))
  unqUoMs <- unique(foi_uom[-1])
  if (nrow(regUoMs) > 0) {
    regUoMs <- regUoMs[,1]
    misUoMs <- unqUoMs[which(sapply(unqUoMs, function(x) is.na(match(x, regUoMs))) & nchar(unqUoMs) > 0)]
  } else {
    misUoMs <- unqUoMs
  }
  
  for (uom in misUoMs) {
    dbSendQuery(db, paste0("INSERT INTO unit (unitid, unit) VALUES (nextval('unitid_seq'), '", uom, "');"))
  }
  
  ### add new columns
  regCols <- dbGetQuery(db, paste0("SELECT dede FROM foidatametadata"))[,1]
  misCols <- which(sapply(foi_header, function(x) is.na(match(x, regCols))))
  
  if (length(misCols > 0)) { 
    for (i in 1:length(misCols)) {# i <- 1
      colId <- sprintf("col%03d", i + length(regCols))
      coltype = switch(class(foi_data[,misCols[i]]),
                       integer = "numeric",
                       numeric = "numeric",
                       character = "character varying(255)")
      
      dbSendQuery(db, paste0("ALTER TABLE foidata ADD COLUMN ", colId, " ", coltype, ";"))
      
      # look-up UoM id
      unitId <- NULL
      if (nchar(foi_uom[misCols[i]]) > 0) {
        unitId <- dbGetQuery(db, paste0("SELECT unitid FROM unit WHERE unit = '", foi_uom[misCols[i]], "'"))[1,1]
      }
      
      if (is.null(unitId)) {
        dbSendQuery(db, paste0("INSERT INTO foidatametadata (columnid, dede)
                               VALUES ('", paste(colId, foi_header[misCols[i]], sep="', '"),"')"))
      } else {
        dbSendQuery(db, paste0("INSERT INTO foidatametadata (columnid, dede, uom)
                               VALUES ('", paste(colId, foi_header[misCols[i]], unitId, sep="', '"),"')"))
      }
    }
  }
  
  # feed data row-wise
  for (i in 1:nrow(foi_data)) {
    nonEmpty <- which(!is.na(foi_data[i,]))
    if (all(!nonEmpty)) next;
    
    # map csv-header to DB header via foidatametadata
    foi_db_col_ids <- dbGetQuery(db, paste0("SELECT columnid, dede FROM foidatametadata WHERE dede IN ('", 
                                            paste(foi_header[nonEmpty], collapse="', '"),"')"))
    
    # mind the ordering
    foi_db_col_ids <- foi_db_col_ids[match(foi_header[nonEmpty], foi_db_col_ids$dede), "columnid"]
    
    # find the FoI idntifier
    foi_db_id <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier ='", 
                                       foi_data[[reqColFoI$id]][i],"'"))
    
    # check whether the FoI has already some data
    if (nrow(dbGetQuery(db, paste0("SELECT id FROM foidata WHERE featureofinterestid = ", foi_db_id))) > 0) {
      if(input$owFoI) {
        dbSendQuery(db, paste0("UPDATE foidata SET ", 
                               paste(foi_db_col_ids, foi_data[i, nonEmpty], sep = " = '", collapse = "', "),
                               "' WHERE featureofinterestid = ", foi_db_id, ";"))
      } else {
        next()
      }
    } else {
      dbSendQuery(db, paste0("INSERT INTO foidata ( featureofinterestid, ", paste(foi_db_col_ids, collapse=", "), ") ",
                             "VALUES ('", foi_db_id, "', '", paste(foi_data[i, nonEmpty], collapse="', '"), "')"))
    }
  }
  
  showModal(modalDialog(
    title = "Vorgang abgeschlossen.",
    "Kläranlagen/Verfahrensschritte erfolgreich angelegt.",
    easyClose = TRUE,
    footer = NULL
  ))
})
