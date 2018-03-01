## server logic for the FoIs


###############################################################################
###################################         ###################################
###################################   FoI   ###################################
###################################         ###################################
###############################################################################

inCSVFoI <- reactiveValues()
vali <- reactiveValues(validated = FALSE)
checkDB <- reactiveValues(checked = FALSE)

## FoI logic
# toAdd: validate FoIs IDs?
# toAdd: restrict hierarchie?
# toAdd: validate UoM?
# add Button "check DB consistency" -> list known vs new variables; matching vs non-matching uom ( -> hottable to fix?) -> manually update csv file

inclRowFoI <- reactive({
  if (is.null(inCSVFoI$df)) 
    return(numeric())
  exclText <- input$exclRowFoI 
  if (is.null(exclText))
    return(1:ncol(inCSVFoI$df))
  exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
  !c(1:nrow(inCSVFoI$df)) %in% exclNum[!is.na(exclNum)]
})

inclColFoI <- reactive({
  if (is.null(inCSVFoI$df))
    return(numeric())
  exclText <- input$exclColFoI
  if (is.null(exclText))
    return(1:ncol(inCSVFoI$df))
  exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
  !c(1:ncol(inCSVFoI$df)) %in% exclNum[!is.na(exclNum)]
})

observeEvent(input$csvFileFoI, {
  vali$validated <- FALSE
  checkDB$checked <- FALSE
  
  inCSVFoI$headAsChar <- as.character(read.csv(input$csvFileFoI$datapath,
                                               header = FALSE,
                                               sep = input$sepFoI, dec = input$decFoi,
                                               nrows = 1, stringsAsFactors = FALSE))
  
  inCSVFoI$UoMs <- read.csv(input$csvFileFoI$datapath, header = FALSE,
                            sep = input$sepFoI, dec = input$decFoi,
                            skip = as.numeric(input$UoMFoI), nrows = 1,
                            stringsAsFactors = FALSE)
  inCSVFoI$UoMs[is.na(inCSVFoI$UoMs)] <- ""
  inCSVFoI$UoMs <- as.character(inCSVFoI$UoMs)
  
  inCSVFoI$df <- read.csv(input$csvFileFoI$datapath, header = FALSE,
                          sep = input$sepFoI, dec = input$decFoi,
                          skip = as.numeric(input$UoMFoI)+1,
                          stringsAsFactors = FALSE)
  colnames(inCSVFoI$df) <- inCSVFoI$headAsChar
  
  ################################
  ## validation of FoI csv-file ##
  ################################
  
  txt <- NULL
  if (!("ID" %in% inCSVFoI$headAsChar) || length(unique(inCSVFoI$headAsChar)) != length(inCSVFoI$headAsChar))
    txt <- paste(txt, "<li>An unique identifier is mandatory for each feature of interest; please supply a non-empty and unique column 'ID'.</li>", sep="")
  if (!("Name" %in% inCSVFoI$headAsChar))
    txt <- paste(txt, "<li>A name is mandatory for each feature of interest; please supply a non-empty column 'Name'.</li>", sep="")
  if (!("lat" %in% inCSVFoI$headAsChar))
    txt <- paste(txt, "<li>Latitude is mandatory for each feature of interest; please supply a non-empty column 'lat'.</li>", sep="")
  if (!("lon" %in% inCSVFoI$headAsChar))
    txt <- paste(txt, "<li>Longitude is mandatory for each feature of interest; please supply a non-empty column 'lon'.</li>", sep="")
  if (!("super_FoI" %in% inCSVFoI$headAsChar))
    txt <- paste(txt, "<li>A superior feature of interest is mandatory for each feature of interest (yet, it might be empty); please supply a column 'super_FoI'.</li>", sep="")
  
  vali$txt <- txt
  
  comp_header <- outer(inCSVFoI$headAsChar, inCSVFoI$headAsChar, "==")
  if(any(comp_header[upper.tri(comp_header)]))
    vali$txt <- paste(vali$txt, "<li>Column names must be unique.</li>", sep="")
  
  vali$validated <- TRUE
})

# look for Name, ID, lat, lon and super_FoI,
# check whether columns have unique names

output$foiValidationOut <- renderUI({
  if (vali$validated) {
    if (is.null(vali$txt)) {
      actionButton("checkDB", "Check DB consistency!")
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
  FoIinDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('", 
                                   paste(inCSVFoI$df$ID[inclRowFoI()], collapse="', '"),"')"))
  if (nrow(FoIinDB) > 0) {
    checkDB$txtInfo <- paste("The following features are already in the DB: <ul><li>",
                             paste0(FoIinDB$identifier, collapse="</li><li>"))
  } else {
    checkDB$txtInfo <- NULL
  }
  
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
          "The following columns have non-matching units of measurement: <ul><li>",
          paste0(
            paste0(checkDB$colInDB$dede[checkDB$uomMissMatchCols], ": [",
                   checkDB$colInDB$unit[checkDB$uomMissMatchCols], "] "),
            collapse = "</li><li>"
          )
        )
    }
  }
  
  checkDB$checked <- TRUE
}, ignoreInit=TRUE)

output$DBConsistencyTxtOut <- renderUI({
  if (checkDB$checked) {
    if (!is.null(checkDB$txtInfo)) {
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
        actionButton("storeDB", "Store in DB!")
      } 
    } 
  } else {
    HTML("")
  }
})

output$tableFoI <- DT::renderDataTable({
  if (!is.null(inCSVFoI$df)) {
    showTab <- inCSVFoI$df[inclRowFoI(), inclColFoI(), drop=F]
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    if (!is.na(input$UoMFoI) && !is.null(input$UoMFoI)) {
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
                                                 scrollX=TRUE, sort=FALSE),
                        escape=FALSE)
    
    # if DB consistency has been checked, apply colors 
    if (checkDB$checked) {
      rowClrs <- rep("white", nrow(showTab))
      
      if (!is.null(checkDB$txtInfo)) {
        rowClrs[which(showTab$ID %in% checkDB$foiInDB)] <- "yellow"
        for (col in c(1, which(inCSVFoI$headAsChar %in% checkDB$colInDB$dede))) {
          if (col %in% checkDB$uomMissMatchCols) next;
          showDT <- formatStyle(showDT, col, "ID", 
                                backgroundColor = styleEqual(showTab$ID, rowClrs))
        }
      }
      
      if (!is.null(checkDB$txtErr)) {
        rowClrs <- rep("red", nrow(showTab))
        for (col in checkDB$uomMissMatchCols) {
          showDT <- formatStyle(showDT, col, "ID", 
                                backgroundColor = styleEqual(showTab$ID, rowClrs))
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
  foi_data <- inCSVFoI$df[inclRowFoI(), inclColFoI(), drop=F]
  foi_header <- inCSVFoI$headAsChar[inclColFoI()]
  foi_uom <- inCSVFoI$UoMs[inclColFoI()]
  
  foi_empty_cols <- apply(foi_data, 2, function(x) all(is.na(x)))
  
  foi_header <- foi_header[!foi_empty_cols]
  foi_uom <- foi_uom[!foi_empty_cols]
  foi_data <- foi_data[,!foi_empty_cols]
  
  # any parent features that need to be inserted before? Those with empty or missing super_FoI column
  par_foi <- is.na(foi_data$super_FoI) | nchar(foi_data$super_FoI) == 0
  
  if (any(par_foi)) {
    for (sfoi in which(par_foi)) {# sfoi <- 1
      # check whether the feature is already in the DB
      foiInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                       foi_data[sfoi,]$ID,"'"))
      if (nrow(foiInDB) > 0) {
        if (!input$owFoI) {
          next;
        } else {
          curId <- foiInDB$featureofinterestid
          
          # retrive relevant part of featurerelation table
          relationTab <- dbGetQuery(db, paste0("SELECT parentfeatureid, childfeatureid FROM featurerelation WHERE parentfeatureid = ",
                                               curId, "OR childfeatureid =",curId))
          
          # delete relevant part of featurerelation table and from feature of interest table
          dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE parentfeatureid = ",
                                 curId, " OR childfeatureid = ", curId))
          dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", curId))
          
          # cache-update!
          POST(url = paste0(SOSWebApp, "admin/cache/reload"), 
               config=authenticate("a","a"), body="a")
          
          reqMsg <- rawToChar(httr::POST(paste0(SOSWebApp, "service"), 
                                         body = SOSreqFoI(foi_data[sfoi,]$ID), # foi_data[sfoi,]$ID
                                         content_type_xml(), accept_json())$content)
          
          while (is.null(fromJSON(reqMsg)$exceptions)) {
            if (verbose)
              message(foi_data[foi,]$ID)
            
            Sys.sleep(0.5)
            reqMsg <- rawToChar(httr::POST(paste0(SOSWebApp, "service"), 
                                           body = SOSreqFoI(foi_data[sfoi,]$ID), # foi_data[sfoi,]$ID
                                           content_type_xml(), accept_json())$content)
          }
          
          # re-insert the FoI
          insMsg <- rawToChar(httr::POST(paste0(SOSWebApp, "service"), 
                                         body = SOSinsFoI(foi_data[sfoi,]$ID, foi_data[sfoi,]$Name,
                                                          foi_data[sfoi,]$lat, foi_data[sfoi,]$lon),
                                         content_type_xml(), accept_xml())$content)
          if (verbose)
            message(insMsg)
          
          # find the new featureofinterest id
          newFoIId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                            foi_data[sfoi,]$ID,"'"))
          # clear featurerelation table first
          dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", newFoIId))
          # re-assign the featureofinterest id to the old one
          dbSendQuery(db, paste0("UPDATE featureofinterest SET featureofinterestid = ", curId, " WHERE identifier = '", 
                                 foi_data[sfoi,]$ID,"'"))
          # restore the featurerelation table
          dbWriteTable(db, "featurerelation", relationTab, append = TRUE, row.names=FALSE)
        }
      } else {
        insMsg <- rawToChar(httr::POST(paste0(SOSWebApp, "service"), 
                                       body = SOSinsFoI(foi_data[sfoi,]$ID, foi_data[sfoi,]$Name, 
                                                        foi_data[sfoi,]$lat, foi_data[sfoi,]$lon),
                                       content_type_xml(), accept_xml())$content)
       if (verbose)
         message(insMsg)
      }
    }
  }
  
  # insert remaining FoI
  for (foi in which(!par_foi)) { # foi <- 2
    # check whether the feature is already in the DB
    foiInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                     foi_data[foi,]$ID,"'"))
    if (nrow(foiInDB) > 0) {
      if (!input$owFoI) {
        next;
      } else {
        curId <- foiInDB$featureofinterestid
        # retrive relevant part of featurerelation table
        relationTab <- dbGetQuery(db, paste0("SELECT parentfeatureid, childfeatureid FROM featurerelation WHERE parentfeatureid = ",
                                             curId, "OR childfeatureid =",curId))
        
        # delete relevant part of featurerelation table and from feature of interest table
        dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE parentfeatureid = ",
                               curId, " OR childfeatureid = ", curId))
        dbSendQuery(db, paste0("DELETE FROM featureofinterest WHERE featureofinterestid = ", curId))
        
        # cache-update!
        POST(url = paste0(SOSWebApp, "admin/cache/reload"), 
             config=authenticate("a","a"), body="a")
        
        reqMsg <- rawToChar(httr::POST(paste0(SOSWebApp, "service"), 
                                       body = SOSreqFoI(foi_data[foi,]$ID), # foi_data[sfoi,]$ID
                                       content_type_xml(), accept_json())$content)
        
        while (is.null(fromJSON(reqMsg)$exceptions)) {
          if (verbose)
            message(foi_data[foi,]$ID)
          
          Sys.sleep(0.5)
          reqMsg <- rawToChar(httr::POST(paste0(SOSWebApp, "service"), 
                                         body = SOSreqFoI(foi_data[foi,]$ID), # foi_data[sfoi,]$ID
                                         content_type_xml(), accept_json())$content)
        }
        
        # re-insert the FoI
        insMsg <- rawToChar(httr::POST(paste0(SOSWebApp, "service"), 
                                       body = SOSinsFoI(foi_data[foi,]$ID, foi_data[foi,]$Name,
                                                        foi_data[foi,]$lat, foi_data[foi,]$lon, 
                                                        foi_data[foi,]$super_FoI),
                                       content_type_xml(), accept_xml())$content)
        if (verbose)
          message(insMsg)
        
        # find the new featureofinterest id
        newFoIId <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                          foi_data[foi,]$ID,"'"))
        # clear featurerelation table first
        dbSendQuery(db, paste0("DELETE FROM featurerelation WHERE childfeatureid = ", newFoIId))
        # re-assign the featureofinterest id to the old one
        dbSendQuery(db, paste0("UPDATE featureofinterest SET featureofinterestid = ", curId, " WHERE identifier = '", 
                               foi_data[foi,]$ID,"'"))
        # restore the featurerelation table
        dbWriteTable(db, "featurerelation", relationTab, append = TRUE, row.names=FALSE)
      }
    } else {
      insMsg <- rawToChar(httr::POST(paste0(SOSWebApp, "service"), 
                                     body = SOSinsFoI(foi_data[foi,]$ID, foi_data[foi,]$Name,
                                                      foi_data[foi,]$lat, foi_data[foi,]$lon, 
                                                      foi_data[foi,]$super_FoI),
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
      dbColumn(db, "foidata", colId, "add", 
               coltype = switch(class(foi_data[,misCols[i]]),
                                integer = "numeric",
                                numeric = "numeric",
                                character = "character varying(255)"))
      
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
                                       foi_data$ID[i],"'"))
    
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
    title = "Upload completed.",
    "Feature of interest upload completed.",
    easyClose = TRUE,
    footer = NULL
  ))
})
