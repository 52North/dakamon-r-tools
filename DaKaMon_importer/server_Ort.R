################################################################################
#############################   Upload des Ortes   #############################
################################################################################

# storage of variables that might change through the GUI
inCSVOrt <- reactiveValues()
valiOrt <- reactiveValues(validated = FALSE)
checkDBOrt <- reactiveValues(checked = FALSE)

sepOrt <- colSep
decOrt <- decSep

observeEvent(input$csvFileOrt, {
  valiOrt$validated <- FALSE
  checkDBOrt$checked <- FALSE
  
  # check whether an encoding has been set; fallback: guess the eoncoding using readr
  if (is.null(csvEncode)) {
    csvEncode <- readr::guess_encoding(input$csvFileOrt$datapath)
    csvEncode <- csvEncode$encoding[which.max(csvEncode$confidence)]
  }
  
  inCSVOrt$csvEncode <- csvEncode
  
  inCSVOrt$df <- read.csv(input$csvFileOrt$datapath,
                          header = TRUE,
                          sep = sepOrt, dec = decOrt,
                          stringsAsFactors = FALSE, 
                          fileEncoding = inCSVOrt$csvEncode)
  
  inCSVOrt$headAsChar <- colnames(inCSVOrt$df)
  
  ## validation of Ort csv-file 
  # look for required column names
  # check whether columns have unique names
  
  txt <- NULL
  if (!(reqColOrt$id %in% inCSVOrt$headAsChar) || length(unique(inCSVOrt$df[,reqColOrt$id])) != length(inCSVOrt$df[,reqColOrt$id]))
    txt <- paste0(txt, "<li>Jeder Ort benötigt eine persistente und eindeutige ID in der Spalte'", reqColOrt$id, "'.</li>")
  for (reqColName in reqColOrt[-1]) {
    if (!(reqColName %in% inCSVOrt$headAsChar))
      txt <- paste0(txt, "<li>Bitte die Spalte '", reqColName, "' ergänzen.</li>", sep="")
  }
  
  if(length(unique(inCSVOrt$headAsChar)) != length(inCSVOrt$headAsChar))
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")
  
  valiOrt$txt <- txt
  valiOrt$validated <- TRUE
})

# write txt feedback as html list - or action button

output$OrtValidationOut <- renderUI({
  if (valiOrt$validated) {
    if (is.null(valiOrt$txt)) {
      actionButton("checkDB", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiOrt$txt, "</ul></div></html"))
    }
  } else {
    return()
  }
})

##########################
## check DB consistency ##
##########################

# find existing Orte

observeEvent(input$checkDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbName, user=dbUser, password=dbPassword, port=dbPort)
  on.exit(dbDisconnect(db), add=T)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add = T)
  
  progress$set(message = "Prüfe bereits registrierte Orte.", value = 0)
  
  # get all Orte from the DB that have any of the identifiers in the CSV
  OrtInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('",
                                   paste(inCSVOrt$df[,reqColOrt$id], collapse="', '"),"')"))
  if (nrow(OrtInDB) > 0) {
    checkDBOrt$txt <- paste("Folgende Orte sind bereits in der DB: <ul><li>",
                            paste0(OrtInDB$identifier, collapse="</li><li>"))
  } else {
    checkDBOrt$txt <- NULL
  }
  
  checkDBOrt$OrtInDB <- OrtInDB
  
  checkDBOrt$checked <- TRUE
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button
output$OrtDBConsistencyOut <- renderUI({
  if (checkDBOrt$checked) {
    if (is.null(checkDBOrt$txt)) {
      actionButton("storeDB", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBOrt$txt, "</li></ul></div></html"))
    }
  } else {
    return()
  }
})

# plot table with CSV
output$tableOrt <- renderDataTable({
  if (!is.null(inCSVOrt$df)) {
    showTab <- inCSVOrt$df
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showHead <- paste0(showHead, "</span>")
    
    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sort=FALSE, dom="t",
                                       language=list(url = lngJSON)),
                        escape=FALSE)
    
    # if DB consistency has been checked, apply colors
    if (checkDBOrt$checked) {
      rowColors <- rep("white", nrow(showTab))
      
      if (nrow(checkDBOrt$OrtInDB) > 0) {
        rowColors[showTab$ID %in% checkDBOrt$OrtInDB] <- "red"
        showDT <- formatStyle(showDT, "ID", target="row",
                              backgroundColor = styleEqual(showTab$ID, rowColors))
      }
    }
    showDT
  }
})

#####################
## Insert Feautres ##
#####################

observeEvent(input$storeDB, {
  #db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbName, user=dbUser, password=dbPassword, port=dbPort)
  db <- connectToDB()
  on.exit(dbDisconnect(db), add=T)
  
  Ort_data <- inCSVOrt$df
  Ort_header <- inCSVOrt$headAsChar
  
  Ort_empty_cols <- apply(Ort_data, 2, function(x) all(is.na(x)))
  
  Ort_header <- Ort_header[!Ort_empty_cols]
  Ort_data <- Ort_data[,!Ort_empty_cols]
  
  nRowDf <- nrow(Ort_data)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add=T)
  
  progress$set(message = "Füge Orte in DB ein.", value = 0)
  
  ## add missign columns
  regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata WHERE prefixid IN ('ort', 'global')"))[,1]
  ortDataCols <- dbGetQuery(db, paste0("SELECT columnid, prefixid, dede FROM column_metadata WHERE prefixid IN ('ort')"))
  misCols <- which(sapply(Ort_header, # TODO drop ID and Name
                          function(x) is.na(match(x, regCols))))
  
  if (length(misCols > 0)) {
    for (i in 1:length(misCols)) {# i <- 1
      colId <- sprintf("col%03d", i + length(regCols))
      coltype = switch(class(Ort_data[,misCols[i]]),
                       integer = "numeric",
                       numeric = "numeric",
                       character = "character varying(255)")
      
      # TODO adopt to new FoI table
      dbSendQuery(db, paste0("ALTER TABLE ort_data ADD COLUMN ", colId, " ", coltype, ";"))
      
      dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, prefixid, dede)
                               VALUES ('", paste(colId, 'ort', Ort_header[misCols[i]], sep="', '"),"')"))
    }
  }

  # if there are already Orte in the DB that are again in the CSV
  if (nrow(checkDBOrt$OrtInDB) > 0) {
    ## UPDATE FoI and data via SQL, returns the id (pkid) of the updated feature ##
    dbSendQuery(db, paste0("with update_ort as (
      UPDATE featureofinterest SET
        name = name_var,
        geom =  ST_GeomFromText('POINT (' || lat_var || ' ' || lon_var || ')', 4326)
      WHERE identifier = var
      RETURNING featureofinterestid
    )
    UPDATE ort_data SET
      ort_col003 = ort_col003_var,
      ort_col004 = ort_col004_var,
      ort_col005 = ort_col005_var,
      ort_col006 = ort_col006_var,
      ort_col007 = ort_col007_var,
      ort_col008 = ort_col008_var,
      ort_col009 = ort_col009_var,
      ort_col010 = ort_col010_var,
      ort_col011 = ort_col011_var,
      ort_col012 = ort_col012_var,
      ort_col013 = ort_col013_var,
      ort_col014 = ort_col014_var,
      ort_col015 = ort_col015_var
    WHERE featureofinterestid = (SELECT featureofinterestid FROM update_ort)
    RETURNING featureofinterestid;"))
  } else {
    ## INSERT FoI and data via SQL, returns the id (pkid) of the inserted feature ##
    for (ort in 1:nrow(Ort_data)) {
      
      dynamicColumns = paste0(ortDataCols[,1], collapse=', ')
      dynamicValues = ""
      for (col in ortDataCols[["dede"]]) {
        value = Ort_data[ort,col]
        if (is.null(value) || is.na(value)) {
          if (class(value) == "character") {
            dynamicValues = paste(dynamicValues, '', sep=', ')
          } else {
            dynamicValues = paste(dynamicValues, -1, sep=', ')
          }
        } else {
          if (class(value) == "character") {
            value = paste0("'", value, "'")       
          }
          dynamicValues = paste(dynamicValues, value, sep=', ') 
        }
      }
      
      insertFeature = paste("INSERT INTO featureofinterest (featureofinterestid, featureofinteresttypeid, identifier, name, geom)
                             VALUES (nextval('featureofinterestid_seq'), 1",
                            paste0("'", Ort_data[ort,reqColOrt$id], "'"),
                            paste0("'", Ort_data[ort,reqColOrt$name], "'"),
                            paste0("ST_GeomFromText('POINT ('|| ", Ort_data[ort,reqColOrt$lat], "|| ' ' ||", Ort_data[ort,reqColOrt$lon], " || ')', 4326)) "), 
                            sep=", ")
      
      insertOrt = paste0("INSERT INTO ort_data (featureofinterestid, rndid, ", dynamicColumns, ")
                          SELECT ort_id, pseudo_encrypt(nextval('rndIdSeq')::int)", dynamicValues, " FROM insert_ort")
      query = paste("WITH insert_ort as (", insertFeature, " RETURNING featureofinterestid as ort_id)",
                    insertOrt, ";");
      dbSendQuery(db, query)
    }
  }
  
  showModal(modalDialog(
    title = "Vorgang abgeschlossen.",
    "Orte erfolgreich angelegt.",
    easyClose = TRUE,
    footer = NULL
  ))
})