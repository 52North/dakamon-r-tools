################################################################################
#####################   Upload der Probenahmestelle (PNS)   ####################
################################################################################

# storage of variables that might change through the GUI
inCSVPNS <- reactiveValues()
valiPNS <- reactiveValues(validated = FALSE)
checkDBPNS <- reactiveValues(checked = FALSE)

sepPNS <- colSep
decPNS <- decSep

observeEvent(input$csvFilePNS, {
  valiPNS$validated <- FALSE
  checkDBPNS$checked <- FALSE
  
  # check whether an encoding has been set; fallback: guess the eoncoding using readr
  if (is.null(csvEncode)) {
    csvEncode <- readr::guess_encoding(input$csvFilePNS$datapath)
    csvEncode <- csvEncode$encoding[which.max(csvEncode$confidence)]
  }
  
  inCSVPNS$csvEncode <- csvEncode
  
  inCSVPNS$df <- read.csv(input$csvFilePNS$datapath,
                          header = TRUE,
                          sep = sepPNS, dec = decPNS,
                          stringsAsFactors = FALSE, 
                          fileEncoding = inCSVPNS$csvEncode)
  
  inCSVPNS$headAsChar <- colnames(inCSVPNS$df)
  
  ## validation of PNS csv-file 
  # look for required column names
  # check whether columns have unique names
  
  txt <- NULL
  if (!(reqColPNS$id %in% inCSVPNS$headAsChar) || length(unique(inCSVPNS$df[,reqColPNS$id])) != length(inCSVPNS$df[,reqColPNS$id]))
    txt <- paste0(txt, "<li>Jede Probenahmestelle benötigt eine persistente und eindeutige ID in der Spalte'", reqColPNS$id, "'.</li>")
  for (reqColName in reqColPNS[-1]) {
    if (!(reqColName %in% inCSVPNS$headAsChar))
      txt <- paste0(txt, "<li>Bitte die Spalte '", reqColName, "' ergänzen.</li>", sep="")
  }
  
  if(length(unique(inCSVPNS$headAsChar)) != length(inCSVPNS$headAsChar))
    txt <- paste0(txt, "<li>Bitte nur eindeutige Spaltennamen verwenden.</li>")
  
  valiPNS$txt <- txt
  valiPNS$validated <- TRUE
})

# write txt feedback as html list - or action button

output$PNSValidationOut <- renderUI({
  if (valiPNS$validated) {
    if (is.null(valiPNS$txt)) {
      actionButton("checkDB", "Prüfe Datenkonsistenz!")
    } else {
      HTML(paste("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\"><ul>", valiPNS$txt, "</ul></div></html"))
    }
  } else {
    return()
  }
})

##########################
## check DB consistency ##
##########################

# find existing PNSe

observeEvent(input$checkDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbName, user=dbUser, password=dbPassword, port=dbPort)
  on.exit(dbDisconnect(db), add=T)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add = T)
  
  progress$set(message = "Prüfe bereits registrierte PNSe.", value = 0)
  
  # get all PNSe from the DB that have any of the identifiers in the CSV
  PNSInDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('",
                                   paste(inCSVPNS$df[,reqColPNS$id], collapse="', '"),"')"))
  if (nrow(PNSInDB) > 0) {
    checkDBPNS$txt <- paste("Folgende PNSe sind bereits in der DB: <ul><li>",
                            paste0(PNSInDB$identifier, collapse="</li><li>"))
  } else {
    checkDBPNS$txt <- NULL
  }
  
  # TODO: check whether referenced super FoIs exist; if not -> error state: no upload
  
  checkDBPNS$PNSInDB <- PNSInDB
  
  checkDBPNS$checked <- TRUE
}, ignoreInit=TRUE)


# output of DB consistency check as html - or action button
output$PNSDBConsistencyOut <- renderUI({
  if (checkDBPNS$checked) {
    if (is.null(checkDBPNS$txt)) {
      actionButton("storeDB", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDBPNS$txt, "</li></ul></div></html"))
    }
  } else {
    return()
  }
})

# plot table with CSV
output$tablePNS <- renderDataTable({
  if (!is.null(inCSVPNS$df)) {
    showTab <- inCSVPNS$df
    
    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
    showHead <- paste0(showHead, "</span>")
    
    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, bFilter=FALSE,
                                       scrollX=TRUE, sPNS=FALSE, dom="t",
                                       language=list(url = lngJSON)),
                        escape=FALSE)
    
    # if DB consistency has been checked, apply colors
    if (checkDBPNS$checked) {
      rowColors <- rep("white", nrow(showTab))
      
      if (nrow(checkDBPNS$PNSInDB) > 0) {
        rowColors[showTab$ID %in% checkDBPNS$PNSInDB] <- "red"
        showDT <- formatStyle(showDT, "ID", target="row",
                              backgroundColor = styleEqual(showTab$ID, rowColors))
      }
    }
    showDT
  }
})

#############################
## Insert Probenamestellen ##
#############################

observeEvent(input$storeDB, {
  db <- dbConnect("PostgreSQL", host=dbHost, dbname=dbName, user=dbUser, password=dbPassword, port=dbPort)
  on.exit(dbDisconnect(db), add=T)
  
  PNS_data <- inCSVPNS$df
  PNS_header <- inCSVPNS$headAsChar
  
  PNS_empty_cols <- apply(PNS_data, 2, function(x) all(is.na(x)))
  
  PNS_header <- PNS_header[!PNS_empty_cols]
  PNS_data <- PNS_data[,!PNS_empty_cols]
  
  nRowDf <- nrow(PNS_data)
  
  progress <- shiny::Progress$new()
  on.exit(progress$close(), add=T)
  
  progress$set(message = "Füge Probenahmestellen in DB ein.", value = 0)
  
  ## add missign columns
  regCols <- dbGetQuery(db, paste0("SELECT dede FROM column_metadata"))[,1]
  misCols <- which(sapply(paste0("pns_", PNS_header), # TODO drop ID, parent identifier
                          function(x) is.na(match(x, regCols))))
  
  if (length(misCols > 0)) {
    for (i in 1:length(misCols)) {# i <- 1
      colId <- paste0("pns_", sprintf("col%03d", i + length(regCols)))
      coltype = switch(class(PNS_data[,misCols[i]]),
                       integer = "numeric",
                       numeric = "numeric",
                       character = "character varying(255)")
      
      # TODO adopt to new FoI table
      dbSendQuery(db, paste0("ALTER TABLE pns_data ADD COLUMN ", colId, " ", coltype, ";"))
      
      dbSendQuery(db, paste0("INSERT INTO column_metadata (columnid, dede)
                               VALUES ('", paste(colId, PNS_header[misCols[i]], sep="', '"),"')"))
    }
  }
  
  # if there are already PNSe in the DB that are again in the CSV
  if (nrow(checkDBPNS$PNSInDB) > 0) {
    ## UPDATE FoI and data via SQL, mind the parental FoI, returns the id (pkid) of the updated feature ##
    dbSendQuery(db, paste0("with update_pns as (
      UPDATE featureofinterest 
      	SET
      		name = name_var,
      		geom =  ST_GeomFromText('POINT (' || lat_var || ' ' || lon_var || ')', 4326)
      WHERE identifier = var
      RETURNING featureofinterestid
      )
      UPDATE pns_data 
      	SET
      		pns_col003 = pns_col003_var,
      		pns_col004 = pns_col004_var,
      		pns_col005 = pns_col005_var,
      		pns_col006 = pns_col006_var,
      		pns_col007 = pns_col007_var
      WHERE featureofinterestid = (SELECT featureofinterestid FROM update_pns)
      RETURNING featureofinterestid;"))
    
    ## if pns - foi relation does not exist, insert relation ## 
    dbSendQuery(db, paste0("INSERT INTO featurerelation
        VALUES 
      	(SELECT featureofinterestid FROM featureofinterest WHERE identifier = 'parent_identifier_var',
      			featureofinterestid);"))
  } else {
    ## INSERT FoI and data via SQL, mind the parental FoI, returns the id (pkid) of the updated feature ##
    dbSendQuery(db, paste0("with insert_pns as (
    INSERT INTO featureofinterest
         (featureofinterestid, featureofinteresttypeid, identifier, name, geom) 
         VALUES 
         (nextval('featureofinterestid_seq'),
         1,
         'identifier_var',
         'name_var',
         ST_GeomFromText('POINT (' || lat_var || ' ' || lon_var || ')', 4326))
         RETURNING featureofinterestid
  ), insert_pns_rel as (
         INSERT INTO featurerelation
         INSERT INTO featurerelation 
         SELECT insert_ort.featureofinterestid, insert_pns.featureofinterestid
         FROM insert_ort, insert_pns
         RETURNING childfeatureid
    )
   INSERT INTO pns_data 
   SELECT featureofinterestid, 
   pseudo_encrypt(nextval('rndIdSeq')::int),
   'pns_col003_var',
   'pns_col004_var',
   'pns_col005_var',
   pns_col006_var,
   pns_col007_var
   FROM insert_pns
   RETURNING featureofinterestid;"))
  }
  
  showModal(modalDialog(
    title = "Vorgang abgeschlossen.",
    "Probenahmestellen erfolgreich angelegt.",
    easyClose = TRUE,
    footer = NULL
  ))
})