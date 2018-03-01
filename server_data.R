################################################################################
###################################          ###################################
###################################   DATA   ###################################
###################################          ###################################
################################################################################

inCSVData <- reactiveValues()
valiData <- reactiveValues(validated = FALSE)
checkDBData <- reactiveValues(checked = FALSE)

## data logic
# toAdd: validate UoM?
# add Button "check DB consistency" -> list known vs new variables; matching vs non-matching uom ( -> hottable to fix?) -> manually update csv file
# check Identifier: mark missing identifier!

inclRowData <- reactive({
  if (is.null(inCSVData$df)) 
    return(numeric())
  exclText <- input$exclRowData 
  if (is.null(exclText))
    return(1:ncol(inCSVData$df))
  exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
  !c(1:nrow(inCSVData$df)) %in% exclNum[!is.na(exclNum)]
})

inclColData <- reactive({
  if (is.null(inCSVData$df))
    return(numeric())
  exclText <- input$exclColData
  if (is.null(exclText))
    return(1:ncol(inCSVData$df))
  exclNum <- as.numeric(strsplit(exclText, fixed = T, split=",")[[1]])
  !c(1:ncol(inCSVData$df)) %in% exclNum[!is.na(exclNum)]
})

observeEvent(input$UoMData, {
  if (!is.null(input$csvFileData$datapath)) {
    inCSVData$UoMs <- read.csv(input$csvFileData$datapath, header = FALSE,
                               sep = input$sepData, dec = input$decData,
                               skip = as.numeric(input$UoMData), nrows = 1,
                               stringsAsFactors = FALSE)
    inCSVData$UoMs[is.na(inCSVData$UoMs)] <- ""
    inCSVData$UoMs <- as.character(inCSVData$UoMs)
    
    inCSVData$df <- read.csv(input$csvFileData$datapath, header = FALSE,
                             sep = input$sepData, dec = input$decData,
                             skip = rowSkip()+1,
                             stringsAsFactors = FALSE)
    colnames(inCSVData$df) <- inCSVData$headAsChar
  }
})

observeEvent(input$BgData, {
  if (!is.null(input$csvFileData$datapath)) {
    inCSVData$bg <- as.character(read.csv(input$csvFileData$datapath,
                                          header = FALSE,
                                          sep = input$sepData, dec = input$decData,
                                          skip = as.numeric(input$BgData), nrows = 1, 
                                          stringsAsFactors = FALSE))
    
    inCSVData$df <- read.csv(input$csvFileData$datapath, header = FALSE,
                             sep = input$sepData, dec = input$decData,
                             skip = rowSkip()+1,
                             stringsAsFactors = FALSE)
    colnames(inCSVData$df) <- inCSVData$headAsChar
  }
})

observeEvent(input$StgrData, {
  if (!is.null(input$csvFileData$datapath)) {
    inCSVData$stgr <- as.character(read.csv(input$csvFileData$datapath,
                                            header = FALSE,
                                            sep = input$sepData, dec = input$decData,
                                            skip = as.numeric(input$StgrData), nrows = 1, 
                                            stringsAsFactors = FALSE))
    
    inCSVData$df <- read.csv(input$csvFileData$datapath, header = FALSE,
                             sep = input$sepData, dec = input$decData,
                             skip = rowSkip()+1,
                             stringsAsFactors = FALSE)
    colnames(inCSVData$df) <- inCSVData$headAsChar
  }
})

rowSkip <- reactive(max(as.numeric(c(input$StgrData, input$BgData, input$UoMData))))

observeEvent(input$csvFileData, {
  vali$validated <- FALSE
  checkDB$checked <- FALSE

  inCSVData$headAsChar <- as.character(read.csv(input$csvFileData$datapath,
                                               header = FALSE,
                                               sep = input$sepData, dec = input$decData,
                                               nrows = 1, stringsAsFactors = FALSE))
  
  inCSVData$UoMs <- read.csv(input$csvFileData$datapath, header = FALSE,
                             sep = input$sepData, dec = input$decData,
                             skip = as.numeric(input$UoMData), nrows = 1,
                             stringsAsFactors = FALSE)
  inCSVData$UoMs[is.na(inCSVData$UoMs)] <- ""
  inCSVData$UoMs <- as.character(inCSVData$UoMs)

  inCSVData$bg <- as.character(read.csv(input$csvFileData$datapath,
                                        header = FALSE,
                                        sep = input$sepData, dec = input$decData,
                                        skip = as.numeric(input$BgData), nrows = 1, 
                                        stringsAsFactors = FALSE))
  
  inCSVData$stgr <- as.character(read.csv(input$csvFileData$datapath,
                                          header = FALSE,
                                          sep = input$sepData, dec = input$decData,
                                          skip = as.numeric(input$StgrData), nrows = 1, 
                                          stringsAsFactors = FALSE))
  
  
  inCSVData$df <- read.csv(input$csvFileData$datapath, header = FALSE,
                          sep = input$sepData, dec = input$decData,
                          skip = rowSkip()+1,
                          stringsAsFactors = FALSE)
  colnames(inCSVData$df) <- inCSVData$headAsChar
  
  #################################
  ## validation of data csv-file ##
  #################################
  
  txt <- NULL
  if (!("ID" %in% inCSVData$headAsChar))
    txt <- paste(txt, "<li>A pre-registered identifier is mandatory for each row; please supply a non-empty column 'ID'.</li>", sep="")
  if (!("Proben-Nr" %in% inCSVData$headAsChar))
    txt <- paste(txt, "<li>A probe number is mandatory (yet, it might be empty); please supply a column 'Proben-Nr'.</li>", sep="")
  if (!("Datum" %in% inCSVData$headAsChar))
    txt <- paste(txt, "<li>Datum is mandatory for each row; please supply a non-empty column 'Datum'.</li>", sep="")

  comp_header <- outer(inCSVData$headAsChar, inCSVData$headAsChar, "==")
  if(any(comp_header[upper.tri(comp_header)]))
    txt <- paste(txt, "<li>Column names must be unique.</li>", sep="")
  
  vali$txt <- txt
  vali$validated <- TRUE
})

output$DataValidationOut <- renderUI({
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

#################
## print table ##
#################

output$tableData <- DT::renderDataTable({
  # input$csvFileData
  if (!is.null(inCSVData$df)) {
    # update UoM, BG, Stgr
    # inCSVData$UoMs <- read.csv(input$csvFileData$datapath, header = FALSE,
    #                            sep = input$sepData, dec = input$decData,
    #                            skip = as.numeric(input$UoMData), nrows = 1,
    #                            stringsAsFactors = FALSE)
    # inCSVData$UoMs[is.na(inCSVData$UoMs)] <- ""
    # inCSVData$UoMs <- as.character(inCSVData$UoMs)
    # 
    # inCSVData$bg <- as.character(read.csv(input$csvFileData$datapath,
    #                                       header = FALSE,
    #                                       sep = input$sepData, dec = input$decData,
    #                                       skip = as.numeric(input$BgData), nrows = 1, 
    #                                       stringsAsFactors = FALSE))
    # 
    # inCSVData$stgr <- as.character(read.csv(input$csvFileData$datapath,
    #                                         header = FALSE,
    #                                         sep = input$sepData, dec = input$decData,
    #                                         skip = as.numeric(input$StgrData), nrows = 1, 
    #                                         stringsAsFactors = FALSE))
    # 
    # inCSVData$df <- read.csv(input$csvFileData$datapath, header = FALSE,
    #                          sep = input$sepData, dec = input$decData,
    #                          skip = max(as.numeric(c(input$StgrData,
    #                                                  input$BgData,
    #                                                  input$UoMs)))+1,
    #                          stringsAsFactors = FALSE)

    showTab <- inCSVData$df[inclRowData(), inclColData(), drop=F]

    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))
    
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

    # # if DB consistency has been checked, apply colors
    # if (checkDB$checked) {
    #   rowClrs <- rep("white", nrow(showTab))
    # 
    #   if (!is.null(checkDB$txtInfo)) {
    #     rowClrs[which(showTab$ID %in% checkDB$DataInDB)] <- "yellow"
    #     for (col in c(1, which(inCSVData$headAsChar %in% checkDB$colInDB$dede))) {
    #       if (col %in% checkDB$uomMissMatchCols) next;
    #       showDT <- formatStyle(showDT, col, "ID",
    #                             backgroundColor = styleEqual(showTab$ID, rowClrs))
    #     }
    #   }
    # 
    #   if (!is.null(checkDB$txtErr)) {
    #     rowClrs <- rep("red", nrow(showTab))
    #     for (col in checkDB$uomMissMatchCols) {
    #       showDT <- formatStyle(showDT, col, "ID",
    #                             backgroundColor = styleEqual(showTab$ID, rowClrs))
    #     }
    #   }
    # }
    showDT
  }
})



##########################
## check DB consistency ##
##########################

# find existing Datas
# check Parameter and their UoMs

# } else {
#   dataIds <- inCSVData$df$ID[inclRowData()]
#   
#   # fetch registered FoIs from DB and compare them with IDs in data csv-file
#   FoIinDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('", 
#                                    paste(dataIds, collapse="', '"),"')"))
#   missingFoIinDB <- which(!dataIds %in% FoIinDB$identifier)
#   if (length(missingFoIinDB) > 0) {
#     txt <- paste(txt, "<li>The following features are NOT in the DB: <ul><li>",
#                  paste0(dataIds, collapse="</li><li>"), "</li></ul></li>")
#     valiData$idsPresent <- FALSE
#   }
# }


# observeEvent(input$checkDB, {
#   DatainDB <- dbGetQuery(db, paste0("SELECT featureofinterestid, identifier FROM featureofinterest WHERE identifier IN ('", 
#                                    paste(inCSVData$df$ID[inclRowData()], collapse="', '"),"')"))
#   if (nrow(DatainDB) > 0) {
#     checkDB$txtInfo <- paste("The following features are already in the DB: <ul><li>",
#                              paste0(DatainDB$identifier, collapse="</li><li>"))
#   } else {
#     checkDB$txtInfo <- NULL
#   }
#   
#   checkDB$DataInDB <- DatainDB$identifier
#   
#   # find columns already in DB: colInDB with columnid, dede and its unit from the unit table
#   checkDB$colInDB <- dbGetQuery(db, paste0("SELECT columnid, dede, unit.unit FROM Datadatametadata left outer join unit on (unit.unitid = uom) WHERE dede IN ('", 
#                                            paste(inCSVData$headAsChar, collapse="', '"),"')"))
#   
#   # replace NA UoM with ""
#   if (nrow(checkDB$colInDB) > 0 ) {
#     checkDB$colInDB$unit[is.na(checkDB$colInDB$unit)] <- ""
#     
#     # compare UoMs from the csv with the DB for overlapping columns
#     compUoM <- inCSVData$UoMs[match(checkDB$colInDB$dede, inCSVData$headAsChar)] == checkDB$colInDB$unit
#     checkDB$uomMissMatchCols <-which(!compUoM)
#     
#     checkDB$txtErr <- NULL
#     if (length(checkDB$uomMissMatchCols) > 0) {
#       checkDB$txtErr <-
#         paste(
#           "The following columns have non-matching units of measurement: <ul><li>",
#           paste0(
#             paste0(checkDB$colInDB$dede[checkDB$uomMissMatchCols], ": [",
#                    checkDB$colInDB$unit[checkDB$uomMissMatchCols], "] "),
#             collapse = "</li><li>"
#           )
#         )
#     }
#   }
#   
#   checkDB$checked <- TRUE
# }, ignoreInit=TRUE)
# 
# output$DBConsistencyTxtOut <- renderUI({
#   if (checkDB$checked) {
#     if (!is.null(checkDB$txtInfo)) {
#       if (!is.null(checkDB$txtErr)) {
#         HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDB$txtErr, "</li></ul></div></html"))
#       } else {
#         HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", checkDB$txtInfo, "</li></ul></div></html"))
#       }
#     } 
#   } else {
#     HTML("")
#   }
# })
# 
# output$DBConsistencyActionOut <- renderUI({
#   if (checkDB$checked) {
#     if (is.null(checkDB$txtErr)) {
#       if (is.null(checkDB$txtInfo) || input$owData) {
#         actionButton("storeDB", "Store in DB!")
#       } 
#     } 
#   } else {
#     HTML("")
#   }
# })
# 
# 
#   # showModal(modalDialog(
#   #   title = "Upload completed.",
#   #   "Time series upload completed.",
#   #   easyClose = TRUE,
#   #   footer = NULL
#   # ))
# })

  
  # # pre-process: add new units
  # regUoMs <- dbGetQuery(db, paste0("SELECT unit FROM unit"))
  # unqUoMs <- unique(Data_uom[-1])
  # if (nrow(regUoMs) > 0) {
  #   regUoMs <- regUoMs[,1]
  #   misUoMs <- unqUoMs[which(sapply(unqUoMs, function(x) is.na(match(x, regUoMs))) & nchar(unqUoMs) > 0)]
  # } else {
  #   misUoMs <- unqUoMs
  # }
  # 
  # for (uom in misUoMs) {
  #   dbSendQuery(db, paste0("INSERT INTO unit (unitid, unit) VALUES (nextval('unitid_seq'), '", uom, "');"))
  # }
  # 
  # ### add new columns
  # regCols <- dbGetQuery(db, paste0("SELECT dede FROM Datadatametadata"))[,1]
  # misCols <- which(sapply(Data_header, function(x) is.na(match(x, regCols))))
  # 
  # if (length(misCols > 0)) { 
  #   for (i in 1:length(misCols)) {# i <- 1
  #     colId <- sprintf("col%03d", i + length(regCols))
  #     dbColumn(db, "Datadata", colId, "add", 
  #              coltype = switch(class(Data_data[,misCols[i]]),
  #                               integer = "numeric",
  #                               numeric = "numeric",
  #                               character = "character varying(255)"))
  #     
  #     # look-up UoM id
  #     unitId <- NULL
  #     if (nchar(Data_uom[misCols[i]]) > 0) {
  #       unitId <- dbGetQuery(db, paste0("SELECT unitid FROM unit WHERE unit = '", Data_uom[misCols[i]], "'"))[1,1]
  #     }
  #     
  #     if (is.null(unitId)) {
  #       dbSendQuery(db, paste0("INSERT INTO Datadatametadata (columnid, dede)
  #                              VALUES ('", paste(colId, Data_header[misCols[i]], sep="', '"),"')"))
  #     } else {
  #       dbSendQuery(db, paste0("INSERT INTO Datadatametadata (columnid, dede, uom)
  #                              VALUES ('", paste(colId, Data_header[misCols[i]], unitId, sep="', '"),"')"))
  #     }
  #     }
  #   }
  # 
  # # feed data row-wise
  # for (i in 1:nrow(Data_data)) {
  #   nonEmpty <- which(!is.na(Data_data[i,]))
  #   if (all(!nonEmpty)) next;
  #   
  #   # map csv-header to DB header via Datadatametadata
  #   Data_db_col_ids <- dbGetQuery(db, paste0("SELECT columnid, dede FROM Datadatametadata WHERE dede IN ('", 
  #                                           paste(Data_header[nonEmpty], collapse="', '"),"')"))
  #   
  #   # mind the ordering
  #   Data_db_col_ids <- Data_db_col_ids[match(Data_header[nonEmpty], Data_db_col_ids$dede), "columnid"]
  #   
  #   # find the Data idntifier
  #   Data_db_id <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier ='", 
  #                                      Data_data$ID[i],"'"))
  #   
  #   # check whether the Data has already some data
  #   if (nrow(dbGetQuery(db, paste0("SELECT id FROM Datadata WHERE featureofinterestid = ", Data_db_id))) > 0) {
  #     if(input$owData) {
  #       dbSendQuery(db, paste0("UPDATE Datadata SET ", 
  #                              paste(Data_db_col_ids, Data_data[i, nonEmpty], sep = " = '", collapse = "', "),
  #                              "' WHERE featureofinterestid = ", Data_db_id, ";"))
  #     } else {
  #       next()
  #     }
  #   } else {
  #     dbSendQuery(db, paste0("INSERT INTO Datadata ( featureofinterestid, ", paste(Data_db_col_ids, collapse=", "), ") ",
  #                            "VALUES ('", Data_db_id, "', '", paste(Data_data[i, nonEmpty], collapse="', '"), "')"))
  #   }
  # }
