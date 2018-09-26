############################################################################
#############################   File Upload    #############################
############################################################################

# Tools

moveFileToProbablyNotExistingTarget <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) {
    cat("Create missing directories: ", to)
    dir.create(todir, recursive=TRUE)
  }
  file.rename(from = from,  to = to)
}

getReferences <- reactive({
  db <- connectToDB()
  tryCatch({
    if (input$FileUploadCategory == "Ort") {

      colNamesResult <- dbGetQuery(db, paste("SELECT columnid, dede FROM column_metadata",
                                             "WHERE prefixid = 'ort' AND columnid like 'col%';"))

      ortColumns <- paste0("ort.", colNamesResult$columnid)
      dynamicColumns <- ifelse(length(colNamesResult$columnid) > 0 , paste(", ", paste(ortColumns, collapse = ",")), "")
      ortQuery <- paste("SELECT foi.featureofinterestid, foi.identifier", dynamicColumns, "FROM featureofinterest foi",
                        "LEFT JOIN ort_data ort on foi.featureofinterestid = ort.featureofinterestid")
      ortResult <- dbGetQuery(db, ortQuery)

      columnNames <- colNamesResult$dede[match(colnames(ortResult), colNamesResult$columnid)]
      colnames(ortResult) <- c("RefId", "ID", names(which(sapply(columnNames, function(x) !is.na(x)))))

      return(ortResult)
    } else if (input$FileUploadCategory == "Literatur") {

      # TODO

      #result <- dbGetQuery(db, "SELECT identifier, name FROM literatur")
      #namedValues = setNames(as.character(result$identifier), paste0(result$name, " (", result$identifier, ")"))
      #updateSelectInput(session, "FileUploadReference", choices = namedValues)

    } else {
      warning("Unknown category selection: ", input$FileUploadCategory, "\n")
      return(c())
    }

  }, error = modalErrorHandler, finally = poolReturn(db))
})

getSelectedRow <- reactive({
  selectedRow <- input$tableFileUploadReferenz_rows_selected
  if (!is.null(selectedRow)) getReferences()[selectedRow, 2]
})

getCountryCode <- reactive({
  # TODO get country code from user, once available
  sub(".*_(.*)_.*", "\\1", getSelectedRow())
})

# /tools

# storage of variables that might change through the GUI
fileUploadValidation <- reactiveValues(validated = FALSE)
fileUploadDBCheck <- reactiveValues(checked = FALSE)

#################
## File Upload ##
#################
observeEvent(input$FileUpload, {

  fileUploadValidation$validated <- FALSE
  fileUploadDBCheck$checked <- FALSE
  fileUploadDBCheck$txt <- NULL

  readBin(input$FileUpload$datapath, what="raw")
  uploadedFilePath <- input$FileUpload$datapath
  fileName <- input$FileUpload$name

  cat("Uploaded File (", fileName, "): ", uploadedFilePath, "\n")
  fileUploadValidation$validated <- TRUE
})

##################
## Render Table ##
##################
output$tableFileUploadReferenz <- renderDT({
  if (!is.null(input$FileUploadCategory)) {
    showTab <- getReferences()

    showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

    showHead <- paste0(showHead, "</span>")

    showDT <- datatable(showTab, colnames = showHead,
                        options = list(paging=FALSE, scrollX=TRUE, dom="t",
                                       language=list(url = lngJSON)),
                        selection = list(mode='single', target = 'row'),
                        rownames=FALSE, escape=FALSE)

    # TODO DB consistency has been checked, apply colors
    # if (fileUploadDBCheck$checked) {
    #   rowColors <- rep("white", nrow(showTab))
    #
    #   if (nrow(fileUploadDBCheck$exists) > 0) {
    #     rowColors[showTab$RefId %in% fileUploadDBCheck$FileUploadinDB$identifier] <- "red"
    #     showDT <- formatStyle(showDT, "RefId", target="row",
    #                           backgroundColor = styleEqual(showTab$RefId, rowColors))
    #   }
    # }

    showDT
  }
})

observeEvent(input$tableFileUploadReferenz_rows_selected, {
  cat("selection changed")
  if (!input$tableFileUploadReferenz_row_last_clicked %in% input$tableFileUploadReferenz_rows_selected) {
    # force a recheck when user changed selection
    fileUploadDBCheck$checked <- FALSE
  }
})

# write txt feedback as html list - or action button
output$FileUploadValidationOut <- renderUI({
  if (fileUploadValidation$validated) {
    actionButton("fileUploadDBCheck", "Prüfe Datenkonsistenz!")
  } else {
    return()
  }
})

##########################
## check DB consistency ##
##########################

# check if file upload already exists
observeEvent(input$fileUploadDBCheck, {
  db <- connectToDB()
  tryCatch({

    selectedRow <- input$tableFileUploadReferenz_rows_selected
    if (is.null(selectedRow)) {
      fileUploadDBCheck$txt <- "Selektiere Referenz in Tabelle"
      fileUploadDBCheck$checked <- TRUE
    } else {
      selectedCategory <- input$FileUploadCategory
      selectedReference <- getReferences()[selectedRow - 1,]
      countryCode <- getCountryCode()

      fileName <- input$FileUpload$name
      targetDir = ifelse(endsWith(fileUploadDir, "/"), fileUploadDir, paste0(fileUploadDir, "/"))
      targetFile = paste0(targetDir, countryCode, "/", selectedCategory, "/", fileName)

      if (file.exists(targetFile) && !input$overrideFile) {
        fileUploadDBCheck$txt <- paste0("Datei '", fileName, "' ",
                                        "existiert bereits für Kategorie '", selectedCategory, "'!")
        fileUploadDBCheck$validated <- TRUE
      } else {

        query <- paste0("SELECT id FROM file_upload ",
                        "WHERE file_name = '", fileName, "' ",
                        "AND country_code = '", countryCode, "'")
        result <- dbGetQuery(db, query)

        if (length(result) > 0 && !input$overrideFile) {
          fileUploadDBCheck$txt <- paste0("Es existiert bereits eine Referenz zur Datei '", fileName, "'")
        }

        fileUploadDBCheck$checked <- TRUE
      }
    }
  }, error = modalErrorHandler, finally = poolReturn(db))
})

# output of DB consistency check as html - or action button
output$FileUploadDBConsistencyOut <- renderUI({
  if (fileUploadDBCheck$checked) {
    if (is.null(fileUploadDBCheck$txt)) {
      actionButton("fileUploadDBStore", "Einfügen in DB!")
    } else {
      HTML(paste0("<html><div style=\"height:120px;width:100%;border:1px solid #ccc; overflow:auto\">", fileUploadDBCheck$txt, "</li></ul></div></html>"))
    }
  } else {
    return()
  }
})

########################
## Insert File Upload ##
########################
observeEvent(input$fileUploadDBStore, {
  fileUploadDBCheck$checked <- FALSE
  db <- connectToDB()
  tryCatch({
    dbWithTransaction(db, {
      selectedRow <- input$tableFileUploadReferenz_rows_selected
      selectedReference <- getReferences()[selectedRow - 1,]
      selectedCategory <- input$FileUploadCategory
      countryCode <- getCountryCode()

      fileName <- input$FileUpload$name
      targetDir = ifelse(endsWith(fileUploadDir, "/"), fileUploadDir, paste0(fileUploadDir, "/"))
      targetFile = paste0(targetDir, countryCode, "/", selectedCategory, "/", fileName)

      if (file.exists(targetFile)) {

        # TODO update document and reference

      } else {
        # insert new document
        insertFileUploadQuery = paste("INSERT INTO file_upload (id, file_name, country_code)",
                                      "VALUES (nextval('file_upload_seq'),",
                                      paste0("'", fileName, "',"),
                                      paste0("'", countryCode, "'"),
                                      ") RETURNING id as file_upload_id;")
        fileUploadId = dbGetQuery(db, insertFileUploadQuery)

        # TODO insert into corresponding reference table

        if (selectedCategory == "Ort") {
          insertOrtRefQuery = paste0("INSERT INTO file_upload_ort (file_upload_id, ort_id) ",
                                     "VALUES ('", fileUploadId$file_upload_id, "', '", selectedReference$RefId, "')")
          dbGetQuery(db, insertOrtRefQuery)
        } else if(selectedCategory == "Literatur") {
          insertLiteraturRefQuery = paste0("INSERT INTO file_upload_literatur (file_upload_id, literatur_id) ",
                                           "VALUES ('", fileUploadId$file_upload_id, "', '", selectedReference$RefId, "')")
          dbGetQuery(db, insertLiteraturRefQuery)
        } else {
          warning("Try to store file_upload for an unknown category: ", selectedCategory, "\n")
        }

        cat("Saving file (",
            "cat='", selectedCategory, "',",
            "ref='", selectedReference$RefId, "',",
            "code='", countryCode, "'",
            ") to '", targetFile, "'", sep="")

        uploadedFilePath <- input$FileUpload$datapath
        moveFileToProbablyNotExistingTarget(to = targetFile, from = uploadedFilePath)


        showModalMessage(title="Vorgang abgeschlossen", "Dokument wurde erfolgreich hochgeladen.")
      }
    })
  }, error = modalErrorHandler, finally = poolReturn(db))
})

