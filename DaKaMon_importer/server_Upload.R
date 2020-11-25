# Copyright (C) 2017-2018 52°North Initiative for
# Geospatial Open Source Software GmbH
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 as published
# by the Free Software Foundation.
#
# If the program is linked with libraries which are licensed under one of
# the following licenses, the combination of the program with the linked
# library is not considered a "derivative work" of the program:
#
#     - Apache License, version 2.0
#     - Apache Software License, version 1.0
#     - GNU Lesser General Public License, version 3
#     - Mozilla Public License, versions 1.0, 1.1 and 2.0
#     - Common Development and Distribution License (CDDL), version 1.0
#
# Therefore the distribution of the program linked with libraries licensed
# under the aforementioned licenses, is permitted by the copyright holders
# if the distribution is compliant with both the GNU General Public
# License version 2 and the aforementioned licenses.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
########################################################################## #
#############################   File Upload    ########################### #
########################################################################## #

# Tools
moveFileToProbablyNotExistingTarget <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) {
    message("Create missing directories: ", to)
    dir.create(todir, recursive=TRUE)
  }
  file.copy(from = from,to = to)
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

      if (length(ortResult) == 0) {
        ortResult <- data.frame(RefId=character(), ID=character())
      } else {
        # order of column names must match SELECT
        columnNames <- colNamesResult$dede[match(colnames(ortResult), colNamesResult$columnid)]
        colnames(ortResult) <- c("RefId", "ID", names(which(sapply(columnNames, function(x) !is.na(x)))))
      }

      return(ortResult)
    } else if (input$FileUploadCategory == "Literatur") {

      colNamesResultRef <- dbGetQuery(db, paste("SELECT columnid, dede FROM column_metadata",
                                                "WHERE prefixid = 'ref' AND columnid like 'col%'"))
      aliasesRef <- paste0("ref_", colNamesResultRef$columnid)
      columnsRef <- paste0("ref.", colNamesResultRef$columnid, " as ", aliasesRef)
      dynamicColumnsRef <- ifelse(length(colNamesResultRef$columnid) > 0 , paste(", ", paste(columnsRef, collapse = ",")), "")

      colNamesResultLit <- dbGetQuery(db, paste("SELECT columnid, dede FROM column_metadata",
                                                "WHERE prefixid = 'lit' AND columnid like 'col%'"))
      aliasesLit <- paste0("lit_", colNamesResultLit$columnid)
      columnsLit <- paste0("lit.", colNamesResultLit$columnid, " as ", aliasesLit)
      dynamicColumnsLit <- ifelse(length(colNamesResultLit$columnid) > 0 , paste(", ", paste(columnsLit, collapse = ",")), "")

      dynamicColumns <- c(dynamicColumnsRef, dynamicColumnsLit)
      litRefQuery <- paste("SELECT ref.id, ref.identifier, ",
                           "lit.thematik, lit.untersuchungsbeginn, lit.untersuchungsende",
                           dynamicColumns, "FROM referenz ref",
                           "LEFT JOIN literatur lit on lit.referenz_id = ref.id"
                           # TODO LEFT JOIN parameter_data
                           # TODO LEFT JOIN pns_data
                           )
      litRefResult <- dbGetQuery(db, litRefQuery)

      if (length(litRefResult) == 0) {
        litRefResult <- data.frame(RefId=character(), ID=character(),
                                   Thematik=character(), Untersuchungsbeginn=character(), Untersuchungsende=character())
      } else {
        columnNamesRef <- colNamesResultRef$dede[match(colnames(litRefResult), paste0("ref_", colNamesResultRef$columnid))]
        columnNamesLit <- colNamesResultLit$dede[match(colnames(litRefResult), colNamesResultLit$columnid)]
        litRefNames <- c(names(which(sapply(columnNamesRef, function(x) !is.na(x)))),
                         names(which(sapply(columnNamesLit, function(x) !is.na(x)))))

        # order of column names must match SELECT
        colnames(litRefResult) <- c("RefId", "ID", "Thematik", "Untersuchungsbeginn", "Untersuchungsende", litRefNames)
      }
      return(litRefResult)
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

getSubDir <- reactive({
  if (input$FileUploadCategory == "Literatur") {
    fileUploadCodeLiteratur
  } else {
    # get sub dir (country code)
    sub(".*_(.*)_.*", "\\1", getSelectedRow())
  }
})

getTargetFilePath <- reactive({
  fileName <- input$FileUpload$name
  targetDir = ifelse(endsWith(fileUploadDir, "/"), fileUploadDir, paste0(fileUploadDir, "/"))

  subDir <- getSubDir()
  selectedCategory <- input$FileUploadCategory
  if (selectedCategory == "Literatur") {
    paste0(targetDir, subDir, "/", fileName)
  } else {
    paste0(targetDir, subDir, "/", selectedCategory, "/", fileName)
  }
})

# /tools

# storage of variables that might change through the GUI
fileUploadValidation <- reactiveValues(validated = FALSE)
fileUploadDBCheck <- reactiveValues(checked = FALSE)

observeEvent(input$overrideFile, {
  if (input$overrideFile) {
    fileUploadDBCheck$txt <- NULL
  } else {
    fileUploadDBCheck$checked <- FALSE
  }
})

#################
## File Upload ##
#################
observeEvent(input$FileUpload, {

  fileUploadValidation$validated <- FALSE
  fileUploadDBCheck$checked <- FALSE

  readBin(input$FileUpload$datapath, what="raw")
  uploadedFilePath <- input$FileUpload$datapath
  fileName <- input$FileUpload$name

  message("Uploaded File (", fileName, "): ", uploadedFilePath, "\n")
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
  fileUploadDBCheck$checked <- FALSE
  fileUploadDBCheck$txt <- NULL

  db <- connectToDB()
  tryCatch({
    selectedRow <- input$tableFileUploadReferenz_rows_selected
    if (is.null(selectedRow)) {
      fileUploadDBCheck$txt <- "Selektiere Referenz in Tabelle"
    } else {
      fileName <- input$FileUpload$name
      targetFile = getTargetFilePath()
      selectedCategory <- input$FileUploadCategory
      subDir <- getSubDir()

      if (file.exists(targetFile) && !input$overrideFile) {
        fileUploadDBCheck$txt <- paste0("Datei '", fileName, "' ",
                                        "existiert bereits für Kategorie '", selectedCategory, "'!")
      } else {
        query <- paste0("SELECT id FROM file_upload ",
                        "WHERE file_name = '", fileName, "' ",
                        "AND directory = '", subDir, "'")
        result <- dbGetQuery(db, query)

        if (length(result) > 0 && !input$overrideFile) {
          fileUploadDBCheck$txt <- paste0("Es existiert eine (verwaiste) Referenz zur Datei '", fileName, "'. ",
                                          "Wähle 'Überschreiben', um diese mit aktuellem Dokument zu aktualisieren.")
        }
      }
    }

    fileUploadDBCheck$checked <- TRUE
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
      selectedCategory <- input$FileUploadCategory
      selectedRow <- input$tableFileUploadReferenz_rows_selected
      referenceId <- getReferences()[selectedRow,"RefId"]

      fileName <- input$FileUpload$name
      targetFile <- getTargetFilePath()
      subDir <- getSubDir()

      message("Handle Document (",
              "cat='", selectedCategory, "',",
              "ref='", referenceId, "',",
              "dir='", subDir, "'",
              "): '", targetFile, "'", sep="")

      updateFileUpload <- function() {

        message("Updating existing document: ", targetFile)
        if (selectedCategory == "Ort") {
          updateOrtRefQuery <- paste("UPDATE file_upload_ort SET", paste0("ort_id='", referenceId , "'"),
                                     "WHERE", paste0("file_upload_id=", fileUploadId))
          dbGetQuery(db, updateOrtRefQuery)
        } else if (selectedCategory == "Literatur") {
          updateLiteraturRefQuery <- paste("UPDATE file_upload_literatur SET", paste0("literatur_id='", referenceId , "'"),
                                           "WHERE", paste0("file_upload_id=", fileUploadId))
          dbGetQuery(db, updateLiteraturRefQuery)
        } else {
          warning("Try to update file_upload for an unknown category: ", selectedCategory, "\n")
          showModalMessage(title="Vorgang konnte nicht abgeschlossen werden", paste("Unbekannte Kategorie:", selectedCategory))
          return()
        }
      }

      insertFileUpload <- function() {
        # insert new document
        message("Inserting new document: ", targetFile)

        insertFileUploadQuery <- paste("INSERT INTO file_upload (id, file_name, directory)",
                                       "VALUES (nextval('file_upload_seq'),",
                                       paste0("'", fileName, "',"),
                                       paste0("'", subDir, "'"),
                                       ") RETURNING id as file_upload_id;")
        fileUploadId <- dbGetQuery(db, insertFileUploadQuery)$file_upload_id

        if (selectedCategory == "Ort") {
          insertOrtRefQuery <- paste0("INSERT INTO file_upload_ort (file_upload_id, ort_id) ",
                                      "VALUES ('", fileUploadId, "', '", referenceId, "')")
          dbGetQuery(db, insertOrtRefQuery)
        } else if(selectedCategory == "Literatur") {
          insertLiteraturRefQuery <- paste0("INSERT INTO file_upload_literatur (file_upload_id, literatur_id) ",
                                            "VALUES ('", fileUploadId, "', '", referenceId, "')")
          dbGetQuery(db, insertLiteraturRefQuery)
        } else {
          warning("Try to store file_upload for an unknown category: ", selectedCategory, "\n")
          showModalMessage(title="Vorgang konnte nicht abgeschlossen werden", paste("Unbekannte Kategorie:", selectedCategory))
          return()
        }
      }

      if (file.exists(targetFile) || input$overrideFile) {

        fileUploadIdQUery <- paste("SELECT id FROM file_upload",
                                   "WHERE", paste0("directory='", subDir, "'"),
                                   "AND", paste0("file_name='", fileName, "'"))
        fileUploadId <- dbGetQuery(db, fileUploadIdQUery)$id

        if (is.null(fileUploadId)) {
          insertFileUpload()
        }
      } else {
        insertFileUpload()
      }

      uploadedFilePath <- input$FileUpload$datapath
      moveFileToProbablyNotExistingTarget(from = uploadedFilePath, to = targetFile)
      showModalMessage(title="Vorgang abgeschlossen", "Dokument wurde erfolgreich hochgeladen.")
    })
  }, error = modalErrorHandler, finally = poolReturn(db))
})
