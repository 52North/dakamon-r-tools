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
## viewer server Dokumente

# Tools

generateLinkColumn <- function(resultSet, selectedCategory) {
  if (length(fileDownloadBaseUrl) == 0) {
    warning("Please configure 'fileDownloadBaseUrl' parameter!")
  }
  if (length(resultSet) != 0) {
    baseUrl <- ifelse(endsWith(fileDownloadBaseUrl, "/"), fileDownloadBaseUrl, paste0(fileDownloadBaseUrl, "/"))
    resultSet$Link <- paste0("<a href='", paste0(baseUrl, resultSet$directory, "/", selectedCategory, "/", resultSet$filename), "'",
                                "target='_blank'>", resultSet$filename, "</a>")
    resultSet <- resultSet[, !(names(resultSet) %in% c("directory", "filename"))]
  }
  return(resultSet)
}
#
# getOrtReferences ----
#
getOrtReferences <- reactive({
  db <- connectToDB()
  tryCatch({

    noResults <- function() {
      data.frame(RefId=character(), ID=character(), Link=character())
    }

    # TODO filter by country code once available from logged in user

    docsQuery <- "SELECT * FROM file_upload f JOIN file_upload_ort o ON f.id = o.file_upload_id"
    docsResult <- dbGetQuery(db, docsQuery)

    if (length(docsResult) == 0) {
      return(noResults())
    } else {

      colNamesResult <- dbGetQuery(db, paste("SELECT columnid, dede FROM column_metadata",
                                             "WHERE prefixid = 'ort' AND columnid like 'col%';"))

      ortColumns <- paste0("ort.", colNamesResult$columnid)
      dynamicColumns <- ifelse(length(colNamesResult$columnid) > 0 , paste(", ", paste(ortColumns, collapse = ",")), "")
      ortQuery <- paste("WITH upload AS (
                          SELECT u.file_name, u.directory, o.ort_id
                          FROM file_upload u
                          LEFT JOIN file_upload_ort o ON o.file_upload_id = u.id
                          WHERE file_upload_id IN (",
                            paste(docsResult$id, collapse = ','),
                          ")",
                        ")",
                        "SELECT foi.featureofinterestid, foi.identifier,
                         u.file_name AS filename, u.directory",
                        dynamicColumns,
                        "FROM featureofinterest foi
                         LEFT JOIN ort_data ort on foi.featureofinterestid = ort.featureofinterestid
                         LEFT JOIN upload u on u.ort_id = ort.featureofinterestid
                         WHERE ort.featureofinterestid IN (",
                           paste(docsResult$ort_id, collapse = ','),
                        ")")
      ortResult <- dbGetQuery(db, ortQuery)

      if (length(ortResult) == 0) {
        return(noResults())
      } else {

        ortResult <- generateLinkColumn(ortResult, "Ort")
        columnNames <- colNamesResult$dede[match(colnames(ortResult), colNamesResult$columnid)]
        ortNames <- names(which(sapply(columnNames, function(x) !is.na(x))))
        colnames(ortResult) <- c("RefId", "ID", ortNames, "Link")

        # re-order columns
        ortResult <- ortResult[,c("RefId", "ID", "Link", ortNames)]
        return(ortResult)
      }
    }
  }, error = modalErrorHandler, finally = poolReturn(db))
})
#
# getLiteraturReferences ----
#
getLiteraturReferences <- reactive({
  db <- connectToDB()
  tryCatch({
    noResults <- function() {
      data.frame(RefId=character(), ID=character(), Link=character(),
                 Thematik=character(), Untersuchungsbeginn=character(), Untersuchungsende=character())
    }

    docsQuery <- "SELECT * FROM file_upload f JOIN file_upload_literatur o ON f.id = o.file_upload_id"
    docsResult <- dbGetQuery(db, docsQuery)

    if (length(docsResult) == 0) {
      return(noResults())
    } else {

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
      litRefQuery <- paste("WITH upload AS (
                           SELECT u.file_name, u.directory, l.literatur_id
                           FROM file_upload u
                           LEFT JOIN file_upload_literatur l ON l.file_upload_id = u.id
                           WHERE file_upload_id IN (",
                           paste(docsResult$id, collapse = ','),
                           ")
                          ) ",
                           "SELECT ref.id, ref.identifier,
                          u.file_name AS filename, u.directory,
                          lit.thematik, lit.untersuchungsbeginn, lit.untersuchungsende",
                           dynamicColumns,
                           "FROM referenz ref
                          LEFT JOIN literatur lit on lit.referenz_id = ref.id
                          LEFT JOIN upload u on u.literatur_id = lit.id
                          WHERE u.literatur_id IN (",
                           paste(docsResult$literatur_id, collapse = ','),
                           ")")
      litRefResult <- dbGetQuery(db, litRefQuery)

      if (length(litRefResult) == 0) {
        return(noResults())
      } else {

        litRefResult <- generateLinkColumn(litRefResult, "Literatur")
        columnNamesRef <- colNamesResultRef$dede[match(colnames(litRefResult), paste0("ref_", colNamesResultRef$columnid))]
        columnNamesLit <- colNamesResultLit$dede[match(colnames(litRefResult), colNamesResultLit$columnid)]
        litRefNames <- c(names(which(sapply(columnNamesRef, function(x) !is.na(x)))),
                         names(which(sapply(columnNamesLit, function(x) !is.na(x)))))

        # order of non dynamic column names must match SELECT
        colnames(litRefResult) <- c("RefId", "ID", "Thematik", "Untersuchungsbeginn", "Untersuchungsende", litRefNames, "Link")

        # re-order columns
        litRefResult <- litRefResult[,c("RefId", "ID", "Link", "Thematik", "Untersuchungsbeginn", "Untersuchungsende", litRefNames)]
        return(litRefResult)
      }
    }
  }, error = modalErrorHandler, finally = poolReturn(db))
})

# /Tools


###################
## Render Tables ##
###################
output$documentTableOrt <- renderDT({
  showTab <- getOrtReferences()

  showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

  showHead <- paste0(showHead, "</span>")

  showDT <- datatable(showTab, colnames = showHead,
                      options = list(paging=FALSE, scrollX=TRUE, dom="t",
                                     language=list(url = lngJSON)),
                      selection = list(mode = 'none'),
                      rownames=FALSE, escape=FALSE)
  showDT
})

output$documentTableLiteratur <- renderDT({
  showTab <- getLiteraturReferences()

  showHead <- paste0("<span style=\"white-space: nowrap; display: inline-block; text-align: left\">", colnames(showTab))

  showHead <- paste0(showHead, "</span>")

  showDT <- datatable(showTab, colnames = showHead,
                      options = list(paging=FALSE, scrollX=TRUE, dom="t",
                                     language=list(url = lngJSON)),
                      selection = list(mode = 'none'),
                      rownames=FALSE, escape=FALSE)
  showDT
})
