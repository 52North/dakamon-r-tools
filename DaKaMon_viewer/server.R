# server  DaKaMon viewer



server <- function(input, output) {
  # load all super FoI from DB
  superFoi <- dbGetQuery(db, paste0("SELECT featureofinterestid, name, identifier FROM featureofinterest WHERE identifier != 'unknown' AND featureofinterestid IN (SELECT parentfeatureid FROM featurerelation)"))
  superFoiData <- dbGetQuery(db, paste0("SELECT * FROM foidata WHERE featureofinterestid IN (SELECT featureofinterestid FROM featureofinterest WHERE identifier != 'unknown' AND featureofinterestid IN (SELECT parentfeatureid FROM featurerelation))"))
  foiDataMetaData <- dbGetQuery(db, paste0("SELECT * FROM foidatametadata"))
  colnames(superFoiData) <- foiDataMetaData$dede[match(colnames(superFoiData), foiDataMetaData$columnid)]
  
  output$table  <- DT::renderDataTable({
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
    
    # if (!is.null(inCSVData$bg)) {
    #   showBg <- sapply(inCSVData$bg, function(x) {
    #     if (!is.na(x) & nchar(x) > 0 & x != "NA") {
    #       paste0("<br> BG: ", x)
    #     } else {
    #       "<br>"
    #     }
    #   })
    #   if (!is.null(inCSVData$df))
    #     showHead <- paste0(showHead, showBg)
    # }
    
    showHead <- paste0(showHead, "</span>")
    datatable(showTab, colnames = showHead,
              options = list(paging=FALSE, bFilter=FALSE,
                             scrollX=TRUE, sort=FALSE),
              escape=FALSE)
  })
  
  # output$rows = renderPrint({
  #   s = input$table_rows_selected
  #   if (length(s)) {
  #     cat('These rows were selected:\n\n')
  #     cat(sort(s), sep = ', ')
  #   }
  # })

  s <- reactive({
    str(input)
    sr <- input$table_rows_selected
    if(length(sr) == 0) {
      1:nrow(showTab)
    } else {
      sort(sr)
    }})
  
  output$selText <- renderText({
    if (length(s()) == 1) {
      paste("Row", s(), "is selected.")
    } else {
      paste("Rows", paste(s(), collapse=", "), "are selected.")
    }
  })

  #####################
  ######  TAB 2  ######
  #####################
  # load all selcted sub FoI from DB
  # subFoi <- dbGetQuery(db, paste0("SELECT featureofinterestid, name, identifier FROM featureofinterest WHERE identifier != 'unknown' AND featureofinterestid IN (SELECT parentfeatureid FROM featurerelation)"))

  subFoiData <- reactive({
    sfd <- dbGetQuery(db, paste0("SELECT * FROM foidata WHERE featureofinterestid IN (SELECT childfeatureid FROM featurerelation WHERE parentfeatureid IN ('", paste(superFoi[s(),1], collapse="', '") , "'))"))
    colnames(sfd) <- foiDataMetaData$dede[match(colnames(sfd), foiDataMetaData$columnid)]
    sfd[,-1]})
                                   
  
  output$table2 <- DT::renderDataTable(subFoiData(),
                                       filter="top",
                                       options = list(paging=FALSE, dom = 'Bfrtip',
                                                      buttons = list(list(extend = 'colvis',
                                                                          columns = 1:ncol(subFoiData())))),
                                       extensions = 'Buttons')

  sp <- reactive({
    sr <- input$table2_rows_selected
    if(length(sr) == 0) {
      1:nrow(do.call(bind_rows, subFoIs[s()]))
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
   
  # rownames(as.data.frame(bind_cols(lapply(do.call(bind_rows, lapply(phenData[1:4], function(x) do.call(bind_rows, x[1:4]))), function(x) summary(x)[1:6]))) ) <- 
  # 
  # summary(do.call(bind_rows, lapply(phenData[1:4], function(x) do.call(bind_rows, x[1:4])))[,2])
  # 
  # output$tabSummary <- DT::renderDataTable({
  #   df <- as.data.frame(bind_rows(lapply(phenData[s()], function(x) do.call(bind_rows, x[sp()]))))[,-c(1:3)]
  #   sumDf <- NULL     
  #   for (col in 1:ncol(df)) {
  #     sumCol <- summary(df[,col])
  #     if (length(sumCol) < 7) sumCol <- c(sumCol, 0)
  #     #   {
  #     #   tmp <- rep(0, 7)
  #     #   tmp[1:length(sumCol)] <- sumCol
  #     #   sumCol <- tmp
  #     # }
  #     
  #     sumDf <- cbind(sumDf, sumCol)
  #   }
  #   rownames(sumDf) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NAs")
  #   colnames(sumDf) <- colnames(df)
  #   sumDf <- as.data.frame(sumDf)
  #   sumDf}, options = list(paging=FALSE, bFilter=FALSE, bInfo=FALSE))
  # 
  # output$table3 <- DT::renderDataTable(bind_rows(lapply(phenData[s()], function(x) do.call(bind_rows, x[sp()]))),
  #                                      filter="top",
  #                                      options = list(paging=FALSE, 
  #                                                     dom = 'Bfrtip', 
  #                                                     buttons = list(list(extend = 'colvis', columns = 1:3),
  #                                                                    list(extend = "csv", filename="KaMonExport"),
  #                                                                    'copy', 'print')),
  #                                      extensions = 'Buttons')
}
