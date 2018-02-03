library(shiny)
library(DT)
library(dplyr)

# dummy data
KA_table <- data.frame(name=c("KA A", "KA B", "KA C", "KA D"),
                       ausbau=c(400,500,600,700),
                       haushalte=c(80e3,100e3,120e3,140e3))

subFoIs <- list(data.frame(KA=c("KA_A", "KA_A", "KA_A", "KA_A", "KA_A"),
                           Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                           Volumen=c(50,60,80,40,23),
                          Durchsatz=c(12,15,18,16,14)),
                data.frame(KA=c("KA_B", "KA_B", "KA_B", "KA_B", "KA_B"),
                                  Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                  Volumen=c(50,60,80,40,23),
                                  Durchsatz=c(12,15,18,16,14)),
                data.frame(KA=c("KA_C", "KA_C", "KA_C", "KA_C", "KA_C"),
                           Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                           Volumen=c(50,60,80,40,23),
                           Durchsatz=c(12,15,18,16,14)),
                data.frame(KA=c("KA_D", "KA_D", "KA_D", "KA_D", "KA_D"),
                                  Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                  Volumen=c(50,60,80,40,23),
                                  Durchsatz=c(12,15,18,16,14)))
phenData <- list(list(data.frame(FoI=rep("KA_A", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_A", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_A", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_A", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_A", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5))),
                 list(data.frame(FoI=rep("KA_B", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_B", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_B", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 lead = runif(5)),
                      data.frame(FoI=rep("KA_B", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_B", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5))),
                 list(data.frame(FoI=rep("KA_C", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_C", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_c", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_C", 5),time=Sys.time()-1:5*3600,
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 lead=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_C", 5),Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5))),
                 list(data.frame(FoI=rep("KA_D", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_D", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_D", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_D", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5)),
                      data.frame(FoI=rep("KA_D", 5),
                                 Komponente=c("Zulauf", "Vorklärbecken", "Klärbecken", "Nachklärbecken", "Ablauf"),
                                 time=Sys.time()-1:5*3600,
                                 cadmium=runif(5),
                                 zinc = runif(5))))


# DataTables example
shinyApp(
  ui = fluidPage(
    tabsetPanel(
      tabPanel("Ka selection", 
               column(12, dataTableOutput('table'),
                      textOutput("selText"))
               ), 
      tabPanel("Sub FOI",
               column(12, dataTableOutput('table2'),
                      textOutput("selText2"))
               ),
      tabPanel("Data",
               column(12, dataTableOutput('tabSummary')),
               column(12, br()),
               column(12, dataTableOutput('table3'))
      ))),
  

    server = function(input, output) {
      output$rows = renderPrint({
        s = input$table_rows_selected
        if (length(s)) {
          cat('These rows were selected:\n\n')
          cat(s, sep = ', ')
        }
      })
      
    output$table <- DT::renderDataTable(KA_table, 
                                        filter="top",
                                        options = list(paging=FALSE, dom = 'Bfrtip', 
                                                       buttons = list(list(extend = 'colvis', 
                                                                           columns = (1:ncol(KA_table))))),
                                        extensions = 'Buttons')
    
    s <- reactive({
      str(input)
      sr <- input$table_rows_selected
      if(length(sr) == 0) {
        1:nrow(KA_table)
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
    
    output$table2 <- DT::renderDataTable(do.call(bind_rows, subFoIs[s()]),
                                         filter="top",
                                         options = list(paging=FALSE, dom = 'Bfrtip', 
                                                        buttons = list(list(extend = 'colvis', 
                                                                            columns = 1:ncol(subFoIs[[1]])))),
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
  
  output$tabSummary <- DT::renderDataTable({
    df <- as.data.frame(bind_rows(lapply(phenData[s()], function(x) do.call(bind_rows, x[sp()]))))[,-c(1:3)]
    sumDf <- NULL     
    for (col in 1:ncol(df)) {
      sumCol <- summary(df[,col])
      if (length(sumCol) < 7) sumCol <- c(sumCol, 0)
      #   {
      #   tmp <- rep(0, 7)
      #   tmp[1:length(sumCol)] <- sumCol
      #   sumCol <- tmp
      # }
            
      sumDf <- cbind(sumDf, sumCol)
    }
    rownames(sumDf) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NAs")
    colnames(sumDf) <- colnames(df)
    sumDf <- as.data.frame(sumDf)
    sumDf}, options = list(paging=FALSE, bFilter=FALSE, bInfo=FALSE))
  
  output$table3 <- DT::renderDataTable(bind_rows(lapply(phenData[s()], function(x) do.call(bind_rows, x[sp()]))),
                                       filter="top",
                                       options = list(paging=FALSE, 
                                                      dom = 'Bfrtip', 
                                                      buttons = list(list(extend = 'colvis', columns = 1:3),
                                                                     list(extend = "csv", filename="KaMonExport"),
                                                                     'copy', 'print')),
                                       extensions = 'Buttons')
  }

)
