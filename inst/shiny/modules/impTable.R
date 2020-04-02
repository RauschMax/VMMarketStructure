output$decMat <- DT::renderDataTable({

  validate(
    need(DecHierarchy(), "Wait a second!")
  )

  Dt_LevCount <- DecHierarchy()$Dt_LevCount

  Dt_Levels <- DecHierarchy()$Dt_Levels

  DT_pie <- copy(Dt_LevCount)
  DT_pie[, Attribute := names(DecHierarchy()$LevCount)]
  DT_pie[, Importance := DecHierarchy()$Imp]
  DT_pie[, paste0("V", 1:max(defIN()$nlev)) := lapply(.SD,
                                                    function(x) {
                                                      out <- paste((1 - round(x, 3)) * 100, round(x * 100, 1),
                                                                   sep = ",")
                                                      sapply(out,
                                                             function(i) {
                                                               ifelse(i == "NA,NA",
                                                                      NA, i)
                                                             })
                                                    }),
         .SDcols = paste0("V", 1:max(defIN()$nlev))]

  dt_out <- DT_pie[, mget(c("Attribute", "Importance",
                            paste0("V", 1:max(defIN()$nlev))))]

  dt_out[, paste0("V", 1:max(defIN()$nlev)) :=
            lapply(seq_along(.SD),
                   function(i) {
                     out <- sapply(seq_along(.SD[[i]]),
                                   function(j) {
                                     if (is.na(.SD[[i]][j])) {
                                       .SD[[i]][j]
                                     } else {
                                       paste0("<div style = 'font-size: 8px; text-align: center;'>",
                                              Dt_Levels[j, get(paste0("A", i))],
                                              "<br>",
                                              sparkline::spk_chr(unlist(strsplit(.SD[[i]][j], ",")),
                                                      type = "pie",
                                                      sliceColors = c('lightgrey',
                                                                      'yellow'),
                                                      width = 40,
                                                      height = 40),
                                              "</div>")
                                     }
                                   })
                     out
                   }),
          .SDcols = paste0("V", 1:max(defIN()$nlev))]

  DT::datatable(dt_out, selection = list(mode = 'single', target = 'row'),
                escape = FALSE, rownames = FALSE,
                style = "default", class = 'compact',
                options = list(columnDefs = list(list(className = 'dt-center',
                                                      targets = 1:(ncol(dt_out) - 1)),
                                                 list(width = paste0(90 / (ncol(dt_out) - 1), "%"),
                                                      targets = 1:(ncol(dt_out) - 1))),
                               fnDrawCallback = htmlwidgets::JS('function(){
                                                          HTMLWidgets.staticRender();
                                                          }'),
                               pageLength = nrow(dt_out),
                               ordering = FALSE,
                               dom = 't',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                           'color': '#fff'});",
                                 "}"))
  ) %>%
    sparkline::spk_add_deps() %>%
    formatPercentage("Importance",  digits = 1)


})

output$DownDecMat <- downloadHandler(
  filename = function() {
    paste("DecisionHierarchy", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {

    DT_Out <- data.table(DecHierarchy()$Dt_LevCount,
                         DecHierarchy()$Dt_Levels)
    DT_Out[, Attribute := names(DecHierarchy()$LevCount)]
    DT_Out <- DT_Out[, mget(c("Attribute",
                              matrix(rbind(names(DecHierarchy()$Dt_Levels),
                                           names(DecHierarchy()$Dt_LevCount)),
                                     nrow = 1)))]

    DT_Out[, names(DecHierarchy()$Dt_LevCount) := lapply(.SD, round, 3),
           .SDcols = names(DecHierarchy()$Dt_LevCount)]
    DT_Out[, c("Attribute", names(DecHierarchy()$Dt_Levels)) := lapply(.SD, enc2utf8),
           .SDcols = c("Attribute", names(DecHierarchy()$Dt_Levels))]

    # data.table::fwrite(DT_Out, file, sep = ";", dec = ",")
    write.table(DT_Out, file = file, sep = ";", dec = ",", row.names = FALSE, fileEncoding = "UTF-16LE")
  }
)

selectionSize <- reactive({
  length(chosenIDs())
})

output$selectionSizeBox <- renderValueBox({

  valueBox(value = selectionSize(),
           subtitle = "base",
           color = "teal",
           icon = icon("users"))
})

output$segSize <- renderText({
  total <- ""
  if (is.null(input$segs)) total <- " - ALL"
  paste("Base = ", selectionSize(), "\n", total)
})

output$testDecHier <- renderPrint({

  diagramData()

})
