# set up output list; one datatable per attribute --> will be shown by renderUI in the next step
observe({

  lapply(seq_along(Imp_ordered()$LevCount),
         function(x) {
           id <- paste0("decMat", x)

           output[[id]] <- DT::renderDataTable({
             dt <- data.table(matrix(Imp_ordered()$LevCount[[x]],
                                     nrow = 1))
             names(dt) <- Imp_ordered()$attLev_ordered[[x]]

             DT::datatable(dt, selection = list(mode = 'single', target = 'column'),
                           filter = "none", autoHideNavigation = TRUE, rownames = FALSE,
                           escape = FALSE, style = "default", class = 'compact',
                           options = list(columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                          pageLength = 1,
                                          ordering = FALSE,
                                          dom = 't',
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$(this.api().table().header()).css({'background-color': '#989898',
                                             'color': '#fff'});",
                                            "}"))) %>%
               formatPercentage(names(dt),  digits = 1) %>%
               DT::formatStyle(columns = names(dt),
                               background = DT::styleColorBar(c(0, 1),
                                                              '#f2da64', angle = 270),
                               backgroundSize = '98% 90%',
                               backgroundRepeat = 'no-repeat',
                               backgroundPosition = 'center')
           })
         })

})

# table like output for counts
output$decMatUI <- renderUI({

  # lapply(seq_along(Imp_ordered()$LevCount),
  lapply(order(Imp_ordered()$Imp, decreasing = TRUE),
         function(x) {
           id <- paste0("decMat", x)
           div(style = "font-size: 10px; width:99%",
               h6(div(style = "font-weight:bold; display:inline-block;",
                      toupper(names(Imp_ordered()$LevCount)[x])),
                  ": ",
                  format(round(Imp_ordered()$Imp[x], 3) * 100, nsmall = 1)),
               DT::dataTableOutput(id)
           )
         })

})

output$decMatAlt <- DT::renderDataTable({
  Dt_LevCount <- Imp_ordered()$Dt_LevCount

  Dt_Levels <- Imp_ordered()$Dt_Levels

  DT_pie <- copy(Dt_LevCount)
  DT_pie[, Attribute := names(Imp_ordered()$LevCount)]
  DT_pie[, Importance := Imp_ordered()$Imp]
  DT_pie[, paste0("V", 1:max(defIN()$nlev)) := lapply(.SD,
                                                    function(x) {
                                                      out <- paste((1 - round(x, 3)) * 100, round(x * 100, 1),
                                                                   sep = ",")
                                                      sapply(out,
                                                             function(i) {
                                                               ifelse(i == "NA,NA", NA, i)
                                                             })
                                                    }),
         .SDcols = paste0("V", 1:max(defIN()$nlev))]

  dt_out <- DT_pie[, mget(c("Attribute", "Importance", paste0("V", 1:max(defIN()$nlev))))]

  dt_out[, paste0("V", 1:max(defIN()$nlev)) :=
            lapply(seq_along(.SD),
                   function(i) {
                     out <- sapply(seq_along(.SD[[i]]),
                                   function(j) {
                                     if (is.na(.SD[[i]][j])) {
                                       .SD[[i]][j]
                                     } else {
                                       # spk_chr(unlist(strsplit(j, ",")),
                                       #         type = "pie",
                                       #         sliceColors = c('lightgrey', 'yellow'))
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

  # dt_out[, paste0("V", 1:max(defIN()$nlev)) := lapply(.SD,
  #                                                       function(i) {
  #                                                         sapply(i,
  #                                                                function(j) {
  #                                                                  if(is.na(j)) {
  #                                                                    j
  #                                                                  } else {
  #                                                                    spk_chr(unlist(strsplit(j, ",")),
  #                                                                            type = "pie",
  #                                                                            sliceColors = c('lightgrey', 'yellow'))
  #                                                                  }
  #                                                                })
  #                                                       }),
  #         .SDcols = paste0("V", 1:max(defIN()$nlev))]

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

    DT_Out <- data.table(Imp_ordered()$Dt_LevCount,
                         Imp_ordered()$Dt_Levels)
    DT_Out[, Attribute := names(Imp_ordered()$LevCount)]
    DT_Out <- DT_Out[, mget(c("Attribute",
                              matrix(rbind(names(Imp_ordered()$Dt_Levels),
                                           names(Imp_ordered()$Dt_LevCount)),
                                     nrow = 1)))]

    DT_Out[, names(Imp_ordered()$Dt_LevCount) := lapply(.SD, round, 3),
           .SDcols = names(Imp_ordered()$Dt_LevCount)]
    DT_Out[, c("Attribute", names(Imp_ordered()$Dt_Levels)) := lapply(.SD, enc2utf8),
           .SDcols = c("Attribute", names(Imp_ordered()$Dt_Levels))]

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
