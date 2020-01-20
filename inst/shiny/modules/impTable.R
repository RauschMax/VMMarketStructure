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
               h6(div(style = "font-weight:bold; display:inline-block;", toupper(names(Imp_ordered()$LevCount)[x])),
                  ": ",
                  round(Imp_ordered()$Imp[x], 3) * 100),
               DT::dataTableOutput(id)
           )
         })

})

output$selSegment <- shiny::renderUI({

  validate(
    need(segDefIN(), "Please load the data.")
  )

  nSegs <- length(segDefIN()$segLevFact)

  choList <- lapply(seq_along(segDefIN()$segLevFact),
                    function(x) {
                      vec <- paste0(x, "_", seq_along(segDefIN()$segLevFact[[x]]))
                      names(vec) <- segDefIN()$segLevFact[[x]]
                      vec
                    })

  names(choList) <- names(segDefIN()$segLevFact)

  shiny::selectizeInput('segs', 'Select Subgroup', choices = choList, multiple = TRUE)

})

# output$selComb <- renderUI({
#
#   tags$table(style = "font-size:10px; width:100%;",
#              lapply(seq_along(Imp_ordered()$LevCount),
#                     function(x) {
#                       IDselected <- paste0("decMat", x, "_columns_selected")
#                       tags$tr(
#                         tags$td(style = "font-size:10px; padding-right:10px;",
#                                 h6(toupper(names(Imp_ordered()$LevCount)[x]))
#                         ),
#                         tags$td(style = "font-size:12px; padding-right:10px;",
#                                 Imp_ordered()$attLev_ordered[[x]][input[[IDselected]] + 1])
#                         )
#                     }
#              )
#   )
#
# })

output$test2 <- renderPrint({

  orderIN()

})
