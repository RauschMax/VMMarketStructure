# set up output list; one datatable per attribute --> will be shown by renderUI in the next step
observe({

  lapply(seq_along(Importance()$LevelCounts_100),
         function(x) {
           id <- paste0("decMat", x)

           output[[id]] <- DT::renderDataTable({
             dt <- data.table(matrix(Importance()$LevelCounts_100[[x]],
                                     nrow = 1))
             names(dt) <- defIN()$attLev[[x]]

             DT::datatable(dt, selection = list(mode = 'single', target = 'column'),
                           filter = "none", autoHideNavigation = TRUE, rownames = FALSE,
                           escape = FALSE, style = "default", class = 'cell-border',
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

  # lapply(seq_along(Importance()$LevelCounts_100),
  lapply(order(Importance()$Importance, decreasing = TRUE),
         function(x) {
           id <- paste0("decMat", x)
           div(style = "font-size: 10px; width:99%",
               h6(toupper(names(Importance()$LevelCounts_100)[x]),
                  " - ",
                  round(Importance()$Importance[x], 3) * 100),
               DT::dataTableOutput(id)
           )
         })

})

output$selComb <- renderUI({

  tags$table(style = "font-size:10px; width:100%;",
             lapply(seq_along(Importance()$LevelCounts_100),
                    function(x) {
                      IDselected <- paste0("decMat", x, "_columns_selected")
                      tags$tr(
                        tags$td(style = "font-size:10px; padding-right:10px;",
                                h6(toupper(names(Importance()$LevelCounts_100)[x]))
                        ),
                        tags$td(style = "font-size:12px; padding-right:10px;",
                                defIN()$attLev[[x]][input[[IDselected]] + 1])
                        )
                    }
             )
  )

})


## Demand !----
output$demandBox <- renderValueBox({

  validate(
    need(all(sapply(seq_along(Importance()$LevelCounts_100),
                    function(x) {
                      IDselected <- paste0("decMat", x, "_columns_selected")
                      !is.null(input[[IDselected]])
                    })),
         "")
  )

  demand <- sum(sapply(seq_along(Importance()$LevelCounts_100),
                   function(x) {
                     IDselected <- paste0("decMat", x, "_columns_selected")
                     Importance()$LevelCounts_100[[x]][input[[IDselected]] + 1]
                   }))

  valueBox(value = round(demand, 2),
           subtitle = "Demand",
           color = "green",
           icon = icon("heart-o"))
})


## Supply !----
output$supplyBox <- renderValueBox({

  validate(
    need(all(sapply(seq_along(Importance()$LevelCounts_100),
                    function(x) {
                      IDselected <- paste0("decMat", x, "_columns_selected")
                      !is.null(input[[IDselected]])
                    })),
         "Please select a combination across all attributes.")
  )

  # supply <- sum(sapply(seq_along(Importance()$LevelCounts_100),
  #                      function(x) {
  #                        IDselected <- paste0("decMat", x, "_columns_selected")
  #                        Supply()$supply[[x]][input[[IDselected]] + 1]
  #                      }))

  valueBox(value = round(42.42, 2),
           subtitle = "Supply",
           color = "lime",
           icon = icon("shopping-basket"))
})


## Incrementality !----
output$incrementBox <- renderValueBox({

  validate(
    need(all(sapply(seq_along(Importance()$LevelCounts_100),
                    function(x) {
                      IDselected <- paste0("decMat", x, "_columns_selected")
                      !is.null(input[[IDselected]])
                    })),
         "")
  )

  increment <- sum(sapply(seq_along(Importance()$LevelCounts_100),
                       function(x) {
                         IDselected <- paste0("decMat", x, "_columns_selected")
                         Importance()$LevelCounts_100[[x]][input[[IDselected]] + 1]
                       })) - 42.42

  valueBox(value = round(increment, 2),
           subtitle = "Incrementality",
           color = "purple",
           icon = icon("star-o"))
})


output$test2 <- renderPrint({

  all(sapply(seq_along(Importance()$LevelCounts_100),
         function(x) {
           IDselected <- paste0("decMat", x, "_columns_selected")
           !is.null(input[[IDselected]])
         }))


})
