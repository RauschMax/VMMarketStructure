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
                  format(round(Imp_ordered()$Imp[x], 3) * 100, nsmall = 1)),
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

  # shiny::selectizeInput('segs', 'Select Subgroup', choices = choList, multiple = TRUE,
  #                       options = list(dropdownParent = 'body'))

  shinyWidgets::pickerInput(
    inputId = "segs",
    label = "Select Subgroup",
    choices = choList,
    multiple = TRUE,
    width = "100%",
    options = list(
      'live-search' = TRUE,
      'live-search-placeholder' = "Search for segment...",
      'actions-box' = TRUE,
      'deselect-all-text' = "Remove Selection",
      'none-selected-text' = "Total",
      'multiple-separator' = " | ",
      size = 10
    )
  )

})

output$selLevel <- shiny::renderUI({

  validate(
    need(segDefIN(), "Please load the data.")
  )

  nLevs <- length(defIN()$attLev)

  choList <- lapply(seq_along(defIN()$attLev),
                    function(x) {
                      vec <- paste0("A", x, "_", seq_along(defIN()$attLev[[x]]))
                      names(vec) <- defIN()$attLev[[x]]
                      vec
                    })

  names(choList) <- names(defIN()$attLev)

  # shiny::selectizeInput('levels', 'Select Levels', choices = choList, multiple = TRUE,
  #                       options = list(dropdownParent = 'body'))

  shinyWidgets::pickerInput(
    inputId = "levels",
    label = "Select Levels",
    choices = choList,
    multiple = TRUE,
    width = "100%",
    options = list(
      'live-search' = TRUE,
      'live-search-placeholder' = "Search for level...",
      'actions-box' = TRUE,
      'deselect-all-text' = "Remove Selection",
      'none-selected-text' = "No Selection",
      'multiple-separator' = " | ",
      size = 10
    )
  )

})

chosenIDs <- reactive({

  if (is.null(input$segs) & is.null(input$levels)) {
    chosenIDs <- dataIN()[, ID]
  } else {
    # Segment Selection
    if (is.null(input$segs)) {
      validSegSel <- rep(TRUE, nrow(dataIN()))
    } else {
      segChosen <- lapply(seq_along(segDefIN()$segLevFact),
                          function(x) {
                            if (length(grep(paste0(x, "_"), input$segs)) > 0) {
                              # input$segs[grep(paste0(x, "_"), input$segs)]
                              sapply(strsplit(input$segs[grep(paste0(x, "_"), input$segs)], "_"),
                                     function(y) {
                                       segDefIN()$segLevFact[[x]][as.numeric(y[2])]
                                     })
                            }
                          })
      names(segChosen) <- names(segIN()$segFactor)[-1]

      segSelectionList <- lapply(names(segChosen),
                                 function(x) {
                                   if (is.null(segChosen[[x]])) {
                                     NULL
                                   } else {
                                     # segIN$segFactor[get(x) %in% segChosen[[x]], ID]

                                     segIN()$segFactor[, get(x)] %in% segChosen[[x]]
                                   }
                                 })

      validSegSel <- rowSums(Reduce(cbind, segSelectionList)) > 0
    }

    # Level Selection
    if (is.null(input$levels)) {
      validLevelSel <- rep(TRUE, nrow(dataIN()))
    } else {
      levelsChosen <- lapply(seq_along(defIN()$nlev),
                             function(x) {
                               if (length(grep(paste0("A", x), input$levels)) > 0) {
                                 input$levels[grep(paste0("A", x), input$levels)]
                               } else {
                                 NULL
                               }
                             })

      LevSelectionList <- lapply(levelsChosen,
                                 function(x) {
                                   if (is.null(x)) {
                                     NULL
                                   } else {
                                     rowSums(dataIN()[, mget(x)]) > 0
                                   }
                                 })

      validLevelSel <- rowSums(Reduce(cbind, LevSelectionList)) > 0
    }

    # Combine Level AND Segment Selection
    validSel <- validLevelSel & validSegSel

    chosenIDs <- dataIN()[validSel, ID]

  }

  chosenIDs

})

selectionSize <- reactive({
  length(chosenIDs())
})

output$selectionSizeBox <- renderValueBox({

  valueBox(value = selectionSize(),
           subtitle = "base",
           color = "teal",
           icon = icon("users"))
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
