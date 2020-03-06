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

      validSegSel <- apply(Reduce(cbind, segSelectionList), 1, all)
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

      validLevelSel <- apply(Reduce(cbind, LevSelectionList), 1, all)
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

output$segSize <- renderText({
  total <- ""
  if (is.null(input$segs)) total <- " - ALL"
  paste("Base = ", selectionSize(), "\n", total)
})

output$testDecHier <- renderPrint({

  diagramData()

})
