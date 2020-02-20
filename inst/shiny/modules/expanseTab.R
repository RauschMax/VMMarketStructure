# output$selSegmentExpanse <- shiny::renderUI({
#
#   validate(
#     need(segDefIN(), "Please load the data.")
#   )
#
#   nSegs <- length(segDefIN()$segLevFact)
#
#   choList <- lapply(seq_along(segDefIN()$segLevFact),
#                     function(x) {
#                       vec <- paste0(x, "_", seq_along(segDefIN()$segLevFact[[x]]))
#                       names(vec) <- segDefIN()$segLevFact[[x]]
#                       vec
#                     })
#
#   names(choList) <- names(segDefIN()$segLevFact)
#
#   # shiny::selectizeInput('segs', 'Select Subgroup', choices = choList, multiple = TRUE,
#   #                       options = list(dropdownParent = 'body'))
#
#   shinyWidgets::pickerInput(
#     inputId = "segsExpanse",
#     label = "Select Subgroup",
#     choices = choList,
#     multiple = TRUE,
#     width = "100%",
#     options = list(
#       'live-search' = TRUE,
#       'live-search-placeholder' = "Search for segment...",
#       'actions-box' = TRUE,
#       'deselect-all-text' = "Remove Selection",
#       'none-selected-text' = "Total",
#       'multiple-separator' = " | ",
#       size = 10
#     )
#   )
#
# })

SKUinput_Demand <- reactive({


  validate(
    need(!is.null(SKUinput()), "You need a SKU list!")
  )

  # selIDs <- lapply(1:input$segSelect,
  #                  function(i) {
  #                    lc_segs()[get(LCselected) == i, ID]
  #                  })
  # selIDs[[as.numeric(input$segSelect) + 1]] <- lc_segs()[, ID]

  # fixed to 3 segment solution

  input_segSelect <- 4

  selIDs <- lapply(1:input_segSelect,
                   function(i) {
                     lc_segs()[LC4 == i, ID]
                   })
  selIDs[[as.numeric(input_segSelect) + 1]] <- lc_segs()[, ID]

  SKUinput <- SKUinput()

  SKUinput[, c(paste0("Group_", 1:input_segSelect), "Total") :=
             lapply(selIDs,
                    function(i) {
                      sapply(1:nrow(SKUinput),
                             function(x) {
                               selIndex_DT <-
                                 Demand_DT()[,
                                             rowSums(
                                               sapply(1:length(defIN()$nlev),
                                                      function(y) {
                                                        .SD[[y]] %in% c(
                                                          NA, 0, unlist(
                                                            SKUinput[x,
                                                                     mget(paste0("A",
                                                                                 seq_along(defIN()$nlev)))])[y])
                                                      }
                                               )) == length(defIN()$nlev),
                                             .SDcols = paste0("Var", 1:length(defIN()$nlev))]

                               mean(Demand_DT()[selIndex_DT, ][ID %in% i,
                                                               max(demand), by = ID][, V1]) * 100
                             })
                    })]

  SKUinput[, Index := 1:nrow(SKUinput)]

  SKUinput
})



output$SKUperformanceDT <- DT::renderDataTable({

  input_segSelect <- 4

  dtShow <- SKUinput_Demand()[, mget(c("Index", "SKU", paste0("Group_", 1:input_segSelect), "Total", "Portfolio"))]


  DTout <- DT::datatable(dtShow, selection = list(mode = 'multiple', target = 'row'),
                         filter = "none", autoHideNavigation = TRUE, rownames = FALSE,
                         extensions = 'Buttons', escape = FALSE, style = "default", class = 'compact',
                         options = list(pageLength = 20,
                                        dom = 'lrtipB',
                                        buttons = c('csv', 'excel'),
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'background-color': '#989898',
                                          'color': '#fff'});",
                                          "}"),
                                        lengthMenu = list(c(5, 20, -1),
                                                          c('5', '20', 'All'))
                         )
  ) %>%
    DT::formatRound(
      names(dtShow)[-c(1, 2, ncol(dtShow))], digits = 1)

  # help_colors <- colorRampPalette(c("white", "darkgrey"))
  # help_colors <- rep(c("#00b7ff", "#9ee900", "#c800d3", "#fa002a"), length.out = input_segSelect + 1)
  help_colors <- c("#00b7ff", "#9ee900", "#c800d3", "#fa002a", "darkgrey")

  for (i in sequence(input_segSelect + 1)) {
    cols <- c(paste0("Group_", 1:input_segSelect), "Total")[i]
    DTout <- DTout %>%
      formatStyle(cols,
                  background = DT::styleColorBar(c(0, 100),
                                                 help_colors[i], angle = 270),
                  backgroundSize = '98% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  }

  DTout

})