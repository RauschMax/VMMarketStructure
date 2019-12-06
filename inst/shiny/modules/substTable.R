# Combinations !------------------------------------------------------------------------------------------------------

combinations <- reactive({

  VarSelect <- paste0("Att", seq_along(defIN()$nlev))

  combs <- SKU_choice_DT()[, .(.N), by = VarSelect][order(-N)]
  combs[, Combination := do.call(paste0, .SD), .SDcols = VarSelect]

  attFact <- lapply(defIN()$attLev,
                    function(x) {
                      c("All Levels", x)
                      })
  names(attFact) <- VarSelect

  combs[, paste0("Fact", VarSelect) := .SD,
        .SDcols = VarSelect]

  for (j in seq_along(defIN()$nlev)) {
    set(combs, i = NULL, j = paste0("Fact", VarSelect)[j],
        value = factor(combs[[j]],
                       levels = seq_along(attFact[[j]]) - 1,
                       labels = attFact[[j]]))
  }

  combs_labels <- combs[, mget(c("Combination",
                                 paste0("Fact", VarSelect), "N"))]

  names(combs_labels) <- c("Combination",
                           names(defIN()$attLev),
                           "N")

  list(combs = combs,
       combs_labels = combs_labels)

})


output$portTable <- DT::renderDataTable({

  DT::datatable(combinations()$combs_labels, selection = list(mode = 'single', target = 'row'),
                filter = "none", autoHideNavigation = TRUE, rownames = TRUE,
                escape = FALSE, style = "default", class = 'compact',
                options = list(pageLength = 5,
                               dom = 'flrtip',
                               order = list(list(ncol(combinations()$combs_labels), 'desc')),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                 "}"),
                               lengthMenu = list(c(5, 25, -1),
                                                 c('5', '25', 'All'))))
})

output$substitution <- DT::renderDataTable({

  validate(
    need(!is.null(input$portTable_rows_selected), "Please select a combination in the table above.")
  )

  selectedComb <- as.numeric(combinations()$combs_labels[input$portTable_rows_selected,
                                                         Combination])

  IDselect <- SKU_comb_freq()[sapply(SKU_comb_freq()[, Comb],
                                     function(x) {
                                       selectedComb %in% x
                                     }
                                     ), ID]

  substDT <- SKU_choice_DT()[ID %in% IDselect, ]

  tableOut <- substDT[, .(.N), by = .(Comb)][, Distance := N / max(N)][order(-N)][, .(Comb, Distance)]


  DT::datatable(tableOut, selection = list(mode = 'single', target = 'row'),
                filter = "none", autoHideNavigation = TRUE, rownames = TRUE,
                escape = FALSE, style = "default", class = 'compact',
                options = list(pageLength = 10,
                               dom = 'lrtip',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                 "}"),
                               lengthMenu = list(c(10, 25, -1),
                                                 c('10', '25', 'All')))) %>%
    DT::formatStyle(columns = "Distance",
                    background = DT::styleColorBar(c(0, 1.5),
                                                   '#f2da64', angle = 270),
                    backgroundSize = '90% 50%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
    DT::formatRound(columns = "Distance", digits = 2)

})
