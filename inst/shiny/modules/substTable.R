# Combinations !------------------------------------------------------------------------------------------------------

combinations <- reactive({

  combs <- dataIN()$SKU_choice_DT[, .(.N), by = .(Att1, Att2, Att3, Att4, Att5, Att6, Att7, Att8)][order(-N)]
  combs[, Combination := paste0(Att1, Att2, Att3, Att4, Att5, Att6, Att7, Att8)]

  combs_labels <- data.table(Combination = combs[, Combination],
                             sapply(1:length(defIN()$nlev),
                                    function(x) {
                                      sapply(unlist(combs[, x, with = FALSE]),
                                             function(y) {
                                               do.call(switch, c(y, as.list(defIN()$attLev[[x]])))
                                             })
                                    }),
                             N = combs$N)

  names(combs_labels) <- c("Combination", names(defIN()$attLev), "N")

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

# corDist <- reactive({
#
#   dummyChoice <- data.table::dcast(dataIN()$SKU_choice_DT[,.(ID, Comb)], ID ~ Comb)
#
#   helpCor <- data.table::data.table(dummyChoice[, "ID"],
#                                     !is.na(dummyChoice[, -1, with = FALSE])) * 1
#
#   stats::cor(helpCor[, -1, with = FALSE])
#
# })

output$substitution <- DT::renderDataTable({

  validate(
    need(!is.null(input$portTable_rows_selected), "Please select a combination in the table above.")
  )

  # indexCor <- which(combinations()$combs_labels[input$portTable_rows_selected, Combination] == colnames(corDist()))
  #
  # sortDist <- sort(corDist()[, indexCor], decreasing = TRUE)
  # sortDist <- sortDist[sortDist > 0]
  #
  # tableOut <- data.table::data.table(Combination = names(sortDist),
  #                                    Distance = sortDist)

  selectedComb <- as.numeric(combinations()$combs_labels[input$portTable_rows_selected, Combination])

  IDselect <- SKU_comb_freq()[sapply(SKU_comb_freq()[, Comb],
                                     function(x) {
                                       selectedComb %in% x
                                     }), ID]

  IDselect
  length(IDselect)

  substDT <- dataIN()$SKU_choice_DT[ID %in% IDselect, ]

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
