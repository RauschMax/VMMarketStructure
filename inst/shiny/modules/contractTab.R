output$SKUcontractDT1 <- DT::renderDataTable({

  dtShowContract1 <- SKUinput_Demand()[Portfolio == 1,
                                       mget(c("SKU", "Total"))][order(-Total)]


  DT::datatable(dtShowContract1, selection = list(mode = 'multiple', target = 'row'),
                filter = "none", autoHideNavigation = TRUE, rownames = FALSE,
                extensions = 'Buttons', escape = FALSE, style = "default", class = 'compact',
                options = list(pageLength = nrow(dtShowContract1),
                               dom = 'lrtipB',
                               buttons = c('csv', 'excel'),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                 "}"),
                               lengthMenu = list(c(5, nrow(dtShowContract1), -1),
                                                 c('5', as.character(nrow(dtShowContract1)), 'All'))
  )
                ) %>%
    DT::formatRound("Total", digits = 1) %>%
    DT::formatStyle("Total",
                    background = DT::styleColorBar(c(0, 100), "#0060FF", angle = 270),
                    backgroundSize = '98% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')

})

output$SKUcontractDT2 <- DT::renderDataTable({

  input_segSelect <- 4

  dtShowContract2 <- SKUinput_Demand()[Portfolio == 1,
                                       mget(c("SKU", paste0("Group_", 1:input_segSelect), "Total"))]


  DTout <- DT::datatable(dtShowContract2, selection = list(mode = 'multiple', target = 'row'),
                         filter = "none", autoHideNavigation = TRUE, rownames = FALSE,
                         extensions = 'Buttons', escape = FALSE, style = "default", class = 'compact',
                         options = list(pageLength = nrow(dtShowContract2),
                                        dom = 'lrtipB',
                                        buttons = c('csv', 'excel'),
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'background-color': '#989898',
                                          'color': '#fff'});",
                                          "}"),
                                        lengthMenu = list(c(5, nrow(dtShowContract2), -1),
                                                          c('5', as.character(nrow(dtShowContract2)), 'All')))
                         ) %>%
    DT::formatRound(c(paste0("Group_", 1:input_segSelect), "Total"), digits = 1)

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