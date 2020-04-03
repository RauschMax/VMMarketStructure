output$summaryDT <- DT::renderDataTable({

  validate(
    need(DecHierarchy(), "Wait a second!")
  )

  summaryDT <- data.table(Attributes = names(defIN()$attLev),
                        Importance = DecHierarchy()$Importance,
                        nLevels = defIN()$nlev)

  DT::datatable(summaryDT, selection = list(mode = 'single', target = 'row'),
                escape = FALSE, rownames = FALSE,
                style = "default", class = 'compact',
                options = list(pageLength = nrow(summaryDT),
                               ordering = FALSE,
                               dom = 't',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                           'color': '#fff'});",
                                 "}"))) %>%
    formatPercentage("Importance", digits = 1)
})

output$nRespOverview <- renderValueBox({

  validate(
    need(dataIN(), "demand is needed")
  )

  valueBox(value = nrow(dataIN()),
           subtitle = "Number of respondents",
           color = "purple",
           icon = icon("users"))
})