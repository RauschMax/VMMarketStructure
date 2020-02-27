output$selProfileLevel <- shiny::renderUI({

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
    inputId = "levProfile",
    label = "Select Levels",
    choices = choList,
    multiple = FALSE,
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

# output$profileLevel <- renderPrint({
#
#   levProfile_IDs <- dataUSED()[get(input$levProfile) == 1, ID]
#
#   levProfile <- segIN()$segFactor[ID %in% levProfile_IDs]
#
#   # Profile of considerers of selected level
#   lapply(lapply(lapply(levProfile[, -1], table), prop.table),
#          function(x) {
#            round(x * 100, 1)
#          })
#
# })

output$profileLevelDT <- DT::renderDataTable({

  validate(
    need(dataUSED(), "Please load the data.")
  )

  DataInclSeg <- dataUSED()[segIN()$segFactor[ID %in% chosenIDs(), ],
                            on = "ID"][, mget(c("ID",
                                                names(dataUSED())[grep("^A", names(dataUSED()))],
                                                names(segIN()$segFactor)[-1]))]

  levelProfileDT <- rbindlist(
    lapply(seq_along(segDefIN()$segLevFact),
           function(x) {
             selCol <- names(segIN()$segFactor)[x + 1]
             name <- names(segDefIN()$segLevFact)[x]

             out <- lapply(names(dataUSED())[grep("^A", names(dataUSED()))],
                           function(i) {
                             help1 <- dcast(DataInclSeg, get(selCol) ~ get(i),
                                            value.var = 'ID', length)

                             if (!("1" %in% names(help1))) help1[, "1" := 0]

                             help1 <- help1[, c("selCol", "1")]
                             names(help1) <- c("seg", i)

                             help1
                           })

             out <- Reduce("cbind", out)[, mget(c("seg", names(dataUSED())[grep("^A", names(dataUSED()))]))]

             out[, Seg := name]
             names(out) <- c("Segment", names(dataUSED())[grep("^A", names(dataUSED()))], "Seg")

             out[, mget(c("Segment", "Seg", names(dataUSED())[grep("^A", names(dataUSED()))]))]
           }))

  levelProfileDT[, c(names(dataUSED())[grep("^A", names(dataUSED()))]) :=
                   lapply(seq_along(.SD),
                          function(x) {
                            .SD[[x]] / colSums(DataInclSeg[, mget(c(names(dataUSED())[grep("^A",
                                                                                           names(dataUSED()))]))])[x]
                          }),
                 .SDcols = names(dataUSED())[grep("^A", names(dataUSED()))]]


  DTout <- DT::datatable(levelProfileDT, selection = list(mode = 'single', target = 'column'),
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
    DT::formatPercentage(
      names(levelProfileDT)[-(1:2)], digits = 1)

  # help_colors <- colorRampPalette(c("white", "darkgrey"))
  help_colors <- rep(c("darkgrey", "lightgrey"), length.out = length(defIN()$nlev))

  for (i in seq_along((defIN()$nlev))) {
    cols <- names(dataUSED())[grep(paste0("^A", i, "_"), names(dataUSED()))]
    DTout <- DTout %>%
      formatStyle(cols,
                  background = DT::styleColorBar(c(0, 1),
                                                 help_colors[i], angle = 270),
                  backgroundSize = '98% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  }

  DTout

})

# Select Product
output$attLev2 <- renderUI({

  validate(
    need(defIN(), "Please load the data.")
  )

  nAttr <- length(defIN()$nlev)
  lapply(1:(nAttr),
         function(i) {
           choList <- as.list(seq_along(defIN()$attLev[[i]]))
           names(choList) <- defIN()$attLev[[i]]
           selectInput(paste0("selAtt", i),
                       label = paste(names(defIN()$attLev)[i],
                                     " (", length(defIN()$attLev[[i]]), " Levels)",
                                     sep = "", collapse = " "),
                       choices = choList,
                       selected = NULL,
                       width = '100%')
         })
})

# output$profileSKU <- renderPrint({
#
#   selectedLevels <- sapply(1:length(defIN()$nlev),
#                            function(i) {
#                              as.numeric(input[[paste0("selAtt", i)]])
#                            })
#
#   selIndex_DT <- Demand_DT_used[, rowSums(sapply(1:length(defIN()$nlev),
#                                               function(x) {
#                                                 .SD[[x]] %in% c(NA, 0, selectedLevels[x])
#                                               }
#   )) == length(defIN()$nlev),
#   .SDcols = paste0("Var", 1:length(defIN()$nlev))]
#
#   # ASSUPMTION take 90% quantile as "with demand
#   proChoiceProfile_IDs <- unique(Demand_DT_used[selIndex_DT, ][demand > quantile(Demand_DT_used[selIndex_DT,
#                                                                                                 demand], .9), ID])
#
#   proChoiceProfile <- segIN()$segFactor[ID %in% proChoiceProfile_IDs]
#
#   # Profile of considerers of selected level
#   lapply(lapply(lapply(proChoiceProfile[, -1], table), prop.table),
#          function(x) {
#            round(x * 100, 1)
#          })
#
# })


observe({
  selectedLevels <- sapply(1:length(defIN()$nlev),
                           function(i) {
                             as.numeric(input[[paste0("selAtt", i)]])
                           })

  Demand_DT_used <- Demand_DT()[ID %in% chosenIDs(), ]

  selIndex_DT <- Demand_DT_used[, rowSums(sapply(1:length(defIN()$nlev),
                                              function(x) {
                                                .SD[[x]] %in% c(NA, 0, selectedLevels[x])
                                              }
  )) == length(defIN()$nlev),
  .SDcols = paste0("Var", 1:length(defIN()$nlev))]

  # ASSUPMTION take 90% quantile as "with demand
  proChoiceProfile_IDs <- unique(Demand_DT_used[selIndex_DT, ][demand > quantile(Demand_DT_used[selIndex_DT,
                                                                                                demand], .9), ID])

  proChoiceProfile <- segIN()$segFactor[ID %in% proChoiceProfile_IDs]

  # Profile of considerers of selected level
  profList <- lapply(lapply(proChoiceProfile[, -1], table, useNA = "ifany"), prop.table)


  lapply(seq_along(segDefIN()$segLevFact),
         function(x) {
           id <- paste0("profProdSeg", x)

           output[[id]] <- DT::renderDataTable({
             dt <- data.table(profList[[x]])
             # names(dt) <- Imp_ordered()$attLev_ordered[[x]]

             DT::datatable(dt, selection = list(mode = 'single', target = 'column'),
                           filter = "none", autoHideNavigation = TRUE, rownames = FALSE,
                           colnames = names(segDefIN()$segLevFact)[x],
                           escape = FALSE, style = "default", class = 'compact',
                           options = list(columnDefs = list(list(width = '200px', targets = 0)),
                                          # pageLength = 1,
                                          ordering = FALSE,
                                          dom = 't',
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$(this.api().table().header()).css({'background-color': '#989898',
                                             'color': '#fff'});",
                                            "}"))) %>%
               formatPercentage(names(dt)[2],  digits = 1) %>%
               DT::formatStyle(columns = names(dt)[2],
                               background = DT::styleColorBar(c(0, 1),
                                                              '#f2da64', angle = 270),
                               backgroundSize = '98% 90%',
                               backgroundRepeat = 'no-repeat',
                               backgroundPosition = 'center')
           })
         })

})

# table like output for counts
output$profProdSegUI <- renderUI({

  # lapply(seq_along(Imp_ordered()$LevCount),
  lapply(seq_along(segDefIN()$segLevFact),
         function(x) {
           id <- paste0("profProdSeg", x)
           div(style = "width:99%",
               # h6(div(style = "font-weight:bold; display:inline-block;",
               #        toupper(names(segDefIN()$segLevFact)[x]))),
               DT::dataTableOutput(id)
           )
         })

})
