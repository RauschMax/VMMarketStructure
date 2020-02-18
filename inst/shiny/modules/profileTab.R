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

output$profileLevel <- renderPrint({

  levProfile_IDs <- dataUSED()[get(input$levProfile) == 1, ID]

  levProfile <- segIN()$segFactor[ID %in% levProfile_IDs]

  # Profile of considerers of selected level
  lapply(lapply(lapply(levProfile[, -1], table), prop.table),
         function(x) {
           round(x * 100, 1)
         })

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

output$profileSKU <- renderPrint({

  selectedLevels <- sapply(1:length(defIN()$nlev),
                           function(i) {
                             as.numeric(input[[paste0("selAtt", i)]])
                           })

  selIndex_DT <- Demand_DT()[, rowSums(sapply(1:length(defIN()$nlev),
                                              function(x) {
                                                .SD[[x]] %in% c(NA, 0, selectedLevels[x])
                                              }
  )) == length(defIN()$nlev),
  .SDcols = paste0("Var", 1:length(defIN()$nlev))]

  # ASSUPMTION take 90% quantile as "with demand
  proChoiceProfile_IDs <- unique(Demand_DT()[selIndex_DT, ][demand > quantile(Demand_DT()[selIndex_DT,
                                                                                          demand], .9), ID])

  proChoiceProfile <- segIN()$segFactor[ID %in% proChoiceProfile_IDs]

  # Profile of considerers of selected level
  lapply(lapply(lapply(proChoiceProfile[, -1], table), prop.table),
         function(x) {
           round(x * 100, 1)
         })

})