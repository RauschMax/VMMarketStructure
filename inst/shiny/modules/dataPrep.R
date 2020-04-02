# INPUT ELEMENTS

# Segment selection on Decision Hierarchy Tab
output$selSegment <- shiny::renderUI({

  validate(
    need(segLev(), "Please load the data.")
  )

  nSegs <- length(segLev())

  choList <- lapply(seq_along(segLev()),
                    function(x) {
                      vec <- paste0(x, "_", seq_along(segLev()[[x]]))
                      names(vec) <- segLev()[[x]]
                      vec
                    })

  names(choList) <- names(segLev())

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

# SelectedLevel selection on Decision Hierarchy Tab
output$selLevel <- shiny::renderUI({

  validate(
    need(defIN(), "Please load the data.")
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

# Extract Data !--------------------------------------------------------------------------------------------------------

segLev <- reactive({

  validate(
    need(def(), "Wait for definitions!")
  )

  segDefIN <- def()[(grep("[[]Segments[]]", def()) + 1):length(def())]
  # segment info
  segIndex <- grep("^[^ ]", segDefIN)

  # indecies of levels
  segLevIndex <- apply(cbind(segIndex + 1,
                             c(segIndex[-1] - 1, length(segDefIN))), 1,
                       function(x) {seq(x[1], x[2])})


  # named list of levels
  segLev <- lapply(segLevIndex,
                   function(x) {
                     gsub("^[ ]", "", segDefIN[x])
                   })
  names(segLev) <- segDefIN[segIndex]
  segLev
})

chosenIDs <- reactive({

  validate(
    need(dataIN(), "Wait for data!")
  )

  if (is.null(input$segs) & is.null(input$levels)) {
    chosenIDs <- dataIN()[, ID]
  } else {
    # Segment Selection
    if (is.null(input$segs)) {
      validSegSel <- rep(TRUE, nrow(dataIN()))
    } else {
      segChosen <- lapply(seq_along(segLev()),
                          function(x) {
                            if (length(grep(paste0(x, "_"), input$segs)) > 0) {
                              sapply(strsplit(input$segs[grep(paste0(x, "_"), input$segs)], "_"),
                                     function(y) {
                                       # segLev()[[x]][as.numeric(y[2])]
                                       as.numeric(y[2])
                                     })
                            }
                          })
      names(segChosen) <- names(dataIN())[(sum(defIN()$nlev) + length(defIN()$nlev) + 2):ncol(dataIN())]

      segSelectionList <- lapply(names(segChosen),
                                 function(x) {
                                   if (is.null(segChosen[[x]])) {
                                     NULL
                                   } else {
                                     dataIN()[, get(x)] %in% segChosen[[x]]
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

# chosen Subgroup
dataUSED <- reactive({

  validate(
    need(length(chosenIDs()) > 10, "Base size too small!")
  )

  dataIN()[ID %in% chosenIDs(), ]

})

# EXCTRACT Definition DATA !------------------------------------------------------------------------------------------

defIN <- reactive({

  validate(
    need(def(), "Wait for it!")
  )

  defHelp <- def()[2:(grep("[[]Segments[]]", def()) - 1)]
  attIndex <- grep("^[^ ]", defHelp)

  # indecies of levels
  levIndex <- apply(cbind(attIndex + 1,
                          c(attIndex[-1] - 1, length(defHelp))), 1,
                    function(x) {seq(x[1], x[2])})


  # named list of levels
  attLev <- lapply(levIndex,
                   function(x) {
                     gsub("^[ ]", "", defHelp[x])
                   })
  names(attLev) <- defHelp[attIndex]

  # number of levels
  nlev <- sapply(attLev, length)

  rm(defHelp)
  defIN <- list(attLev = attLev,
                nlev = nlev)
  defIN

})

# calculate importances and counts
DecHierarchy <- reactive({

  validate(
    need(dataUSED(), "Wait for data!")
  )

  validate(
    need(defIN(), "Wait for definitions!")
  )

  input$segs
  input$levels

  isolate({
    Data_inverseRanks <- dataUSED()[, mget(c("ID",
                                             names(dataUSED())[grep("^R",
                                                                    names(dataUSED()))]))]

    Data_inverseRanks[, names(dataUSED())[grep("^R", names(dataUSED()))] :=
                        lapply(.SD,
                               function(x) {
                                 if (all(is.na(x))) {
                                   out <- x
                                 } else {
                                   out <- (length(defIN()$nlev) + 1) - x
                                 }
                                 sapply(out,
                                        function(y) {
                                          ifelse(is.na(y), 0, y)
                                        })
                               }),
                      .SDcols = names(dataUSED())[grep("^R", names(dataUSED()))]]

    Importance_invRanks <- sapply(Data_inverseRanks[, mget(names(Data_inverseRanks)[grep("^R",
                                                                                         names(Data_inverseRanks))])],
                                  mean)

    Importance <- Importance <- Importance_invRanks / sum(Importance_invRanks)
    names(Importance) <- names(defIN()$attLev)

    # Level counts - 100% within attributes
    LevelCounts <- lapply(1:length(defIN()$nlev),
                          function(y) {
                            helpPattern <- paste0("^A", 1:length(defIN()$nlev), "_")[y]
                            out <- sapply(names(dataUSED())[grep(helpPattern,
                                                                 x = names(dataUSED()))],
                                          function(i) {
                                            sum(dataUSED()[, get(i)]) /
                                              length(dataUSED()[, get(i)])
                                          })

                            names(out) <- defIN()$attLev[[y]]
                            out
                          })
    names(LevelCounts) <- names(defIN()$attLev)

    orderAtt <-  order(Importance,
                       decreasing = TRUE)

    LevelCounts_ordered <- LevelCounts[orderAtt]

    attLev_ordered <- defIN()$attLev[orderAtt]

    Dt_LevCount <- rbindlist(lapply(LevelCounts_ordered,
                                    function(x) {
                                      out <- rep(NA, max(defIN()$nlev))
                                      out[1:length(x)] <- x
                                      data.table(t(out))
                                    }))

    Dt_Levels <- rbindlist(lapply(attLev_ordered,
                                  function(x) {
                                    out <- rep("", max(defIN()$nlev))
                                    out[1:length(x)] <- x
                                    data.table(t(out))
                                  }))
    names(Dt_Levels) <- paste0("A", 1:max(defIN()$nlev))

    DecHierarchy <- list(Imp = Importance[orderAtt],
                         LevCount = LevelCounts_ordered,
                         attLev_ordered = attLev_ordered,
                         Dt_LevCount = Dt_LevCount,
                         Dt_Levels = Dt_Levels)
    DecHierarchy
  })
})

# DATA extracted !------------------------------------------------------------------------------------------------------
