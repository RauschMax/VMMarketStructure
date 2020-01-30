## Demand !----

DemandList <- reactive({

  Data <- dataUSED()

  DemandList <- lapply(SKU_choice_DT()$SKU_choice,
         function(i) {
           Importance_scaled <- Importance()$Importance * (sapply(i[, mget(paste0("Att", 1:length(defIN()$nlev)))],
                                                                max) != 0)

           Importance_scaled <- Importance_scaled / sum(Importance_scaled)


           IndChoHelp <- lapply(seq_along(defIN()$nlev),
                                function(x) {
                                  (tabulate(i[, get(paste0("Att", x))],
                                            nbins = defIN()$nlev[x]) != 0) * Importance_scaled[x]
                                })

           helpInd <- sapply(IndChoHelp,
                             function(x) {
                               out <- which(x != 0)

                               if (length(out) > 0) {
                                 c(0, out)
                               } else {
                                 NA
                               }
                             })

           if (length(unique(i[, ID])) != 1) {break}

           help_DT <- data.table(ID = i[1, ID],
                                 expand.grid(helpInd))

           help_DT[, c(paste0("Val", seq_along(defIN()$nlev))) := lapply(seq_along(defIN()$nlev),
                                                                 function(x) {
                                                                   varIter <- .SD[[x]]
                                                                   sapply(varIter,
                                                                          function(y) {
                                                                            if (any(is.na(y))) {
                                                                              out <- NA
                                                                            } else {
                                                                              out <- IndChoHelp[[x]][y]
                                                                              if (length(out) > 0) {
                                                                                out
                                                                              } else {
                                                                                0
                                                                              }
                                                                            }
                                                                          })
                                                                 }),
                   .SDcols = paste0("Var", seq_along(defIN()$nlev))]

           help_DT[, demand := rowSums(.SD, na.rm = TRUE), .SDcols = paste0("Val", seq_along(defIN()$nlev))]

           help_DT
         })

  names(DemandList) <- Data[, ID]

  DemandList

})

demandAnalysis <- reactive({

  Data <- dataUSED()

  nAttr <- length(defIN()$nlev)

  selectedLevels <- sapply(1:(nAttr),
         function(i) {
           as.numeric(input[[paste0("ShowAtt", i)]])
         })

  demandAnalysis <- lapply(DemandList(),
                           function(x) {
                             helpDT_in <- copy(x)
                             helpDT_in[, c(paste0("Val", seq_along(defIN()$nlev))) := NULL]

                             selIndex <- helpDT_in[, rowSums(sapply(1:length(defIN()$nlev),
                                                                    function(x) {
                                                                      .SD[[x]] %in% c(NA, 0, selectedLevels[x])
                                                                    })
                                                             ) == length(defIN()$nlev),
                                                   .SDcols = paste0("Var", 1:length(defIN()$nlev))]

                             helpDT <- helpDT_in[selIndex]

                             demandHelp <- max(helpDT[, demand])

                             compHelp <- helpDT_in[demand > demandHelp]
                             compHelp

                             list(demand = demandHelp,
                                  DT = helpDT,
                                  competitors = compHelp,
                                  nComp = nrow(compHelp),
                                  nConcepts = nrow(helpDT_in))
                           })

  names(demandAnalysis) <- Data[, ID]

  demandAnalysis

})

output$demandBox <- renderValueBox({

  validate(
    need(demandAnalysis(), "demand is being calculated")
  )

  demand <- mean(sapply(demandAnalysis(),
                        function(x) {
                          x$demand
                        }))
  #
  # valueBox(value = round(demand, 2),
  #          subtitle = "Demand",
  #          color = "green",
  #          icon = icon("heart-o"))

  valueBox(value = format(round(demand, 3) * 100, nsmall = 1),
           subtitle = "Demand of selected product",
           color = "red",
           icon = icon("heart-o"))
})

## Number of competitor products !----
output$compBox <- renderValueBox({

  validate(
    need(demandAnalysis(), "demand is being calculated")
  )

  nComp <- mean(sapply(demandAnalysis(),
                       function(x) {
                         x$nComp
                       }))

  valueBox(value = round(nComp, 0),
           subtitle = "Mean number of competitor products",
           color = "aqua",
           icon = icon("bolt"))
})


# ## Supply !----
# output$supplyBox <- renderValueBox({
#
#   validate(
#     need(demandAnalysis(), "demand is being calculated")
#   )
#   )
#
#   # supply <- sum(sapply(seq_along(Imp_ordered()$LevCount),
#   #                      function(x) {
#   #                        IDselected <- paste0("decMat", x, "_columns_selected")
#   #                        Supply()$supply[[x]][input[[IDselected]] + 1]
#   #                      }))
#
#   valueBox(value = round(42.42, 2),
#            subtitle = "Supply",
#            color = "lime",
#            icon = icon("shopping-basket"))
# })


## Incrementality !----
output$uniquenessBox <- renderValueBox({

  validate(
    need(demandAnalysis(), "demand is being calculated")
  )

  uniqueness <- mean(sapply(demandAnalysis(),
                            function(x) {
                              1 - (x$nComp / x$nConcepts)
                            }))

  valueBox(value = format(round(uniqueness, 3) * 100, nsmall = 1),
           subtitle = "Uniqueness of the product",
           color = "yellow",
           icon = icon("star-o"))
})

output$demandHist <- renderPlot({

  df_demand <- data.table(demand = sapply(demandAnalysis(),
                                          function(x) {
                                            x$demand
                                          }))

  ggplot(df_demand, aes(x = demand)) +
    geom_histogram(alpha = 0.7, position = "identity",
                   aes(y  =  ..density..),
                   color = "black", bins = 15) +
    geom_density(alpha = .2, fill = "#FF6666") +
    geom_vline(aes(xintercept = mean(demand)),
               color = "blue", linetype = "dashed", size = 1)
  })

output$testDemand <- renderPrint({
  demandAnalysis()[1:3]
})
