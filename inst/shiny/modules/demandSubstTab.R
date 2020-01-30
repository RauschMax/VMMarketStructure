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

  Demand_DT <- rbindlist(DemandList)[, mget(c("ID", paste0("Var", 1:length(defIN()$nlev)), "demand"))]

  list(DemandList = DemandList,
       Demand_DT = Demand_DT)

})

demandAnalysis <- reactive({

  Data <- dataUSED()

  selectedLevels <- sapply(1:length(defIN()$nlev),
         function(i) {
           as.numeric(input[[paste0("ShowAtt", i)]])
         })

  demandAnalysis <- lapply(DemandList()$DemandList,
                           function(x) {
                             helpDT_in <- copy(x)
                             helpDT_in[, c(paste0("Val", seq_along(defIN()$nlev))) := NULL]

                             selIndex <- helpDT_in[, rowSums(sapply(1:length(defIN()$nlev),
                                                                    function(x) {
                                                                      .SD[[x]] %in% c(NA, 0, selectedLevels[x])
                                                                    })
                                                             ) == length(defIN()$nlev),
                                                   .SDcols = paste0("Var", 1:length(defIN()$nlev))]

                             if (any(selIndex)) {
                               helpDT <- helpDT_in[selIndex]
                               demandHelp <- max(helpDT[, demand])
                             } else {
                               helpDT <- NA
                               demandHelp <- 0
                             }

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

# output$demandHist <- renderPlot({
#
#   df_demand <- data.table(demand = sapply(demandAnalysis(),
#                                           function(x) {
#                                             x$demand
#                                           }))
#
#   ggplot(df_demand, aes(x = demand)) +
#     geom_histogram(alpha = 0.7, position = "identity",
#                    aes(y  =  ..density..),
#                    color = "black", bins = 15) +
#     geom_density(alpha = .2, fill = "#FF6666") +
#     geom_vline(aes(xintercept = mean(demand)),
#                color = "blue", linetype = "dashed", size = 1)
#   })

output$strategyProfile <- DT::renderDataTable({

  validate(
    need(lc_segs(), "Please load the data.")
  )

  demand_selected <- mean(sapply(demandAnalysis(),
                                 function(x) {
                                   x$demand
                                 }))

  selectedLevels <- sapply(1:length(defIN()$nlev),
                           function(i) {
                             as.numeric(input[[paste0("ShowAtt", i)]])
                           })

  stratRotation <- lapply(seq_along(selectedLevels),
                          function(x) {
                            lapply(1:defIN()$nlev[x],
                                   function(y) {
                                     out <- selectedLevels
                                     out[x] <- y
                                     out
                                   })
                          })

  stratDemand <- sapply(stratRotation,
                        function(i) {
                          sapply(i,
                                 function(j) {
                                   selectedLevels <- j
                                   selIndexDT <- DemandList()$Demand_DT[, rowSums(sapply(1:length(defIN()$nlev),
                                                                            function(x) {
                                                                              .SD[[x]] %in% c(NA, 0, selectedLevels[x])
                                                                            }
                                   )) == length(defIN()$nlev),
                                   .SDcols = paste0("Var", 1:length(defIN()$nlev))]

                                   mean(DemandList()$Demand_DT[selIndexDT, ][, max(demand), by = ID][, V1]) -
                                     demand_selected
                                 })
                        })

  stratDT <- data.table(Attribute = rep(names(defIN()$attLev), defIN()$nlev),
                        Level = unlist(defIN()$attLev),
                        Change = Reduce(c, stratDemand) * 100)


  color_from_middle <- function(data, color1, color2) {
    max_val <- max(abs(data))
    JS(sprintf(paste0("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent,",
                      "transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,",
                      "transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%,",
                      "%s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'"),
               max_val, color1, max_val, color1, color2, color2, max_val, max_val))
  }

  DT::datatable(stratDT, selection = list(mode = 'single', target = 'row'),
                filter = "none", autoHideNavigation = TRUE, rownames = FALSE,
                escape = FALSE, style = "default", class = 'compact',
                options = list(columnDefs = list(list(className = 'dt-right', targets = 2),
                                                 list(width = '40%', targets = 2)),
                               autoWidth = TRUE,
                               pageLength = nrow(stratDT),
                               ordering = FALSE,
                               dom = 't',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                             'color': '#fff'});",
                                 "}"))) %>%
    DT::formatRound("Change", digits = 1) %>%
    DT::formatStyle(columns = "Change",
                    background = color_from_middle(stratDT$Change, 'red', 'blue'),
                    backgroundSize = '98% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
    DT::formatStyle(columns = "Attribute",
                    target = 'row',
                    backgroundColor =
                      DT::styleEqual(unique(stratDT$Attribute),
                                     grDevices::colorRampPalette(c("white",
                                                                   "grey"))(length(unique(stratDT$Attribute)))))

})

output$testDemand <- renderPrint({
  "HI THERE"
})
