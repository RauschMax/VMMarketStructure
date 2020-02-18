# Study Overview
output$attLev <- renderUI({

  validate(
    need(defIN(), "Please load the data.")
  )

  nAttr <- length(defIN()$nlev)
  lapply(1:(nAttr),
         function(i) {
           choList <- as.list(seq_along(defIN()$attLev[[i]]))
           names(choList) <- defIN()$attLev[[i]]
           selectInput(paste0("ShowAtt", i),
                       label = paste(names(defIN()$attLev)[i],
                                     " (", length(defIN()$attLev[[i]]), " Levels)",
                                     sep = "", collapse = " "),
                       choices = choList,
                       selected = NULL,
                       width = '100%')
         })
})

## Demand !----

demandAnalysis <- reactive({

  selectedLevels <- sapply(1:length(defIN()$nlev),
         function(i) {
           as.numeric(input[[paste0("ShowAtt", i)]])
         })

  selIndex_DT <- Demand_DT()[, rowSums(sapply(1:length(defIN()$nlev),
                                            function(x) {
                                              .SD[[x]] %in% c(NA, 0, selectedLevels[x])
                                            }
  )) == length(defIN()$nlev),
  .SDcols = paste0("Var", 1:length(defIN()$nlev))]

  mean(Demand_DT()[selIndex_DT, ][, max(demand), by = ID][, V1])
  demandSelected <- Demand_DT()[Demand_DT()[selIndex_DT, ][, max(demand), by = ID], on = "ID"]

  demandSummary <- Demand_DT()[selIndex_DT, ][, max(demand), by = ID]
  demandSummary[is.na(V1), V1 := 0]
  demandSummary <- demandSelected[demand > V1, .N, by = ID][demandSummary, on = "ID"]
  demandSummary[is.na(N), N := 0]
  demandSummary <- demandSummary[demandSelected[, .N, by = ID], on = "ID"]
  demandSummary[is.na(i.N), i.N := 0]
  setnames(demandSummary, c("V1", "N", "i.N"), c("Demand", "nComp", "nConc"))
  demandSummary <- demandSummary[, .(ID, Demand, nComp, nConc)]
  demandSummary[, CompRatio := nComp / nConc]

  list(demandSelected = demandSelected,
       demandSummary = demandSummary,
       means = colMeans(demandSummary))
})

output$demandBox <- renderValueBox({

  validate(
    need(demandAnalysis(), "demand is being calculated")
  )

  demand <- demandAnalysis()$means["Demand"]

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

  nComp <- demandAnalysis()$means["nComp"]

  valueBox(value = round(nComp, 0),
           subtitle = "Mean number of competitor products",
           color = "aqua",
           icon = icon("bolt"))
})


## Incrementality !----
output$uniquenessBox <- renderValueBox({

  validate(
    need(demandAnalysis(), "demand is being calculated")
  )

  uniqueness <- 1 - demandAnalysis()$means["CompRatio"]

  valueBox(value = format(round(uniqueness, 3) * 100, nsmall = 1),
           subtitle = "Uniqueness of the product",
           color = "yellow",
           icon = icon("star-o"))
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

  demand_selected <- demandAnalysis()$means["Demand"]

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
                                   selIndexDT <- Demand_DT()[, rowSums(sapply(1:length(defIN()$nlev),
                                                                              function(x) {
                                                                                .SD[[x]] %in% c(NA, 0,
                                                                                                selectedLevels[x])
                                                                              }
                                   )) == length(defIN()$nlev),
                                   .SDcols = paste0("Var", 1:length(defIN()$nlev))]

                                   mean(Demand_DT()[selIndexDT, ][, max(demand), by = ID][, V1]) -
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
  demandAnalysis()
})
