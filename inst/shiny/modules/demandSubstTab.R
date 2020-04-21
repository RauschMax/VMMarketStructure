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
                       # selected = 1,
                       # multiple = TRUE,
                       width = '100%')
         })
})

## Demand !----

demandAnalysis <- reactive({

  selectedLevels <- sapply(1:length(defIN()$nlev),
         function(i) {
           as.numeric(input[[paste0("ShowAtt", i)]])
         })

  Demand_DT_used <- Demand_DT()[ID %in% chosenIDs(), ]

  selIndex_DT <- Demand_DT_used[,
                                rowSums(sapply(1:length(defIN()$nlev),
                                               function(x) {
                                                 .SD[[x]] %in% c(NA, 0,
                                                                 selectedLevels[x])
                                               }
                                )) == length(defIN()$nlev),
                                .SDcols = paste0("Var", 1:length(defIN()$nlev))]

  Demand_DT_selected <- Demand_DT_used[selIndex_DT, ]

  thresholdUsed <- quantile(Demand_DT_selected[, demand],
                            input$demandThreshold)

  ntakers <- length(unique(Demand_DT_selected[demand >= thresholdUsed, ID]))
  ntakersAll <- length(unique(Demand_DT_used[, ID]))

  meanDemand <- round(ntakers / ntakersAll, 3) * 100


  meanCompProds <- round(Demand_DT_used[demand >= thresholdUsed,
                                        .N, by = "ID"][, mean(N)], 1)

  nDemandUsed <- Demand_DT_used[demand >= thresholdUsed,
                                .N, by = "ID"]

  nDemandSelected <- Demand_DT_selected[demand >= thresholdUsed,
                                        .N, by = "ID"]

  nDemandUsed <- nDemandUsed[nDemandSelected, on = "ID"][, ratio := i.N / N]

  uniqueness <- round(nDemandUsed[, mean(ratio)], 3) * 100

  demandAnalysis <- list(meanDemand = meanDemand,
                         meanCompProds = meanCompProds,
                         uniqueness = uniqueness,
                         Demand_DT_used = Demand_DT_used,
                         Demand_DT_selected = Demand_DT_selected,
                         thresholdUsed = thresholdUsed)

  demandAnalysis

})

output$demandBox <- renderValueBox({

  validate(
    need(demandAnalysis(), "demand is needed")
  )

  valueBox(value = paste(format(demandAnalysis()$meanDemand,
                          nsmall = 1), "%"),
           subtitle = "with demand for selected product",
           color = "red",
           icon = icon("check-square-o"))
})

output$compBox <- renderValueBox({

  validate(
    need(demandAnalysis(), "demand is needed")
  )

  valueBox(value = format(demandAnalysis()$meanCompProds,
                          nsmall = 1),
           subtitle = "Number of competitor products",
           color = "purple",
           icon = icon("shopping-cart"))
})

output$uniquenessBox <- renderValueBox({

  validate(
    need(demandAnalysis(), "demand is being calculated")
  )

  valueBox(value = format(demandAnalysis()$uniqueness,
                          nsmall = 1),
           subtitle = "Uniqueness of the product",
           color = "yellow",
           icon = icon("star-o"))
})


output$demandGrid <- scatterD3::renderScatterD3({

  validate(
    need(demandAnalysis(), "Awaiting data!")
  )

  # scatterplot - demand vs. uniqueness
  plot_demand <- data.frame(demand = demandAnalysis()$meanDemand,
                            unique = demandAnalysis()$uniqueness,
                            label = "Selected Product")

  tooltips <- "Mouseover Text"

  medianDemand <- median(demandAnalysis()$Demand_DT_selected[, demand]) * 100
  medianUnique <- median(demandAnalysis()$Demand_DT_used[, demand]) * 100

  scatterD3::scatterD3(data = plot_demand, x = unique, y = demand, lab = label,
                       labels_size = 10, colors = "#bd9b08",
                       hover_size = 4, hover_opacity = 1,
                       ylab = "Demand", xlab = "Uniqueness",
                       caption = paste("Demand vs. Uniqueness;",
                                       "Dotted lines indicate the median of each dimension."),
                       tooltip_text = tooltips,
                       xlim = c(0, 100), ylim = c(0, 100),
                       # xlim = c(0, 100), ylim = c(0, max(plot_imp$TakeRate) + 10),
                       lines = data.frame(slope = c(0, Inf, 0, Inf),
                                          intercept = c(0, 0, medianDemand, medianUnique),
                                          stroke_width = c(1, 1, 1, 1),
                                          stroke = c("black", "black", "grey", "grey"),
                                          stroke_dasharray = c(5, 5, 2, 2)))

})


output$strategyProfile <- DT::renderDataTable({

  validate(
    need(demandAnalysis(), "Demand needs to be calculated.")
  )

  demand_selected <- demandAnalysis()$meanDemand

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

  Demand_DT_used <- demandAnalysis()$Demand_DT_used

  stratDemand <- sapply(stratRotation,
                        function(i) {
                          sapply(i,
                                 function(j) {
                                   selectedLevels <- j
                                   selIndex_DT <- Demand_DT_used[,
                                                                 rowSums(sapply(1:length(defIN()$nlev),
                                                                                function(x) {
                                                                                  .SD[[x]] %in% c(NA, 0,
                                                                                                  selectedLevels[x])
                                                                                }
                                                                 )) == length(defIN()$nlev),
                                                                 .SDcols = paste0("Var", 1:length(defIN()$nlev))]

                                   DT_selected <- Demand_DT_used[selIndex_DT, ]

                                   thresholdUsed_ST <- quantile(DT_selected[, demand],
                                                                input$demandThreshold)

                                   ntakers <- length(unique(DT_selected[demand >= thresholdUsed_ST, ID]))
                                   ntakersAll <- length(unique(Demand_DT_used[, ID]))

                                   meanDemand <- round(ntakers / ntakersAll, 3) * 100
                                   meanDemand - demand_selected
                                 })
                        })


  stratDT <- data.table(Attribute = rep(names(defIN()$attLev), defIN()$nlev),
                        Level = unlist(defIN()$attLev),
                        Change = Reduce(c, stratDemand))


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
                extensions = 'Buttons', escape = FALSE, style = "default", class = 'compact',
                options = list(columnDefs = list(list(className = 'dt-right', targets = 2),
                                                 list(width = '40%', targets = 2)),
                               autoWidth = TRUE,
                               pageLength = 23,
                               ordering = FALSE,
                               dom = 'lrtpB',
                               buttons = c('csv', 'excel'),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                             'color': '#fff'});",
                                 "}"),
                               lengthMenu = list(c(23, -1),
                                                 c('Part', 'All')))) %>%
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
