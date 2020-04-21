output$attLevPort <- renderUI({

  validate(
    need(defIN(), "Please load the data.")
  )

  nAttr <- length(defIN()$nlev)

  if (is.null(input$portfolioDT_columns_selected)) {
    "Please select a product."
  } else if (!(input$portfolioDT_columns_selected > 0)) {
    "Please select a product."
  } else {
    div(
      h5("Please change the product here and click 'Change Product'."),
      textInput("prodName", label = "Product name:",
                value = names(portfolio$DT)[input$portfolioDT_columns_selected],
                width = "100%"),
      hr(),
      div(
        style = "overflow-y: scroll; height:60vh; font-size:80%;",
        lapply(1:(nAttr),
               function(i) {
                 selLevel <- portfolio$DT[i, get(names(portfolio$DT)[input$portfolioDT_columns_selected])]

                 choList <- as.list(seq_along(defIN()$attLev[[i]]))
                 names(choList) <- defIN()$attLev[[i]]
                 selectInput(paste0("prodAtt", i),
                             label = paste(names(defIN()$attLev)[i],
                                           " (", length(defIN()$attLev[[i]]), " Levels)",
                                           sep = "", collapse = " "),
                             choices = choList,
                             selected = selLevel,
                             # multiple = TRUE,
                             width = '100%')
               })
      )
    )
  }
})

portfolio <- reactiveValues()
portfolio$DTold <- NA

observe({

  portfolio$DT <- data.table(sapply(seq_along(defIN()$attLev[[1]]),
                                    function(i) {
                                      c(i, rep(1, length(defIN()$nlev[-1])))
                                    }))
  setnames(portfolio$DT,
           names(portfolio$DT),
           paste0("Prod", seq_along(names(portfolio$DT))))

  # names(portfolio$DT) <- paste0("Prod", seq_along(defIN()$attLev[[1]]))

  portDT <- copy(portfolio$DT)
  portDT <- portDT[, c(names(portDT)) := lapply(.SD,
                                                function(y) {
                                                  sapply(seq_along(y),
                                                         function(x) {
                                                           defIN()$attLev[[x]][y[x]]
                                                         })}),
                   .SDcols = names(portDT)]

  portfolio$portDT <- portDT
})



observeEvent(input$addProd, {
               portfolio$DTold <- portfolio$DT

               inc <- ncol(portfolio$DT) + 1
               repeat {
                 newVar <- paste0("Prod", inc)
                 if (!(newVar %in% names(portfolio$DT))) {break}
                 inc <- inc + 1
               }

               portfolio$DT <- portfolio$DT[, c(newVar) := rep(1, length(defIN()$nlev))]

               portDT <- copy(portfolio$DT)
               portDT <- portDT[, c(names(portDT)) := lapply(.SD,
                                                             function(y) {
                                                               sapply(seq_along(y),
                                                                      function(x) {
                                                                        defIN()$attLev[[x]][y[x]]
                                                                      })}),
                                .SDcols = names(portDT)]

               portfolio$portDT <- portDT
             })

observeEvent(input$removeProd, {
               if (is.null(input$portfolioDT_columns_selected)) {
                 showNotification("Please select a column from the table.",
                                  duration = 1, type = "message")
               } else {
                 portfolio$DTold <- portfolio$DT
                 varSel <- names(portfolio$DT)[input$portfolioDT_columns_selected]
                 portfolio$DT <- portfolio$DT[, c(varSel) := NULL]

                 portDT <- copy(portfolio$DT)
                 portDT <- portDT[, c(names(portDT)) := lapply(.SD,
                                                               function(y) {
                                                                 sapply(seq_along(y),
                                                                        function(x) {
                                                                          defIN()$attLev[[x]][y[x]]
                                                                        })}),
                                  .SDcols = names(portDT)]

                 portfolio$portDT <- portDT
               }
             })

observeEvent(input$changeProd, {
               if (is.null(input$portfolioDT_columns_selected)) {
                 showNotification("Please select a column from the table.",
                                  duration = 1, type = "message")
               } else {
                 selectedLevels <- sapply(1:length(defIN()$nlev),
                                          function(i) {
                                            as.numeric(input[[paste0("prodAtt", i)]])
                                          })

                 portfolio$DTold <- portfolio$DT
                 varSel <- names(portfolio$DT)[input$portfolioDT_columns_selected]
                 portfolio$DT <- portfolio$DT[, c(varSel) := selectedLevels]
                 setnames(portfolio$DT, varSel, input$prodName)

                 portDT <- copy(portfolio$DT)
                 portDT <- portDT[, c(names(portDT)) := lapply(.SD,
                                                               function(y) {
                                                                 sapply(seq_along(y),
                                                                        function(x) {
                                                                          defIN()$attLev[[x]][y[x]]
                                                                        })}),
                                  .SDcols = names(portDT)]

                 portfolio$portDT <- portDT
               }
             })

output$portfolioDT <- DT::renderDataTable({

  validate(
    need(portfolio$DT, "Wait a second!")
  )

  input$addProd
  input$changeProd
  input$removeProd

  isolate({
    portDT <- portfolio$portDT

    DT::datatable(portDT, selection = list(mode = 'single', target = 'column'),
                  escape = FALSE, rownames = names(defIN()$nlev),
                  style = "default", class = 'compact',
                  options = list(pageLength = nrow(portDT),
                                 ordering = FALSE,
                                 dom = 't',
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                   "}"))) %>%
      formatStyle(columns = 0, backgroundColor = "lightgrey", fontWeight = "bold")
  })

  })

portfolioOverview <- reactive({

  input$addProd
  input$changeProd
  input$removeProd

  Demand_DT_used <- Demand_DT()[ID %in% chosenIDs(), ]

  selIndex_Portfolio_2 <- data.table(
    sapply(1:ncol(portfolio$DT),
           function(i) {
             Demand_DT_used[,
                            rowSums(sapply(1:length(defIN()$nlev),
                                           function(x) {
                                             .SD[[x]] %in% c(NA, 0,
                                                             unlist(portfolio$DT[x, i,
                                                                                 with = FALSE]))
                                           }
                            )) == length(defIN()$nlev),
                            .SDcols = paste0("Var", 1:length(defIN()$nlev))]
           }))

  portfolioDT <- data.table(ID = unique(Demand_DT_used[, ID]))
  portHelp <- Reduce(merge,
    lapply(1:ncol(selIndex_Portfolio_2),
           function(x) {

             Demand_port <- Demand_DT_used[unlist(selIndex_Portfolio_2[, x,
                                                                       with = FALSE]), ]
             thresholdUsed_PO <- quantile(Demand_port[, demand],
                                          input$demandThreshold)

             Demand_port <- Demand_port[demand >= thresholdUsed_PO, .N, by = "ID"]
             Demand_port[, names(portfolio$DT)[x] := 1]

             Demand_port[portfolioDT, on = "ID"][, mget(c("ID", names(portfolio$DT)[x]))]
           })
  )

  portHelp[is.na(portHelp)] <- 0
  portHelp[, Portfolio := (rowSums(portHelp[, !"ID"]) != 0) * 1]

  segData <- dataUSED()[, c(1, (sum(defIN()$nlev) + length(defIN()$nlev) + 2):ncol(dataUSED())),
                        with = FALSE]

  portDTseg <- portHelp[segData, on = "ID"]
  portDTseg[, c(names(segData)[-1]) := lapply(seq_along(.SD),
                                              function(x) {
                                                factor(.SD[[x]],
                                                       levels = seq_along(segLev()[[x]]),
                                                       labels = segLev()[[x]])
                                              }),
            .SDcols = names(segData)[-1]]

  # test1 <- "Prod1"
  # portDTseg[get(test1) == 1, .N / nrow(portDTseg[get(test1) == 1]), by = "zzAge"]

  portfolioProfile <- lapply(names(portfolio$DT),
                             function(i) {
                               rbindlist(lapply(seq_along(segLev()),
                                                function(x) {
                                                  selHelp <- names(segData)[x + 1]
                                                  out <- portDTseg[get(i) == 1, .N, by = selHelp]
                                                  out[, N := N / sum(N)]
                                                  out[, Segment := names(segLev())[x]]
                                                  names(out) <- c("Group", i, "Segment")
                                                  out[, mget(c("Segment", "Group", i))]
                                                }))
                             })

  portfolioProfileDT <- data.table(Segment = rep(names(segLev()), sapply(segLev(), length)),
                                   Group = unlist(segLev()))

  portfolioProfile[[1]][portfolioProfile[[2]], on = c("Segment", "Group")]

  portfolioProfile_Help <- Reduce(function(...) {
    merge(..., by = c("Segment", "Group"), all = TRUE)
  },
  portfolioProfile)


  portfolioProfileDT <- portfolioProfile_Help[portfolioProfileDT, on = c("Segment", "Group")]


  portfolioOverview <- list()

  portfolioOverview$Demand <- round(colSums(portHelp[, !"ID"]) /
                                      nrow(portHelp), 3) * 100

  portfolioOverview$crosstab <- cor(portHelp[, !c("ID", "Portfolio")])

  portfolioOverview$profile <- portfolioProfileDT

  portfolioOverview
})


## OUTPUTS !------------------------------------------------------------------------------------------------------------
output$grossDemandBox <- renderValueBox({

  validate(
    need(portfolioOverview(), "portfolio info is needed")
  )

  valueBox(value = paste0(format(portfolioOverview()$Demand['Portfolio'],
                                 nsmall = 1), "%"),
           subtitle = "Demand for portfolio",
           color = "red",
           icon = icon("shopping-basket"))
})

output$portfolioSingleDemands <- DT::renderDataTable({

  validate(
    need(portfolioOverview(), "portfolio info is needed")
  )

  singleDemand <- data.table(Product = names(portfolio$DT),
                             Demand = portfolioOverview()$Demand[-length(portfolioOverview()$Demand)])

  DT::datatable(singleDemand, selection = list(mode = 'single', target = 'column'),
                escape = FALSE, rownames = FALSE,
                style = "default", class = 'compact',
                options = list(pageLength = nrow(singleDemand),
                               ordering = FALSE,
                               dom = 't',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                 "}"))) %>%
    formatStyle(columns = 1, backgroundColor = "lightgrey", fontWeight = "bold") %>%
    DT::formatStyle(columns = "Demand",
                    background = DT::styleColorBar(c(0, 100),
                                                   '#E10000', angle = 270),
                    backgroundSize = '90% 50%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
})

output$portfolioCrosstab <- DT::renderDataTable({

  validate(
    need(portfolioOverview(), "portfolio info is needed")
  )

  crosstabDT <- data.table(portfolioOverview()$crosstab,
                           Product = names(portfolio$DT))
  names(crosstabDT) <- c(names(portfolio$DT), "Product")

  brks <- quantile(portfolioOverview()$crosstab, probs = seq(.01, .99, .01),
                   na.rm = TRUE)
  clrs <- colorRampPalette(c("steelblue", "white", "red"))(length(brks) + 1)[sequence(length(brks) + 1)]
  clrsText <- colorRampPalette(c("white", "black", "grey"))(length(brks) + 1)[sequence(length(brks) + 1)]

  DT::datatable(crosstabDT, selection = list(mode = 'none'),
                escape = FALSE, rownames = FALSE,
                style = "default", class = 'compact',
                options = list(pageLength = nrow(crosstabDT),
                               ordering = FALSE,
                               dom = 't',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                 "}"))) %>%
    formatStyle(names(portfolio$DT),
                'font-size' = '10px') %>%
    formatStyle("Product",
                backgroundColor = "lightgrey",
                fontWeight = "bold") %>%
    formatStyle(names(portfolio$DT),
                backgroundColor = styleInterval(brks, clrs),
                color = styleInterval(brks, clrsText)) %>%
    formatRound(names(portfolio$DT),  digits = 2)
  })

output$showPortfolioDT <- DT::renderDataTable({
  summaryDT <- melt(data.table(Att = names(defIN()$attLev),
                               portfolio$portDT), id.vars = "Att")

  names(summaryDT) <- c("Attribute", "Product", "Level")

  DT::datatable(summaryDT, selection = list(mode = 'none'),
                escape = FALSE, rownames = FALSE,
                style = "default", class = 'compact',
                options = list(pageLength = length(defIN()$nlev),
                               ordering = FALSE,
                               dom = 'tp',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                 "}"),
                               lengthMenu = list(c(length(defIN()$nlev), -1),
                                                 c('1 product', 'All'))))
})


output$portfolioProfileDT <- DT::renderDataTable({
  colHelp <- sample(c("#0060FF", "#00B6FF", "#00E5BA", "#9EE900", "#00B600",
                      "#802AB7", "#C700D3", "#EB0064", "#FF5000", "#FEDB00"))

  DT::datatable(portfolioOverview()$profile, selection = list(mode = 'single', target = 'row'),
                filter = "none", autoHideNavigation = TRUE, rownames = FALSE,
                extensions = 'Buttons', escape = FALSE, style = "default", class = 'compact',
                options = list(pageLength = 23,
                               dom = 'lrtpB',
                               buttons = c('csv', 'excel'),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                               'color': '#fff'});",
                                 "}"),
                               lengthMenu = list(c(23, -1),
                                                 c('Part', 'All')))) %>%
    DT::formatPercentage(columns = names(portfolioOverview()$profile)[-(1:2)], digits = 1) %>%
    DT::formatStyle(columns = names(portfolioOverview()$profile)[-(1:2)],
                    background = DT::styleColorBar(c(0, 1),
                                                   '#0060FF', angle = 270),
                    backgroundSize = '98% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
    DT::formatStyle(columns = "Segment",
                    target = 'row',
                    backgroundColor =
                      DT::styleEqual(unique(portfolioOverview()$profile$Segment),
                                     grDevices::colorRampPalette(c("white",
                                                                   "darkgrey"))(length(
                                                                     unique(portfolioOverview()$profile$Segment)))))
  })

output$testPortfolio <- renderPrint({

  list(input$portfolioDT_columns_selected,
       names(portfolio$DT)[input$portfolioDT_columns_selected],
       portfolio$DT,
       portfolio$portDT,
       melt(data.table(Att = names(defIN()$attLev),
                       portfolio$portDT), id.vars = "Att"))

})
