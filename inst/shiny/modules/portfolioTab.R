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



observeEvent(input$addProd,
             {
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

observeEvent(input$removeProd,
             {
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

observeEvent(input$changeProd,
             {
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
             Demand_port[, paste0("Prod", x) := 1]

             Demand_port[portfolioDT, on = "ID"][, mget(c("ID", paste0("Prod", x)))]
           })
  )

  portHelp[is.na(portHelp)] <- 0
  portHelp[, Portfolio := (rowSums(portHelp[, !"ID"]) != 0) * 1]

  portfolioOverview <- list()

  portfolioOverview$Demand <- round(colSums(portHelp[, !"ID"]) /
                                      nrow(portHelp), 3) * 100

  portfolioOverview$crosstab <- cor(portHelp[, !c("ID", "Portfolio")])

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

  DT::datatable(crosstabDT, selection = list(mode = 'single', target = 'column'),
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
                'font-size' = '8px') %>%
    formatStyle("Product",
                backgroundColor = "lightgrey",
                fontWeight = "bold") %>%
    formatStyle(names(portfolio$DT),
                backgroundColor = styleInterval(brks, clrs),
                color = "grey") %>%
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

output$testPortfolio <- renderPrint({

  list(input$portfolioDT_columns_selected,
       names(portfolio$DT)[input$portfolioDT_columns_selected],
       portfolio$DT,
       portfolio$portDT,
       melt(data.table(Att = names(defIN()$attLev),
                       portfolio$portDT), id.vars = "Att"))

})
