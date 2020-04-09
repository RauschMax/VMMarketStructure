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
  portfolio$DT <- data.table(Prod1 = rep(1, length(defIN()$nlev)),
                             Prod2 = rep(1, length(defIN()$nlev)),
                             Prod3 = rep(1, length(defIN()$nlev)),
                             Prod4 = rep(1, length(defIN()$nlev)))
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
             })

observeEvent(input$removeProd,
             {
               if (input$portfolioDT_columns_selected > 0) {
                 showNotification("Please select a column from the table.",
                                  duration = 1, type = "message")
               } else {
                 portfolio$DTold <- portfolio$DT
                 varSel <- names(portfolio$DT)[input$portfolioDT_columns_selected]
                 portfolio$DT <- portfolio$DT[, c(varSel) := NULL]
               }
             })

observeEvent(input$changeProd,
             {
               if (input$portfolioDT_columns_selected > 0) {
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
               }
             })

output$portfolioDT <- DT::renderDataTable({

  validate(
    need(portfolio$DT, "Wait a second!")
  )

  input$addProd
  input$changeProd
  input$removeProd

  portDT <- copy(portfolio$DT)
  portDT <- portDT[, c(names(portDT)) := lapply(.SD,
                                                function(y) {
                                                  sapply(seq_along(y),
                                                         function(x) {
                                                           defIN()$attLev[[x]][y[x]]
                                                         })}),
                   .SDcols = names(portDT)]

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
                                 "}")))
  })

output$testPortfolio <- renderPrint({

  input$portfolioDT_columns_selected

})
