segSankeyData <- reactive({

  validate(
    need(lc_segs(), "Please load the data.")
  )

  ## Sankey - LC Runs
  Help_LC_Sankey_1 <- copy(lc_segs())[, mget(paste0("LC", 2:10))]

  for (i in 3:(ncol(Help_LC_Sankey_1) + 1)) {
    sel1 <- paste0("LC", i)
    sel2 <- paste0("LC", i - 1)
    Help_LC_Sankey_1[, c(sel1) := get(sel1) + max(get(sel2))]
  }

  Help_LC_Sankey_1 <- Help_LC_Sankey_1 - 1

  sankeyLinks_LC <- rbindlist(
    lapply(1:(ncol(Help_LC_Sankey_1) - 1),
           function(x) {

             LoopDT <- Help_LC_Sankey_1[, c(x, x + 1), with = FALSE]
             names(LoopDT) <- c("L1", "L2")

             LoopDT[, T1 := L1]
             LoopDT[, T2 := L2]

             LoopDT[, c(total = .(.N)),
                    by = .(Var1 = pmin(T1, T2),
                           Var2 = pmax(T1, T2))][order(Var1, Var2)]

           })
  )
  names(sankeyLinks_LC) <- c("source", "target", "value")

  sankeyNodes_LC <- data.frame(name = paste0("LC_", rep((1:ncol(Help_LC_Sankey_1)) + 1,
                                                        (1:ncol(Help_LC_Sankey_1)) + 1),
                                             "_", sequence(1:ncol(Help_LC_Sankey_1)) + 1))

  list(links = sankeyLinks_LC,
       nodes = sankeyNodes_LC)

})

output$SankeyLC <- renderSankeyNetwork({

  sankeyNetwork(Links = segSankeyData()$links,
                Nodes = segSankeyData()$nodes,
                Source = "source",
                Target = "target",
                Value = "value",
                NodeID = "name",
                fontSize = 12,
                nodeWidth = 10,
                nodePadding = 10,
                sinksRight = TRUE) %>%
    htmlwidgets::onRender('function(el) { el.getElementsByTagName("svg")[0].removeAttribute("viewBox") }')


})

output$segTable <- DT::renderDataTable({

  validate(
    need(lc_segs(), "Please load the data.")
  )

  DT::datatable(lc_segs(), selection = list(mode = 'single', target = 'column'),
                filter = "none", autoHideNavigation = TRUE, rownames = TRUE,
                escape = FALSE, style = "default", class = 'compact',
                options = list(pageLength = 25,
                               dom = 'lrtip',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                 "}"),
                               lengthMenu = list(c(5, 25, -1),
                                                 c('5', '25', 'All'))))
})

output$segSelectUI <- shiny::renderUI({
  choList <- as.list(seq_along(names(lc_segs())[-1]) + 1)
  names(choList) <- paste(names(lc_segs())[-1], "-", choList, "group solution")

  div(h4("Select a segment solution"),
      shiny::selectInput("segSelect", label = NULL,
                         choices = choList, selected = 4))

})


output$profileFactors <- renderPlot({
  if (is.null(input$segSelect)) {
    LCselected <- paste0("LC", 4)
  } else {
    LCselected <- paste0("LC", input$segSelect)
  }

  segsInclLC <- segIN()$segFactor[lc_segs(), on = "ID"]

  barplotList <- lapply(names(segIN()$segFactor)[-1],
                        function(x) {
                          plotData <- segsInclLC[, mget(c(x, LCselected))]
                          plotData <- plotData[, .N, by = mget(c(x, LCselected))][order(get(names(plotData)[1]),
                                                                                        get(LCselected))]

                          plotData[, c(LCselected) := factor(get(LCselected))]

                          plotData[, freq := N / sum(N) * 100, by = LCselected]

                          names(plotData) <- c("group", "LC", "count", "freq")

                          ggplot(plotData,
                                 aes(fill = LC, y = freq, x = group)) +
                            geom_bar(position = "dodge", stat = "identity") +
                            theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
                                  axis.title.x = element_text(size = 10)) +
                            xlab(x) + ylab(NULL)

                          # barchart(freq ~ group, data = plotData, groups = LC,
                          #          scales = list(x = list(rot = 40, cex = 0.6)))
                        })


  do.call(gridExtra::grid.arrange,
          c(barplotList, ncol = min(3, ceiling((length(names(segIN()$segFactor)) - 1) / 2))))


  })


# output$profileNumeric <- renderPlot({
#   if (is.null(input$segSelect)) {
#     LCselected <- paste0("LC", 4)
#   } else {
#     LCselected <- paste0("LC", input$segSelect)
#   }
#
#   segsInclLCNum <- segIN()$segNumeric[lc_segs(), on = "ID"]
#   plotData2 <- segsInclLCNum[, mget(c(names(segIN()$segNumeric)[-1], LCselected))]
#
#   plotData2[, c(LCselected) := factor(get(LCselected))]
#
#   plot_multi_histogram <- function(df, feature, label_column) {
#     plt <- ggplot(df, aes(x = eval(parse(text = feature)),
#                           fill = eval(parse(text = label_column)))) +
#       geom_histogram(alpha = 0.7, position = "identity",
#                      aes(y = ..density..), color = "black",
#                      bins = 15) +
#       geom_density(alpha = 0.7) +
#       geom_vline(aes(xintercept = mean(eval(parse(text = feature)))),
#                  color = "black", linetype = "dashed", size = 1) +
#       labs(x = feature, y = "Density")
#     plt + guides(fill = guide_legend(title = label_column))
#   }
#
#   histList <- lapply(names(plotData2)[1],
#                      function(x) {
#                        plot_multi_histogram(plotData2, x, LCselected)
#                      })
#
#   do.call(gridExtra::grid.arrange,
#           c(histList, ncol = min(3, ceiling((length(names(segIN()$segNumeric)) - 1) / 2))))
#
# })


output$profileChoices <- renderPlot({
  if (is.null(input$segSelect)) {
    LCselected <- paste0("LC", 4)
  } else {
    LCselected <- paste0("LC", input$segSelect)
  }

  dataLC <- dataIN()[lc_segs(), on = "ID"]

  barplotListCho <- lapply(seq_along(defIN()$attLev),
                           function(x) {
                             plotDataCho <- dataLC[, mget(c(paste0("A", x, "_",
                                                                   sequence(defIN()$nlev[x])), LCselected))]

                             plotDataCho <- plotDataCho[, colSums(.SD),
                                                        .SDcols = paste0("A", x, "_",
                                                                         sequence(defIN()$nlev[x])),
                                                        by = LCselected]


                             plotDataCho[, c(LCselected) := factor(get(LCselected))]
                             plotDataCho[, freq := V1 / sum(V1) * 100, by = LCselected]
                             plotDataCho[, group := defIN()$attLev[[x]]]

                             names(plotDataCho) <- c("LC", "count", "freq", "group")

                             ggplot(plotDataCho,
                                    aes(fill = LC, y = freq, x = group)) +
                               geom_bar(position = "dodge", stat = "identity") +
                               theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
                                     axis.title.x = element_text(size = 10)) +
                               xlab(names(defIN()$nlev)[x]) + ylab(NULL)

                             # barchart(freq ~ group, data = plotData, groups = LC,
                             #          scales = list(x = list(rot = 40, cex = 0.6)))
                           })


  do.call(gridExtra::grid.arrange, c(barplotListCho, nrow = 2))

})


output$profileSegDT <- DT::renderDataTable({

  validate(
    need(lc_segs(), "Please load the data.")
  )

  if (is.null(input$segSelect)) {
    LCselected <- paste0("LC", 4)
    LCnames <- paste0("LC", 1:4)
  } else {
    LCselected <- paste0("LC", input$segSelect)
    LCnames <- paste0("LC", sequence(as.numeric(input$segSelect)))
  }

  segsInclLC <- segIN()$segFactor[lc_segs(), on = "ID"]

  segProfileDT <- rbindlist(
    lapply(seq_along(segDefIN()$segLevFact),
           function(x) {
             selCol <- names(segIN()$segFactor)[x + 1]
             name <- names(segDefIN()$segLevFact)[x]
             out <- dcast(segsInclLC, get(selCol) ~ get(LCselected), value.var = 'ID', length)
             out[, Seg := name]
             names(out) <- c("Segment", LCnames, "Seg")

             out[, mget(c("Seg", "Segment", LCnames))]
           }))

  brks <- quantile(segProfileDT[, mget(LCnames)],
                   probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}

  DT::datatable(segProfileDT, selection = list(mode = 'single', target = 'column'),
                filter = "none", autoHideNavigation = TRUE, rownames = TRUE,
                escape = FALSE, style = "default", class = 'compact',
                options = list(pageLength = 25,
                               dom = 'lrtip',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                 "}"),
                               lengthMenu = list(c(5, 25, -1),
                                                 c('5', '25', 'All')))) %>%
    formatStyle(LCnames, backgroundColor = styleInterval(brks, clrs))
})


output$profileChoDT <- DT::renderDataTable({

  validate(
    need(lc_segs(), "Please load the data.")
  )

  if (is.null(input$segSelect)) {
    LCselected <- paste0("LC", 4)
    LCnames <- paste0("LC", 1:4)
  } else {
    LCselected <- paste0("LC", input$segSelect)
    LCnames <- paste0("LC", sequence(as.numeric(input$segSelect)))
  }

  dataLC <- dataIN()[lc_segs(), on = "ID"]

  choProfileDT <- rbindlist(
    lapply(seq_along(defIN()$attLev),
           function(x) {

             attName <- names(defIN()$attLev)[x]
             out1 <- rbindlist(
               lapply(seq_along(defIN()$attLev[[x]]),
                      function(y) {
                        selCol <- paste0("A", x, "_", y)
                        levName <- defIN()$attLev[[x]][y]
                        out2 <- dcast(dataLC, get(selCol) ~ get(LCselected),
                                      value.var = 'ID', length)[2, ]
                        setnames(out2, "selCol", "level")
                        out2[, level := levName]
                        out2
                      }))

             names(out1) <- c("Level", LCnames)

             out1[, Attribute := attName]

             out1[, mget(c("Attribute", "Level", LCnames))]

           })
  )

  brks <- quantile(choProfileDT[, mget(LCnames)],
                   probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}

  DT::datatable(choProfileDT, selection = list(mode = 'single', target = 'column'),
                filter = "none", autoHideNavigation = TRUE, rownames = TRUE,
                escape = FALSE, style = "default", class = 'compact',
                options = list(pageLength = 25,
                               dom = 'lrtip',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                 "}"),
                               lengthMenu = list(c(5, 25, -1),
                                                 c('5', '25', 'All')))) %>%
    formatStyle(LCnames, backgroundColor = styleInterval(brks, clrs))
})


output$testSegTab <- renderPrint({

  input$segSelect
  LCselected <- paste0("LC", input$segSelect)

  segIN()$segFactor[lc_segs()[, get(LCselected)] == 1,
                    mget(names(segIN()$segFactor)[-1])]

})

output$showLCselected <- renderText({
  if (is.null(input$segSelect)) {
    out <- paste0(4, "segment solution selected")
  } else {
    out <- paste0(input$segSelect, "segment solution selected")
  }
  out
})
