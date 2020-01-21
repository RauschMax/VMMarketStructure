segSankeyData <- reactive({

  validate(
    need(lc_segs(), "Please load the data.")
  )

  ## Sankey - LC Runs
  Help_LC_Sankey_1 <- copy(lc_segs())[, mget(paste0("LC" , 2:10))]

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
