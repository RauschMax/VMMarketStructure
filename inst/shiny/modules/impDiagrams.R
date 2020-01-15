# SANKEY DIAGRAMM

diagramData <- reactive({

  # sankey network
  orderAtt <-  order(Importance()$Importance,
                     decreasing = TRUE)

  DThelp <- SKU_choice_DT()[, c(1, grep("^Att", names(SKU_choice_DT()))[orderAtt]), with = FALSE]

  DThelp[, (names(DThelp)) := lapply(.SD,
                                     function(x) {
                                       out <- x
                                       out[out == 0] <- NA
                                       out
                                     })]

  helpList <- lapply(1:(length(defIN()$nlev) - 1),
                     function(x) {

                       LoopDT <- DThelp[, c(1, x + 1, x + 2), with = FALSE]
                       names(LoopDT) <- c("ID", "L1", "L2")

                       LoopDT <- unique(LoopDT)

                       LoopDT[, T1 := L1 + x * 100]
                       LoopDT[, T2 := L2 + (x + 1) * 100]

                       LoopHelp <- LoopDT[, c(total = .(.N)),
                                          by = .(Var1 = pmin(T1, T2),
                                                 Var2 = pmax(T1, T2))][order(Var1, Var2)]

                       LoopHelp <- LoopHelp[!is.na(Var1)]
                       LoopHelp[, total := total / sum(total) * Importance()$Importance[orderAtt[x]] * 10000]

                     })

  lookup <- data.table(Var1 = rep(seq_along(defIN()$nlev[orderAtt]),
                                  defIN()$nlev[orderAtt]) * 100 + sequence(defIN()$nlev[orderAtt]),
                       Var2 = rep(seq_along(defIN()$nlev[orderAtt]),
                                  defIN()$nlev[orderAtt]) * 100 + sequence(defIN()$nlev[orderAtt]),
                       code = sequence(sum(defIN()$nlev[orderAtt])) - 1)

  helpLinks <- Reduce(rbind, helpList)

  helpLinks <- helpLinks[lookup[, .(Var1, code)], on = "Var1", nomatch = 0]
  helpLinks <- helpLinks[lookup[, .(Var2, code)], on = "Var2", nomatch = 0]


  sankeyLinks <- helpLinks[, .(code, i.code, total)][order(code, i.code)]
  names(sankeyLinks) <- c("source", "target", "value")

  sankeyNodes <- data.frame(name = unlist(defIN()$attLev[orderAtt]))


  # treemap data
  group <- rep(names(defIN()$attLev[order(-Importance()$Importance)]),
               defIN()$nlev[order(-Importance()$Importance)])

  # subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep = "-")
  subgroup <- as.vector(unlist(defIN()$attLev[order(-Importance()$Importance)]))

  # value <- c(13,5,22,12,11,7,3,1,23)
  value <- unlist(Importance()$LevelCounts_rel[order(-Importance()$Importance)])

  treedata <- data.frame(group, subgroup, value)

  list(links = sankeyLinks,
       nodes = sankeyNodes,
       treedata = treedata)

})

output$Sankey <- renderSankeyNetwork({

  sankeyNetwork(Links = diagramData()$links,
                Nodes = diagramData()$nodes,
                Source = "source",
                Target = "target",
                Value = "value",
                NodeID = "name",
                fontSize = 12,
                nodeWidth = 15)

})


# Treemap
output$treemap <- renderPlot({

    treemap(diagramData()$treedata,
          index = c("group", "subgroup"),
          vSize = "value",
          type = "index"
          )

})