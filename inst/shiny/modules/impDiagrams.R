# SANKEY DIAGRAMM

diagramData <- reactive({

  # sankey network
  orderAtt <-  order(Importance()$Importance,
                     decreasing = TRUE)

  # DThelp <- data.table(sapply(seq_along(defIN()$attLev),
  #                             function(x) {
  #                               sapply(unlist(dataIN()$SKU_choice_DT[, paste0("Att", orderAtt[x]),
  #                                                                    with = FALSE]),
  #                                      function(y) {
  #                                        paste0(sprintf("%02d", x), sprintf("%02d", y), "_",
  #                                               defIN()$attLev[[orderAtt[x]]][y])
  #                                      })
  #                             }))

  DThelp <- SKU_choice_DT[, grep("^Att", names(dataIN()$SKU_choice_DT))[orderAtt], with = FALSE]

  # helpList <- lapply(1:(length(defIN()$nlev) - 1),
  #                    function(x) {
  #
  #                      LoopDT <- DThelp[, c(x, x + 1), with = FALSE]
  #                      names(LoopDT) <- c("L1", "L2")
  #
  #                      LoopDT[, c(total = .(.N)),
  #                             by = .(Var1 = pmin(L1, L2),
  #                                    Var2 = pmax(L1, L2))][order(Var1, Var2)]
  #                    })

  helpList <- lapply(1:(length(nlev) - 1),
                     function(x) {

                       LoopDT <- DThelp[, c(x, x + 1), with = FALSE]
                       names(LoopDT) <- c("L1", "L2")

                       LoopDT[, T1 := L1 + x * 100]
                       LoopDT[, T2 := L2 + (x + 1) * 100]

                       LoopDT[, c(total = .(.N)),
                              by = .(Var1 = pmin(T1, T2),
                                     Var2 = pmax(T1, T2))][order(Var1, Var2)]

                     })

  # lookup <- data.table(Var1 = unlist(sapply(seq_along(defIN()$attLev),
  #                                           function(x) {
  #                                             sapply(seq_along(defIN()$attLev[[orderAtt[x]]]),
  #                                                    function(y) {
  #                                                      paste0(sprintf("%02d", x), sprintf("%02d", y), "_",
  #                                                             defIN()$attLev[[orderAtt[x]]][y])
  #                                                    })
  #                                           })),
  #                      Var2 = unlist(sapply(seq_along(defIN()$attLev),
  #                                           function(x) {
  #                                             sapply(seq_along(defIN()$attLev[[orderAtt[x]]]),
  #                                                    function(y) {
  #                                                      paste0(sprintf("%02d", x), sprintf("%02d", y), "_",
  #                                                             defIN()$attLev[[orderAtt[x]]][y])
  #                                                    })
  #                                           })),
  #                      code = sequence(sum(defIN()$nlev)) - 1)

  lookup <- data.table(Var1 = rep(seq_along(defIN()$nlev[orderAtt]),
                                  nlev[orderAtt]) * 100 + sequence(defIN()$nlev[orderAtt]),
                       Var2 = rep(seq_along(defIN()$nlev[orderAtt]),
                                  nlev[orderAtt]) * 100 + sequence(defIN()$nlev[orderAtt]),
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