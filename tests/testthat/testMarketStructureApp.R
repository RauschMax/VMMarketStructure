rm(list = ls())
library(shiny)
library(shinydashboard)
library(kantardashboard)
library(KTShiny)
library(data.table)
library(DT)
library(formattable)
library(networkD3)
library(treemap)

# READ DATA !---------------------------------------------------------------------------------------------------------
input <- list(study = "316401010")

# read Password !----
get_pw <- BeastRServer::azure_blob_call("GET",
                                        storage_account = "shinyapp",
                                        storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                             "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                             "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                        container = paste0("ms", input$study),
                                        blob = paste0(input$study, "_pw.txt"))

pw <- httr::content(get_pw, as = "text", encoding = "UTF-8")

pw


# labels
get_def <- BeastRServer::azure_blob_call("GET",
                                         storage_account = "shinyapp",
                                         storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                              "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                              "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                         container = paste0("ms", input$study),
                                         blob = "attributes.def")

def <- strsplit(httr::content(get_def, as = "text", encoding = "UTF-8"), "\r\n")[[1]]
def

# choices, combinations and ranks
#
system.time({
  get_data <- BeastRServer::azure_blob_call("GET",
                                            storage_account = "shinyapp",
                                            storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                 "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                 "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                            container = paste0("ms", input$study),
                                            blob = "data.csv")

  data <- as.data.table(httr::content(get_data, type = "text/csv", encoding = "UTF-8"))
})

data

system.time({
  get_comb <- BeastRServer::azure_blob_call("GET",
                                            storage_account = "shinyapp",
                                            storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                 "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                 "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                            container = paste0("ms", input$study),
                                            blob = "comb.csv")

  comb <- as.data.table(httr::content(get_comb, type = "text/csv", encoding = "UTF-8"))
})

comb
# DATA READ COMPLETED !-----------------------------------------------------------------------------------------------



# EXCTRACT Definition DATA !------------------------------------------------------------------------------------------

attIndex <- grep("^[^ ]", def)

# indecies of levels
levIndex <- apply(cbind(attIndex + 1,
                        c(attIndex[-1] - 1, length(def))), 1, function(x) {seq(x[1], x[2])})


# named list of levels
attLev <- lapply(levIndex,
                 function(x) {
                   gsub("^[ ]", "", def[x])
                 })
names(attLev) <- def[attIndex]

# number of levels
nlev <- sapply(attLev, length)


# Definition data extracted !-----------------------------------------------------------------------------------------


# EXCTRACT DATA !-----------------------------------------------------------------------------------------------------

# choice data
SKU_choice_DT <- comb

Data <- data


# calculate importances and counts
Importance <- sapply(Data[, grep("^Rank",
                                        names(Data)),
                                 with = FALSE],
                            function(x) {
                              sum(x %in% 1:2, na.rm = TRUE)
                            })
Importance <- Importance / sum(Importance)
names(Importance) <- names(attLev)
Importance

# Alternative Importance calculation !----------------------------------------------------------------------------------

Data_Weighted <- copy(Data)
for (i in 1:length(nlev)) {
  sel1 <- paste0("A", i, "_", seq_along(grep(paste0("A", i, "_"), names(Data_Weighted))))
  sel2 <- paste0("RankAtt", i)
  # print(sel1)
  Data_Weighted[, (sel1) := lapply(.SD,
                                   function(x) {
                                     x * (length(nlev) -
                                            ifelse(is.na(Data_Weighted[[sel2]]),
                                                   0, Data_Weighted[[sel2]]) + 1)
                                   }), .SDcols = sel1]
}
Data_Weighted

DT_melted <- melt(Data_Weighted[, mget(c("ID", names(Data_Weighted)[grep("^A", names(Data_Weighted))]))],
                  id.vars = "ID")


DT_importance <- DT_melted[, sum(value), by = variable]
# raw imp based on choices weighted by rank
DT_importance <- DT_importance[, grp := rep(seq_along(nlev), nlev)][, sum(V1), by = grp]
DT_importance[, imp := V1 / sum(V1)]
# original Importance based on Top2 ranks
DT_importance[, impOrig := Importance]
# Imp corrected for number of levels
DT_importance[, V2 := V1 / nlev]
DT_importance[, imp2 := V2 / sum(V2)]
DT_importance

Importance_OLD <- Importance

Importance <- DT_importance[, imp2]

# Level counts - 100% within attributes
LevelCounts_100 <- lapply(paste0("^A", 1:length(nlev), "_"),
                          function(y) {
                            out <- apply(Data[, grep(y, x = names(Data)),
                                                       with = FALSE], 2, sum) /
                              sum(apply(Data[, grep(y, x = names(Data)),
                                                      with = FALSE], 2, sum))

                            names(out) <- attLev[[y]]
                            out
                          })
names(LevelCounts_100) <- names(attLev)
LevelCounts_100

# Level counts - summing to attribute importance within attributes
LevelCounts_rel <- lapply(1:length(nlev),
                          function(y) {
                            out <- apply(Data[, grep(paste0("^A", y, "_"),
                                                              x = names(Data)),
                                                       with = FALSE], 2, sum) /
                              sum(apply(Data[, grep(paste0("^A", y, "_"),
                                                             x = names(Data)),
                                                      with = FALSE], 2, sum)) * Importance[y]

                            names(out) <- attLev[[y]]
                            out
                          })

names(LevelCounts_rel) <- names(attLev)
LevelCounts_rel
# DATA extracted !------------------------------------------------------------------------------------------------------


# Decision Matrix Table !-----------------------------------------------------------------------------------------------

lapply(LevelCounts_100, round, digits = 3)

## Sort attributes based on proximity !---------------------------------------------------------------------------------
orderAtt <-  order(Importance,
                   decreasing = TRUE)

attLev_ordered <- attLev[orderAtt]
nlev_ordered <- nlev[orderAtt]

LevCountOrder <- LevelCounts_100[orderAtt]
names(LevCountOrder[[1]]) <- attLev[orderAtt][[1]]

optOrder2 <- vector("list", length(nlev_ordered))
optOrder2[[1]] <- seq_along(attLev_ordered[[1]])

system.time({
  for (i in 2:length(nlev_ordered)) {

    AttSel0 <- paste0("Att", orderAtt[i - 1])
    AttSel1 <- paste0("Att", orderAtt[i])

    DTdistHelp1 <- SKU_choice_DT[, lapply(.SD, tabulate,
                                          nbins = nlev_ordered[i]),
                                 by = get(AttSel0),
                                 .SDcols = AttSel1]

    DTdistHelp1[, grp := rep(1:nlev_ordered[i],
                             nlev_ordered[i - 1])]

    DTdistHelp <- scale(dcast(DTdistHelp1, get ~ grp,
                              value.var = AttSel1,
                              fun = sum)[, get := NULL])[optOrder2[[i - 1]], ]


    # correct for attributes without variance (no chosen at all)
    attributes(DTdistHelp)$`scaled:center` == 0
    DTdistHelp[, which(attributes(DTdistHelp)$`scaled:center` == 0)] <- 0

    increment_low <- seq(-1, 0, length = nlev_ordered[i - 1])
    increment_high <- seq(0, 1, length = nlev_ordered[i - 1])

    distHelp <- rbindlist(
      lapply(sequence(nlev_ordered[i - 1]),
             function(j) {
               data.frame(matrix(seq(increment_low[j],
                                     increment_high[j],
                                     length = nlev_ordered[i]),
                                 nrow = 1))
             }))

    helpSample <- rbindlist(
      lapply(1:10000,
             function(x) {
               set.seed(x)
               data.table(
                 matrix(sample(1:nlev_ordered[i],
                               size = nlev_ordered[i]), nrow = 1))
             })
    )

    helpSample[, i := .I]
    helpSample[, criteria := mean(sapply(DTdistHelp[, unlist(.SD)] - distHelp,
                                         mean)),
               by = i, .SDcols = paste0("V", 1:nlev_ordered[i])]
    helpSample <- helpSample[order(criteria)]

    orderInd <- unlist(helpSample[1, as.numeric(1:nlev_ordered[i]),
                                  with = FALSE])

    optOrder2[[i]] <- orderInd

    LevCountOrder[[i]] <- LevCountOrder[[i]][orderInd]
    names(LevCountOrder[[i]]) <- attLev[orderAtt][[i]][orderInd]
  }
})
# User      System verstrichen
# 151.40        1.61      156.94

pathOUT <- "J:/Proj/CAPR/AKT/316401010_Pilotstudie_MarketStructure"
jsonlite::write_json(optOrder2, paste0(pathOUT, "/appData/order.json"))

optOrder2

LevCountOrder




# END Decision Matrix Table !-------------------------------------------------------------------------------------------


# Alternative SKU_choice_DT !-------------------------------------------------------------------------------------------
# NA --> 0 instead of expand.grid(x)
SKU_choice_2 <- lapply(Data$ID,
                       function(x) {
                         dataHelp <- Data[Data$ID == x, grep("^A", names(Data)), with = FALSE]

                         chosenLevels <- lapply(1:length(nlev),
                                                function(y) {
                                                  outHelp <- which(dataHelp[, grep(paste0("^A", y, "_"),
                                                                                   names(dataHelp)), with = FALSE] == 1)
                                                  if (length(outHelp) == 0) {
                                                    0
                                                  } else {
                                                    outHelp
                                                  }
                                                })

                         out <- data.frame(ID = x, expand.grid(chosenLevels))
                         names(out) <- c("ID", paste0("Att", 1:length(nlev)))

                         out$Comb <- gsub("NA", "_", apply(out[, -1], 1, paste0, collapse = ""))

                         out

                       })

# str(SKU_choice)
SKU_choice_2[[1]]
unique(SKU_choice_2[[1]])
Data[1, ]

system.time({
  # SKU_choice_DT <- data.table(Reduce(rbind, SKU_choice))
  SKU_choice_DT_ALTERNATIVE <- rbindlist(SKU_choice_2)
})
SKU_choice_DT[order(ID)]
SKU_choice_DT_ALTERNATIVE[order(ID)]

SKU_choice_DT[, .N, by = ID]
SKU_choice_DT_ALTERNATIVE[, .N, by = ID]
table(SKU_choice_DT[, .N, by = ID]$N)
table(SKU_choice_DT_ALTERNATIVE[, .N, by = ID]$N)

# SANKEY DIAGRAMM - ALTERNATIVE CODING !--------------------------------------------------------------------------------

system.time({
  DThelp2 <- SKU_choice_DT_ALTERNATIVE[, grep("^Att", names(SKU_choice_DT_ALTERNATIVE))[orderAtt], with = FALSE]
})
# User      System verstrichen
# 0              0           0

system.time({
  helpList2 <- lapply(1:(length(nlev) - 1),
                      function(x) {

                        LoopDT <- DThelp2[, c(x, x + 1), with = FALSE]
                        names(LoopDT) <- c("L1", "L2")

                        LoopDT[, T1 := L1 + x * 100]
                        LoopDT[, T2 := L2 + (x + 1) * 100]

                        LoopDT[, c(total = .(.N)),
                               by = .(Var1 = pmin(T1, T2),
                                      Var2 = pmax(T1, T2))][order(Var1, Var2)]

                      })
})
# User      System verstrichen
# 0.04        0.00        0.03

lookup2 <- data.table(Var1 = rep(seq_along(nlev[orderAtt]), nlev[orderAtt]) * 100 + sequence(nlev[orderAtt]),
                      Var2 = rep(seq_along(nlev[orderAtt]), nlev[orderAtt]) * 100 + sequence(nlev[orderAtt]),
                      code = sequence(sum(nlev[orderAtt])) - 1)

system.time({
  helpLinks2 <- rbindlist(helpList2)
})
# User      System verstrichen
# 0              0           0

helpLinks2 <- helpLinks2[lookup2[, .(Var1, code)], on = "Var1", nomatch = 0]
helpLinks2 <- helpLinks2[lookup2[, .(Var2, code)], on = "Var2", nomatch = 0]

sankeyLinks2 <- helpLinks2[, .(code, i.code, total)][order(code, i.code)]
names(sankeyLinks2) <- c("source", "target", "value")

sankeyNodes2 <- data.frame(name = unlist(attLev[orderAtt]))

sankeyNetwork(Links = sankeyLinks2,
              Nodes = sankeyNodes2,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "name",
              fontSize = 12,
              nodeWidth = 15)
# !!! END !!! SANKEY DIAGRAMM - ALTERNATIVE CODING !--------------------------------------------------------------------


# SANKEY DIAGRAMM !-----------------------------------------------------------------------------------------------------
# orderAtt <-  order(Importance,
#                    decreasing = TRUE)

# system.time({
#   DThelp1 <- data.table(sapply(seq_along(attLev),
#                               function(x) {
#                                 sapply(unlist(SKU_choice_DT[, paste0("Att", orderAtt[x]),
#                                                             with = FALSE]),
#                                        function(y) {
#                                          paste0(sprintf("%02d", x), sprintf("%02d", y), "_",
#                                                 attLev[[orderAtt[x]]][y])
#                                        })
#                               }))
# })
# # User      System verstrichen
# # 404.90       17.81      442.47
# DThelp1
# prop.table(table(DThelp1[, V1]))

# LevelCounts_100[[1]]
#
# DThelp1[, .(V1, V2)]
# DThelp1[, .(.N), by = .(V1)][, N / sum(N)]
#
# table(SKU_choice_DT[, Att1], SKU_choice_DT[, A1_3])
#
# SKU_choice_DT[, Att1]
#
# colSums(Data[, grep("^A1_", x = names(Data)), with = FALSE]) /
#   sum(colSums(Data[, grep("^A1_", x = names(Data)), with = FALSE]))

system.time({
  DThelp2 <- SKU_choice_DT[, grep("^Att", names(SKU_choice_DT))[orderAtt], with = FALSE]
})
# User      System verstrichen
# 0.02        0.13        0.31

# system.time({
#   helpList1 <- lapply(1:(length(nlev) - 1),
#                       function(x) {
#
#                         LoopDT <- DThelp1[, c(x, x + 1), with = FALSE]
#                         names(LoopDT) <- c("L1", "L2")
#
#                         LoopDT[, c(total = .(.N)),
#                                by = .(Var1 = pmin(L1, L2),
#                                       Var2 = pmax(L1, L2))][order(Var1, Var2)]
#                       })
# })
# User       System verstrichen
# 72.06        1.41       74.94

system.time({
  helpList2 <- lapply(1:(length(nlev) - 1),
                     function(x) {

                       LoopDT <- DThelp2[, c(x, x + 1), with = FALSE]
                       names(LoopDT) <- c("L1", "L2")

                       LoopDT[, T1 := L1 + x * 100]
                       LoopDT[, T2 := L2 + (x + 1) * 100]

                       LoopDT[, c(total = .(.N)),
                              by = .(Var1 = pmin(T1, T2),
                                     Var2 = pmax(T1, T2))][order(Var1, Var2)]

                     })
})
# User      System verstrichen
# 2.75        0.70        3.68


# cbind(
#   helpList2[[1]],
#   helpList1[[1]]
# )
#
# lapply(seq_along(helpList2),
#        function(i) {
#          cbind(helpList2[[i]],
#                helpList1[[i]])
#        })
#
# lapply(seq_along(helpList2),
#        function(i) {
#          all(helpList2[[i]]$total == helpList1[[i]]$total)
#        })


# helpList1[[1]][, sum(total), by = .(Var1)][, .(Var1, V1, V1 / sum(V1))]
# helpList1[[1]][, sum(total), by = .(Var2)][, .(Var2, V1, V1 / sum(V1))]
#
# helpList1[[2]][, sum(total), by = .(Var1)]

# lookup1 <- data.table(Var1 = unlist(sapply(seq_along(attLev),
#                                           function(x) {
#                                             sapply(seq_along(attLev[[orderAtt[x]]]),
#                                                    function(y) {
#                                                      paste0(sprintf("%02d", x), sprintf("%02d", y), "_",
#                                                             attLev[[orderAtt[x]]][y])
#                                                    })
#                                           })),
#                      Var2 = unlist(sapply(seq_along(attLev),
#                                           function(x) {
#                                             sapply(seq_along(attLev[[orderAtt[x]]]),
#                                                    function(y) {
#                                                      paste0(sprintf("%02d", x), sprintf("%02d", y), "_",
#                                                             attLev[[orderAtt[x]]][y])
#                                                    })
#                                           })),
#                      code = sequence(sum(nlev)) - 1)

lookup2 <- data.table(Var1 = rep(seq_along(nlev[orderAtt]), nlev[orderAtt]) * 100 + sequence(nlev[orderAtt]),
                      Var2 = rep(seq_along(nlev[orderAtt]), nlev[orderAtt]) * 100 + sequence(nlev[orderAtt]),
                      code = sequence(sum(nlev[orderAtt])) - 1)

# lookup2 <- data.table(Var1 = rep(seq_along(nlev)[orderAtt], nlev[orderAtt]) * 100 + sequence(nlev[orderAtt]),
#                      Var2 = rep(seq_along(nlev)[orderAtt], nlev[orderAtt]) * 100 + sequence(nlev[orderAtt]),
#                      code = sequence(sum(nlev[orderAtt])) - 1)

# lookup2 <- data.table(Var1 = rep(seq_along(nlev), nlev) * 100 + sequence(nlev),
#                       Var2 = rep(seq_along(nlev), nlev) * 100 + sequence(nlev),
#                       code = sequence(sum(nlev)) - 1)

# lookup2 == lookup3
# identical(lookup2, lookup3)

# cbind(lookup1, lookup2)

# system.time({
#   helpLinks1 <- Reduce(rbind, helpList1)
# })
# User      System verstrichen
# 0.15        0.20        0.36

system.time({
  # helpLinks2 <- Reduce(rbind, helpList2)
  helpLinks2 <- rbindlist(helpList2)
})
# User      System verstrichen
# 0              0           0

# helpLinks1 <- helpLinks1[lookup1[, .(Var1, code)], on = "Var1", nomatch = 0]
# helpLinks1 <- helpLinks1[lookup1[, .(Var2, code)], on = "Var2", nomatch = 0]


# sankeyLinks1 <- helpLinks1[, .(code, i.code, total)][order(code, i.code)]
# names(sankeyLinks1) <- c("source", "target", "value")

# sankeyNodes1 <- data.frame(name = unlist(attLev[orderAtt]))

# sankeyNetwork(Links = sankeyLinks1,
#               Nodes = sankeyNodes1,
#               Source = "source",
#               Target = "target",
#               Value = "value",
#               NodeID = "name",
#               fontSize = 12,
#               nodeWidth = 15)


helpLinks2 <- helpLinks2[lookup2[, .(Var1, code)], on = "Var1", nomatch = 0]
helpLinks2 <- helpLinks2[lookup2[, .(Var2, code)], on = "Var2", nomatch = 0]

sankeyLinks2 <- helpLinks2[, .(code, i.code, total)][order(code, i.code)]
names(sankeyLinks2) <- c("source", "target", "value")

sankeyNodes2 <- data.frame(name = unlist(attLev[orderAtt]))

sankeyNetwork(Links = sankeyLinks2,
              Nodes = sankeyNodes2,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "name",
              fontSize = 12,
              nodeWidth = 15)



# treemap data
group <- rep(names(attLev[order(-Importance)]),
             nlev[order(-Importance)])

# subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep = "-")
subgroup <- as.vector(unlist(attLev[order(-Importance)]))

# value <- c(13,5,22,12,11,7,3,1,23)
value <- unlist(LevelCounts_rel[order(-Importance)])

treedata <- data.frame(group, subgroup, value)

treemap(treedata,
        index = c("group", "subgroup"),
        vSize = "value",
        type = "index"
)


# Substitution !--------------------------------------------------------------------------------------------------------

VarSelect <- paste0("Att", seq_along(nlev))

# combs <- SKU_choice_DT[, .(.N), by = .(Att1, Att2, Att3, Att4, Att5, Att6, Att7, Att8)][order(-N)]
combs <- SKU_choice_DT[, .(.N), by = VarSelect][order(-N)]
combs

combs[, Combination := do.call(paste0, .SD), .SDcols = VarSelect]
combs

attFact <- attLev
names(attFact) <- VarSelect

combs[, paste0("Fact", VarSelect) := .SD,
      .SDcols = VarSelect]
combs

system.time({
  for (j in seq_along(nlev)) {
    set(combs, i = NULL, j = paste0("Fact", VarSelect)[j],
        value = factor(combs[[j]], labels = attFact[[j]]))
  }
})
# User      System verstrichen
# 0.30        0.00        0.29
combs

combs_labels <- combs[, mget(c("Combination", paste0("Fact", VarSelect), "N"))]
combs_labels

# system.time({
#   helpTest <- sapply(seq_along(nlev),
#                      function(i) {
#                        helpDT <- combs[, mget(VarSelect)][, i, with = FALSE]
#                        FactLabels <- attFact[[names(helpDT)]]
#                        FactLabels
#                        # factor(helpDT, labels = FactLabels)
#                        helpDT[, lapply(.SD, factor, labels = FactLabels)]
#                      })
# })
# # User      System verstrichen
# # 0.51        0.30        0.83
# as.data.table(helpTest)
#
# system.time({
#   combs_labels <- data.table(Combination = combs[, Combination],
#                              sapply(1:length(nlev),
#                                     function(x) {
#                                       sapply(unlist(combs[, x, with = FALSE]),
#                                              function(y) {
#                                                do.call(switch, c(y, as.list(attLev[[x]])))
#                                              })
#                                     }),
#                              N = combs$N)
# })
# # User       System verstrichen
# # 17.22        0.56       17.91
# names(combs_labels) <- c("Combination", names(attLev), "N")
# combs_labels



SKU_choice_DT[, .(.N), by = .(Comb)][order(-N)]

SKUs_per_person <- SKU_choice_DT[, lapply(.SD, list), by = ID,
                                .SDcols = "Comb"]

37132222 %in% SKUs_per_person[1, Comb][[1]]

selectedComb <- as.numeric(combs_labels[1, Combination])

IDselect <- SKUs_per_person[sapply(SKUs_per_person[, Comb],
                                   function(x) {
                                     selectedComb %in% x
                                   }
                                   ), ID]

IDselect
length(IDselect)

substDT <- SKU_choice_DT[ID %in% IDselect, ]

substDT[, .(.N), by = .(Comb)][, Substitution := N / max(N)][order(-N)]

# DAUERT ZU LANGE UND IST ZU SPEICHERINTENIV !--------------------------------------------------------------------------
# dummyChoice <- data.table::dcast(SKU_choice_DT[, .(ID, Comb)], ID ~ Comb)
#
# helpCor <- data.table::data.table(dummyChoice[, "ID"],
#                                   !is.na(dummyChoice[, -1, with = FALSE])) * 1
#
# stats::cor(helpCor[, -1, with = FALSE])

# Clustering Tests !----------------------------------------------------------------------------------------------------

SKUs_per_person

SKUs_per_person[2, Comb][[1]] %in% SKUs_per_person[1, Comb][[1]]

testDist <- lapply(1:nrow(SKUs_per_person),
                   function(y) {
                     data.table(t(sapply(SKUs_per_person[, Comb],
                                         function(x) {
                                           helpIncl <- x %in% SKUs_per_person[y, Comb][[1]]

                                           out <- sum(helpIncl) / max(length(x[[1]]),
                                                                      length(SKUs_per_person[y, Comb][[1]]))

                                           out
                                         })))
                   })


lapply(1:25,
       function(y) {
         data.table(t(sapply(SKUs_per_person[1:25, Comb],
                function(x) {
                  helpIncl <- x %in% SKUs_per_person[y, Comb][[1]]

                  out <- sum(helpIncl) / max(length(x[[1]]), length(SKUs_per_person[y, Comb][[1]]))

                  out
                })))
       })

testDist_DT <- rbindlist(testDist)
testDist_DT[, 1:10]

testDist_DT[V1 > 0, 1]
which(testDist_DT[, 1] > 0)

SKUs_per_person[which(testDist_DT[, 1] > 0)]
SKUs_per_person[which(testDist_DT[, 1] == 1)]

SKUs_per_person[1, Comb]
SKUs_per_person[1, Comb][[1]]
SKUs_per_person[25, Comb]

SKUs_per_person[1, Comb][[1]] %in% SKUs_per_person[25, Comb][[1]]

summary(testDist_DT[, 1])

SKUs_per_person[]
SKUs_per_person[, Lag1 := shift(Comb, 1, NA, "lag")]
SKUs_per_person[, Lag1 := c(NA, Comb[-.N])]
SKUs_per_person[, test := lapply(Comb, function(x) {x %in% Lag1})]
SKUs_per_person


testClust <- hclust(as.dist(testDist_DT))
plot(testClust,  labels = FALSE, hang = -1, main = "Original Tree")
table(cutree(testClust, k = 10))

Data


