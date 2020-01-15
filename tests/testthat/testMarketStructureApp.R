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
library(GA)

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

# system.time({
#   get_comb <- BeastRServer::azure_blob_call("GET",
#                                             storage_account = "shinyapp",
#                                             storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
#                                                                  "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
#                                                                  "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
#                                             container = paste0("ms", input$study),
#                                             blob = "comb.csv")
#
#   comb <- as.data.table(httr::content(get_comb, type = "text/csv", encoding = "UTF-8"))
# })
#
# comb
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

table(rowSums(!is.na(data[, grep("^R", names(data)), with = FALSE])))

Data <- data[rowSums(!is.na(data[, grep("^R", names(data)), with = FALSE])) > 4]

# choice data
# SKU_choice_DT_1 <- comb

# Alternative SKU_choice_DT !-------------------------------------------------------------------------------------------
# NA --> 0 instead of expand.grid(x)


SKU_choice <- lapply(Data$ID,
                     function(x) {
                       dataHelp <- Data[Data$ID == x, grep("^A", names(Data)), with = FALSE]

                       chosenLevels <- lapply(1:length(nlev),
                                              function(y) {
                                                outHelp <- which(dataHelp[, grep(paste0("^A", y, "_"),
                                                                                 names(dataHelp)), with = FALSE] != 0)
                                                if (length(outHelp) == 0) {
                                                  0
                                                } else {
                                                  outHelp
                                                }
                                              })

                       out <- data.table(ID = x, expand.grid(chosenLevels))
                       names(out) <- c("ID", paste0("Att", 1:length(nlev)))

                       out[, paste0("T", 1:length(nlev)) := lapply(.SD, sprintf, fmt = "%02d"),
                           .SDcols = names(out)[-1]]

                       out[, Comb := do.call(paste0, .SD), .SDcols = paste0("T", 1:length(nlev))]

                       out[, mget(c("ID", paste0("Att", 1:length(nlev)), "Comb"))]

                     })

# str(SKU_choice)
SKU_choice[[1]]
unique(SKU_choice[[1]])
Data[1, ]

system.time({
  # SKU_choice_DT <- data.table(Reduce(rbind, SKU_choice))
  SKU_choice_DT <- rbindlist(SKU_choice)
})
# SKU_choice_DT_1[order(ID)]
SKU_choice_DT[order(ID)]

# SKU_choice_DT_1[, .N, by = ID]
# SKU_choice_DT[, .N, by = ID]
# table(SKU_choice_DT_1[, .N, by = ID]$N)
# table(SKU_choice_DT[, .N, by = ID]$N)

# calculate importances and counts
Importance_Top2 <- sapply(Data[, grep("^Rank",
                                        names(Data)),
                                 with = FALSE],
                            function(x) {
                              sum(x %in% 1:2, na.rm = TRUE)
                            })
Importance_Top2 <- Importance_Top2 / sum(Importance_Top2)
names(Importance_Top2) <- names(attLev)
Importance_Top2

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
DT_importance[, impOrig := Importance_Top2]
# Imp corrected for number of levels
DT_importance[, V2 := V1 / nlev]
DT_importance[, imp2 := V2 / sum(V2)]
DT_importance

Data_inverseRanks <- Data[, mget(c("ID", names(Data)[grep("^R", names(Data))]))]

Data_inverseRanks[, names(Data)[grep("^R", names(Data))] :=
                    lapply(.SD,
                           function(x) {
                             out <- (length(nlev) + 1) - x
                             sapply(out,
                                    function(y) {
                                      ifelse(is.na(y), 0, y)
                                    })
                           }), .SDcols = names(Data)[grep("^R", names(Data))]]

Data_inverseRanks

Importance_invRanks <- sapply(Data_inverseRanks[, mget(names(Data_inverseRanks)[grep("^R", names(Data_inverseRanks))])],
                              mean)
Importance_invRanks / sum(Importance_invRanks)


DT_importance[, imp3 := Importance_invRanks / sum(Importance_invRanks)]
DT_importance

Importance_Compare <- DT_importance[, .(impOrig, imp2, imp3)]
Importance_Compare[, paste0("orderImp", 1:3) := lapply(-.SD, rank),
                   .SDcols = c("impOrig", "imp2", "imp3")]
Importance_Compare[, attNames := names(attLev)]
Importance_Compare

fwrite(Importance_Compare, file = "Importance_Compare_Options.csv",
       sep = ";", dec = ",")

Importance <- DT_importance[, imp2]
names(Importance) <- names(attLev)
Importance

# Level counts - 100% within attributes
LevelCounts_100 <- lapply(1:length(nlev),
                          function(y) {
                            out <- apply(Data[, grep(paste0("^A", y, "_"),
                                                     x = names(Data)),
                                              with = FALSE], 2, sum) /
                              sum(apply(Data[, grep(paste0("^A", y, "_"),
                                                    x = names(Data)),
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
orderAtt <-  order(Importance,
                   decreasing = TRUE)

lapply(LevelCounts_100, round, digits = 3)

# write counts in csv file
lapply(seq_along(LevelCounts_100),
       function(x) {
         cat(paste(names(attLev)[orderAtt[x]]), "\n", file = "DecisionMatOut_unsortiert.csv", append = TRUE)
         out <- as.data.table(t(LevelCounts_100[[orderAtt[x]]]))
         write.table(out, file = "DecisionMatOut_unsortiert.csv", append = TRUE,
                     sep = ";", dec = ",", row.names = FALSE)
       })


## Sort attributes based on proximity !---------------------------------------------------------------------------------
# orderAtt <-  order(Importance,
#                    decreasing = TRUE)

# Übersicht: Level correlation pro Attribut
for (i in 1:length(nlev)) {
  print(cor(Data[, grep(paste0("^A", i, "_"), names(Data)), with = FALSE]))
}


# library(GA)
optOrdeGA <- lapply(1:length(nlev),
                    function(i) {
                      # i <- 1

                      indVal <- sapply(Data[, grep(paste0("^A", i, "_"), names(Data)), with = FALSE], sd) != 0

                      selVars <- names(indVal[indVal])

                      CorIter <- cor(Data[, mget(selVars)])

                      out <- 1:nlev[i]
                      names(out) <- attLev[[i]]

                      optDist_GA <- function(x) {
                        mean(diag(CorIter[x[-nlev[i]], x[-1]]))
                      }

                      if (length(selVars) > 2) {
                        GAiter <- ga("permutation", optDist_GA, min = 1:length(selVars), max = length(selVars):1)

                        out[indVal] <- GAiter@solution[1, ]
                        names(out[indVal]) <- attLev[[i]][indVal][GAiter@solution[1, ]]
                      } else {
                        out <- order(LevelCounts_100[[i]], decreasing = TRUE)
                        names(out) <- attLev[[i]]
                      }
                      out

                      # summary(GAiter)
                    })

names(optOrdeGA) <- names(attLev)
optOrdeGA
optOrdeGA[orderAtt]

pathOUT <- "J:/Proj/CAPR/AKT/316400971_RedBull_Italien/data/decision_tree"
jsonlite::write_json(optOrdeGA[orderAtt], paste0(pathOUT, "/appData/orderGA.json"))


lapply(seq_along(LevelCounts_100),
       function(x) {
         cat(paste(names(attLev)[orderAtt[x]]), "\n", file = "DecisionMatOut_sortiert_GA.csv", append = TRUE)
         out <- as.data.table(t(LevelCounts_100[[orderAtt[x]]][optOrdeGA[[orderAtt[x]]]]))
         write.table(out, file = "DecisionMatOut_sortiert_GA.csv", append = TRUE,
                     sep = ";", dec = ",", row.names = FALSE)
       })




# END Decision Matrix Table !-------------------------------------------------------------------------------------------




# SANKEY DIAGRAMM - ALTERNATIVE CODING !--------------------------------------------------------------------------------

system.time({
  DThelp2 <- SKU_choice_DT[, grep("^Att", names(SKU_choice_DT))[orderAtt], with = FALSE]
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

attFact <- lapply(attLev,
                  function(x) {
                    c("All Levels", x)
                  })
names(attFact) <- VarSelect

combs[, paste0("Fact", VarSelect) := .SD,
      .SDcols = VarSelect]
combs

system.time({
  for (j in seq_along(nlev)) {
    set(combs, i = NULL, j = paste0("Fact", VarSelect)[j],
        value = factor(combs[[j]],
                       levels = seq_along(attFact[[j]]) - 1,
                       labels = attFact[[j]]))
  }
})
# User      System verstrichen
# 0.30        0.00        0.29
combs

combs_labels <- combs[, mget(c("Combination", paste0("Fact", VarSelect), "N"))]
combs_labels


# graphical models !----------------------------------------------------------------------------------------------------

# library(pcalg)
# example(pcalg)

library(huge)

help(huge)


test_huge <- huge(as.matrix(Data[, mget(names(Data)[grep("^A", names(Data))[validVars]])]))
plot(test_huge)				 #Not aligned
plot(test_huge, align = TRUE) #Aligned
huge.plot(test_huge$path[[3]])

out.select = huge.select(test_huge)
plot(out.select)


# data_huge <- as.matrix(Data[, mget(names(Data)[grep("^A", names(Data))[validVars]])])
data_huge <- as.matrix(Data[, mget(names(Data)[grep("^nAtt[1-9]", names(Data))])])[, -1]
# data_huge <- as.matrix(Data[, mget(names(Data)[grep("^R", names(Data))])])[, -1]
# data_huge[is.na(data_huge)] <- 0

out.glasso = huge(data_huge, method = "glasso")
plot(out.glasso)				 #Not aligned
plot(out.glasso, align = TRUE) #Aligned
out.select = huge.select(out.glasso, criterion = "ebic")
plot(out.select)

summary(out.select)

out.select$data

qgraph(out.select$refit,
       layout = 'spring',
       labels = colnames(data_huge))

library(mgm)
library(qgraph)

# data_mgm <- as.matrix(Data[, mget(names(Data)[grep("^A", names(Data))[validVars]])])
data_mgm <- as.matrix(Data[, mget(names(Data)[grep("^nAtt[1-9]", names(Data))])])[, -1]
# data_mgm <- as.matrix(Data[, mget(names(Data)[grep("^R", names(Data))])])[, -1]
# data_mgm[is.na(data_mgm)] <- 0

type_mgm <- rep("p", ncol(data_mgm))
cat_mgm <- rep(1, ncol(data_mgm))
# cat_mgm <- apply(data_mgm, 2, max, na.rm = TRUE) + c(1, 0, 0, 0, 0, 0, 0)

fit <- mgm(data_mgm, type_mgm, cat_mgm, lamda.sel = "EBIC")

qgraph(fit$pairwise$wadj,
       edge.color = fit$pairwise$edgecolor,
       layout = 'spring',
       labels =  colnames(data_mgm))



# PCA

data_pca <- Data[, mget(names(Data)[grep("^A", names(Data))])]
validVars_pca <- lapply(data_pca, var) != 0
data_pca <- data_pca[, which(validVars_pca), with = FALSE]
data_pca[is.na(data_pca)] <- 0

# data_pca <- Data[, mget(names(Data)[grep("^R", names(Data))])]
# data_pca[is.na(data_pca)] <- 0

# data_pca <- Data[, -1]
# validVars_pca <- lapply(data_pca, var) != 0
# data_pca <- data_pca[, which(validVars_pca), with = FALSE]
# data_pca[is.na(data_pca)] <- 0

data_pca <- Data[, mget(names(Data)[grep("^nAtt", names(Data))])]
validVars_pca <- lapply(data_pca, var) != 0
data_pca <- data_pca[, which(validVars_pca), with = FALSE]
data_pca[is.na(data_pca)] <- 0

# test_pca <- prcomp(Data[, mget(names(Data)[grep("^A", names(Data))])[validVars]],
#                    center = TRUE,
#                    scale. = TRUE)

test_pca <- prcomp(data_pca,
                   center = TRUE,
                   scale. = TRUE)

summary(test_pca)
plot(test_pca, type = "l")

# library(devtools)
# install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(test_pca, obs.scale = 1, var.scale = 1,
              groups = NULL, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

# END - graphical models !----------------------------------------------------------------------------------------------


# Respondent Segmentation !---------------------------------------------------------------------------------------------

Data_SegHelp <- melt(Data, measure.vars = patterns("^A"), id.vars = "ID")[order(ID)]
Data_SegHelp[, grp := rep(1:length(nlev), nlev)]
Data_nChoAtt <- Data_SegHelp[, sum(value), by = .(ID, grp)]

Data_aggCho <- dcast(Data_nChoAtt, ID ~ grp, value.var = "V1")
names(Data_aggCho) <- c("ID", paste0("nAtt", 1:length(nlev)))
Data_aggCho

lapply(Data_aggCho, table)
nlev

Data <- Data[Data_aggCho, on = "ID"]
Data[, nAttSel := rowSums(!is.na(Data[, grep("^R", names(Data)), with = FALSE]))]

testClust_Resp <- hclust(as.dist(cor(t(Data[, grep("^nAtt", names(Data))[-1], with = FALSE]))))
summary(testClust_Resp)
plot(testClust_Resp)
table(cutree(testClust_Resp, k = 10))



# LC segmentation - TEST
validVars <- lapply(Data[, mget(names(Data)[grep("^A", names(Data))])], var) != 0

formulaList <- lapply(paste(names(Data)[grep("^A", names(Data))[validVars]], 1, sep = "~"), as.formula)

library(depmixS4)
# LCtestRun <- mix(formulaList,
#                  data = Data, # the dataset to use
#                  nstates = 5, # the number of latent classes
#                  family = lapply(formulaList,
#                                  function(x) {
#                                    multinomial()
#                                  }))
# LCtestRun
#
# LCtestRun_fit <- fit(LCtestRun)
# summary(LCtestRun_fit)
#
# LCtestRun_fit_posterior <- depmixS4::posterior(LCtestRun_fit) # Saving Class Assignments
# head(round(LCtestRun_fit_posterior, 3))
#
# table(LCtestRun_fit_posterior$state)


# test for 2-10 solutions ...

LCtest_Grid <- lapply(2:10,
                      function(x) {
                        set.seed(x)
                        LCRun <- mix(formulaList,
                                     data = Data, # the dataset to use
                                     nstates = x, # the number of latent classes
                                     family = lapply(formulaList,
                                                     function(x) {
                                                       multinomial()
                                                     }))

                        fit(LCRun)
                      })


LCtest_Grid_posterior <- lapply(LCtest_Grid, posterior)

LCtest_Grid_segs <- data.table(sapply(LCtest_Grid_posterior,
                                     function(x) {
                                       x$state
                                     }))

names(LCtest_Grid_segs) <- paste0("LC", 2:10)

LCtest_Grid_segs

lapply(LCtest_Grid_segs, table)

lapply(2:9,
       function(x) {
         table(LCtest_Grid_segs[, get(paste0("LC", x))],
               LCtest_Grid_segs[, get(paste0("LC", x + 1))])
       })


## Sankey - LC Runs
Help_LC_Sankey_1 <- copy(LCtest_Grid_segs)

for (i in 3:10) {
  sel1 <- paste0("LC", i)
  sel2 <- paste0("LC", i - 1)
  Help_LC_Sankey_1[, c(sel1) := get(sel1) + max(get(sel2))]
}

Help_LC_Sankey_1 <- Help_LC_Sankey_1 - 1

sankeyLinks_LC <- rbindlist(
  lapply(1:8,
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

sankeyNodes_LC <- data.frame(name = paste0("LC_", rep(2:10, 2:10), "_", sequence(2:10)))

sankeyNetwork(Links = sankeyLinks_LC,
              Nodes = sankeyNodes_LC,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "name",
              fontSize = 12,
              nodeWidth = 15)

## END - Sankey - LC Runs


# Test Reproducability LC segments !------------------------------------------------------------------------------------
LCtest_4 <- lapply(1:10,
                    function(x) {
                      set.seed(x)
                      LCRun <- mix(formulaList,
                                   data = Data, # the dataset to use
                                   nstates = 4, # the number of latent classes
                                   family = lapply(formulaList,
                                                   function(x) {
                                                     multinomial()
                                                   }))

                      print(x)
                      fit(LCRun, verbose = TRUE)
                    })

lapply(1:9,
       function(x) {
         table(posterior(LCtest_4[[x]])$state,
               posterior(LCtest_4[[x + 1]])$state)
       })

LCtest_6 <- lapply(1:4,
                   function(x) {
                     set.seed(x)
                     LCRun <- mix(formulaList,
                                  data = Data, # the dataset to use
                                  nstates = 6, # the number of latent classes
                                  family = lapply(formulaList,
                                                  function(x) {
                                                    multinomial()
                                                  }))

                     print(x)
                     fit(LCRun, verbose = TRUE)
                   })

lapply(1:3,
       function(x) {
         table(posterior(LCtest_6[[x]])$state,
               posterior(LCtest_6[[x + 1]])$state)
       })


LCtest_10 <- lapply(1:10,
                    function(x) {
                      set.seed(x)
                      LCRun <- mix(formulaList,
                                   data = Data, # the dataset to use
                                   nstates = 10, # the number of latent classes
                                   family = lapply(formulaList,
                                                   function(x) {
                                                     multinomial()
                                                   }))

                      print(x)
                      fit(LCRun, verbose = FALSE)
                    })


LCtest_10_posterior <- lapply(LCtest_10, posterior)

LCtest_10_segs <- data.table(sapply(LCtest_10_posterior,
                                      function(x) {
                                        x$state
                                      }))

lapply(1:9,
       function(x) {
         table(LCtest_10_segs[, get(paste0("V", x))],
               LCtest_10_segs[, get(paste0("V", x + 1))])
       })

lapply(1:9,
       function(x) {
         chisq.test(table(LCtest_10_segs[, get(paste0("V", x))],
               LCtest_10_segs[, get(paste0("V", x + 1))]))
       })

# END - Test Reproducability LC segments !------------------------------------------------------------------------------


# Profile Segmentations !-----------------------------------------------------------------------------------------------
SPSS_data <- foreign::read.spss("J:/Proj/CAPR/AKT/316401010_Pilotstudie_MarketStructure/Clean_SPSS_DEU.SAV",
                                to.data.frame = TRUE,
                                use.value.labels = TRUE)

frame_data <- data.table(SPSS_data[, -c(grep("AttributChoice", names(SPSS_data)),
                                        grep("FilterConjoint", names(SPSS_data)),
                                        grep("DataCollection", names(SPSS_data)),
                                        grep("F12", names(SPSS_data)),
                                        grep("zzAutoJump", names(SPSS_data)))])
frame_data[, ID := Respondent_Serial]

LCtest_Grid_segs_ID <- copy(LCtest_Grid_segs)[, ID := Data[, ID]]

frame_data <- frame_data[LCtest_Grid_segs_ID, on = "ID"]
frame_data

seg2profile <- "LC4"

lapply(c("zzAge", "zzGender", "SC06", "SC08", "S04"),
       function(y) {
         sapply(split(frame_data[, mget(c(y, seg2profile))][order(get(seg2profile))], by = seg2profile),
                function(x) {
                  table(x[, 1, with = FALSE])
                })
       })

lapply(c("zzAge", "zzGender", "SC06", "SC08", "S04"),
       function(y) {
         sapply(split(frame_data[, mget(c(y, seg2profile))][order(get(seg2profile))], by = seg2profile),
                function(x) {
                  round(prop.table(table(x[, 1, with = FALSE])), 3)
                })
       })

for (y in c("zzAge", "zzGender", "SC06", "SC08", "S04")) {
         heatmap(
           sapply(split(frame_data[, mget(c(y, seg2profile))][order(get(seg2profile))], by = seg2profile),
                  function(x) {
                    prop.table(table(x[, 1, with = FALSE]))
                  })
         )
       }


# !---------------------------------------------------------------------------------------------------------------------


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


