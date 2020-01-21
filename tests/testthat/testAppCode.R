rm(list = ls())
setwd("C:/03_RAND/Analyse_R/11_Git/VMMarketStructure/tests/testthat")

####Packages####
library(shiny)
library(shinydashboard)
library(kantardashboard)
library(KTShiny)
library(data.table)
library(DT)
library(formattable)
library(networkD3)
library(treemap)
#Add all required packages here

input <- list(study = "316401010")

## START - Code "readData.R" !------------------------------------------------------------------------------------------
##
## READ Data !----------------------------------------------------------------------------------------------------------

# read Password !----
# pw <- eventReactive(input$go, {
#
#   validate(
#     need(!is.null(input$study), "Wait for it!")
#   )

get_pw <- BeastRServer::azure_blob_call("GET",
                                        storage_account = "shinyapp",
                                        storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                             "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                             "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                        container = paste0("ms", input$study),
                                        blob = paste0(input$study, "_pw.txt"))

pw <- httr::content(get_pw, as = "text", encoding = "UTF-8")

print("PW read")
pw

# })


# labels
# def <- eventReactive(input$go, {
#
#   validate(
#     need(!is.null(input$study), "Wait for it!")
#   )

# if (isolate(input$pw) == isolate(pw())) {
get_def <- BeastRServer::azure_blob_call("GET",
                                         storage_account = "shinyapp",
                                         storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                              "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                              "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                         container = paste0("ms", input$study),
                                         blob = "attributes.def")

print("def read")
def <- strsplit(httr::content(get_def, as = "text", encoding = "UTF-8"), "\r\n")[[1]]
def
# }

# })

# choices, combinations and ranks
# Data <- eventReactive(input$go, {
#
#   validate(
#     need(input$study, "Wait for it!")
#   )
#
#   validate(
#     need(pw(), "Wait for it!")
#   )
#
#   if (isolate(input$pw) == isolate(pw())) {
get_data <- BeastRServer::azure_blob_call("GET",
                                          storage_account = "shinyapp",
                                          storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                               "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                               "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                          container = paste0("ms", input$study),
                                          blob = "data.csv")

Data <- as.data.table(httr::content(get_data, type = "text/csv", encoding = "UTF-8"))

print("Data read")
Data
#   }
#
# })

# choices, combinations and ranks
# lc_segs <- eventReactive(input$go, {
#
#   validate(
#     need(input$study, "Wait for it!")
#   )
#
#   validate(
#     need(pw(), "Wait for it!")
#   )
#
#   if (isolate(input$pw) == isolate(pw())) {
get_lcSegs <- BeastRServer::azure_blob_call("GET",
                                            storage_account = "shinyapp",
                                            storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                 "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                 "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                            container = paste0("ms", input$study),
                                            blob = "LC_segs.csv")

lc_segs <- as.data.table(httr::content(get_lcSegs, type = "text/csv", encoding = "UTF-8"))

print("LC read")
lc_segs

#   }
#
# })

# choices, combinations and ranks
# segments <- eventReactive(input$go, {
#
#   validate(
#     need(input$study, "Wait for it!")
#   )
#
#   validate(
#     need(pw(), "Wait for it!")
#   )
#
#   if (isolate(input$pw) == isolate(pw())) {
# read segments
get_segs <- BeastRServer::azure_blob_call("GET",
                                          storage_account = "shinyapp",
                                          storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                               "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                               "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                          container = paste0("ms", input$study),
                                          blob = "segments.csv")

segments <- as.data.table(httr::content(get_segs, type = "text/csv", encoding = "UTF-8"))

print("Segments read")
segments

#   }
#
# })

# segment labels
# segDef <- eventReactive(input$go, {
#
#   validate(
#     need(!is.null(input$study), "Wait for it!")
#   )
#
#   if (isolate(input$pw) == isolate(pw())) {
# read segment labels
get_segDef <- BeastRServer::azure_blob_call("GET",
                                            storage_account = "shinyapp",
                                            storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                 "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                 "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                            container = paste0("ms", input$study),
                                            blob = "def_segments.def")

segDef <- strsplit(httr::content(get_segDef, as = "text", encoding = "UTF-8"), "\r\n")[[1]]
segDef
#   }
#
# })

# choices, combinations and ranks
# orderIN <- eventReactive(input$go, {
#
#   validate(
#     need(input$study, "Wait for it!")
#   )
#
#   validate(
#     need(pw(), "Wait for it!")
#   )
#
#   if (isolate(input$pw) == isolate(pw())) {
get_order <- BeastRServer::azure_blob_call("GET",
                                           storage_account = "shinyapp",
                                           storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                           container = paste0("ms", input$study),
                                           blob = "order.json")

orderIN <- httr::content(get_order, type = "application/json", encoding = "UTF-8")

print("orderIN read")
orderIN <- lapply(orderIN, unlist)
orderIN
#   }
#
# })
# Data READ COMPLETED !-----------------------------------------------------------------------------------------------



# EXCTRACT Definition Data !------------------------------------------------------------------------------------------

# defIN <- reactive({
#
#   validate(
#     need(def(), "Wait for it!")
#   )

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

# # ASD adjustments
# attLev_asd <- attLev[-(3:5)]
# attLev_asd$Jahresbeitrag <- paste0(rep(names(attLev[3:5]), nlev[3:5]), " - ", sequence(nlev[3:5]))
# attLev_asd <- attLev_asd[c(1, 2, length(attLev_asd), 3:(length(attLev_asd) - 1))]
# nlev_asd <- sapply(attLev_asd, length)

defIN <- list(attLev = attLev,
              nlev = nlev)
defIN

# })

# segDefIN <- reactive({
#
#   validate(
#     need(segDef(), "Wait for it!")
#   )

# segment info
segIndex <- grep("^[^ ]", segDef)

# indecies of levels
segLevIndex <- apply(cbind(segIndex + 1,
                           c(segIndex[-1] - 1, length(segDef))), 1,
                     function(x) {seq(x[1], x[2])})


# named list of levels
segLev <- lapply(segLevIndex,
                 function(x) {
                   gsub("^[ ]", "", segDef[x])
                 })
names(segLev) <- segDef[segIndex]

# number of levels
segCode <- sapply(segLev,
                  function(x) {
                    if (all(x == "") & length(x) == 1) {
                      "numeric"
                    } else {
                      "factor"
                    }
                  })

segDefIN <- list(segLev = segLev,
                 segCode = segCode,
                 segLevFact = segLev[segCode == "factor"])

segDefIN
# })
# Definition Data extracted !-----------------------------------------------------------------------------------------


# EXCTRACT Data !-----------------------------------------------------------------------------------------------------

factorVars <- names(segments)[which(segDefIN$segCode == "factor") + 1]
segFactor <- copy(segments[, mget(c("ID", factorVars))])

segFactor[, (factorVars) := lapply(seq_along(.SD),
                                   function(x) {
                                     factor(.SD[[x]],
                                            labels = segDefIN$segLev[[which(segDefIN$segCode == "factor")[x]]])
                                   }),
          .SDcols = factorVars]

segNumeric <- copy(segments[, mget(c("ID", names(segments)[which(segDefIN$segCode == "numeric") + 1]))])

segIN <- list(segFactor = segFactor,
              segNumeric = segNumeric)

segIN

# })

# calculate importances and counts
# Importance <- reactive({
#
#   validate(
#     need(Data(), "Wait for Data!")
#   )

# Alternative Importance calculation !----------------------------------------------------------------------------------

# Data <- Data

Data_Weighted <- copy(Data)
for (i in 1:length(defIN$nlev)) {
  sel1 <- paste0("A", i, "_", seq_along(grep(paste0("A", i, "_"),
                                             names(Data_Weighted))))
  sel2 <- paste0("RankAtt", i)
  # print(sel1)
  Data_Weighted[, (sel1) := lapply(.SD,
                                   function(x) {
                                     x * (length(defIN$nlev) -
                                            ifelse(is.na(Data_Weighted[[sel2]]),
                                                   0, Data_Weighted[[sel2]]) + 1)
                                   }), .SDcols = sel1]
}

DT_melted <- melt(Data_Weighted[, mget(c("ID", names(Data_Weighted)[grep("^A", names(Data_Weighted))]))],
                  id.vars = "ID")

DT_importance <- DT_melted[, sum(value), by = variable]
# raw imp based on choices weighted by rank
DT_importance <- DT_importance[, grp := rep(seq_along(defIN$nlev),
                                            defIN$nlev)][, sum(V1),
                                                         by = grp]
DT_importance[, imp := V1 / sum(V1)]
# Imp corrected for number of levels
DT_importance[, V2 := V1 / defIN$nlev]
DT_importance[, imp2 := V2 / sum(V2)]

Data_inverseRanks <- Data[, mget(c("ID", names(Data)[grep("^R", names(Data))]))]

Data_inverseRanks[, names(Data)[grep("^R", names(Data))] :=
                    lapply(.SD,
                           function(x) {
                             out <- (length(defIN$nlev) + 1) - x
                             sapply(out,
                                    function(y) {
                                      ifelse(is.na(y), 0, y)
                                    })
                           }), .SDcols = names(Data)[grep("^R", names(Data))]]

Importance_invRanks <- sapply(Data_inverseRanks[, mget(names(Data_inverseRanks)[grep("^R", names(Data_inverseRanks))])],
                              mean)

DT_importance[, imp3 := Importance_invRanks / sum(Importance_invRanks)]

# Attribute counts == attribute importance

Importance <- DT_importance[, imp3]
names(Importance) <- names(defIN$attLev)

# Level counts - 100% within attributes
LevelCounts_100 <- lapply(paste0("^A", 1:length(defIN$nlev), "_"),
                          function(y) {
                            out <- apply(Data[, grep(y, x = names(Data)),
                                              with = FALSE], 2, sum) /
                              sum(apply(Data[, grep(y, x = names(Data)),
                                             with = FALSE], 2, sum))

                            names(out) <- defIN$attLev[[y]]
                            out
                          })
names(LevelCounts_100) <- names(defIN$attLev)

# Level counts - summing to attribute importance within attributes
LevelCounts_rel <- lapply(1:length(defIN$nlev),
                          function(y) {
                            out <- apply(Data[, grep(paste0("^A", y, "_"),
                                                     x = names(Data)),
                                              with = FALSE], 2, sum) /
                              sum(apply(Data[, grep(paste0("^A", y, "_"),
                                                    x = names(Data)),
                                             with = FALSE], 2, sum)) * Importance[y]

                            names(out) <- defIN$attLev[[y]]
                            out
                          })

names(LevelCounts_rel) <- names(defIN$attLev)

Importance <- list(Importance = Importance,
                   LevelCounts_100 = LevelCounts_100,
                   LevelCounts_rel = LevelCounts_rel)

Importance
# })

# Imp_ordered <- reactive({
#
#   validate(
#     need(try(Importance(), silent = TRUE),
#          "Wait for Imps!")
#   )

orderAtt <-  order(Importance$Importance,
                   decreasing = TRUE)

attLev_ordered <- defIN$attLev

optOrder <- orderIN

LevelCounts_100_ordered <- lapply(seq_along(optOrder),
                                  function(x) {
                                    out <- Importance$LevelCounts_100[[x]][optOrder[[x]]]
                                    names(out) <- attLev_ordered[[x]][optOrder[[x]]]
                                    out
                                  })

names(LevelCounts_100_ordered) <- names(defIN$attLev)

LevelCounts_100_ordered <- LevelCounts_100_ordered[orderAtt]

attLev_ordered <- lapply(seq_along(attLev_ordered),
                         function(k) {
                           attLev_ordered[[k]][optOrder[[k]]]
                         })[orderAtt]

Imp_ordered <- list(Imp = Importance$Importance[orderAtt],
                    LevCount = LevelCounts_100_ordered,
                    attLev_ordered = attLev_ordered)

lapply(Imp_ordered$LevCount, round, digits = 3)

# })

# SKU_choice_DT <- reactive({
# Alternative SKU_choice_DT !-------------------------------------------------------------------------------------------
# NA --> 0 instead of expand.grid(x)
SKU_choice_2 <- lapply(Data$ID,
                       function(x) {
                         DataHelp <- Data[Data$ID == x, grep("^A", names(Data)), with = FALSE]

                         chosenLevels <- lapply(1:length(defIN$nlev),
                                                function(y) {
                                                  outHelp <- which(DataHelp[, grep(paste0("^A", y, "_"),
                                                                                   names(DataHelp)),
                                                                            with = FALSE] == 1)
                                                  if (length(outHelp) == 0) {
                                                    0
                                                  } else {
                                                    outHelp
                                                  }
                                                })

                         out <- data.frame(ID = x, expand.grid(chosenLevels))
                         names(out) <- c("ID", paste0("Att", 1:length(defIN$nlev)))

                         out$Comb <- gsub("NA", "_", apply(out[, -1], 1,
                                                           paste0,
                                                           collapse = ""))

                         out

                       })


SKU_choice_DT_ALTERNATIVE <- rbindlist(SKU_choice_2)

SKU_choice_DT <- SKU_choice_DT_ALTERNATIVE[order(ID)]

SKU_choice_DT
# })

## !--------------------------------------------------------------------------------------------------------------------
## END Code "readData.R" !----------------------------------------------------------------------------------------------
## !--------------------------------------------------------------------------------------------------------------------

## !--------------------------------------------------------------------------------------------------------------------
## START - Code ImpTable.R ---------------------------------------------------------------------------------------------
## !--------------------------------------------------------------------------------------------------------------------

lapply(seq_along(Imp_ordered$LevCount),
       function(x) {
         id <- paste0("decMat", x)

         dt <- data.table(matrix(Imp_ordered$LevCount[[x]],
                                 nrow = 1))
         names(dt) <- Imp_ordered$attLev_ordered[[x]]

         dt
       })


input$segs <- c("1_1", "1_2", "2_2")

input$levels <- c("2_1", "2_2", "2_3")

segments



Data

