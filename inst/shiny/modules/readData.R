# READ DATA !---------------------------------------------------------------------------------------------------------

# read Password !----
pw <- eventReactive(input$go, {

  validate(
    need(!is.null(input$study), "Please load Study with StudyID and password!")
  )

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

})


# labels
def <- eventReactive(input$go, {

  validate(
    need(!is.null(input$study), "Please load Study with StudyID and password!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    get_def <- BeastRServer::azure_blob_call("GET",
                                             storage_account = "shinyapp",
                                             storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                  "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                  "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                             container = paste0("ms", input$study),
                                             blob = "attributes.def")

    print("def read")
    strsplit(httr::content(get_def, as = "text", encoding = "UTF-8"), "\r\n")[[1]]
  }

})

# choices, combinations and ranks
dataIN <- eventReactive(input$go, {

  validate(
    need(input$study, "Please load Study with StudyID and password!")
  )

  validate(
    need(pw(), "Wait for it!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    get_data <- BeastRServer::azure_blob_call("GET",
                                              storage_account = "shinyapp",
                                              storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                   "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                   "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                              container = paste0("ms", input$study),
                                              blob = "data.csv")

    data <- data.table(httr::content(get_data, type = "text/csv", encoding = "UTF-8"))

    print("data read")
    data
  }

})

# chosen Subgroup
dataUSED <- reactive({

  validate(
    need(length(chosenIDs()) > 10, "Base size too small!")
  )

  dataIN()[ID %in% chosenIDs(), ]

})

# chosen combinations per respondent
SKU_choice_DT <- eventReactive(input$go, {

  validate(
    need(input$study, "Please load Study with StudyID and password!")
  )

  validate(
    need(pw(), "Please enter correct password!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    get_combs <- BeastRServer::azure_blob_call("GET",
                                               storage_account = "shinyapp",
                                               storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                    "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                    "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                               container = paste0("ms", input$study),
                                               blob = "comb.csv")

    SKU_choice_DT <- data.table(httr::content(get_combs, type = "text/csv", encoding = "UTF-8"))
    rm(get_combs)
    SKU_choice_DT
  }
})

# demand data.table
Demand_DT <- eventReactive(input$go, {

  validate(
    need(input$study, "Please load Study with StudyID and password!")
  )

  validate(
    need(pw(), "Please enter correct password!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    get_demand <- BeastRServer::azure_blob_call("GET",
                                                storage_account = "shinyapp",
                                                storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                     "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                     "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                                container = paste0("ms", input$study),
                                                blob = "demand.csv")
    Demand_DT <- data.table(httr::content(get_demand, type = "text/csv", encoding = "UTF-8"))
    rm(get_demand)
    Demand_DT
  }
})

# choices, combinations and ranks
lc_segs <- eventReactive(input$go, {

  validate(
    need(input$study, "Please load Study with StudyID and password!")
  )

  validate(
    need(pw(), "Please enter correct password!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    get_lcSegs <- BeastRServer::azure_blob_call("GET",
                                              storage_account = "shinyapp",
                                              storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                   "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                   "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                              container = paste0("ms", input$study),
                                              blob = "LC_segs.csv")

    lc_segs <- data.table(httr::content(get_lcSegs, type = "text/csv", encoding = "UTF-8"))

    print("LC read")
    lc_segs

  }

})

# choices, combinations and ranks
segments <- eventReactive(input$go, {

  validate(
    need(input$study, "Please load Study with StudyID and password!")
  )

  validate(
    need(pw(), "Please enter correct password!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    # read segments
    get_segs <- BeastRServer::azure_blob_call("GET",
                                              storage_account = "shinyapp",
                                              storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                   "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                   "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                              container = paste0("ms", input$study),
                                              blob = "segments.csv")

    segments <- data.table(httr::content(get_segs, type = "text/csv", encoding = "UTF-8"))

    print("Segments read")

    segments

  }

})

# segment labels
segDef <- eventReactive(input$go, {

  validate(
    need(!is.null(input$study), "Please load Study with StudyID and password!")
  )

  if (isolate(input$pw) == isolate(pw())) {
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
  }

})


# choices, combinations and ranks
orderIN <- eventReactive(input$go, {

  validate(
    need(input$study, "Please load Study with StudyID and password!")
  )

  validate(
    need(pw(), "Please enter correct password!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    get_order <- BeastRServer::azure_blob_call("GET",
                                              storage_account = "shinyapp",
                                              storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                   "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                   "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                              container = paste0("ms", input$study),
                                              blob = "order.json")

    orderIN <- httr::content(get_order, type = "application/json", encoding = "UTF-8")

    print("orderIN read")
    lapply(orderIN, unlist)
  }

})

# existing SKUs
SKUinput <- eventReactive(input$go, {

  validate(
    need(!is.null(input$study), "Please load Study with StudyID and password!")
  )

  validate(
    need(defIN(), "Wait for def file to be extracted!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    get_SKUs <- BeastRServer::azure_blob_call("GET",
                                              storage_account = "shinyapp",
                                              storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                   "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                   "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                              container = paste0("ms", input$study),
                                              blob = "existingSKUs.csv")
    SKUinput <- data.table(httr::content(get_SKUs, type = "text/csv", encoding = "UTF-8"))
    rm(get_SKUs)

    names(SKUinput) <- c("Portfolio", "SKU",
                         paste0("A", rep(seq_along(defIN()$nlev),
                                         defIN()$nlev), "_",
                                sequence(defIN()$nlev)))

    # add SKU by Attribute part
    sapply(seq_along(defIN()$nlev),
           function(x) {
             SKUinput[, paste0("A", x) := apply(.SD, 1, which.max),
                      .SDcols = paste0("A", x, "_", sequence(defIN()$nlev[x]))]

             SKUinput[, paste0("A", x) := sapply(get(paste0("A", x)),
                                                 function(y) {
                                                   check_y <- ifelse(length(y) == 0, NA, y)
                                                   if (is.na(check_y)) {
                                                     0
                                                   } else {
                                                     check_y
                                                   }
                                                 })]
           })

    SKUinput
  }
})
# DATA READ COMPLETED !-----------------------------------------------------------------------------------------------



# EXCTRACT Definition DATA !------------------------------------------------------------------------------------------

defIN <- reactive({

  validate(
    need(def(), "Wait for it!")
  )

  attIndex <- grep("^[^ ]", def())

  # indecies of levels
  levIndex <- apply(cbind(attIndex + 1,
                          c(attIndex[-1] - 1, length(def()))), 1, function(x) {seq(x[1], x[2])})


  # named list of levels
  attLev <- lapply(levIndex,
                   function(x) {
                     gsub("^[ ]", "", def()[x])
                   })
  names(attLev) <- def()[attIndex]

  # number of levels
  nlev <- sapply(attLev, length)

  # # ASD adjustments
  # attLev_asd <- attLev[-(3:5)]
  # attLev_asd$Jahresbeitrag <- paste0(rep(names(attLev[3:5]), nlev[3:5]), " - ", sequence(nlev[3:5]))
  # attLev_asd <- attLev_asd[c(1, 2, length(attLev_asd), 3:(length(attLev_asd) - 1))]
  # nlev_asd <- sapply(attLev_asd, length)

  list(attLev = attLev,
       nlev = nlev)

})

segDefIN <- reactive({

  validate(
    need(segDef(), "Wait for it!")
  )

  # segment info
  segIndex <- grep("^[^ ]", segDef())

  # indecies of levels
  segLevIndex <- apply(cbind(segIndex + 1,
                             c(segIndex[-1] - 1, length(segDef()))), 1,
                       function(x) {seq(x[1], x[2])})


  # named list of levels
  segLev <- lapply(segLevIndex,
                   function(x) {
                     gsub("^[ ]", "", segDef()[x])
                   })
  names(segLev) <- segDef()[segIndex]

  # number of levels
  segCode <- sapply(segLev,
                    function(x) {
                      if (all(x == "") & length(x) == 1) {
                        "numeric"
                      } else {
                        "factor"
                      }
                    })

  list(segLev = segLev,
       segCode = segCode,
       segLevFact = segLev[segCode == "factor"])

})
# Definition data extracted !-----------------------------------------------------------------------------------------


# EXCTRACT DATA !-----------------------------------------------------------------------------------------------------

segIN <- reactive({

  validate(
    need(segments(), "Wait for data!")
  )

  validate(
    need(segDefIN(), "Wait for segment definitions!")
  )

  factorVars <- names(segments())[which(segDefIN()$segCode == "factor") + 1]
  segFactor <- copy(segments()[, mget(c("ID", factorVars))])

  segFactor[, (factorVars) := lapply(seq_along(.SD),
                                     function(x) {
                                       levs <- seq_along(segDefIN()$segLev[[which(segDefIN()$segCode == "factor")[x]]])
                                       factor(.SD[[x]],
                                              levels = levs,
                                              labels = segDefIN()$segLev[[which(segDefIN()$segCode == "factor")[x]]])
                                     }),
            .SDcols = factorVars]

  segNumeric <- copy(segments()[, mget(c("ID", names(segments())[which(segDefIN()$segCode == "numeric") + 1]))])

  list(segFactor = segFactor,
       segNumeric = segNumeric)

})

# calculate importances and counts
Importance <- reactive({

  validate(
    need(dataIN(), "Wait for data!")
  )

  # Alternative Importance calculation !--------------------------------------------------------------------------------

  Data <- dataUSED()

  Data_Weighted <- copy(Data)
  for (i in 1:length(defIN()$nlev)) {
    sel1 <- paste0("A", i, "_", seq_along(grep(paste0("A", i, "_"),
                                               names(Data_Weighted))))
    sel2 <- paste0("RankAtt", i)
    # print(sel1)
    Data_Weighted[, (sel1) := lapply(.SD,
                                     function(x) {
                                       x * (length(defIN()$nlev) -
                                              ifelse(is.na(Data_Weighted[[sel2]]),
                                                     0, Data_Weighted[[sel2]]) + 1)
                                     }),
                  .SDcols = sel1]
  }

  DT_melted <- melt(Data_Weighted[, mget(c("ID", names(Data_Weighted)[grep("^A", names(Data_Weighted))]))],
                    id.vars = "ID")

  DT_importance <- DT_melted[, sum(value), by = variable]
  # raw imp based on choices weighted by rank
  DT_importance <- DT_importance[, grp := rep(seq_along(defIN()$nlev),
                                              defIN()$nlev)][, sum(V1),
                                                             by = grp]
  DT_importance[, imp := V1 / sum(V1)]
  # Imp corrected for number of levels
  DT_importance[, V2 := V1 / defIN()$nlev]
  DT_importance[, imp2 := V2 / sum(V2)]

  Data_inverseRanks <- Data[, mget(c("ID", names(Data)[grep("^R", names(Data))]))]

  Data_inverseRanks[, names(Data)[grep("^R", names(Data))] :=
                      lapply(.SD,
                             function(x) {
                               if (all(is.na(x))) {
                                 out <- x
                               } else {
                                 out <- (length(defIN()$nlev) + 1) - x
                               }
                               sapply(out,
                                      function(y) {
                                        ifelse(is.na(y), 0, y)
                                      })
                             }),
                    .SDcols = names(Data)[grep("^R", names(Data))]]

  Importance_invRanks <- sapply(Data_inverseRanks[, mget(names(Data_inverseRanks)[grep("^R",
                                                                                       names(Data_inverseRanks))])],
                                mean)

  DT_importance[, imp3 := Importance_invRanks / sum(Importance_invRanks)]

  # Attribute counts == attribute importance

  Importance <- DT_importance[, imp3]
  names(Importance) <- names(defIN()$attLev)

  # Level counts - 100% within attributes
  LevelCounts_100 <- lapply(paste0("^A", 1:length(defIN()$nlev), "_"),
                            function(y) {
                              out <- apply(dataUSED()[, grep(y, x = names(dataUSED())),
                                                  with = FALSE], 2, sum) /
                                sum(apply(dataUSED()[, grep(y, x = names(dataUSED())),
                                                 with = FALSE], 2, sum))

                              names(out) <- defIN()$attLev[[y]]
                              out
                            })
  names(LevelCounts_100) <- names(defIN()$attLev)

  # Level counts - summing to attribute importance within attributes
  LevelCounts_rel <- lapply(1:length(defIN()$nlev),
                            function(y) {
                              out <- apply(dataUSED()[, grep(paste0("^A", y, "_"),
                                                         x = names(dataUSED())),
                                                  with = FALSE], 2, sum) /
                                sum(apply(dataUSED()[, grep(paste0("^A", y, "_"),
                                                        x = names(dataUSED())),
                                                 with = FALSE], 2, sum)) * Importance[y]

                              names(out) <- defIN()$attLev[[y]]
                              out
                            })

  names(LevelCounts_rel) <- names(defIN()$attLev)

  list(Importance = Importance,
       LevelCounts_100 = LevelCounts_100,
       LevelCounts_rel = LevelCounts_rel)

})

Imp_ordered <- reactive({

  validate(
    need(try(Importance(), silent = TRUE),
         "Wait for Imps!")
  )

  orderAtt <-  order(Importance()$Importance,
                     decreasing = TRUE)

  attLev_ordered <- defIN()$attLev

  optOrder <- orderIN()

  LevelCounts_100_ordered <- lapply(seq_along(optOrder),
                                    function(x) {
                                      out <- Importance()$LevelCounts_100[[x]][optOrder[[x]]]
                                      names(out) <- attLev_ordered[[x]][optOrder[[x]]]
                                      out
                                    })

  names(LevelCounts_100_ordered) <- names(attLev_ordered)

  LevelCounts_100_ordered <- LevelCounts_100_ordered[orderAtt]

  attLev_ordered <- lapply(seq_along(attLev_ordered),
                           function(k) {
                             attLev_ordered[[k]][optOrder[[k]]]
                           }
                           )[orderAtt]

  list(Imp = Importance()$Importance[orderAtt],
       LevCount = LevelCounts_100_ordered,
       attLev_ordered = attLev_ordered)

})

# DATA extracted !------------------------------------------------------------------------------------------------------
