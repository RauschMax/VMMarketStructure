# READ DATA !---------------------------------------------------------------------------------------------------------

# read Password !----
pw <- eventReactive(input$go, {

  validate(
    need(!is.null(input$study), "Wait for it!")
  )

  get_pw <- BeastRServer::azure_blob_call("GET",
                                          storage_account = "shinyapp",
                                          storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                               "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                               "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                          container = paste0("ms", input$study),
                                          blob = paste0(input$study, "_pw.txt"))

  pw <- httr::content(get_pw, as = "text", encoding = "UTF-8")

  pw

})


# labels
def <- eventReactive(input$go, {

  validate(
    need(!is.null(input$study), "Wait for it!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    get_def <- BeastRServer::azure_blob_call("GET",
                                             storage_account = "shinyapp",
                                             storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                  "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                  "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                             container = paste0("ms", input$study),
                                             blob = "Attributes.def")

    strsplit(httr::content(get_def, as = "text", encoding = "UTF-8"), "\r\n")[[1]]
  }

})

# choices, combinations and ranks
csv <- eventReactive(input$go, {

  validate(
    need(input$study, "Wait for it!")
  )

  validate(
    need(pw(), "Wait for it!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    get_csv <- BeastRServer::azure_blob_call("GET",
                                             storage_account = "shinyapp",
                                             storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                  "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                  "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                             container = paste0("ms", input$study),
                                             blob = "Data.csv")

    csvIN <- as.data.table(httr::content(get_csv, type = "text/csv", encoding = "UTF-8"))

    csvIN
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
# Definition data extracted !-----------------------------------------------------------------------------------------


# EXCTRACT DATA !-----------------------------------------------------------------------------------------------------

# choice data
dataIN <- reactive({

  validate(
    need(csv(), "Wait for it!")
  )

  SKU_choice_DT <- csv()

  Data <- unique(SKU_choice_DT[, -(2:(length(defIN()$nlev) + 2)), with = FALSE])

  list(SKU_choice_DT = SKU_choice_DT,
       Data = Data)

})

# calculate importances and counts
Importance <- reactive({

  # Attribute counts == attribute importance

  Importance <- sapply(dataIN()$Data[, grep("^Rank",
                                   names(dataIN()$Data)),
                            with = FALSE],
                       function(x) {
                         sum(x %in% 1:2, na.rm = TRUE)
                       })
  Importance <- Importance / sum(Importance)
  names(Importance) <- names(defIN()$attLev)

  # Level counts - 100% within attributes
  LevelCounts_100 <- lapply(paste0("^A", 1:length(defIN()$nlev), "_"),
                            function(y) {
                              out <- apply(dataIN()$Data[, grep(y, x = names(dataIN()$Data)),
                                                             with = FALSE], 2, sum) /
                                sum(apply(dataIN()$Data[, grep(y, x = names(dataIN()$Data)),
                                                            with = FALSE], 2, sum))

                              names(out) <- defIN()$attLev[[y]]
                              out
                            })
  names(LevelCounts_100) <- names(defIN()$attLev)

  # Level counts - summing to attribute importance within attributes
  LevelCounts_rel <- lapply(1:length(defIN()$nlev),
                            function(y) {
                              out <- apply(dataIN()$Data[, grep(paste0("^A", y, "_"),
                                                                    x = names(dataIN()$Data)),
                                                             with = FALSE], 2, sum) /
                                sum(apply(dataIN()$Data[, grep(paste0("^A", y, "_"),
                                                                   x = names(dataIN()$Data)),
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
         "Wait for it!")
  )

  orderAtt <-  order(Importance()$Importance,
                     decreasing = TRUE)

  attLev_ordered <- defIN()$attLev[orderAtt]
  nlev_ordered <- defIN()$nlev[orderAtt]

  optOrder <- vector("list", length(nlev_ordered))
  optOrder[[1]] <- seq_along(attLev_ordered[[1]])

  optOrder[2:length(nlev_ordered)] <- lapply(2:length(nlev_ordered),
                                             function(i) {

                                               AttSel <- paste0("A",
                                                                rep(orderAtt[i], nlev_ordered[i]),
                                                                "_",
                                                                sequence(nlev_ordered[i]))

                                               DTdistHelp <- dataIN()$SKU_choice_DT[, lapply(.SD, sum),
                                                                           by = get(paste0("Att", orderAtt[i - 1])),
                                                                           .SDcols = AttSel]

                                               DTdistHelp <- scale(DTdistHelp[, get := NULL])

                                               # correct for attributes without variance (no chosen at all)
                                               attributes(DTdistHelp)$`scaled:center` == 0
                                               DTdistHelp[, which(attributes(DTdistHelp)$`scaled:center` == 0)] <- 0

                                               increment_low <- seq(-1, 0, length = nlev_ordered[i - 1])
                                               increment_high <- seq(0, 1, length = nlev_ordered[i - 1])

                                               distHelp <- rbindlist(lapply(sequence(nlev_ordered[i - 1]),
                                                                            function(j) {
                                                                              data.frame(matrix(seq(increment_low[j], increment_high[j],
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
                                               helpSample[order(criteria)]

                                               helpSample[, as.numeric(1:nlev_ordered[i]), with = FALSE]

                                               orderInd <- unlist(helpSample[1, as.numeric(1:nlev_ordered[i]),
                                                                             with = FALSE])

                                               orderInd
                                             })

  optOrder

  LevelCounts_100_ordered <- lapply(seq_along(optOrder),
                                    function(x) {
                                      out <- Importance()$LevelCounts_100[orderAtt][[x]][optOrder[[x]]]
                                      names(out) <- attLev_ordered[[x]][optOrder[[x]]]
                                      out
                                    })

  names(LevelCounts_100_ordered) <- names(attLev_ordered)

  attLev_ordered <- lapply(seq_along(attLev_ordered),
                           function(k) {
                             attLev_ordered[[k]][optOrder[[k]]]
                             })

  list(Imp = Importance()$Importance[orderAtt],
       LevCount = LevelCounts_100_ordered,
       attLev_ordered = attLev_ordered)


})

# choice frequencies for combinations
SKU_comb_freq <- reactive({

  # list(SKU_choice_freq = SKU_choice_DT[, .(.N), by = .(Comb)][order(-N)],
  #      SKUs_per_person = SKU_choice_DT[, lapply(.SD, list), by = ID,
  #                                      .SDcols = "Comb"])

  SKUs_per_person <- dataIN()$SKU_choice_DT[, lapply(.SD, list), by = ID,
                                            .SDcols = "Comb"]

  SKUs_per_person

})
# DATA extracted !------------------------------------------------------------------------------------------------------