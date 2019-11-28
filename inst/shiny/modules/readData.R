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