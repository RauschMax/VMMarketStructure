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


# def file
def <- eventReactive(input$go, {

  validate(
    need(!is.null(input$study), "Please load Study with StudyID and password!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    # def file
    get_def <- BeastRServer::azure_blob_call("GET",
                                             storage_account = "shinyapp",
                                             storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                  "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                  "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                             container = paste0("ms", input$study),
                                             blob = "def.txt")

    def <- strsplit(httr::content(get_def, as = "text", encoding = "UTF-8"),
                    "\r\n")[[1]]
    print("def file read")
    def
  }

})

# choices, combinations and ranks + segments
dataIN <- eventReactive(input$go, {

  validate(
    need(input$study, "Please load Study with StudyID and password!")
  )

  validate(
    need(pw(), "Wait for it!")
  )

  if (isolate(input$pw) == isolate(pw())) {
    # choice data
    get_data <- BeastRServer::azure_blob_call("GET",
                                              storage_account = "shinyapp",
                                              storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                   "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                   "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                              container = paste0("ms", input$study),
                                              blob = "data.csv")

    dataIN <- data.table(httr::content(get_data, type = "text/csv", encoding = "UTF-8"))
    print("data file read")
    dataIN
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
    # Demand
    get_demand <- BeastRServer::azure_blob_call("GET",
                                                storage_account = "shinyapp",
                                                storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
                                                                     "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
                                                                     "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
                                                container = paste0("ms", input$study),
                                                blob = "demand.csv")
    Demand_DT <- data.table(httr::content(get_demand, type = "text/csv",
                                          encoding = "UTF-8"))
    rm(get_demand)
    print("demand file read")
    Demand_DT
  }
})


# Latent Class segmentation
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


# # existing SKUs
# SKUinput <- eventReactive(input$go, {
#
#   validate(
#     need(!is.null(input$study), "Please load Study with StudyID and password!")
#   )
#
#   validate(
#     need(defIN(), "Wait for def file to be extracted!")
#   )
#
#   if (isolate(input$pw) == isolate(pw())) {
#     get_SKUs <- BeastRServer::azure_blob_call("GET",
#                                               storage_account = "shinyapp",
#                                               storage_key = paste0("o4PoNgKwzu76hDcjdqgOEdH+J5d6",
#                                                                    "Qp+UYHW8CCyOf/WBtYTspa0VT+z7",
#                                                                    "DJcAWE80GlefAbw+XKp6DUtZKQIFCw=="),
#                                               container = paste0("ms", input$study),
#                                               blob = "existingSKUs.csv")
#
#     if  (get_SKUs$status_code == 404) {
#       SKUinput <- NULL
#     } else {
#       SKUinput <- data.table(httr::content(get_SKUs, type = "text/csv", encoding = "UTF-8"))
#       rm(get_SKUs)
#
#       names(SKUinput) <- c("Portfolio", "SKU",
#                            paste0("A", rep(seq_along(defIN()$nlev),
#                                            defIN()$nlev), "_",
#                                   sequence(defIN()$nlev)))
#
#       # add SKU by Attribute part
#       sapply(seq_along(defIN()$nlev),
#              function(x) {
#                SKUinput[, paste0("A", x) := apply(.SD, 1, which.max),
#                         .SDcols = paste0("A", x, "_", sequence(defIN()$nlev[x]))]
#
#                SKUinput[, paste0("A", x) := sapply(get(paste0("A", x)),
#                                                    function(y) {
#                                                      check_y <- ifelse(length(y) == 0, NA, y)
#                                                      if (is.na(check_y)) {
#                                                        0
#                                                      } else {
#                                                        check_y
#                                                      }
#                                                    })]
#              })
#
#     }
#
#     SKUinput
#   }
# })
# DATA READ COMPLETED !-----------------------------------------------------------------------------------------------
