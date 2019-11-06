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

####Active Directory####
# app_id <- NULL # Azure Active Directory ID

####Server####
server <- function(input, output, session) {


  # READ DATA !---------------------------------------------------------------------------------------------------------

  # labels
  def <- readLines("www/VersicherungsCase.def", encoding = "UTF-8")

  # choices, combinations and ranks
  SKU_choice_DT <- data.table::fread("www/simulated_data_VersicherungsCase_ASD.csv")

  # DATA READ COMPLETED !-----------------------------------------------------------------------------------------------



  # EXCTRACT Definition DATA !------------------------------------------------------------------------------------------

  defIN <- reactive({

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

    # ASD adjustments
    attLev_asd <- attLev[-(3:5)]
    attLev_asd$Jahresbeitrag <- paste0(rep(names(attLev[3:5]), nlev[3:5]), " - ", sequence(nlev[3:5]))
    attLev_asd <- attLev_asd[c(1, 2, length(attLev_asd), 3:(length(attLev_asd) - 1))]
    nlev_asd <- sapply(attLev_asd, length)

    list(attLev_asd = attLev_asd,
         nlev_asd = nlev_asd)

  })
  # Definition data extracted !-----------------------------------------------------------------------------------------


  # EXCTRACT DATA !-----------------------------------------------------------------------------------------------------

  # choice data
  dataIN <- reactive({

    Data_asd <- unique(SKU_choice_DT[, -(2:(length(defIN()$nlev_asd) + 2)), with = FALSE])

    list(SKU_choice_DT = SKU_choice_DT,
         Data_asd = Data_asd)

  })

  # calculate importances and counts
  Importance <- reactive({

    # Attribute counts == attribute importance

    Importance <- sapply(lapply(dataIN()$Data_asd[, grep("^Rank",
                                                         names(dataIN()$Data_asd)),
                                                  with = FALSE],
                                table), prop.table)[1, ]
    names(Importance) <- names(defIN()$attLev_asd)

    # Level counts - 100% within attributes
    LevelCounts_100 <- lapply(paste0("^Att", 1:length(defIN()$nlev_asd), "L"),
                              function(y) {
                                out <- apply(dataIN()$Data_asd[, grep(y, x = names(dataIN()$Data_asd)),
                                                               with = FALSE], 2, sum) /
                                  sum(apply(dataIN()$Data_asd[, grep(y, x = names(dataIN()$Data_asd)),
                                                              with = FALSE], 2, sum))

                                names(out) <- defIN()$attLev_asd[[y]]
                                out
                              })
    names(LevelCounts_100) <- names(defIN()$attLev_asd)

    # Level counts - summing to attribute importance within attributes
    LevelCounts_rel <- lapply(1:length(defIN()$nlev_asd),
                              function(y) {
                                out <- apply(dataIN()$Data_asd[, grep(paste0("^Att", y, "L"),
                                                                      x = names(dataIN()$Data_asd)),
                                                               with = FALSE], 2, sum) /
                                  sum(apply(dataIN()$Data_asd[, grep(paste0("^Att", y, "L"),
                                                                     x = names(dataIN()$Data_asd)),
                                                              with = FALSE], 2, sum)) * Importance[y]

                                names(out) <- defIN()$attLev_asd[[y]]
                                out
                              })

    names(LevelCounts_rel) <- names(defIN()$attLev_asd)

    list(Importance = Importance,
         LevelCounts_100 = LevelCounts_100,
         LevelCounts_rel = LevelCounts_rel)

  })

  # # choice frequencies for combinations
  # SKU_comb_freq <- reactive({
  #
  #   list(SKU_choice_freq = SKU_choice_DT[, .(.N), by = .(Comb)][order(-N)],
  #        SKUs_per_person = SKU_choice_DT[, lapply(.SD, list), by = ID,
  #                                        .SDcols = "Comb"])
  #
  # })
  # DATA extracted !----------------------------------------------------------------------------------------------------


  # data <- list(brand = c(.5, .4, .1),
  #              price = c(.1, .2, .3, .4),
  #              benefit = c(.2, .24, .56, .4))
  #
  #
  # combs <- data.frame(expand.grid(lapply(data, seq_along)),
  #                     freq = apply(expand.grid(data), 1, prod))

  # OUTPUTS !-----------------------------------------------------------------------------------------------------------

  # set up output list; one datatable per attribute --> will be shown by renderUI in the next step
  observe({

    lapply(seq_along(Importance()$LevelCounts_100),
           function(x) {
             id <- paste0("decMat", x)

             output[[id]] <- DT::renderDataTable({
               dt <- data.table(matrix(Importance()$LevelCounts_100[[x]],
                                       nrow = 1))
               names(dt) <- defIN()$attLev_asd[[x]]

               DT::datatable(dt, selection = list(mode = 'single', target = 'column'),
                             filter = "none", autoHideNavigation = TRUE, rownames = FALSE,
                             escape = FALSE, style = "default", class = 'cell-border',
                             options = list(columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                            pageLength = 1,
                                            ordering = FALSE,
                                            dom = 't',
                                            initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#989898',
                                             'color': '#fff'});",
                                              "}"))) %>%
                 formatPercentage(names(dt),  digits = 1)
             })
           })

  })

  # table like output for counts
  output$decMatUI <- renderUI({

    lapply(seq_along(Importance()$LevelCounts_100), function(x) {
      id <- paste0("decMat", x)
      kantarBox(DT::dataTableOutput(id),
                width = 12, title = toupper(names(Importance()$LevelCounts_100)[x]))
    })

  })


  # SANKEY DIAGRAMM
  output$Sankey <- renderSankeyNetwork({

    orderAtt <-  order(Importance()$Importance,
                       decreasing = TRUE)

    DThelp <- data.table(sapply(seq_along(defIN()$attLev_asd),
                                function(x) {
                                  sapply(unlist(dataIN()$SKU_choice_DT[, paste0("Att", orderAtt[x]), with = FALSE]),
                                         function(y) {
                                           paste0(sprintf("%02d", x), sprintf("%02d", y), "_",
                                                  defIN()$attLev_asd[[orderAtt[x]]][y])
                                         })
                                }))

    helpList <- lapply(1:(length(defIN()$nlev_asd) - 1),
                       function(x) {

                         LoopDT <- DThelp[, c(x, x + 1), with = FALSE]
                         names(LoopDT) <- c("L1", "L2")

                         LoopDT[, c(total = .(.N)),
                                by = .(Var1 = pmin(L1, L2),
                                       Var2 = pmax(L1, L2))][order(Var1, Var2)]
                       })

    lookup <- data.table(Var1 = unlist(sapply(seq_along(defIN()$attLev_asd),
                                              function(x) {
                                                sapply(seq_along(defIN()$attLev_asd[[orderAtt[x]]]),
                                                       function(y) {
                                                         paste0(sprintf("%02d", x), sprintf("%02d", y), "_",
                                                                defIN()$attLev_asd[[orderAtt[x]]][y])
                                                       })
                                              })),
                         Var2 = unlist(sapply(seq_along(defIN()$attLev_asd),
                                              function(x) {
                                                sapply(seq_along(defIN()$attLev_asd[[orderAtt[x]]]),
                                                       function(y) {
                                                         paste0(sprintf("%02d", x), sprintf("%02d", y), "_",
                                                                defIN()$attLev_asd[[orderAtt[x]]][y])
                                                       })
                                              })),
                         code = sequence(sum(defIN()$nlev_asd)) - 1)

    helpLinks <- Reduce(rbind, helpList)

    helpLinks <- helpLinks[lookup[, .(Var1, code)], on = "Var1", nomatch = 0]
    helpLinks <- helpLinks[lookup[, .(Var2, code)], on = "Var2", nomatch = 0]


    sankeyLinks <- helpLinks[, .(code, i.code, total)][order(code, i.code)]
    names(sankeyLinks) <- c("source", "target", "value")

    sankeyNodes <- data.frame(name = unlist(defIN()$attLev_asd[orderAtt]))

    sankeyNetwork(Links = sankeyLinks, Nodes = sankeyNodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize = 12, nodeWidth = 15)

  })


  # Treemap
  output$treemap <- renderPlot({

    group <- rep(names(defIN()$attLev_asd[order(-Importance()$Importance)]),
                 defIN()$nlev_asd[order(-Importance()$Importance)])

    # subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep = "-")
    subgroup <- as.vector(unlist(defIN()$attLev_asd[order(-Importance()$Importance)]))

    # value <- c(13,5,22,12,11,7,3,1,23)
    value <- unlist(Importance()$LevelCounts_rel[order(-Importance()$Importance)])

    data <- data.frame(group, subgroup, value)

    # treemap
    treemap(data,
            index = c("group", "subgroup"),
            vSize = "value",
            type = "index"
    )

  })



  # Combinations !------------------------------------------------------------------------------------------------------

  combinations <- reactive({

    combs <- dataIN()$SKU_choice_DT[, .(.N), by = .(Att1, Att2, Att3, Att4, Att5, Att6, Att7, Att8)][order(-N)]
    combs[, Combination := paste0(Att1, Att2, Att3, Att4, Att5, Att6, Att7, Att8)]

    combs_labels <- data.table(Combination = combs[, Combination],
                               sapply(1:length(defIN()$nlev_asd),
                                      function(x) {
                                        sapply(unlist(combs[, x, with = FALSE]),
                                               function(y) {
                                                 do.call(switch, c(y, as.list(defIN()$attLev_asd[[x]])))
                                               })
                                      }),
                               N = combs$N)

    names(combs_labels) <- c("Combination", names(defIN()$attLev_asd), "N")

    list(combs = combs,
         combs_labels = combs_labels)

  })


  output$portTable <- DT::renderDataTable({

    DT::datatable(combinations()$combs_labels, selection = list(mode = 'single', target = 'row'),
                  filter = "none", autoHideNavigation = TRUE, rownames = TRUE,
                  escape = FALSE, style = "default", class = 'compact',
                  options = list(pageLength = 5,
                                 dom = 'flrtip',
                                 order = list(list(ncol(combinations()$combs_labels), 'desc')),
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                   "}"),
                                 lengthMenu = list(c(5, 25, -1),
                                                   c('5', '25', 'All'))))
  })

  corDist <- reactive({

    dummyChoice <- data.table::dcast(dataIN()$SKU_choice_DT[,.(ID, Comb)], ID ~ Comb)

    helpCor <- data.table::data.table(dummyChoice[, "ID"],
                          !is.na(dummyChoice[, -1, with = FALSE])) * 1

    stats::cor(helpCor[, -1, with = FALSE])

  })

  output$substitution <- DT::renderDataTable({

    indexCor <- which(combinations()$combs_labels[input$portTable_rows_selected, Combination] == colnames(corDist()))

    sortDist <- sort(corDist()[, indexCor], decreasing = TRUE)
    sortDist <- sortDist[sortDist > 0]

    tableOut <- data.table::data.table(Combination = names(sortDist),
                                       Distance = sortDist)

    DT::datatable(tableOut, selection = list(mode = 'single', target = 'row'),
                  filter = "none", autoHideNavigation = TRUE, rownames = TRUE,
                  escape = FALSE, style = "default", class = 'compact',
                  options = list(pageLength = 10,
                                 dom = 'lrtip',
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#989898',
                                 'color': '#fff'});",
                                   "}"),
                                 lengthMenu = list(c(10, 25, -1),
                                                   c('10', '25', 'All')))) %>%
      DT::formatStyle(columns = "Distance",
                      background = DT::styleColorBar(c(0, 1.5),
                                                     '#f2da64', angle = 270),
                      backgroundSize = '90% 50%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center') %>%
      DT::formatRound(columns = "Distance", digits = 2)

  })

  output$test <- renderPrint({

    which(combinations()$combs_labels[input$portTable_rows_selected, Combination] == colnames(corDist()))


  })


  # output$portGRID <- DT::renderDataTable({
  #
  #   combs <- dataIN()$SKU_choice_DT[, .(.N), by = .(Att1, Att2, Att3, Att4, Att5, Att6, Att7, Att8)][order(-N)]
  #
  #   helpVec <- rep(NA, 100)
  #   helpVec[1:min(100, length(combs$N))] <- sort(combs$N, decreasing = TRUE)[1:min(100, length(combs$N))]
  #   # helpVec[!is.na(helpVec)] <- 1
  #   dt <- data.table::data.table(matrix(helpVec, nrow = 10, ncol = 10, byrow = TRUE))
  #
  #   DT::datatable(dt, selection = list(mode = 'single', target = 'cell'),
  #                 filter = "none", autoHideNavigation = TRUE, rownames = TRUE,
  #                 escape = FALSE, style = "default", class = 'cell-border',
  #                 options = list(pageLength = 10,
  #                                dom = 't',
  #                                ordering = FALSE,
  #                                initComplete = JS(
  #                                  "function(settings, json) {",
  #                                  "$(this.api().table().header()).css({'background-color': '#989898',
  #                                'color': '#fff'});",
  #                                  "}"))) %>%
  #     formatStyle(names(dt),
  #                 color = "#f2da64",
  #                 backgroundColor = "#f2da64",
  #                 # background = DT::styleColorBar(c(0, 1),
  #                 #                                '#f2da64', angle = 270),
  #                 backgroundSize = '90% 80%',
  #                 backgroundRepeat = 'no-repeat',
  #                 backgroundPosition = 'center') %>%
  #     formatRound(names(dt),  digits = 0)
  # })
  #
  # output$portGRID2 <- renderFormattable({
  #
  #   combs <- dataIN()$SKU_choice_DT[, .(.N), by = .(Att1, Att2, Att3, Att4, Att5, Att6, Att7, Att8)][order(-N)]
  #
  #   helpVec <- rep(NA, 100)
  #   helpVec[1:min(100, length(combs$N))] <- sort(combs$N, decreasing = TRUE)[1:min(100, length(combs$N))]
  #   # helpVec[!is.na(helpVec)] <- 1
  #   helpVec[is.na(helpVec)] <- 0
  #   dt <- data.table::data.table(matrix(helpVec, nrow = 10, ncol = 10, byrow = TRUE))
  #
  #   formattable(dt, list(
  #     V1 = color_tile("#f2da64", "#f2da64"),
  #     V2 = color_tile("#f2da64", "#f2da64"),
  #     V3 = color_tile("#f2da64", "#f2da64"),
  #     V4 = color_tile("#f2da64", "#f2da64"),
  #     V5 = color_tile("#f2da64", "#f2da64"),
  #     V6 = color_tile("#f2da64", "#f2da64"),
  #     V7 = color_tile("#f2da64", "#f2da64"),
  #     V8 = color_tile("#f2da64", "#f2da64"),
  #     V9 = color_tile("#f2da64", "#f2da64"),
  #     V10 = color_tile("#f2da64", "#f2da64")
  #   ))
  #
  # })

  # Log-out button - leave at end
  # observeEvent(input$logout, {
  #   session <- NULL
  #   stopApp()
  # })
}

# KTShiny::kantar_auth_server(server = server, app_id = app_id)
