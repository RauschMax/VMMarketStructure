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
app_id <- NULL # Azure Active Directory ID

options(shiny.port = 8100)
# app_id <- '534994ee-483d-4797-9130-6e68a4d3d671'
resource_id <- '642effb4-3a09-4b9a-8361-0dcf5eb4e9ed'
app_url <- ifelse(interactive(), "http://localhost:8100", "http://10.236.230.79/BeastApps/VMMarketStructure/")


####Server####
server <- function(input, output, session) {

  # LOAD MODULES !------------------------------------------------------------------------------------------------------
  ## Read Files from BLOB storage
  source("./modules/readData.R",
         local = TRUE)$value

  ## Decision Matrix
  source("./modules/impTable.R",
         local = TRUE)$value

  ## Importance Diagrams - Sankey, Treemap
  source("./modules/impDiagrams.R",
         local = TRUE)$value

  ## Segmentation Tab
  source("./modules/segTab.R",
         local = TRUE)$value

  ## Demand & Substitution Table
  source("./modules/demandSubstTab.R",
         local = TRUE)$value

  ## Substitution Table
  source("./modules/substTable.R",
         local = TRUE)$value

  # MODULES LOADED !----------------------------------------------------------------------------------------------------


  # Study Overview
  output$attLev <- renderUI({

    validate(
      need(defIN(), "Please load the data.")
    )

    nAttr <- length(defIN()$nlev)
    lapply(1:(nAttr),
           function(i) {
             choList <- as.list(seq_along(defIN()$attLev[[i]]))
             names(choList) <- defIN()$attLev[[i]]
             selectInput(paste0("ShowAtt", i),
                         label = paste(names(defIN()$attLev)[i],
                                       " (", length(defIN()$attLev[[i]]), " Levels)",
                                       sep = "", collapse = " "),
                         choices = choList,
                         selected = NULL,
                         width = '100%')
           })
  })



  output$test <- renderPrint({

    list(names(session$clientData),
         session$clientData$pixelratio,
         session$clientData$url_search)


  })



  output$test2 <- renderPrint({

    list(dataUSED(),
         selectionSize())

  })

  output$testDecHier <- renderPrint({

    diagramData()

  })


  # output$portGRID <- DT::renderDataTable({
  #
  #   combs <- SKU_choice_DT()[, .(.N), by = .(Att1, Att2, Att3, Att4, Att5, Att6, Att7, Att8)][order(-N)]
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
  #   combs <- SKU_choice_DT()[, .(.N), by = .(Att1, Att2, Att3, Att4, Att5, Att6, Att7, Att8)][order(-N)]
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
  observeEvent(input$logout, {
    session <- NULL
    stopApp()
  })
}

# KTShiny::kantar_auth_server(server = server, app_id = app_id)
