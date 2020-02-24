####Packages####
library(shiny)
library(shinydashboard)
library(kantardashboard)
library(data.table)
library(DT)
library(formattable)
library(networkD3)
library(treemap)
library(sparkline)
library(scatterD3)
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

  ## Demand & Substitution Table
  source("./modules/demandSubstTab.R",
         local = TRUE)$value

  ## Segmentation Tab
  source("./modules/profileTab.R",
         local = TRUE)$value

  ## Segmentation Tab
  source("./modules/segTab.R",
         local = TRUE)$value

  ## Segmentation Tab
  source("./modules/expanseTab.R",
         local = TRUE)$value

  ## Segmentation Tab
  source("./modules/contractTab.R",
         local = TRUE)$value

  # ## Importance Diagrams - Sankey, Treemap
  # source("./modules/impDiagrams.R",
  #        local = TRUE)$value

  # ## Substitution Table
  # source("./modules/substTable.R",
  #        local = TRUE)$value

  # MODULES LOADED !----------------------------------------------------------------------------------------------------


  output$test <- renderPrint({

    list(summary(Demand_DT()[, demand]),
         nrow(SKUinput()),
         selectionSize())

  })

  # Log-out button - leave at end
  observeEvent(input$logout, {
    session <- NULL
    shiny::stopApp()
  })
}

# KTShiny::kantar_auth_server(server = server, app_id = app_id)
