####Packages####
library(shiny)
library(shinydashboard)
library(kantardashboard)
library(KTShiny)
library(data.table)
library(DT)
#Add all required packages here

####Active Directory####
# app_id <- NULL # Azure Active Directory ID

####Server####
server <- function(input, output, session) {

  data <- list(brand = c(.5, .4, .1),
               price = c(.1, .2, .3, .4),
               benefit = c(.2, .24, .56, .4))

 lapply(seq_along(data),
         function(x) {
           id <- paste0("decMat", x)

           output[[id]] <- DT::renderDataTable({
             dt <- data.table(matrix(data[[x]], nrow = 1))
             names(dt) <- toupper(paste(names(data)[x], seq_along(data[[x]])))

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
                                            "}")))
           })
         })

  output$decMatUI <- renderUI({

    lapply(seq_along(data), function(x) {
      id <- paste0("decMat", x)
      kantarBox(DT::dataTableOutput(id),
                width = 12, title = toupper(names(data)[x]))
    })

  })

  # Log-out button - leave at end
  # observeEvent(input$logout, {
  #   session <- NULL
  #   stopApp()
  # })
}

# KTShiny::kantar_auth_server(server = server, app_id = app_id)
