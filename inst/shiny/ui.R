####Packages####
library(shiny)
library(shinydashboard)
library(kantardashboard)
library(KTShiny)
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

####USER INTERFACE####
ui <- kantarPage(
  ####Header####
  #Title is Kantar Logo - DO NOT REMOVE
  header = fluidRow(
    column(width = 12,
           tags$div(class = "head",
                    tags$div(class = "color-line nav-header"),
                    tags$div(class = "textMenu",
                             tags$span(class = "KT", "Kantar "),
                             tags$span(class = "kgold", 'MarketStructure')),
                    tags$div(class = "LogoMenu",
                             tags$img(class = "mb-3 mt-3 logo",
                                      src = web_dependencies$img),
                             tags$li(class = 'dropdown',
                                     tags$a(href = '#', class = 'dropdown-toggle', `data-toggle` = 'dropdown',
                                            tags$i(class = 'fa fa-power-off')
                                     ),
                                     tags$ul(class = 'dropdown-menu',
                                             tags$li(tags$li(actionLink('logout', 'Log out')))
                                     )
                             ),
                             tags$li(class = 'dropdown',
                                     tags$a(href = '#',
                                            class = 'dropdown-toggle', `data-toggle` = 'dropdown',
                                            tags$i(class = 'fa fa-info')
                                     ),
                                     tags$ul(class = 'dropdown-menu',
                                             tags$li(a(href = "https://www.kantartns.de/valuemanager/index.asp",
                                                       tags$i(class = 'fa fa-globe'),
                                                       HTML(" ValueManager<br>Website"))),
                                             tags$li(a(href = "https://www.kantardeutschland.de/impressum/",
                                                       tags$i(class = 'fa fa-info'),
                                                       HTML(" Impressum")))
                                     )),
                             tags$li(class = 'dropdown',
                                     tags$a(href = '#',
                                            class = 'dropdown-toggle', `data-toggle` = 'dropdown',
                                            tags$i(class = 'fa fa-sign-in')
                                     ),
                                     tags$ul(class = 'dropdown-menu',
                                             tags$li(actionButton("go", "GO")),
                                             tags$li(textInput("study", "Enter Study ID:")),
                                             tags$li(passwordInput("pw", "Enter Password:"))
                                     )))
           )
  )
  ),

  ####Sidebar####
  sidebar = sidebarMenu(
    menuItem(tabName = 'home', text = 'Overview', icon = icon('home'), selected = TRUE),
    menuItem(tabName = 'decision', text = 'decision matrix', icon = icon('shopping-cart')),
    menuItem(tabName = 'sankey', text = 'Sankey Diagram', icon = icon('road')),
    menuItem(tabName = 'tree', text = 'Treemap', icon = icon('tree')),
    menuItem(tabName = 'portfolio', text = 'Portfolio Rankings', icon = icon('trophy'))
  ),

  ####Body####
  body = kantarBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(
        HTML(".shiny-input-container {margin-bottom:2px;
                                      margin-top: 2px;
                                      font-size:10px;}
              .form-control {font-size:10px;
                             height:20px;}
              .shiny-notification {position: fixed;
                                   top: 33%;
                                   left: 33%;
                                   right: 33%;}
              .text-left {height: 93vh !important;}
              .dropdown {list-style-type: none;
                         margin-top: 2%;
                         margin-right: 20px;
                         float: right;}
              .row {margin-right: 0;
                    margin-left: -15px;}"
        ))),

    conditionalPanel("($('html').hasClass('shiny-busy'))",
                     absolutePanel(style = "text-align:center; z-index:500;",
                                   tags$img(src = 'loading_small.gif', width = '50px'),
                                   fixed = TRUE, draggable = TRUE)),

    tabItems(
      tabItem(
        tabName = 'home',
        h2(),
        kantarBox(uiOutput("attLev"))
      ),


      tabItem(
        tabName = 'decision',
        column(
          div(style = "overflow-y:scroll; overflow-x:auto;
                       height:92vh; font-size:80%",
              uiOutput("decMatUI")
              ),
               width = 8),
        kantarBox(title = "Selected Combination",
          uiOutput("selComb"),
          width = 4),
        valueBoxOutput("demandBox", width = 4),
        valueBoxOutput("supplyBox", width = 4),
        valueBoxOutput("incrementBox", width = 4),
        kantarBox(
          div(style = "overflow-y:scroll; overflow-x:scroll;
                       height:42vh; width:100%; font-size:80%",
              verbatimTextOutput("test2")),
          width = 4)
      ),


      tabItem(
        tabName = 'sankey',
        sankeyNetworkOutput("Sankey")
      ),


      tabItem(
        tabName = 'tree',
        plotOutput("treemap")
      ),


      tabItem(
        tabName = 'portfolio',
        kantarBox(div(style = "overflow-y: scroll; height: 38vh; font-size: 80%",
                      DT::dataTableOutput("portTable")),
                  width = 12),
        kantarBox(div(style = "overflow-y: scroll; height: 45vh; font-size: 80%",
                      DT::dataTableOutput("substitution"))),
        kantarBox(div(style = "overflow-y: scroll; height: 45vh; font-size: 80%",
                      verbatimTextOutput("test")))
        # kantarBox(DT::dataTableOutput("portGRID")),
        # kantarBox(formattableOutput("portGRID2"))
      )
    )
  ),

  #Title change from default
  title = 'Market Structure'
)

# KTShiny::kantar_auth_ui(ui = ui, app_id = app_id)
