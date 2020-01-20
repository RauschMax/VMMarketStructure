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
    menuItem(tabName = 'home', text = 'Overview', icon = icon('eye'), selected = TRUE),
    menuItem(tabName = 'decision', text = 'Decision Hierarchy', icon = icon('sort-amount-desc')),
    menuItem(tabName = 'buyseg', text = 'Segmentation of Buyers', icon = icon('users')),
    menuItem(tabName = 'demand', text = 'Demand & Substitution', icon = icon('shopping-basket')),
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
        h2("HOME TAB"),
        # kantarBox(uiOutput("attLev")),
        kantarBox(verbatimTextOutput("test2"))
      ),


      tabItem(
        tabName = 'decision',
        tabBox(
            tabPanel(
              title = 'Decision Matrix',
              div(style = "overflow-y:scroll; overflow-x:auto;
                           height:72vh; font-size:80%; margin-bottom:10px;",
                  uiOutput("decMatUI")
              )),
            tabPanel(
              title = 'Decision Sankey',
              div(style = "height:72vh; margin-bottom:10px;",
                  sankeyNetworkOutput("Sankey"))
            ), width = 12),
        kantarBox(h4("Segment Selection"),
                  uiOutput("selSegment"), width = 6),
        kantarBox(h4("Level Selection"), width = 6)

        # style = "overflow-y:scroll; overflow-x:auto; height:15vh;",

        # ,
        # kantarBox(title = "Selected Combination",
        #   uiOutput("selComb"),
        #   width = 4),
        # valueBoxOutput("demandBox", width = 4),
        # valueBoxOutput("supplyBox", width = 4),
        # valueBoxOutput("incrementBox", width = 4),
        # kantarBox(
        #   div(style = "overflow-y:scroll; overflow-x:scroll;
        #                height:42vh; width:100%; font-size:80%",
        #       verbatimTextOutput("test2")),
        #   width = 4)
      ),


      tabItem(
        tabName = 'buyseg',
        tabBox(
          tabPanel(
            title = 'Select Segment Solution',
            column(kantarBox(width = 12,
                             sankeyNetworkOutput("SankeyLC")),
                   kantarBox(width = 12,
                             uiOutput("segSelectUI")),
                   width = 8),
            column(div(style = "overflow-y: scroll; height:80vh; font-size:80%;",
                                 DT::dataTableOutput("segTable")),
                   width = 4)
          ),
          tabPanel(
            title = 'Segment Profile',
            "Segment Profile"),
          tabPanel(
            title = 'Personas',
            "Segment Personas"),
          width = 12)
      ),


      tabItem(
        tabName = 'demand',
        column(kantarBox(valueBoxOutput("demandBox", width = 6),
                         valueBoxOutput("compBox", width = 6),
                         width = 12),
               kantarBox("Substituion Matrix",
                         width = 12),
               width = 8),
        kantarBox(uiOutput("attLev"),
                  width = 4)

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
