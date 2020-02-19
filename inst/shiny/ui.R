####Packages####
library(shiny)
library(shinydashboard)
library(kantardashboard)
library(formattable)
library(networkD3)
library(treemap)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(sparkline)
library(scatterD3)
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
    menuItem(tabName = 'demand', text = 'Demand', icon = icon('shopping-basket')),
    menuItem(tabName = 'profile', text = 'Profiling', icon = icon('users')),
    menuItem(tabName = 'buyseg', text = 'Segmentation', icon = icon('filter')),
    menuItem(tabName = 'expansion', text = 'Expansion', icon = icon('expand')),
    menuItem(tabName = 'contract', text = 'Contraction', icon = icon('compress'))
    # ,
    # menuItem(tabName = 'portfolio', text = 'Portfolio Rankings', icon = icon('trophy'))
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
                    margin-left: -15px;}
              .nav-pills > li[class=active] > a:focus {background-color: lightgrey;
                                                       color: black}
              .nav-pills > li[class=active] > a {background-color: lightgrey;
                                                 color: black}"
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
        kantarBox(verbatimTextOutput("test"))
      ),


      tabItem(
        tabName = 'decision',
        kantarBox(div(style = "overflow-y:scroll; overflow-x:auto;
                           height:72vh; font-size:80%; margin-bottom:10px;",
                      uiOutput("decMatUI")), width = 12),
        kantarBox(uiOutput("selSegment"), br(), width = 5),
        kantarBox(uiOutput("selLevel"), br(), width = 5),
        valueBoxOutput("selectionSizeBox", width = 2)
        # ,
        # verbatimTextOutput("testDecHier")

      ),


      tabItem(
        tabName = 'demand',
        fluidRow(
          column(
            tabsetPanel(
              tabPanel(
                title = "Demand Overview",
                fluidRow(valueBoxOutput("demandBox", width = 4),
                         valueBoxOutput("compBox", width = 4),
                         valueBoxOutput("uniquenessBox", width = 4),
                         width = 12),
                kantarBox(
                  scatterD3::scatterD3Output("demandGrid"),
                          width = 12)),
              tabPanel(
                title = "Demand Strategy",
                div(style = "overflow-y: scroll; height: 80vh; font-size: 80%",
                    DT::dataTableOutput("strategyProfile"))
              )
            ),
            width = 8),
          column(kantarBox(uiOutput("attLev"),
                           title = "Select a Product",
                           width = 12),
                 width = 4)
        ),
        br(),
        hr(),
        fluidRow(verbatimTextOutput("testDemand"))
      ),


      tabItem(
        tabName = 'profile',

        tabsetPanel(
          tabPanel(
            title = "Profile Single Level",
            column(kantarBox(
              div(style = "overflow-y: scroll; overflow-x: scroll;
                  height: 80vh; font-size: 80%",
                  DT::dataTableOutput("profileLevelDT")),
                title = "Profile",
                width = 12),
              width = 8),
            kantarBox(uiOutput("selProfileLevel"),
                      br(),
                      title = "Select a Level",
                      width = 4)),
          tabPanel(
            title = "Profile Product",
            column(kantarBox(verbatimTextOutput("profileSKU"),
                             title = "Profile",
                             width = 12),
                   width = 8),
            kantarBox(uiOutput("attLev2"),
                      title = "Select a Product",
                      width = 4))
        )
      ),


      tabItem(
        tabName = 'buyseg',
        tabsetPanel(
          tabPanel(
            title = 'Segment Profile Tables',
            kantarBox(div(style = "overflow-y: scroll; height:80vh; font-size:80%;",
                          DT::dataTableOutput("profileSegDT"))),
            kantarBox(div(style = "overflow-y: scroll; height:80vh; font-size:80%;",
                          DT::dataTableOutput("profileChoDT")))),
          tabPanel(
              title = 'Select Segment Solution',
              column(kantarBox(width = 12,
                               sankeyNetworkOutput("SankeyLC")),
                     kantarBox(width = 12,
                               uiOutput("segSelectUI")
                               ),
                     width = 8),
              column(div(style = "overflow-y: scroll; height:80vh; font-size:80%;",
                         DT::dataTableOutput("segTable")),
                     width = 4)
            ))
        # , verbatimTextOutput("testSegTab")
      ),


      tabItem(
        tabName = 'expansion',
        kantarBox(div(style = "overflow-y: scroll; height: 80vh; font-size: 80%",
                      "Review overall product performance: Look for demand gaps in segments", br(), br(),
                      "Search for hypothetical new products with maximum demand and minimum portfolio overlap"),
                  title = "Expansion",
                  width = 12)
      ),


      tabItem(
        tabName = 'contract',
        kantarBox(div(style = "overflow-y: scroll; height: 80vh; font-size: 80%",
                      "Review overall product performance: Rank portfolio products by demand in total sample", br(), br(),
                      "Review overall product performance: Compare products by demand in segments", br(), br(),
                      "Count portfolio products with same or higher demand value"),
                  title = "Contraction",
                  width = 12)
      )
    )
  ),

  #Title change from default
  title = 'Market Structure'
)

# KTShiny::kantar_auth_ui(ui = ui, app_id = app_id)
