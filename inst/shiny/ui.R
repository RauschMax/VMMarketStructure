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
# app_id <- NULL # Azure Active Directory ID

####USER INTERFACE####
ui <- kantarPage(
  ####Header####
  #Title is Kantar Logo - DO NOT REMOVE
  header = fluidRow(column(width = 12,
                           tags$div(class = "head",
                                    tags$div(class = "color-line nav-header"),
                                    tags$div(class = "textMenu",
                                             tags$span(class = "KT", "Kantar "),
                                             tags$span(class = "kgold", 'MarketStructure')),
                                    tags$div(class = "LogoMenu",
                                             tags$img(class = "mb-3 mt-3 logo",
                                                      src = web_dependencies$img),
                                             tags$li(class = 'dropdown',
                                                     tags$a(href = '#',
                                                            class = 'dropdown-toggle', `data-toggle` = 'dropdown',
                                                            tags$i(class = 'fa fa-sign-in')
                                                     ),
                                                     tags$ul(class = 'dropdown-menu',
                                                             tags$li(actionButton("go", "GO")),
                                                             tags$li(textInput("study", "Enter Study ID:")),
                                                             tags$li(passwordInput("pw", "Enter Password:"))
                                                     )),
                                             tags$li(class = 'dropdown',
                                                     tags$a(href = '#',
                                                            class = 'dropdown-toggle', `data-toggle` = 'dropdown',
                                                            tags$i(class = 'fa fa-envelope-o')
                                                     ),
                                                     tags$ul(class = 'dropdown-menu',
                                                             tags$li(a(href = "mailto:choice.models@kantar.com",
                                                                       HTML("Contact<br>ValueManager<br>Team")))
                                                     )))
                           )
  )
  ),

  ####Sidebar####
  sidebar = sidebarMenu(
    menuItem(tabName = 'home', text = 'Overview', icon = icon('home'), selected = TRUE),
    menuItem(tabName = 'decision', text = 'decision matrix', icon = icon('tree')),
    menuItem(tabName = 'sankey', text = 'Sankey Diagram', icon = icon('tree')),
    menuItem(tabName = 'tree', text = 'Treemap', icon = icon('tree')),
    menuItem(tabName = 'portfolio', text = 'Portfolio Rankings', icon = icon('trophy'))
  ),

  ####Body####
  body = kantarBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(
        HTML(".shiny-notification {position: fixed;
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

    tabItems(
      tabItem(
        tabName = 'home',
        h2(),
        kantarBox(),
        kantarBox()
      ),


      tabItem(
        tabName = 'decision',
        uiOutput("decMatUI")
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
        kantarBox(div(style = "overflow-y: scroll; height: 88vh; font-size: 80%",
                      DT::dataTableOutput("portTable"))),
        kantarBox(DT::dataTableOutput("portGRID")),
        kantarBox(formattableOutput("portGRID2"))
      )
    )
  ),

  #Title change from default
  title = 'Market Structure'
)

# KTShiny::kantar_auth_ui(ui = ui, app_id = app_id)
