## ****************************
## SUDOANG 
## Shiny ui
## May 2019
## ****************************

## ****************************
## Libraries
## ****************************
library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(purrr)
library(sf)
library(rhandsontable)
library(leaflet.extras)
library(leafem)

## ****************************
## Data
## ****************************

load("data/dams.spain.Rdata")

## ****************************
## ui
## ****************************

 ## Header
  header <- dashboardHeader(
    title = "SUDOANG Shiny"
)


 ## Body  
  body <- dashboardBody(
           
    # tags$head(
    #          tags$style(HTML('#saveBtn{background-color:#4393C3}')) # color the button
    #      ), 
    useShinyjs(),
    
    tabItems(
      ##-------
      ## Home
      ##-------
      tabItem(tabName = "home",
              br(),
              HTML('<center><img src="logoSUDOANG.png"></center>'),
              includeHTML ("data/DescriptionSUDOANG.txt")
              ),
      
      ##-------
      ## Readme
      ##-------
      tabItem(tabName = "readme",
              br(),
              includeHTML ("data/ReadmeSUDOANG.txt")
              ),
      
      ##-----------
      ## Map
      ##-----------
      tabItem(tabName = "map",
      fluidRow(
      column(width = 8,
           box(width = NULL, status = "primary", solidHeader = TRUE, 
               leafletOutput("map", height = 800)
           )),
      column (width = 4,
           box(id ="tablebox", width = NULL,status = "primary", title = "Table edits",
               #tableOutput("table")
               rHandsontableOutput("table"),
               br(),
               actionButton("saveBtn", "Save edits")
               #downloadButton("downloadExcelSheet", "Send Data"),
               #verbatimTextOutput("print")
           ), 
           box(id ="tablebox2", width = NULL,status = "primary", title = "Table add new ",
               #tableOutput("table")
               rHandsontableOutput("table2"),
               br(),
               actionButton("saveBtn2", "Save new")
               )
    )
  )
)
)
)# end of the dashboardbody
  
 ## Sidebar
  sidebar <- dashboardSidebar(
                tags$style(HTML(".sidebar-menu li a { font-size: 20px; }")),
              sidebarMenu (
                width = 200,
                id = "tabs",
                menuItem ("Home", tabName = "home", icon = icon("home"), selected = T),
                menuItem ("Readme", tabName = "readme", icon = icon("readme"), selected = F),
                menuItem ("Map", tabName = "map", icon = icon("map-marker-alt"), selected = F)
              )
)# end of the dashboardsidebar

  
 ## All together
  ui <- fluidPage(
    
  tags$style(type="text/css", "#passScreen {font-size:18px; margin-top: 100px;}"),
  
  useShinyjs(),
  # password panel
  
  fluidRow(column(width = 4, offset = 4,
  div(
    id="passScreen",
    wellPanel(
      textInput("user",label = tags$div(HTML('<i class="fa fa-user-circle" style = "color:#000000;"></i> Enter username:'))),
      passwordInput("pass", label = tags$div(HTML('<i class="fa fa-lock" style = "color:#000000;"></i> Enter password:'))),
      actionButton("okpassword",h4("Login")),
      actionButton("okregister",h4("Sign up")),
      br()#, hr(),
      #actionLink("passforgot", "Click here if you forgot your password ")
      )
    
  ))),
  
  # app showed when password is correct
  shinyjs::hidden(
    div(id="myapp",

  dashboardPage(
    header,
    #dashboardSidebar(disable = TRUE),
    sidebar,
    body
)
)
)
)# end of the ui

