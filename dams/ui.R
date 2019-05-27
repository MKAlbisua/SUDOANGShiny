## ****************************
## SUDOANG 
## Shiny ui
## May 2019
## ****************************

## ****************************
## Libraries
## ****************************

library(shinydashboard)
library(leaflet)
library(tidyverse)
library(purrr)
library(sf)
#library(DT)
library(shinyjs)
library(rhandsontable)

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
           
    tags$head(
             tags$style(HTML('#saveBtn{background-color:#4393C3}'))
         ),
    useShinyjs(),
    fluidRow(
      column(width = 8,
           box(width = NULL, status = "primary", solidHeader = TRUE, 
               leafletOutput("map", height = 800)
           )),
      column (width = 4,
           box(id ="tablebox", width = NULL,status = "primary", title = "Table",
               #tableOutput("table")
               rHandsontableOutput("table"),
               actionButton("saveBtn", "Save edits")
               #downloadButton("downloadExcelSheet", "Send Data"),
               #verbatimTextOutput("print")
           ))
    )
  )
#) 

  ## All together

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

