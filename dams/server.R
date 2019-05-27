## ****************************
## SUDOANG 
## Shiny server
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
## server
## ****************************

function(input, output, session) {

  ## ****************************
  ## MAP BOX
  ## ****************************

  output$map <- renderLeaflet({
  
  ## Color palette
  pal<-colorFactor("RdBu", levels(dams.spain$type))

  ## Poplist
  dams.spain$popuplist.2 <-paste0( "Obstacle name: "
                                   , dams.spain$obs_name
                                   , "<br>"
                                   ,"Obstacle height: "
                                   , dams.spain$obs_height
                                   , "<br>"
                                   , "Presence eel pass: "
                                   , dams.spain$type
                                   , "<br>"
                                   , "Data source: "
                                   , dams.spain$datasource
  )

  ## Map
  l <- leaflet() %>% addTiles() %>%
    setView(lng = -5,lat =  41, zoom = 6)
  

  ## layer control
  dams.spain.df <- split(dams.spain, dams.spain$type)
  
  names(dams.spain.df) %>%
    purrr::walk( function(df) {
      l <<- l %>%
        addCircleMarkers(data=dams.spain.df[[df]],
                         #lng=~long, lat=~lat,
                         #label=~as.character(mag),
                         popup = ~ popuplist.2,
                         group = df,
                         color = ~ pal(type),
                         #opacity= 0.9,
                         fillOpacity = 1,
                         weight = 10,
                         stroke=T,
                         clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T),
                         #popup = popuplist, It does work properly
                         labelOptions = labelOptions(noHide = T,
                                                     direction = 'auto'))
    })
  
  l %>%
    addLayersControl(
      overlayGroups = names(dams.spain.df),
      # overlayGroups = c("<img src= 'www/dam.png' height='20' width='20'>", names(damssf_reproj.df)[1],
      #                "<img src= 'www/greendam.png' height='20' width='20'> names(damssf_reproj.df)[2]",
      #                "<img src= 'www/smalldam.png' height='20' width='20'> names(damssf_reproj.df)[3]"),
      options = layersControlOptions(collapsed = T)
    )%>%
    addLegend( "bottomleft", pal=pal, values=levels(dams.spain$type))
})


  ## Get info of specific marker one by one

  dams.spain.coord <- dams.spain %>%
    mutate(lat = unlist(map(dams.spain$geometry,2)),
         long = unlist(map(dams.spain$geometry,1)))

  dams.spain.coord$lat<- as.character(dams.spain.coord$lat)
  dams.spain.coord$long <- as.character(dams.spain.coord$long)

  st_geometry(dams.spain.coord) <- NULL
  
  ## variable into the editable table
  
  dams.spain.coord<-dams.spain.coord[,c(16:17, 4:6, 8:12)]

  ## Reactive data to click on map
  
  dam <- eventReactive(input$map_marker_click, {
    click <- input$map_marker_click
    dam <-dams.spain.coord[dams.spain.coord$lat == click$lat & dams.spain.coord$long ==click$lng,]  
    dam <- dam [,]
})
  
  ## show/hide actionbutton
  
  observe({
    shinyjs::hide("saveBtn")
    if(nrow(dam())!=0)
    shinyjs::show("saveBtn")
})

  ## Returns rhandsontable type objetc - editable excel type grid data

    output$table <-renderRHandsontable({
    rhandsontable(t(dam()), rowHeaderWidth = 200)%>%
        hot_cols(colWidths = 200) # converst R dataframe to rhandsontable object and transpose for view
})

  ## On click on button the data will be save to the working directory
  
    observeEvent(input$saveBtn, 
           write.table (t(hot_to_r(input$table)), 
                        file = "data/databaseedits.csv", 
                        append = T, row.names = F, sep = ",", col.names = F))

# hot_to_r converst the rhandsontable to R object
# append = T, bind different edits to the same file

}# end of the server