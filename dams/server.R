## ****************************
## SUDOANG 
## Shiny server
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
## server
## ****************************

function(input, output, session) {

  ## ********************************************************************************************************************
  ## Login and register
  ## ********************************************************************************************************************
  
  ##-------
  ## show/hide app
  ##-------
  
  observeEvent(input$okpassword, {
    load("commentsTable.RData")
    #if (nrow(subset(users, user==input$user & password==input$pass))!=0){
    if (nrow(subset(commentsTable, user==input$user & password==input$pass))!=0){
      shinyjs::show("myapp", FALSE)
      shinyjs::hide("passScreen", FALSE)
    }
  })
  
  ##-------
  ## login
  ##-------
  
  observeEvent(input$okpassword,{
    load("commentsTable.RData")
    #if (!input$user%in%users$user) 
    if (!input$user%in%commentsTable$user) 
      showModal(modalDialog("This user doesn't exist"))
    else 
      #if (nrow(subset(users, user==input$user & password==input$pass))==0)
      if (nrow(subset(commentsTable, user==input$user & password==input$pass))==0)
        showModal(modalDialog("Password incorrect"))
    else
      showModal(modalDialog(paste("Welcome", input$user)))
  })
  
  ##-------
  ## Register
  ##-------
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("userregister", "Choose your email address",
                placeholder = 'example: mkorta@azti.es'
      ),
      textInput("passregister", "Choose your password",
                placeholder = 'Try characters and numers together'
      ),
      #span('(Try the name of a valid data object like "mtcars", ',
      #     'then a name of a non-existent object like "abc")'),
      if (failed)
        div(tags$b("Please select an email address and a password", style = "color: red;")),
      #footer = tagList(
      #modalButton("Cancel"),
      actionButton("add", "Add")
      #)
    )
  }
  
  # Show modal when button is clicked.
  observeEvent(input$okregister,{
    showModal(dataModal())
  })
  
  
  ##-----------------------------------
  ## save credentials for after login 
  ##----------------------------------
  
  observeEvent(input$add, {
    load("commentsTable.RData")
    temp <- data.frame(user = input$userregister,
                       password = input$passregister)
    commentsTable <- rbind(commentsTable, temp)
    save(commentsTable, file="commentsTable.RData")
  })
  
  
  
  
  ## ********************************************************************************************************************
  ## MAP BOX
  ## ********************************************************************************************************************
  
  ##-------
  ## map
  ##-------
  
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
                                   , "Fish pass type: "
                                   , dams.spain$fishway_type_name
                                   , "<br>"
                                   , "Fish pass available for eels: "
                                   , dams.spain$obs_eel_pass
                                   , "<br>"
                                   , "Data source: "
                                   , dams.spain$datasource
  )

  ## Map
  l <- leaflet() %>% addTiles() %>%
    setView(lng = -5,lat =  41, zoom = 6) %>% addMouseCoordinates() %>% addScaleBar ("bottomleft")
  

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
    addDrawToolbar(
      targetGroup = "markers",
      polylineOptions = FALSE,
      polygonOptions = FALSE,
      rectangleOptions = FALSE,
      circleOptions = FALSE,
      markerOptions = FALSE,
      editOptions = editToolbarOptions()) %>%
      #editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
    #addStyleEditor()   %>%
    addLayersControl(
      overlayGroups = c(names(dams.spain.df),"markers"),
      # overlayGroups = c("<img src= 'www/dam.png' height='20' width='20'>", names(damssf_reproj.df)[1],
      #                "<img src= 'www/greendam.png' height='20' width='20'> names(damssf_reproj.df)[2]",
      #                "<img src= 'www/smalldam.png' height='20' width='20'> names(damssf_reproj.df)[3]"),
      options = layersControlOptions(collapsed = T)
    )%>%
    addLegend( "bottomleft", pal=pal, values=levels(dams.spain$type))
})

  
  
  ## ********************************************************************************************************************
  ## EDITABLE TABLE
  ## ********************************************************************************************************************

  ##show/hide tables based on nrow(df())
  observe({
    shinyjs::hide(id ="tablebox")
    if(nrow(dam())!=0)
      shinyjs::show(id = "tablebox")
  })
  
  observe({
    shinyjs::hide(id ="tablebox2")
    if(nrow(val())!=0)
      shinyjs::show(id = "tablebox2")
  })
  
  
  ## Get info of specific marker one by one
  dams.spain.coord <- dams.spain %>%
    mutate(lat = unlist(map(dams.spain$geometry,2)),
         long = unlist(map(dams.spain$geometry,1)))

  dams.spain.coord$lat<- as.character(dams.spain.coord$lat)
  dams.spain.coord$long <- as.character(dams.spain.coord$long)

  st_geometry(dams.spain.coord) <- NULL
  
  
  ## variable into the editable table
  dams.spain.coord<-dams.spain.coord[,c(24:25, 8, 16, 4:6, 10,19,22,21)]
    
  
  ## Reactive data to click on map
  dam <- eventReactive(input$map_marker_click, {
    click <- input$map_marker_click
    dam <-dams.spain.coord[dams.spain.coord$lat == click$lat & dams.spain.coord$long ==click$lng,]  
    dam <- dam [,]
  })
  


  ## Returns rhandsontable type objetc - editable excel type grid data
  output$table <-renderRHandsontable({
    rhandsontable(t(dam()), rowHeaderWidth = 200)%>%
        hot_cols(colWidths = 200) # converst R dataframe to rhandsontable object and transpose for view
})
  
  ## Add user name to the edits
  ## On click on button the data will be save to the working directory
   observeEvent(input$saveBtn, 
       write.table (cbind(t(hot_to_r(input$table)),input$user), 
                      file = "data/databaseedits.csv", 
                      append = T, row.names = F, sep = ",", col.names = F))
   
   
   ## rective data with new dams/markers to the map
   val<-eventReactive(input$map_draw_new_feature,{
      feature <- input$map_draw_new_feature
      lat <-feature$geometry$coordinates[1]
      long<-feature$geometry$coordinates[2]
      datasource <-"unknown"
      obs_name<-"unknown"
      obs_height <- 0
      fishway_type_name <-"Unknown"
      obs_eel_pass <-"no"
      obs_actual_stand<-"no"
      basin <-"unknown"
      country<-"unknown"
      val<-data.frame(cbind(lat,long, country, basin,datasource,obs_name, obs_height,fishway_type_name, 
                               obs_eel_pass, obs_actual_stand))
      val<-data.frame(lapply(val, as.character), stringsAsFactors=FALSE)
      rownames(val)<-nrow(val) # ! otherwise it will not transpose second time and save
      val
   })
   
   
 ## render a second editable table for new markers
  output$table2 <-renderRHandsontable({
     rhandsontable(t(val()), rowHeaderWidth = 200)%>%
       hot_cols(colWidths = 200) # converst R dataframe to rhandsontable object and transpose for view
   })
   
  
  ## save new markers
   observeEvent(input$saveBtn2, 
        write.table (cbind(t(hot_to_r(input$table2)), input$user), 
                              file = "data/databasenew.csv", 
                              append = T, row.names = F, sep = ",", col.names = F
                              ))

# hot_to_r converst the rhandsontable to R object
# append = T, bind different edits to the same file

}# end of the server