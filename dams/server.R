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
library(shinyWidgets)

## ****************************
## Data
## ****************************

load("data/dams.spain.RData")
load("data/altitude.spain.RData")
load("data/delta.coord.RData")
load("data/deltagamma.coord.RData")

## ****************************
## Function for url
## ****************************

js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"

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
      #shinyjs::show("myapp", FALSE)
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
  ## logout
  ##-------
  

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
  ## README
  ## ********************************************************************************************************************
  
  output$readmeUi <- renderUI({
    req(input$okpassword)
    load("commentsTable.RData")
    if (nrow(subset(commentsTable, user==input$user & password==input$pass))!=0){
      includeHTML ("data/ReadmeSUDOANG.txt")
    }
  })
  
  
  
  ## ********************************************************************************************************************
  ## MAP DAMS BOX
  ## ********************************************************************************************************************
  
  
  output$mapUi <- renderUI ({
    req(input$okpassword)
    load("commentsTable.RData")
    if (nrow(subset(commentsTable, user==input$user & password==input$pass))!=0){
          fluidRow(
          column(width = 8,
               box(width = NULL, status = "primary", solidHeader = TRUE,
                   addSpinner(leafletOutput("map", height = 800), spin = "circle", color = "#E41A1C")#,
                   #br(),
                   #checkboxInput("show.alt", "Show/hide altitude layer", value = FALSE)
               )),
          column (width = 4,
               box(id ="tablebox", width = NULL,status = "primary", title = "Table edits",
                   #tableOutput("table")
                   rHandsontableOutput("table"),
                   br(),
                   actionButton("saveBtn", "Save edits"),
                   extendShinyjs(text = js_code, functions = 'browseURL'),
                   actionButton ("google", "Google maps")
                   #actionButton("google", "Google Maps", onclick ="window.open('http://google.com', '_blank')")
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
    }
  })
  
  
  
  ##-------
  ## map
  ##-------
  
  output$map <- renderLeaflet({
  

  ## Color palettes dams and polylines
  pal.dam<-colorFactor("Dark2", levels(dams.spain$type))
  
  
  altitude.spain$DELTACLASS<- factor(altitude.spain$DELTACLASS, 
                                     levels=c(">300",  ">50" , ">10"  ,">0"),
                                     labels=c("Cumheightdam-altitude > 300 m", "Cumheightdam-altitude > 50 m", "Cumheightdam-altitude > 10 m", 
                                              "Cumheightdam-altitude > 0 m"))
  collist<-c( "#D1E5F0",  "#2166AC", "#B2182B", "#FDDBC7")
  pal.alt<-colorFactor(collist, levels(altitude.spain$DELTACLASS))

  

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
    #addPolylines(data = altitude.spain, color = ~ pal2(DELTACLASS), weight = 2, group = "outline") %>% #weight = 5 by default; it makes narrower the polyline.
    #addLegend("topleft", title = "Accumulated height (m)", pal = pal2, values = levels(altitude.spain$DELTACLASS))
  

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
                         color = ~ pal.dam(type),
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
    #Drawtool layer
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
    
    #Polylines layer
    addPolylines(data = altitude.spain, color = ~ pal.alt(DELTACLASS), weight = 2, group = "altitude") %>% #weight = 5 by default; it makes narrower the polyline.
    
    #Control  layers
    addLayersControl(
      overlayGroups = c(names(dams.spain.df),"markers", "altitude"),
      # overlayGroups = c("<img src= 'www/dam.png' height='20' width='20'>", names(damssf_reproj.df)[1],
      #                "<img src= 'www/greendam.png' height='20' width='20'> names(damssf_reproj.df)[2]",
      #                "<img src= 'www/smalldam.png' height='20' width='20'> names(damssf_reproj.df)[3]"),
      options = layersControlOptions(collapsed = T))%>%
    
    # Legend layers
    addLegend( "bottomleft", pal=pal.dam, values=levels(dams.spain$type))%>%
    addLegend("bottomleft", title = "Problem altitude", pal = pal.alt, values = levels(altitude.spain$DELTACLASS), group = "altitude")%>%
    
    #Hide altitude layer by default
    hideGroup("outline")

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
  dams.coord<-dams.spain.coord[,c(25:26, 8, 16, 4:6, 10,19,22,21)]
  
  
   ## Reactive data to click on map
   dam <- eventReactive(input$map_marker_click, {
     click <- input$map_marker_click
     dam <-dams.coord[dams.coord$lat == click$lat & dams.coord$long ==click$lng,]  
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
   
   
   ## ********************************************************************************************************************
   ## Google maps 
   ## ********************************************************************************************************************
   
   ## reactive url data.frame
   dams.spain.url<-dams.spain.coord[,c(24:26)]
   
   ## Reactive url to click on map
   url <- eventReactive(input$map_marker_click, {
     click <- input$map_marker_click
     url <-dams.spain.url[dams.spain.url$lat == click$lat & dams.spain.url$long == click$lng,]
     url <- url [,]
   })


   ## Open the browser with the url
   observeEvent(input$google, {
       js$browseURL(url()$googlemapcoords)
       Sys.sleep(1)                #Short delay of 1 second
   })
   
   
   ## ********************************************************************************************************************
   ## MAP RESIDUALS BOX
   ## ********************************************************************************************************************
   
   
   output$mapRUi <- renderUI ({
     req(input$okpassword)
     load("commentsTable.RData")
     if (nrow(subset(commentsTable, user==input$user & password==input$pass))!=0){
       fluidRow(
         column(width = 8,
                box(width = NULL, status = "primary", solidHeader = TRUE,
                    leafletOutput("map.R", height = 800)
                )),
         column (width = 4,
                 box(id ="tablebox.R", width = NULL,status = "primary", title = "Residual edits",
                     radioButtons("dataset", "Plot", choices = c("Presence/absence" = "Presence/absence", "Density" = "Density"), selected= "Presence/absence"),
                     conditionalPanel( condition = "input.dataset == 'Presence/absence'",
                                       sliderInput("res", "Residuals", round(min(delta.coord$rdelta), digits=2), round(max(delta.coord$rdelta), digits=2),
                                                   value = round(range(delta.coord$rdelta), digits=2), step = 0.1)),
                     conditionalPanel( condition = "input.dataset == 'Density'",
                                       sliderInput("res.g", "Residuals", round(min(deltagamma.coord$rdeltagamma), digits=2), round(max(deltagamma.coord$rdeltagamma), digits=2),
                                                   value = round(range(deltagamma.coord$rdeltagamma), digits=2), step = 0.1)),
                     textAreaInput("caption", "", rows = 5, "Type here..." #, width = "1000px"
                                   ),
                     helpText("Note: for example, Wrong location, eels transported, etc."),
                     #test dataset input is ok:
                     verbatimTextOutput("summary"))
                 )
         )
     }
   })
   
   
   # Return the requested dataset ----
   # updated when the user clicks the button
   
   dat<- reactive ({
     switch(input$dataset,
            "Presence/absence" = delta.coord,
            "Density" = deltagamma.coord)
   })
   
   
   df.res <- reactive({
     
     validate(need(input$dataset,"please choose a type of residuals"))
     
     if (input$dataset == "Presence/absence"){
    aux <- dat()[dat()$rdelta >= input$res[1] & dat()$rdelta <= input$res[2],]}
     else{
     if (input$dataset == "Density"){
     aux <- dat()[dat()$rdeltagamma >= input$res.g[1] & dat()$rdeltagamma <= input$res.g[2],]}
     }
     aux
   })

  #test dataset input is ok:
   output$summary <- renderPrint({
     summary(df.res())
   })
   
   
   
   ## Map residuals
   
    output$map.R<- renderLeaflet({

      leaflet(dat()) %>% addTiles()%>%
        setView(lng = -3,lat =  41, zoom = 5) #%>%
       #addCircleMarkers( lat = ~lat, lng = ~long )
    })
   
    
    # observe({
    #     leafletProxy("map.R", df.res()) %>%
    #       addCircleMarkers(lat = ~lat, lng = ~long )
    #   
    #   })

 
 
   observe({

     aux<-leafletProxy("map.R", data=df.res())%>%clearMarkers()

     if (input$dataset == "Presence/absence"){
       pal<- colorNumeric(palette = "RdYlBu", domain = delta.coord$rdelta)
       aux%>%
         #leafletProxy("map", data = df())%>%clearMarkers()%>%
         #mapres %>%
         addCircleMarkers(lat=~lat, lng=~long,#radius = ~ round(rdelta*10, digits=2),
                          radius = 7,
                          popup = ~ as.character(round(rdelta, digits = 2)),
                          stroke =F,
                          fillOpacity = 0.9,
                          color = ~pal(rdelta) )
     }
     if(input$dataset == "Density"){
       pal<- colorNumeric(palette = "BrBG", domain = deltagamma.coord$rdeltagamma)
       aux%>%
         #leafletProxy("map", data=df())%>%clearMarkers()%>%
         #mapres%>%
         addCircleMarkers(lat=~lat, lng=~long,#radius = ~ round(rdeltagamma*10, digits=2),
                          radius = 7,
                          popup = ~ as.character(round(rdeltagamma, digits =2)),
                          stroke = F,
                          fillOpacity = 0.9,
                          color = ~pal(rdeltagamma))
     }
   })


   # ## A separate observer is used to create the legends as needed:
   # 
   # observe({
   #   proxy<-leafletProxy ("map.R")
   #   proxy%>%clearControls()
   # 
   #   if (input$dataset == "Presence/absence"){
   #     pal<- colorNumeric(palette = "RdYlBu", domain = delta.coord$rdelta)
   #     values <- round(delta.coord$rdelta, digits =2)}
   # 
   #   if(input$dataset == "Density"){
   #     pal<- colorNumeric(palette = "BrBG", domain = deltagamma.coord$rdeltagamma)
   #     values <- round(deltagamma.coord$rdeltagamma, digits=2)}
   # 
   #   proxy%>%addLegend( "bottomleft", pal = pal, values = values)
   # })# end of the observer


}# end of the server