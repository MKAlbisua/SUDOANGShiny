## ****************************
## SUDOANG Shiny 
## May 2019
## Draft script
## ****************************

## *************
## load data
## *************

#load(file="c:/temp/DamSpain.Rdata")
# setwd("C:/Users/cedric.briand/Documents/GitHub/SUDOANGShiny/dams")
load("data/DamSpain.Rdata")
head(dams)
str(dams)

## *************
## get sf object
## *************

require(sf)

damssf <- st_as_sf(dams, coords = c("X", "Y"), crs = 3035)

## *************
## The Leaflet package expects data to be specified 
## in lat-long using WGS 84 (a.k.a. EPSG:4326)
## *************

require(leaflet)
#packageVersion("leaflet") # 2.0.2
damssf_reproj <- st_transform(damssf, 4326)

## *************
## plot(damssf_reproj) itself
## *************

# leaflet(damssf_reproj) %>% 
#   addCircleMarkers(radius = 1) %>% 
#   addTiles()


## *************
## plot(damssf_reproj) to cluster a large number of markers on a map
## *************

# leaflet(damssf_reproj) %>% 
#   addTiles() %>% 
#   addCircleMarkers( #addMarkers
#   clusterOptions = markerClusterOptions()
# )

## *************
## plot(damssf_reproj) Pop up list
## *************
# Cedric : Maria is this working ? It doesn't in my computer
# Maria : as talked, it is because most of the obs_name  == <NA>, it is ok with "id" for example

# leaflet(damssf_reproj) %>% 
#   addTiles() %>% 
#   addCircleMarkers( #addMarkers
#     clusterOptions = markerClusterOptions(), popup = ~ obs_name
#   )


## *************
## MODIFYING DATAFRAME
## *************

# $obs_name
# Maria: MM& Cb; Based on the variables, we should change all NA to -> "No available"
# Maria: MM&CB, which variables do you want to appear in the dams popup?

damssf_reproj$obs_name <-as.factor(damssf_reproj$obs_name)
levels <- levels(damssf_reproj$obs_name)
levels[length(levels) + 1] <- "No available"
damssf_reproj$obs_name <- factor(damssf_reproj$obs_name, levels = levels)
damssf_reproj$obs_name[is.na(damssf_reproj$obs_name)] <- "No available"


# $obs_height
# $obs_height == 0 not possible cover to NA.
damssf_reproj$obs_height[damssf_reproj$obs_height == 0] <- NA
# round($obs_height, to 2 digits) 
damssf_reproj$obs_height <- ifelse(!is.na(damssf_reproj$obs_height),round(damssf_reproj$obs_height, 2),damssf_reproj$obs_height) 


# $obs_presence_eel_pass
head(damssf_reproj$obs_id)

# From "obs_id _with_eel pass.r obtained from MM 
# Convert uppercase letters to lowercase
damssf_reproj$obs_id_lower <- sapply(damssf_reproj$obs_id, tolower)
damssf_reproj$obs_presence_eel_pass <- ifelse(damssf_reproj$obs_id_lower %in% c("f93674a2-82f0-4220-bd52-361edcb0a591", 
                                                                                "2ba90f2b-57ff-414d-b840-b4845ee0dfa9", 
                                                                                "ef63fc70-b920-4c4b-b34c-ce036e9eada5", 
                                                                                "275a3d36-3983-42c6-9ea7-fcdd4a5efe91", 
                                                                                "dee8fbab-2e99-474e-8139-9c49108aba12", 
                                                                                "2154d3a7-03e2-4f59-86ad-d8a21e2c751e", 
                                                                                "064a0a27-cb1e-42c6-a6ff-403d8cef9d80", 
                                                                                "1078d8b6-a62d-4819-8ebc-e89f26c7c104", 
                                                                                "3063ee73-cbec-4d77-a43e-4bf4c611fed4", 
                                                                                "31d34c93-54ba-4bd5-b43a-a347617ec93b", 
                                                                                "594974b6-e8b9-40c2-93d4-59478396a95c", 
                                                                                "60948da3-a7f0-4523-a594-bc3f8c4d9e4e", 
                                                                                "6bd3abca-96cc-4a71-b4bd-4fa5db5a610c", 
                                                                                "75509673-b73c-43e7-9d25-fdca26b227d2", 
                                                                                "9ab224b5-46ca-4d20-b5ab-122e573d5dd2", 
                                                                                "9e8aaa5e-dfdc-4eeb-a4e9-27d5a8cbcb0b", 
                                                                                "9f7c25c6-f15a-4534-840f-75743dfd3c51", 
                                                                                "b26fa796-aa29-441f-846d-313e0052f7d3", 
                                                                                "bdf137d6-1c1b-462e-9bfb-e614cc486a71", 
                                                                                "c3e177f1-e838-46e8-892e-fab2da287c59", 
                                                                                "eb173a78-039c-4c31-b370-ffa20482e359", 
                                                                                "4062f664-4d64-4b3a-a8d1-b2515f0f3e94", 
                                                                                "290f84d6-f85c-478b-aa74-469e07b2c817", 
                                                                                "b9aee786-38d0-4c11-bc3f-319390fab7ff", 
                                                                                "1c0ca4b0-e182-4432-8166-678d83570608", 
                                                                                "5b5b4393-aff7-44c1-8b65-ba301011fe4b", 
                                                                                "c86fb0b6-4d7e-4d4e-8998-c2372960cb06", 
                                                                                "32eb7459-b00c-4ea5-a1e7-2418dbccf25d", 
                                                                                "3d41b2f0-db5f-48af-a58a-2f032216bc1c", 
                                                                                "27bf4bb6-c03c-417a-b0d1-cd6f5dfa631f", 
                                                                                "98cfc961-c79e-4167-80e1-3e6fe6b0fda6", 
                                                                                "edb3af1d-8426-4e03-898d-632083892e94", 
                                                                                "97a61a32-094c-4d05-b40b-ae1c2c12d4bd", 
                                                                                "2ba238a6-bd94-43cb-9279-cbbad80dcbe9", 
                                                                                "513fe265-75ef-4afb-88d6-3219c263b56d",
                                                                                "81484769-f1a0-4ddb-8401-f410e3546147", 
                                                                                "d72c8ef6-437b-438f-9336-a71944fa37ee"), 1, 0)



# Type of the dam having eel pass in order to paint the dams icon in green
names(damssf_reproj)
range(damssf_reproj[damssf_reproj$obs_presence_eel_pass==1,]$obs_height, na.rm=T)
# Maria : small and big ones have eel pass.

## *************
## Popup list
## *************

popuplist = paste0( "Obstacle name: "
                    , damssf_reproj$obs_name
                    , "<br>"
                    ,"Obstacle height: "
                    , damssf_reproj$obs_height
                    , "<br>"
                    , "Presence eel pass: "
                    , damssf_reproj$type
                    , "<br>"
                    , "Data source: "
                    , damssf_reproj$datasource
)


## *************
## plot (damssf_reproj) with modification
## *************

leaflet(damssf_reproj) %>% 
  addTiles() %>% 
  addCircleMarkers( #addMarkers
    clusterOptions = markerClusterOptions(), popup = popuplist
  )

## *************
## Add Control layer to map
## *************

# Classifying the type of dam based on height and presence eel pas
# require(dplyr)
# damssf_reproj$type <-case_when(
#   damssf_reproj$obs_presence_eel_pass == 1  ~ "greendam",
#   damssf_reproj$obs_height > 10 & damssf_reproj$obs_presence_eel_pass == 0 ~ "dam",
#   damssf_reproj$obs_height < 10 & damssf_reproj$obs_presence_eel_pass == 0 ~ "smalldam",
#   TRUE ~  "smalldam") 

damssf_reproj$type[damssf_reproj$obs_height > 10 & damssf_reproj$obs_presence_eel_pass == 1] <-"greendam"
damssf_reproj$type[damssf_reproj$obs_height > 0 & damssf_reproj$obs_height < 10 & damssf_reproj$obs_presence_eel_pass == 1] <-"greensmall"
damssf_reproj$type[damssf_reproj$obs_height > 10 & damssf_reproj$obs_presence_eel_pass == 0]<-"dam"
damssf_reproj$type[damssf_reproj$obs_height > 0 & damssf_reproj$obs_height < 10 & damssf_reproj$obs_presence_eel_pass == 0] <- "smalldam"
damssf_reproj$type[is.na(damssf_reproj$obs_height)] <-"smalldam"

damssf_reproj$type <- as.factor(damssf_reproj$type)
dams.spain<-damssf_reproj

## *************
## plot (damssf_reproj) with modification. Refresh popuplist
## *************

pal<-colorFactor("RdBu", levels(dams.spain$type))


leaflet(dams.spain) %>% 
  addTiles() %>% 
  addCircleMarkers( #addMarkers
    color = ~ pal(type),
    opacity= 0.9,
    fillOpacity = 0.5,
    clusterOptions = markerClusterOptions(), popup = popuplist
  )%>%
  addLegend( "bottomleft", pal=pal, values=levels(dams.spain$type))


#save(dams.spain, file= "dams.spain.RData")

# Maria: this doesn´t work with layercontrol weir groups.

dams.spain.df <- split(dams.spain, dams.spain$type)


# maria: this workds but see below
l <- leaflet() %>% addTiles()

# Maria: popuplist not working with layercontrol
# Solved with: popuplist_2

dams.spain$popuplist.2 <-paste0( "Obstacle name: "
                    , damssf_reproj$obs_name
                    , "<br>"
                    ,"Obstacle height: "
                    , damssf_reproj$obs_height
                    , "<br>"
                    , "Presence eel pass: "
                    , damssf_reproj$type
                    , "<br>"
                    , "Data source: "
                    , damssf_reproj$datasource
)

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

## Maria: image is not rendered inside the layercontrol
l %>%
  addLayersControl(
    overlayGroups = names(dams.spain.df),
    # overlayGroups = c("<img src= 'www/dam.png' height='20' width='20'>", names(damssf_reproj.df)[1],
    #                "<img src= 'www/greendam.png' height='20' width='20'> names(damssf_reproj.df)[2]",
    #                "<img src= 'www/smalldam.png' height='20' width='20'> names(damssf_reproj.df)[3]"),
    options = layersControlOptions(collapsed = T)
  )%>%
  addLegend( "bottomleft", pal=pal, values=levels(dams.spain$type))


## *************
## specific icons
## *************

# # Maria: Now shadow and sizes work.
# leafIcons <- icons(
#   iconUrl = ifelse(damssf_reproj$obs_height>10 & (damssf_reproj$obs_height),
#                    "www/dam.png",
#                    "www/smalldam.png"),
#       iconWidth = ifelse(damssf_reproj$obs_height>10 & !is.na(damssf_reproj$obs_height) , 25, 20),
#       iconHeight = ifelse(damssf_reproj$obs_height>10 &!is.na(damssf_reproj$obs_height) , 20, 10),
#       iconAnchorX = 0, iconAnchorY = 20,
#   shadowUrl = ifelse(damssf_reproj$obs_height>10 & !is.na(damssf_reproj$obs_height),
#                     "www/damshadow.png",""),
#       shadowWidth = 40,
#       shadowHeight = 10,
#       shadowAnchorX = 5, shadowAnchorY =0
#  )

# leaflet(damssf_reproj[1:200,]) %>% 
#   addTiles() %>% 
#   addMarkers(icon = leafIcons)
# 
# 
# leaflet(damssf_reproj[damssf_reproj$obs_height>10,]) %>% 
#   addTiles() %>% 
#   addMarkers(icon = leafIcons)



# case_when instead of ifelse
# leafIcons = icons(
#   iconUrl =  case_when(
#               damssf_reproj$obs_height > 10 & damssf_reproj$type =="greendam" ~ "www/greendam.png",
#               damssf_reproj$obs_height > 10 & damssf_reproj$type =="dam" ~ "www/bigdam.png",
#               damssf_reproj$obs_height < 10 & damssf_reproj$type =="greendam" ~ "www/greensmall.png",
#               TRUE ~  "www/smalldam.png"), 
#     iconWidth = case_when(
#               damssf_reproj$obs_height > 10 ~  30,
#               damssf_reproj$obs_height < 10 ~  25,
#               TRUE ~ 25),
#     iconHeight = case_when(
#               damssf_reproj$obs_height > 10 ~  30,
#               damssf_reproj$obs_height < 10 ~  25,
#               TRUE ~ 25),
#     iconAnchorX = 0, iconAnchorY = 20,
# shadowUrl = case_when(
#             damssf_reproj$obs_height > 10  ~ "www/damshadow.png",
#             damssf_reproj$obs_height == 0  ~ "www/damshadow.png",
#             TRUE ~ ""),
#   shadowWidth = 40,
#   shadowHeight = 10,
#   shadowAnchorX = 5, shadowAnchorY =0
#  )

require(dplyr)
leafIcons = icons(
  iconUrl =  case_when(
    dams.spain$obs_height >= 10 & dams.spain$obs_presence_eel_pass == 1  ~ "www/greendam.png",
    dams.spain$obs_height < 10 & dams.spain$obs_presence_eel_pass == 1  ~ "www/greensmall.png",
    dams.spain$obs_height >= 10 & dams.spain$obs_presence_eel_pass == 0 ~ "www/bigdam.png",
    dams.spain$obs_height < 10 & dams.spain$obs_presence_eel_pass == 0 ~ "www/smalldam.png",
    TRUE ~  "smalldam"),
  iconWidth = case_when(
    dams.spain$obs_height > 10 ~  30,
    dams.spain$obs_height < 10 ~  25,
    TRUE ~ 25),
  iconHeight = case_when(
    dams.spain$obs_height > 10 ~  30,
    dams.spain$obs_height < 10 ~  25,
    TRUE ~ 25),
  iconAnchorX = 0, iconAnchorY = 20,
)

## Maria: Popup also works.(refresh popolist)
 leaflet(dams.spain) %>%
   addTiles() %>%
   addMarkers(
     clusterOptions = markerClusterOptions(),icon = leafIcons, popup = popuplist
   )
 

## *************
## Add image to leyend
## *************
# Maria: images on the legend not dispalying well.

html_legend <- "<img src='www/greendam.png'>dam with eel pass<br/>
                <img src='www/bigdam.png'>dam<br/>
                <img src='www/greensmall.png'>small dam with eel pass<br/>
                <img src='www/smalldam.png'>smalldam"
                
                
leaflet(dams.spain) %>% 
  addTiles() %>%
  #addProviderTiles("Esri.WorldImagery")%>%
  addMarkers( 
    clusterOptions = markerClusterOptions(),icon = leafIcons, popup = popuplist
  )%>%
  addControl(html=html_legend , position = "bottomleft")


## *************
## Plot all together: icons, layer control, image to legend
## *************
# Maria: icons with layercontrol not working well

library(purrr)

l <- leaflet() %>% addTiles()

dams.spain.df <- split(dams.spain, dams.spain$type)

# Maria: when popup active, cluster not working properly.
names(dams.spain.df) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addMarkers(data=dams.spain.df[[df]],
                       #lng=~long, lat=~lat,
                       #label=~as.character(mag),
                       popup= ~popuplist.2,
                       group = df,
                       clusterOptions = markerClusterOptions(),
                       icon = leafIcons#,
                       #popup = popuplist
                       )
  })

## Maria: image is not rendered inside the layercontrol
l %>%
  addLayersControl(
    overlayGroups = names(dams.spain.df),
    # overlayGroups = c("<img src= 'www/dam.png' height='20' width='20'>", names(damssf_reproj.df)[1], 
    #                "<img src= 'www/greendam.png' height='20' width='20'> names(damssf_reproj.df)[2]",
    #                "<img src= 'www/smalldam.png' height='20' width='20'> names(damssf_reproj.df)[3]"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addControl(html= html_legend, position = "bottomleft")

## table output
## when click on the map

# [leafletOutput alias]_marker_click


## *************
## Converting Geometry to Longitude/Latitude coordinates 
## *************

library(tidyverse)
require(sf)
require(purrr) #map function

dams.spain.coord <- dams.spain %>%
  mutate(lat = unlist(map(dams.spain$geometry,2)),
         long = unlist(map(dams.spain$geometry,1)))


dams.spain.coord$lat<- as.character(dams.spain.coord$lat)
dams.spain.coord$long <- as.character(dams.spain.coord$long)

st_geometry(dams.spain.coord) <- NULL

#dams.spain.coord[dams.spain.coord$long ==  "0.689496604406601",]

class(dams.spain.coord) # data.frame

## *************
## Tidy the data.frame to show in tha editable table
## *************

# names(dams.spain.coord)
# str(dams.spain.coord)
# head(dams.spain.coord)

# dams.spain.coord<-dams.spain.coord[,c(16:17, 4:6, 8:12)]

# str(dams.spain$type)

library(plyr)
levels(dams.spain$type)
dams.spain$type<-revalue(dams.spain$type, c("dam"="Dam > 10m", "greendam"="Dam > 10m with eel pass",
                           "greensmall" = "Dam < 10m with eel pass", "smalldam" = "Dam < 10m"))
levels(as.factor(dams.spain$obs_presence_eel_pass))
save(dams.spain, file = "dams.spain.RData")










