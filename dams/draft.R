#load(file="c:/temp/DamSpain.Rdata")
# setwd("C:/Users/cedric.briand/Documents/GitHub/SUDOANGShiny/dams")
load("data/DamSpain.Rdata")
head(dams)
str(dams)

# get sf object
require(sf)
damssf <- st_as_sf(dams, coords = c("X", "Y"), crs = 3035)

# The Leaflet package expects data to be specified 
#in lat-long using WGS 84 (a.k.a. EPSG:4326)

require(leaflet)
packageVersion("leaflet") # 2.0.2
damssf_reproj <- st_transform(damssf, 4326)
# plot(damssf_reproj)

leaflet(damssf_reproj) %>% 
  addCircleMarkers(radius = 1) %>% 
  addTiles()

# to cluster a large number of markers on a map
leaflet(damssf_reproj) %>% 
  addTiles() %>% 
  addCircleMarkers( #addMarkers
  clusterOptions = markerClusterOptions()
)

# Pop up list
# Cedric : Maria is this working ? It doesn't in my computer
# Maria : as talked, it is because most of the obs_name  == <NA>, it is ok with "id" for example
leaflet(damssf_reproj) %>% 
  addTiles() %>% 
  addCircleMarkers( #addMarkers
    clusterOptions = markerClusterOptions(), popup = ~ obs_name
  )


# Popup list_2
# Maria: MM& Cb; Based on the variables, we should change all NA to -> "No available"

# $obs_name
damssf_reproj$obs_name <-as.factor(damssf_reproj$obs_name)
levels <- levels(damssf_reproj$obs_name)
levels[length(levels) + 1] <- "No available"
damssf_reproj$obs_name <- factor(damssf_reproj$obs_name, levels = levels)
damssf_reproj$obs_name[is.na(damssf_reproj$obs_name)] <- "No available"


# $obs_height
damssf_reproj$obs_height <- ifelse(!is.na(damssf_reproj$obs_height),round(damssf_reproj$obs_height, 2),damssf_reproj$obs_height) 

# Maria: MM&CB, which variables do you want to appear in the dams popup?

popuplist = paste0( "Obstacle name: "
                    , damssf_reproj$obs_name
                    , "<br>"
                    ,"Obstacle height: "
                    , damssf_reproj$obs_height
                    , "<br>"
                    , "Data source: "
                    , damssf_reproj$datasource
)

leaflet(damssf_reproj) %>% 
  addTiles() %>% 
  addCircleMarkers( #addMarkers
    clusterOptions = markerClusterOptions(), popup = popuplist
  )

# specific icons
# Maria: Now shadow and sizes work.
leafIcons <- icons(
  iconUrl = ifelse(damssf_reproj$obs_height>10 & !is.na(damssf_reproj$obs_height),
                   "www/dam.png",
                   "www/smalldam.png"),
      iconWidth = ifelse(damssf_reproj$obs_height>10 & !is.na(damssf_reproj$obs_height) , 25, 20),
      iconHeight = ifelse(damssf_reproj$obs_height>10 &!is.na(damssf_reproj$obs_height) , 20, 10),
      iconAnchorX = 0, iconAnchorY = 20,
  shadowUrl = ifelse(damssf_reproj$obs_height>10 & !is.na(damssf_reproj$obs_height),
                    "www/damshadow.png",""),
      shadowWidth = 40,
      shadowHeight = 10,
      shadowAnchorX = 5, shadowAnchorY =0
 )

# leaflet(damssf_reproj[1:200,]) %>% 
#   addTiles() %>% 
#   addMarkers(icon = leafIcons)
# 
# 
# leaflet(damssf_reproj[damssf_reproj$obs_height>10,]) %>% 
#   addTiles() %>% 
#   addMarkers(icon = leafIcons)


# Maria: Popup also works.
leaflet(damssf_reproj) %>% 
  addTiles() %>% 
  addMarkers( 
    clusterOptions = markerClusterOptions(),icon = leafIcons, popup = popuplist
  )





