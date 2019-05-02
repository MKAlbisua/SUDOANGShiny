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
leaflet(damssf_reproj) %>% 
  addTiles() %>% 
  addCircleMarkers( #addMarkers
    clusterOptions = markerClusterOptions(), popup = ~ obs_name
  ) 


# specific icons

leafIcons <- icons(
  iconUrl = ifelse(damssf_reproj$obs_height>10&!is.na(damssf_reproj$obs_height) ,
                   "www/dam.png",
                   "www/smalldam.png"
  ),
  iconWidth = ifelse(damssf_reproj$obs_height>10&!is.na(damssf_reproj$obs_height) , 25,16),
  iconHeight = ifelse(damssf_reproj$obs_height>10&!is.na(damssf_reproj$obs_height) ,20,10),
  iconAnchorX = 0, iconAnchorY = 20,
  shadowUrl =  ifelse(damssf_reproj$obs_height&!is.na(damssf_reproj$obs_height)>10 ,"www/damshadow.png",""),
   shadowWidth = 40, shadowHeight = 10,
   shadowAnchorX = 5, shadowAnchorY =0
)

leaflet(damssf_reproj[1:200,]) %>% 
  addTiles() %>% 
  addMarkers(icon = leafIcons)


leaflet(damssf_reproj[damssf_reproj$obs_height>10,]) %>% 
  addTiles() %>% 
  addMarkers(icon = leafIcons)
