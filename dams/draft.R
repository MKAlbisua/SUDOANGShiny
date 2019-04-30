#load(file="c:/temp/DamSpain.Rdata")
load("data/DamSpain.Rdata")
head(dams)
str(dams)

# get sf object
require(sf)
damssf <- st_as_sf(dams, coords = c("X", "Y"), crs = 3035)

# The Leaflet package expects data to be specified 
#in lat-long using WGS 84 (a.k.a. EPSG:4326)

require(leaflet)
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
leaflet(damssf_reproj) %>% 
  addTiles() %>% 
  addCircleMarkers( #addMarkers
    clusterOptions = markerClusterOptions(), popup = ~ obs_name
  ) 
