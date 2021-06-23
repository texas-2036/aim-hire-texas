library(tidyverse)
library(tigris)
library(here)
library(sf)
library(leaflet)
library(leaflet.extras)
library(rmapshaper)

options(tigris_use_cache = TRUE, tigris_class = "sf")
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

crosswalk <- read.csv(here::here("raw-data", "county_wda_crosswalk.csv"))

counties <- tigris::counties(state = "48") %>% 
  dplyr::select(county = NAME, geometry) %>% 
  st_transform(crs = wgs84) %>% 
  ms_simplify(0.05) %>% 
  left_join(crosswalk)

wda_sf <- left_join(counties, crosswalk) %>% 
  group_by(wda) %>% 
  summarize(wda_number = wda_number[1]) %>% 
  st_transform(crs = wgs84) %>% 
  # manually assign a color category so we can have a map w 4 colors where no contiguous are the same color
  # https://docs.google.com/presentation/d/1Op61zVEh0M9EfREZEu0GQi8pMvI_i6wvDfN_z09Ckng/edit#slide=id.p
  mutate(color_category = case_when(wda_number %in% c(2, 25, 8, 12, 28, 22) ~ 1,
                              wda_number %in% c(1, 11, 4, 16, 19, 23) ~ 2,
                              wda_number %in% c(9, 7, 13, 15, 27, 18) ~ 3,
                              wda_number %in% c(10, 3, 26, 20, 21, 17) ~ 4))

# add color category to county shapefile
colors <- wda_sf %>% 
  st_drop_geometry() %>% 
  select(wda_number, color_category)
counties <- left_join(counties, colors)
# wda_centroids <- st_centroid(wda_sf) %>% 
#   mutate(lat = sf::st_coordinates(.)[,1],
#          lon = sf::st_coordinates(.)[,2]) %>% 
#   st_drop_geometry()

saveRDS(wda_sf, here::here("clean-data", "wda_shapefile.rds"))
saveRDS(counties, here::here("clean-data", "county_shapefile.rds")) 
#saveRDS(wda_centroids, here::here("clean-data", "wda_centroids.rds"))

pal <- colorFactor(palette = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"), wda_sf$color_category)
leaflet() %>% #options = leafletOptions(zoomControl = FALSE, minZoom = 5, maxZoom = 5)) %>%
  #addProviderTiles("CartoDB.Positron") %>%
  setView(-99.9018, 30.9686, zoom = 5) %>% 
  addPolygons(stroke = F,
              fill = T,
              fillOpacity = 0.8,
              fillColor = ~pal(color_category),
              data = wda_sf) %>%
  addPolygons(color = "black",
              stroke = T,
              weight = 1,
              fill = F,
              data = counties) %>%
  addPolygons(stroke = T, 
              weight = 2,
              color = "black",
              opacity = 1,
              fill = T,
              fillOpacity = 0,
              label = wda_sf$wda,
              data = wda_sf) %>% 
  setMapWidgetStyle(list(background= "white")) %>% 
  htmlwidgets::onRender("function(el, x) { 
               map = this
               map.dragging.disable();
               }")
