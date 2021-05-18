library(tidyverse)
library(tigris)
library(here)
library(sf)

options(tigris_use_cache = TRUE, tigris_class = "sf")
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

crosswalk <- read.csv(here::here("raw-data", "county_wda_crosswalk.csv"))

counties <- tigris::counties(state = "Texas") %>% 
  dplyr::select(county = NAME, geometry)

wda_sf <- left_join(counties, crosswalk) %>% 
  group_by(wda) %>% 
  summarize(wda_number = wda_number[1])

saveRDS(wda_sf, here::here("clean-data", "wda_shapefile.rds"))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-96.3, 31.3, zoom = 7) %>%
  addPolygons(color = "black",
              stroke = T,
              weight = 1,
              fill = F,
              data = wda_sf)
