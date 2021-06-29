library(tidyverse)
library(jastyle)
library(leaflet) 
library(leaflet.extras)
library(highcharter)
library(tigris)
library(sf)
library(jastyle)
library(shiny)
library(sever)
library(shinyjs)
library(rmapshaper)
library(scroller)
library(shinydashboard)
library(texas2036)
library(gt)
library(glue)
library(paletteer)


options(tigris_use_cache = TRUE)
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# "#2a366c" dark blue
# "#f26852" red
# "#5f6fc1" light blue
# "#3ead92" green

# c("#2a366c", "#f26852", "#5f6fc1", "#3ead92")

#actual tx2036 colors
# c("#002D74", "#F26852", "#2A7DE1", "#00A9C5", "#3A4A9F")

# ###--- Load data -------------------------
wda_sf <- readRDS(here::here("clean-data", "wda_shapefile.rds"))
counties <- readRDS(here::here("clean-data", "county_shapefile.rds"))
crosswalk <- read.csv(here::here("clean-data", "county_wda_crosswalk.csv"))
# lmi <- readRDS(here::here("clean-data", "lmi-wda-jobs-2028.rds"))
waa <- readRDS(here::here("clean-data", "working-age-pop-2036.rds"))
aj <- readRDS(here::here("clean-data", "brookings-data.rds"))
# demand <- readRDS(here::here("clean-data", "faethm-jobs-2036.rds"))
# edu <- readRDS(here::here("clean-data", "wda_edu_employment.rds"))
# lw <- readRDS(here::here("clean-data", "twc_living_wage_bands.rds"))
# load(here::here("clean-data", "pseo-data.RData"))

###--- Helper functions ------------------------
disconnected <- sever_default(
  title = "Howdy!", 
  subtitle = "There's a lot of data here, so this app has been resting while you were away.", 
  button = "Push to Wake", 
  button_class = "info"
)
