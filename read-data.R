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
library(kableExtra)
library(formattable)
library(paletteer)
library(reactable)

options(tigris_use_cache = TRUE)
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# "#2a366c" dark blue
# "#f26852" red
# "#5f6fc1" light blue
# "#3ead92" green

# c("#2a366c", "#f26852", "#5f6fc1", "#3ead92")

#actual tx2036 colors
# c("#002D74", "#F26852", "#2A7DE1", "#00A9C5", "#3A4A9F")

###--- Highcharts theme ------------------

tx2036_hc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(chart = list(backgroundColor = "transparent", 
                        style = list(fontFamily = "Montserrat", fontSize = "28px", 
                                     color="#fff",fontWeight="500", textTransform="uppercase")),
           title = list(style = list(fontFamily = "Montserrat", 
                                     fontWeight = "bold",
                                     color="white"),
                        align = "left"), 
           subtitle = list(style = list(fontFamily = "Montserrat", 
                                        color="#fff",
                                        textTransform="initial",
                                        fontWeight="400",
                                        fontSize = "14px"),
                           align = "left"), 
           legend = list(align = "right", 
                         style = list(fontFamily = "Montserrat", color="white"),
                         itemStyle = list(fontFamily = 'Montserrat', color = 'white'),
                         itemHoverStyle = list(color = 'gray'),   
                         verticalAlign = "top"),
           credits = list(style = list(color = "#fff")),
           xAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                        title = list(style = list(color = "#fff", fontSize = "12px", 
                                                  color="#fff",fontWeight="500")),
                        gridLineWidth = 0,
                        gridLineColor = "#F3F3F3", 
                        lineColor = 'rgba(255,255,255,0.7)', 
                        minorGridLineColor = 'rgba(243,243,243,0.7)', 
                        tickColor = "#F3F3F3", 
                        tickWidth = 1), 
           yAxis = list(labels =list(style = list(fontFamily = "Montserrat", color="#fff")), 
                        title = list(style = list(color = "#fff", fontSize = "12px", 
                                                  color="#fff",fontWeight="500")), 
                        gridLineWidth = .5,
                        gridLineColor = 'rgba(243,243,243,0.15)', 
                        lineColor = 'rgba(255,255,255,0.15)', 
                        minorGridLineColor = 'rgba(243,243,243,0.15)', 
                        tickColor = "#F3F3F3", 
                        tickWidth = 2)))

###--- Load data -------------------------
load(here::here("clean-data", "alice_living_wage_hh.RData"))
wda_sf <- readRDS(here::here("clean-data", "wda_shapefile.rds"))
counties <- readRDS(here::here("clean-data", "county_shapefile.rds"))
crosswalk <- read.csv(here::here("clean-data", "county_wda_crosswalk.csv"))
# lmi <- readRDS(here::here("clean-data", "lmi-wda-jobs-2028.rds"))
waa <- readRDS(here::here("clean-data", "working-age-pop-2036.rds"))
aj <- readRDS(here::here("clean-data", "brookings-data.rds"))
# demand <- readRDS(here::here("clean-data", "faethm-jobs-2036.rds"))
edu <- readRDS(here::here("clean-data", "wda_edu_employment.rds"))
# lw <- readRDS(here::here("clean-data", "twc_living_wage_bands.rds"))
# load(here::here("clean-data", "pseo-data.RData"))

###--- Helper functions ------------------------
disconnected <- sever_default(
  title = "Howdy!", 
  subtitle = "There's a lot of data here, so this app has been resting while you were away.", 
  button = "Push to Wake", 
  button_class = "info"
)
