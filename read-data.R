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
# library(scroller)
library(shinydashboard)
library(texas2036)
library(kableExtra)
library(formattable)
library(paletteer)
library(reactable)
library(slickR)
library(sparkline)
library(DT)
library(purrr)
library(waiter)

options(tigris_use_cache = TRUE)
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# "#f26852" red
# "#2a366c" dark blue
# "#3ead92" green
# "#5f6fc1" light blue
# "#f9cd21" yellow
# 

###--- Highcharts theme ------------------

tx2036_hc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(colors = c("#f26852", "#2a366c", "#3ead92", "#5f6fc1", "#f9cd21"), 
           marker = list(fillColor = c("#f26852", "#2a366c", "#3ead92", "#5f6fc1", "#f9cd21"), 
                         lineColor = "#000", radius = 3, lineWidth = 1, symbol = "circle"), 
           chart = list(backgroundColor = "transparent", 
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
           caption = list(style = list(color = "fff", 
                                       fontSize = "12px",
                                       fontWeight = "500"),
                          x = 15),
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

ggtheme <- function (base_size = 14,
                     base_family = "Montserrat",
                     title_size = 23,
                     subtitle_size = 12,
                     caption_size = 10,
                     ...)
{
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family, ...) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = title_size,
        color = "#3A4A9F",
        family = "Montserrat-ExtraBold"
      ),
      plot.subtitle = ggtext::element_markdown(size = subtitle_size,
                                               family = "Montserrat"),
      plot.caption = ggplot2::element_text(
        family = "Montserrat-Regular",
        color = "#8C8F93",
        size = caption_size,
        lineheight = 1,
        hjust = 0,
        vjust = -5
      ),
      axis.title.x = ggplot2::element_text(
        family = "Montserrat-Bold",
        size = 8,
        color = "#6B6D6F"
      ),
      axis.title.y = ggplot2::element_text(
        family = "Montserrat-Bold",
        size = 8,
        color = "#6B6D6F"
      ),
      legend.position = "none",
      plot.margin = ggplot2::unit(c(
        t = 1,
        r = 1.5,
        b = 2,
        l = 1
      ), "lines")
    ) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(color = "#5d5d5d",
                                          size = 0.8),
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      panel.grid.major.y = ggplot2::element_line(color = "#e3e3e3"),
      panel.grid.minor.y = ggplot2::element_line(
        linetype = 2,
        size = 0,
        color = "#e3e3e3"
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(hjust = 1),
      axis.title.y = ggplot2::element_text(hjust = 1),
      axis.ticks.x = ggplot2::element_line(size = 0.5)
    )
}
###--- Load data -------------------------
load(here::here("clean-data", "alice_living_wage_hh.RData"))
wda_sf <- readRDS(here::here("clean-data", "wda_shapefile.rds"))
texas_sf <- readRDS(here::here("clean-data", "texas_shapefile.rds"))
counties <- readRDS(here::here("clean-data", "county_shapefile.rds"))
county_list <- readRDS(here::here("clean-data", "wda_county_list.RDS"))
crosswalk <- read.csv(here::here("clean-data", "county_wda_crosswalk.csv"))
# lmi <- readRDS(here::here("clean-data", "lmi-wda-jobs-2028.rds"))
waa <- readRDS(here::here("clean-data", "working-age-pop-2036.rds"))
aj <- readRDS(here::here("clean-data", "brookings-data.rds"))
idj <- readRDS(here::here("clean-data", "in-demand-jobs-summary.rds"))
#idj <- readRDS(here::here("clean-data", "wda-jobs-proj-with-wages.rds"))

edu <- readRDS(here::here("clean-data", "wda_edu_employment.rds"))
# lw <- readRDS(here::here("clean-data", "twc_living_wage_bands.rds"))
load(here::here("clean-data", "pseo-data.RData"))
pseo_wda_df <- pseo_wda_df %>% 
  mutate(wda = case_when(is.na(wda) ~ "Texas",
                         T ~ wda),
         wda_number = case_when(is.na(wda_number) ~ 0,
                                T ~ wda_number))
lwj_industry <- readRDS(here::here("clean-data", "twc_living_wage_bands_by_industry.rds"))
lwj_wages <- readRDS(here::here("clean-data", "wda-jobs-proj-with-wages.rds"))

comparison_people <- readRDS(here::here("clean-data", "comparison_table_people_sparkline.rds"))
comparison_jobs <- readRDS(here::here("clean-data", "comparison_table_jobs.rds"))

###--- Helper functions ------------------------
disconnected <- sever_default(
  title = "Howdy!", 
  subtitle = "There's a lot of data here, so this app has been resting while you were away.", 
  button = "Push to Wake", 
  button_class = "info"
)

waiting_screen <- tagList(
  spin_solar(),
  h4("Loading data - one moment!")
) 



