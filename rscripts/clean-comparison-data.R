library(tidyverse)
library(jastyle)
library(leaflet) 
library(leaflet.extras)
library(highcharter)
library(tigris)
library(sf)
library(jastyle)
library(rmapshaper)

options(tigris_use_cache = TRUE)
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

###--- Load data -------------------------

# lwh - living wage households
load(here::here("clean-data", "alice_living_wage_hh.RData"))

# waa - working age adults/future workforce
waa <- readRDS(here::here("clean-data", "working-age-pop-2036.rds"))

# idj - indemand jobs
idj_raw <- readRDS(here::here("clean-data", "wda-jobs-proj-with-wages.rds"))
idj_summary <- readRDS(here::here("clean-data", "in-demand-jobs-summary.rds"))
lmi <- readRDS(here::here("clean-data", "lmi-wda-jobs-2028.rds")) # demand and earnings

# lwj - living wage jobs
lw <- readRDS(here::here("clean-data", "twc_living_wage_bands.rds"))
lw_industry <- readRDS(here::here("clean-data", "twc_living_wage_bands_by_industry.rds"))

# aj - attractive jobs 
aj <- readRDS(here::here("clean-data", "brookings-data.rds"))

# edu - education pipeline
edu <- readRDS(here::here("clean-data", "wda_edu_employment.rds")) # census
load(here::here("clean-data", "pseo-data.RData"))                  # pseo

# shapefiles
wda_sf <- readRDS(here::here("clean-data", "wda_shapefile.rds"))
counties <- tigris::counties(state = "48") %>% 
  dplyr::select(county = NAME, geometry) %>% 
  st_transform(crs = wgs84) %>% 
  ms_simplify(0.05)


###--- comparison table ---------------------------------

demand <- readRDS(here::here("clean-data", "in-demand-jobs-summary.rds")) %>% 
  filter(type == "top")
attractive <- aj %>% 
  select(wda = wfb, job = occupation)

t1 <- waa %>% 
  filter(year == "2036") %>% 
  select(wda, wda_number, contains("total")) %>% 
  mutate(wda = case_when(wda == "State of Texas" ~ "Texas",
                         T ~ wda))
