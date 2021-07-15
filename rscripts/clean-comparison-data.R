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

###--- people table ---------------------------------

# workforce total and re breakdown
t1 <- waa %>% 
  filter(year == "2036") %>% 
  select(wda, wda_number, contains("total")) %>% 
  select(wda, wda_number, 
         "Working age adults" = total, 
         "Working age adults: White" = nh_white_total,
         "Working age adults: Black" = nh_black_total,
         "Working age adults: Hispanic" = hispanic_total,
         "Working age adults: Asian" = nh_asian_total,
         "Working age adults: Other" = nh_other_total) %>% 
  mutate(across("Working age adults":"Working age adults: Other",
                ~ prettyNum(., big.mark = ","))) %>% 
  #pivot_longer(White:Other, names_to = "race", values_to = "number") %>% 
  mutate(wda = case_when(wda == "State of Texas" ~ "Texas",
                         T ~ wda)) 

t2_wda <- alice_hh_counts %>% 
  ungroup() %>% 
  filter(year == "2018")
t2_texas <- t2_wda %>% 
  ungroup() %>% 
  summarize(year = "2018", wda = "Texas", wda_number = NA, 
            household = sum(household),
            poverty_household = sum(poverty_household),
            alice_household = sum(alice_household),
            above_alice_household = sum(above_alice_household)) %>% 
  mutate(below_poverty_hh_share = 100 * poverty_household / household,
         below_alice_hh_share = 100 * (poverty_household + alice_household) / household,
         above_alice_hh_share = 100 * above_alice_household / household,)
t2 <- rbind(t2_wda, t2_texas) %>% 
  select(wda, wda_number, 
         "Number of households above ALICE threshold" = above_alice_household, 
         "Share of households above ALICE threshold" = above_alice_hh_share) %>% 
  mutate("Number of households above ALICE threshold" = prettyNum(`Number of households above ALICE threshold`, big.mark = ","),
         "Share of households above ALICE threshold" = round(`Share of households above ALICE threshold`, 1),
         "Share of households above ALICE threshold" = as.character(`Share of households above ALICE threshold`))

View(t2)

t3_wda <- edu %>% 
  filter(education == "hs") %>% 
  select(wda, wda_number, wda_number_people, median_income, pct_employed)
t3_texas <- t3_wda %>% 
  summarize(wda = "Texas", wda_number = NA,
            median_income = weighted.mean(median_income, wda_number_people, na.rm = T),
            pct_employed = weighted.mean(pct_employed, wda_number_people, na.rm = T),
            wda_number_people = sum(wda_number_people))
t3 <- rbind(t3_wda, t3_texas) %>% 
  select(wda, wda_number, 
         "Median income for high school grads" = median_income, 
         "Employment rate of high school grads" = pct_employed) %>% 
  mutate("Median income for high school grads" = prettyNum(`Median income for high school grads`, big.mark = ","),
         "Employment rate of high school grads" = round(`Employment rate of high school grads`, 1),
         "Employment rate of high school grads" = as.character(`Employment rate of high school grads`))

people <- left_join(t1, t2) %>% 
  left_join(t3) %>% 
  ungroup() %>% 
  select(-wda_number) %>% 
  pivot_longer("Working age adults":"Employment rate of high school grads") %>% 
  pivot_wider(id_cols = name, names_from = wda, values_from = value) %>% 
  select(Characteristic = name, Texas, everything())
saveRDS(people, here::here("clean-data", "comparison_table_people.rds"))
###--- jobs table ---------------------------------
# idea: just list top 10 in-demand jobs and add a symbol for living wage ($) and/or attractive (*)
# but which data source to use... all brookings? can't join brookings and lmi
demand <- readRDS(here::here("clean-data", "in-demand-jobs-summary.rds")) %>% 
  filter(type == "top")
attractive <- aj %>% 
  select(wda = wfb, job = occupation)