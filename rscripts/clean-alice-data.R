#Clean ALICE data
library(tidyverse)
library(here)
library(janitor)
library(readxl)

#Load data --------
wda_crosswalk <- read_csv(here::here("raw-data", "county_wda_crosswalk.csv"))
alice_download <- readxl::read_excel(here::here("raw-data", "alice-data-tx-download.xlsx"), sheet = 2) %>% clean_names()
alice_upwork <- read_csv(here::here("raw-data", "alice-data-upwork.csv"))

#Number and share of households earning a living wage
#Single families, married w/ kids, older adults

alice_hh_counts<- alice_download %>% 
  filter(year==2018) %>% 
  mutate(geo_id2 = as.numeric(geo_id2)) %>% 
  dplyr::select(fips_county = geo_id2, county, household, poverty_household, alice_household, above_alice_household) %>%
  left_join(., wda_crosswalk, by="fips_county") %>% 
  group_by(wda, wda_number) %>% 
  summarise(
    across(household:above_alice_household, sum, na.rm=T)
  ) %>% 
  mutate(
    below_poverty_hh_share = poverty_household/household * 100,
    below_alice_hh_share = (poverty_household + alice_household)/household * 100,
    above_alice_hh_share = 100-below_alice_hh_share
  )

alice_demographics <- alice_upwork %>% 
  left_join(., wda_crosswalk, by=c("geoid"="fips_county")) %>% 
  group_by(wda, wda_number, category) %>% 
  summarise(
    across(poverty:above_alice, sum, na.rm=T)
  )

#   gather(type, est, poverty:above_alice) %>% 
#   group_by(county_name, category) %>% 
#   mutate(
#     pct = est/sum(est) * 100
#   ) %>% 
#   ungroup() %>% 
#   gather(measure, value, est:pct) %>% 
#   unite("category_type_measure", category:measure, sep=".") %>% 
#   spread(category_type_measure, value)




