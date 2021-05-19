#Create ALICE dummy dataset
library(tidyverse)
library(here)
library(janitor)
library(jaspatial)
jaspatial::load_geo_packages()
jaspatial::set_geo_options()

counties <- tigris::counties(48, cb=T) %>% 
  st_drop_geometry() %>% 
  clean_names() %>% 
  dplyr::select(geoid, county_name = name) %>% 
  arrange(county_name)

alice_categories <- c("single_cohab", "fam_kids", "over_65")

combos <- expand.grid(counties$county_name, alice_categories) %>% 
  mutate(
    poverty = "",
    alice = "",
    above_alice = ""
  ) %>% 
  dplyr::rename(county_name = "Var1", category = "Var2") %>% 
  left_join(., counties) %>% 
  dplyr::select(geoid, everything()) %>% 
  arrange(county_name)

write_csv(combos, here::here("raw-data", "alice-data-blank.csv"))


