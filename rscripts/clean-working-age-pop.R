#Clean labor force projections (ages 25-64) by gender and race, 2010-2036
#Downloaded from state demographer's site: https://demographics.texas.gov/data/tpepp/projections/
#Methodology: https://demographics.texas.gov/Resources/TPEPP/Projections/2018/LaborForce/Methodology.pdf
#Too big to push to github

library(tidyverse)
library(here)
library(janitor)

#Load data --------
wda_crosswalk <- read_csv(here::here("raw-data", "county_wda_crosswalk.csv"))

df <- read_csv(here::here("raw-data", "2018allcntyindage.csv"))

clean <- df %>% 
  clean_names() %>% 
  mutate(
    fips_county = as.numeric(paste0("48", fips))
  ) %>% 
  filter(age_in_yrs_num>=25 & age_in_yrs_num<=64) %>% 
  left_join(., wda_crosswalk, by="fips_county") %>% 
  mutate(
    wda = ifelse(area_name=="State of Texas", area_name, wda)
  ) %>% 
  group_by(wda, wda_number, year) %>% 
  summarise(
    across(total:nh_other_female, sum, na.rm=T)
  ) %>% 
  filter(year<=2036)

write_rds(clean, here::here("clean-data", "working-age-pop-2036.rds"))

