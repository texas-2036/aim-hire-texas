#Living wage jobs
library(tidyverse)
library(here)
library(janitor)

#For definitions start on slide 114 here: https://docs.google.com/presentation/d/1u2ku2drUqQrQFzIqrPYAD29Y21XeZtas/edit#slide=id.p114

twc_2019 <- readxl::read_excel(here::here("raw-data/twc-lmi/twc-lmi-wages-2019.xlsx")) %>% 
  clean_names() %>% 
  mutate(
    median_wage = ifelse(median_wage=="N/A", NA, as.numeric(median_wage)),
    wage_band = case_when(
      median_wage>65000 ~ "High Wage",
      median_wage>45000 & median_wage<=65000 ~ "Mid-High Wage",
      median_wage>=25000 & median_wage<=45000 ~ "Mid-Low Wage",
      median_wage<25000 ~ "Low Wage"
    ),
    wage_band = factor(wage_band, levels = c("Low Wage", "Mid-Low Wage","Mid-High Wage","High Wage"))
  ) %>% 
  filter(!is.na(wage_band)) %>% 
  dplyr::select(wda = area, soc_code, occupation_title, no_of_employed, median_wage, wage_band)

saveRDS(twc_2019, file=here::here("clean-data", "twc_living_wage_bands.rds"))
