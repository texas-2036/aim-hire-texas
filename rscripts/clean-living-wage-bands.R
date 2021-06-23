#Living wage jobs
library(tidyverse)
library(here)
library(janitor)
library(readxl)

#For definitions start on slide 114 here: https://docs.google.com/presentation/d/1u2ku2drUqQrQFzIqrPYAD29Y21XeZtas/edit#slide=id.p114

wda_crosswalk <- read_csv(here::here("raw-data", "county_wda_crosswalk.csv")) %>% 
  mutate(
    wda28_name = case_when(
      wda=="DFW" & !(county %in% c("Dallas", "Tarrant")) ~ "North Central",
      wda=="DFW" & county %in% c("Tarrant") ~ "Tarrant County",
      wda=="DFW" & county %in% c("Dallas") ~ "Dallas",
      county=="Travis" ~ "Capital Area",
      wda=="Greater Austin" & !(county %in% c("Travis")) ~ "Rural Capital",
      county=="Cameron" ~ "Cameron County",
      wda=="Rio Grande Valley" & !(county %in% c("Cameron")) ~ "Lower Rio Grande",
      wda=="The Heart of Texas" ~ "Heart of Texas",
      wda=="Northeast Texas" ~ "North East",
      wda=="West Central Texas" ~ "West Central",
      wda=="Southeast Texas" ~ "South East Texas",
      TRUE ~ wda
    )
  )

wda28_crosswalk <- wda_crosswalk %>% 
  group_by(wda28_name) %>% 
  summarise(
    across(wda:wda_number, first)
  )

#This now exists in clean-lmi-data.R
# #Living wages by occupation ---------
# twc_2019 <- readxl::read_excel(here::here("raw-data/twc-lmi/twc-lmi-wages-2019.xlsx")) %>% 
#   clean_names() %>% 
#   mutate(
#     no_of_employed = ifelse(no_of_employed=="N/A", NA, as.numeric(no_of_employed)),
#     median_wage = ifelse(median_wage=="N/A", NA, as.numeric(median_wage))
#   ) %>% 
#   filter(!is.na(median_wage) & soc_code != "00-0000") %>% 
#   dplyr::select(wda28_name = area, soc_code, occupation_title, no_of_employed, median_wage) %>% 
#   left_join(., wda28_crosswalk) %>% 
#   mutate(
#     wda = ifelse(is.na(wda), "Texas", wda)
#   ) %>% 
#   group_by(wda, wda_number, soc_code, occupation_title) %>% 
#   summarise(
#     median_wage = weighted.mean(median_wage, wt=no_of_employed, na.rm=T),
#     no_of_employed = sum(no_of_employed, na.rm=T)  
#   ) %>% 
#   mutate(
#     wage_band = case_when(
#       median_wage>65000 ~ "High Wage",
#       median_wage>45000 & median_wage<=65000 ~ "Mid-High Wage",
#       median_wage>=25000 & median_wage<=45000 ~ "Mid-Low Wage",
#       median_wage<25000 ~ "Low Wage"
#     ),
#     wage_band = factor(wage_band, levels = c("Low Wage", "Mid-Low Wage","Mid-High Wage","High Wage"))
#   )
# 
# 
# saveRDS(twc_2019, file=here::here("clean-data", "twc_living_wage_bands.rds"))


#By industry-occupation ------------
texas <- readxl::read_excel(here::here("raw-data/twc-lmi-occ-ind/Wages Report.xlsx"))

pull_data <- lapply(1:26, function(x){
  readxl::read_excel(here::here("raw-data", "twc-lmi-occ-ind", paste0("Wages Report (", x, ").xlsx")))
})

wda_df <- do.call(rbind, pull_data) %>% 
  rbind(., texas) %>% 
  clean_names() %>% 
  mutate(
    no_of_employed = ifelse(no_of_employed=="N/A", NA, as.numeric(no_of_employed)),
    median_wage = ifelse(median_wage=="N/A", NA, as.numeric(median_wage))
  ) %>% 
  filter(!is.na(median_wage) & soc_code != "00-0000") %>% 
  dplyr::select(wda28_name = area, naics_code, industry_title, soc_code, occupation_title, no_of_employed, median_wage) %>% 
  left_join(., wda28_crosswalk, by="wda28_name") %>% 
  mutate(
    wda = ifelse(is.na(wda), "Texas", wda)
  ) %>% 
  group_by(wda, wda_number, naics_code, industry_title, soc_code, occupation_title) %>% 
  summarise(
    median_wage = weighted.mean(median_wage, wt=no_of_employed, na.rm=T),
    no_of_employed = sum(no_of_employed, na.rm=T)  
  ) %>% 
  mutate(
    wage_band = case_when(
      median_wage>65000 ~ "High Wage",
      median_wage>45000 & median_wage<=65000 ~ "Mid-High Wage",
      median_wage>=25000 & median_wage<=45000 ~ "Mid-Low Wage",
      median_wage<25000 ~ "Low Wage"
    ),
    wage_band = factor(wage_band, levels = c("Low Wage", "Mid-Low Wage","Mid-High Wage","High Wage"))
  )


saveRDS(wda_df, file=here::here("clean-data", "twc_living_wage_bands_by_industry.rds"))








