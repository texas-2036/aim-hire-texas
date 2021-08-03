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

#County population
acs <- load_variables(2019, "acs5", cache=T)
county_pop <- tidycensus::get_acs(
  geography = "county", state = 48,
  variables = "B01001_001", cache_table = T
) %>% 
  clean_names() %>% 
  mutate(geoid = as.numeric(geoid)) %>% 
  dplyr::select(geoid, totpop=estimate)


#Alice wage bands
alice <- readRDS(here::here("raw-data", "alice-wage-data.rds")) %>% 
  filter(x=="Two Adults, 2 in Child Care") %>% 
  left_join(., wda_crosswalk, by="county") %>% 
  left_join(., county_pop, by=c("fips_county" = "geoid")) %>% 
  group_by(wda, wda_number) %>% 
  summarise(
    alice_wage_avg = weighted.mean(annually, wt=totpop, na.rm=T),
    alice_high = alice_wage_avg + 20000,
    alice_low = alice_wage_avg - 20000
  )




#This now exists in clean-lmi-data.R
# #Living wages by occupation ---------
twc_2019 <- readxl::read_excel(here::here("raw-data/twc-lmi/twc-lmi-wages-2019.xlsx")) %>%
  clean_names() %>%
  mutate(
    no_of_employed = ifelse(no_of_employed=="N/A", NA, as.numeric(no_of_employed)),
    median_wage = ifelse(median_wage=="N/A", NA, as.numeric(median_wage))
  ) %>%
  filter(!is.na(median_wage) & soc_code != "00-0000") %>%
  mutate(detail= str_sub(soc_code, -1)) %>%
  filter(detail!=0) %>%
  dplyr::select(wda28_name = area, soc_code, occupation_title, no_of_employed, median_wage) %>%
  left_join(., wda28_crosswalk) %>%
  mutate(
    wda = ifelse(is.na(wda), "Texas", wda)
  ) %>%
  group_by(wda, wda_number, soc_code, occupation_title) %>%
  summarise(
    median_wage = weighted.mean(median_wage, wt=no_of_employed, na.rm=T),
    no_of_employed = sum(no_of_employed, na.rm=T)
  ) %>%
  left_join(., alice) %>% 
  mutate(
    wage_band = case_when(
      median_wage>alice_high ~ "High Wage",
      median_wage>alice_wage_avg & median_wage<=alice_high ~ "Mid-High Wage",
      median_wage>=alice_low & median_wage<=alice_wage_avg ~ "Mid-Low Wage",
      median_wage<alice_low ~ "Low Wage"
    ),
    wage_band = factor(wage_band, levels = c("Low Wage", "Mid-Low Wage","Mid-High Wage","High Wage"))
  )

twc_2019_texas <- twc_2019 %>% 
  ungroup() %>% 
  clean_names() %>%
  mutate(
    no_of_employed = ifelse(no_of_employed=="N/A", NA, as.numeric(no_of_employed)),
    median_wage = ifelse(median_wage=="N/A", NA, as.numeric(median_wage))
  ) %>%
  filter(!is.na(median_wage) & soc_code != "00-0000") %>%
  mutate(detail= str_sub(soc_code, -1)) %>%
  filter(detail!=0) %>%
  group_by(soc_code, occupation_title) %>% 
  summarize(median_wage = weighted.mean(median_wage, wt=no_of_employed, na.rm=T),
            no_of_employed = sum(no_of_employed, na.rm=T)) %>% 
  mutate(
    alice_wage_avg = 55000,
    alice_high = alice_wage_avg + 20000,
    alice_low = alice_wage_avg - 20000,
    wage_band = case_when(
      median_wage>alice_high ~ "High Wage",
      median_wage>alice_wage_avg & median_wage<=alice_high ~ "Mid-High Wage",
      median_wage>=alice_low & median_wage<=alice_wage_avg ~ "Mid-Low Wage",
      median_wage<alice_low ~ "Low Wage"
    ),
    wage_band = factor(wage_band, levels = c("Low Wage", "Mid-Low Wage","Mid-High Wage","High Wage"))
  ) %>% 
  mutate(wda = "Texas",
         wda_number = 0)
  
twc_2019 <- rbind(twc_2019, twc_2019_texas)

saveRDS(twc_2019, file=here::here("clean-data", "twc_living_wage_bands.rds"))


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
  filter(!is.na(median_wage) & soc_code != "00-0000"
         ) %>% 
  mutate(detail= str_sub(soc_code, -1)) %>%
  filter(detail!=0) %>%
  dplyr::select(wda28_name = area, naics_code, industry_title, soc_code, occupation_title, no_of_employed, median_wage) %>% 
  left_join(., wda28_crosswalk, by="wda28_name") %>% 
  mutate(
    wda = ifelse(is.na(wda), "Texas", wda)
  ) %>% 
  # remove soc_code and occupation_title?
  group_by(wda, wda_number, naics_code, industry_title, soc_code, occupation_title) %>% 
  #group_by(wda, wda_number, naics_code, industry_title) %>% 
  summarise(
    median_wage = weighted.mean(median_wage, wt=no_of_employed, na.rm=T),
    no_of_employed = sum(no_of_employed, na.rm=T)  
  ) %>% 
  left_join(., alice) %>% 
  mutate(
    alice_wage_avg = ifelse(is.na(alice_wage_avg), 55000, alice_wage_avg) ,
    alice_high = ifelse(is.na(alice_high), 55000 + 20000, alice_high) ,
    alice_low = ifelse(is.na(alice_low), 55000 - 20000, alice_low) ,
    wage_band = case_when(
      median_wage>alice_high ~ "High Wage",
      median_wage>alice_wage_avg & median_wage<=alice_high ~ "Mid-High Wage",
      median_wage>=alice_low & median_wage<=alice_wage_avg ~ "Mid-Low Wage",
      median_wage<alice_low ~ "Low Wage"
    ),
    wage_band = factor(wage_band, levels = c("Low Wage", "Mid-Low Wage","Mid-High Wage","High Wage"))
  ) %>% 
  ungroup() %>% 
  group_by(wda, wda_number, industry_title, wage_band) %>% 
  summarize(no_of_employed = sum(no_of_employed)) 


saveRDS(wda_df, file=here::here("clean-data", "twc_living_wage_bands_by_industry.rds"))


# test <- wda_df %>%
#   group_by(wda, industry_title, wage_band) %>%
#   summarise(
#     n_jobs = sum(no_of_employed, na.rm=T)
#   )
# 
# # testdf <- do.call(rbind, pull_data) %>% 
# #   clean_names() %>% 
# #   filter(area == "Capital Area") %>% 
# #   mutate(detail= str_sub(soc_code, -1)) %>% 
# #   filter(detail!=0)
# # 
# test <- testdf %>%
#   mutate(
#     no_of_employed = ifelse(no_of_employed=="N/A", NA, as.numeric(no_of_employed)),
#     median_wage = ifelse(median_wage=="N/A", NA, as.numeric(median_wage))
#   ) %>%
#   group_by(industry_title) %>%
#   summarise(
#     n_jobs = sum(no_of_employed, na.rm=T)
#   )

