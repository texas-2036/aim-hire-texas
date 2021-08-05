#PSEO institutions data on Texas university graduates
library(tidyverse)
library(here)
library(janitor)
library(readxl)

#PSEO data: https://lehd.ces.census.gov/data/pseo_experimental.html
#PSEO codebook: https://lehd.ces.census.gov/data/schema/latest/lehd_public_use_schema.html

#Load data ----------
pseo_inst <- read_csv(here::here("raw-data/pseo/pseo_tx_institutions.csv")) %>% 
  mutate(label = str_trim(label))

pseoe <- read_csv(here::here("raw-data/pseo/pseoe_tx.csv"))

pseof <- read_csv(here::here("raw-data/pseo/pseof_tx.csv"))


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


geo_crosswalk <- readxl::read_excel(here::here("raw-data/pseo/institution_geo_crosswalk.xlsx")) %>% 
  clean_names() %>% 
  mutate(
    label_institution_earnings = str_replace_all(label_institution_earnings, "\r\n", " ")  
  ) %>% 
  left_join(., wda28_crosswalk, by=c("wda" = "wda28_name")) %>% 
  mutate(
    wda.y = ifelse(wda == "Lower Rio Grande Valley", "Rio Grande Valley", wda.y),
    wda_number = ifelse(wda == "Lower Rio Grande Valley", 23, wda_number)
  ) %>% 
  dplyr::select(-wda) %>% 
  dplyr::rename(wda = wda.y)

#Clean data ----------
#All Cohorts, By Degree Bachelors/Associates/Certificates, 10 years after graduation
earnings <- pseoe %>% 
  filter(grad_cohort=="0000") %>% 
  filter(cipcode=="00") %>% 
  filter(degree_level %in% c("01", "02", "03", "04", "05")) %>% 
  filter(status_y10_earnings==1) %>% 
  dplyr::select(institution, degree_level, starts_with("y10")) #%>% 
  #gather(measure, estimate, y10_p25_earnings:y10_ipeds_count) %>% 
  #separate(measure, into=c("years_out", "measure"), sep="0_") %>% 
  #mutate(years_out = "10", topic = "earnings")

#Also filter for ALL industries, national
employment <- pseof %>% 
  filter(grad_cohort=="0000") %>% 
  filter(cipcode=="00") %>% 
  filter(degree_level %in% c("01", "02", "03", "04", "05")) %>% 
  filter(status_y10_grads_emp==1) %>% 
  filter(industry=="00") %>% 
  filter(geo_level == "N") %>% 
  dplyr::select(institution, degree_level, starts_with("y10")) #%>% 
  #gather(measure, estimate, y10_grads_emp:y10_grads_nme) %>% 
  #separate(measure, into=c("years_out", "measure"), sep="0_") %>% 
  #mutate(years_out = "10", topic = "employment")


#Combined and aggregate data ----------------
pseo_inst_df <- left_join(earnings, employment) %>% 
  left_join(., pseo_inst) %>% 
  left_join(., geo_crosswalk, by=c("label" = "label_institution_earnings")) 


pseo_wda_df <- pseo_inst_df %>% 
  group_by(wda, wda_number, degree_level) %>% 
  summarise(
    across(y10_p25_earnings:y10_p75_earnings, weighted.mean, wt=y10_grads_earn, na.rm=T),
    across(y10_grads_earn:y10_grads_nme, sum, na.rm=T),
    num_of_institutions = n()
  )

pseo_tx_df <- pseo_wda_df %>% 
  group_by(degree_level) %>% 
  summarise(y10_p25_earnings = weighted.mean(y10_p25_earnings, y10_grads_earn, na.rm = T),
            y10_p50_earnings = weighted.mean(y10_p50_earnings, y10_grads_earn, na.rm = T), 
            y10_p75_earnings = weighted.mean(y10_p75_earnings, y10_grads_earn, na.rm = T),
            y10_grads_earn = sum(y10_grads_earn, na.rm = T),
            y10_ipeds_count = sum(y10_ipeds_count, na.rm = T),
            y10_grads_emp = sum(y10_grads_emp, na.rm = T),
            y10_grads_emp_instate = sum(y10_grads_emp_instate, na.rm = T),
            y10_grads_nme = sum(y10_grads_nme, na.rm = T),
            num_of_institutions = sum(num_of_institutions, na.rm = T)) %>% 
  mutate(wda_name = "Texas")

pseo_wda_df <- rbind(pseo_wda_df, pseo_tx_df)

save(pseo_inst_df, pseo_wda_df, file=here::here("clean-data", "pseo-data.RData"))





