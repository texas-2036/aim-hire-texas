library(tidyverse)
library(here)
library(janitor)



#Brookings data
setwd(here::here("raw-data", "brookings"))

#WDA28 to WDA24 crosswalk
wda_crosswalk <- read_csv(here::here("raw-data","wda_brookings_crosswalk.csv"))

#Import and clean data
files <- dir(path = here::here("raw-data", "brookings"), pattern = "*.csv") 
data <- tibble(File = files) %>% 
  mutate(Data = lapply(File, read_csv)) %>% #This loads and binds all the files together
  unnest(Data) %>% 
  clean_names() %>% 
  rename(quality_index = quality_index_5_to_5,
         demand_index = demand_index_5_to_5,
         absorption_index = absorption_index_5_to_5,
         pct_w_assoc = share_with_at_least_an_associates_degree,
         mobility_score = mid_term_mobility_score) %>%
  mutate(across(where(is.numeric), round, 3)) %>% 
  mutate(wfb = sub(".* - ", "", file),
         wfb = gsub(".csv", "", wfb)) %>% 
  select(-file) %>% 
  select(wda = wfb, occupational_code, occupation, share_of_local_jobs_percent, quality_index, demand_index, quality_and_demand_quadrant) %>% 
  left_join(., wda_crosswalk, by=c("wda" = "wda28"))

#Aggregate up to our WDA 24
data_wda24 <- data %>% 
  group_by(wda.y, occupational_code, occupation) %>% 
  summarize(share_of_local_jobs_percent = round(mean(share_of_local_jobs_percent, na.rm = T), 3),
            quality_index = round(mean(quality_index, na.rm = T), 3),
            demand_index = round(mean(demand_index, na.rm = T), 3)) %>% 
  mutate(quality_score = case_when(quality_index >= 0 ~ "High quality",
                                   quality_index < 0 ~ "Low quality"),
         demand_score = case_when(demand_index >= 0 ~ "High demand",
                                  demand_index < 0 ~ "Low demand")) %>% 
  mutate(quality_and_demand_quadrant = paste0(quality_score, " - ", demand_score)) %>% 
  rename(wda = wda.y) %>% 
  select(-quality_score, -demand_score)

#Aggregate up to Texas
data_tx <- data %>% 
  group_by(occupational_code, occupation) %>% 
  summarize(share_of_local_jobs_percent = mean(share_of_local_jobs_percent, na.rm = T),
            quality_index = mean(quality_index, na.rm = T),
            demand_index = mean(demand_index, na.rm = T)) %>% 
  mutate(quality_score = case_when(quality_index >= 0 ~ "High quality",
                                   quality_index < 0 ~ "Low quality"),
         demand_score = case_when(demand_index >= 0 ~ "High demand",
                                  demand_index < 0 ~ "Low demand")) %>% 
  mutate(quality_and_demand_quadrant = paste0(quality_score, " - ", demand_score),
         wda = "Texas") %>% 
  select(-quality_score, -demand_score)
    
  
export <- rbind(data_wda24, data_tx) %>% 
  filter(!is.na(occupation)) %>% 
  saveRDS(here::here("clean-data", "brookings-data.rds"))
