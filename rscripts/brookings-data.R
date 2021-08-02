library(tidyverse)
library(here)
library(janitor)

setwd(here::here("raw-data", "brookings"))
files <- dir(path = here::here("raw-data", "brookings"), pattern = "*.csv")
data <- tibble(File = files) %>%
  mutate(Data = lapply(File, read_csv)) %>%
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
  select(wda = wfb, occupational_code, occupation, share_of_local_jobs_percent, quality_index, demand_index, quality_and_demand_quadrant) 

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
    
  
export <- rbind(data, data_tx) %>% 
  saveRDS(here::here("clean-data", "brookings-data.rds"))
