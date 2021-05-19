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
  select(wfb, everything()) %>% 
  saveRDS(here::here("clean-data", "brookings-data.rds"))
