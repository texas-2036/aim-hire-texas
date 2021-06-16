library(tidyverse)
library(jastyle)

###--- Working age adults ----------------

waa <- readRDS(here::here("clean-data", "working-age-pop-2036.rds"))
View(waa)
waa %>% 
  filter(wda == "Gulf Coast") %>% 
  hchart(type = "line", color = "#8da0cb",hcaes(x = year, y = total)) %>% 
  hc_yAxis(title = list(text = "Number of working age adults")) %>% 
  hc_title(text = "Projected Number of Working Age Adults Through 2036")

waa %>% 
  filter(wda == "Gulf Coast") %>% 
  select(year, nh_white_total, nh_black_total, hispanic_total, nh_asian_total, nh_other_total) %>% 
  pivot_longer(nh_white_total:nh_other_total) %>% 
  hchart(type = "line", hcaes(x = year, y = value, group = name)) %>% 
  hc_yAxis(title = list(text = "Number of working age adults")) %>% 
  hc_title(text = "Projected Number of Working Age Adults Through 2036, by Race-Ethnicity")

demand <- readRDS(here::here("clean-data", "faethm-jobs-2036.rds"))
demand %>% 
  filter(faethm_wda_name == "Alamo") %>% 
  filter(percent_of_total_workforce > 1) %>% 
  arrange(-percent_of_total_workforce) %>% 
  hchart(type = "bar", hcaes(job_name, percent_of_total_workforce), name = "") %>% 
  hc_title(text = "Jobs that will be the most in-demand in 2036") %>% 
  hc_xAxis(title = "") 
