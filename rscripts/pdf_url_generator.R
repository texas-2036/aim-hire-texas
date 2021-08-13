texas <- data.frame(wda = "Texas",
                    wda_number = 0)

read.csv(here::here("clean-data", "county_wda_crosswalk.csv")) %>% 
  select(wda, wda_number) %>% 
  distinct() %>% 
  rbind(texas) %>% 
  mutate(wda_url = gsub(" ", "", wda),
         url = paste0("https://aimhiretx.s3.us-east-2.amazonaws.com/aht-2036-", wda_url, ".pdf")) %>% 
  select(-wda_url) %>% 
  write_rds(here::here("clean-data", "pdf_url.rds"))


