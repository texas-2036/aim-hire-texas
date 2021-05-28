#Faethm job projections
#This is pulled from Faethm jobs reports, 15 year projections, People Impacts > Job Impact
library(tidyverse)
library(here)
library(janitor)

#Load data --------
wda_crosswalk <- read_csv(here::here("raw-data", "county_wda_crosswalk.csv"))

wda_list <- data.frame(
  wda_name = unique(wda_crosswalk$wda),
  wda_number = unique(wda_crosswalk$wda_number)
) %>% 
  arrange(wda_number)

wda_csv_list <- data.frame(
  faethm_wda_name = c("ALAMO WDA","BORDERPLEX WDA",
                      "BRAZOS VALLEY WDA","CAMERON COUNTY WDA",
                      "CAPITAL AREA & RURAL CAPITAL WDA","CENTRAL TEXAS WDA",
                      "COASTAL BEND WDA","CONCHO VALLEY WDA",
                      "DALLAS & TARRANT CO. WDA","EAST TEXAS WDA",
                      "GOLDEN CRESCENT WDA","GULF COAST WDA",
                      "THE HEART OF TEXAS WDA","LOWER RIO GRANDE VALLEY WDA",
                      "NORTH TEXAS WDA","PANHANDLE WDA",
                      "PERMIAN BASIN WDA","SOUTHEAST TEXAS WDA",
                      "SOUTH PLAINS WDA","SOUTH TEXAS WDA",
                      "TEXOMA WDA","WEST CENTRAL TEXAS WDA")
) %>% 
  mutate(
    faethm_csv_num = row_number(),
    faethm_wda_name = str_remove(faethm_wda_name, " WDA"),
    faethm_wda_name = str_to_title(faethm_wda_name),
    
    wda_name = case_when(
      faethm_wda_name %in% c("Dallas & Tarrant Co.") ~ "DFW",
      faethm_wda_name %in% c("Cameron County", "Lower Rio Grande Valley") ~ "Rio Grande Valley",
      faethm_wda_name %in% c("Capital Area & Rural Capital") ~ "Greater Austin",
      faethm_wda_name %in% c("The Heart Of Texas") ~ "The Heart of Texas",
      
      TRUE ~ faethm_wda_name
      
    )
  )

#Missing: Northeast, Deep East Texas, Middle Rio Grande 
#Combine Lower Rio Grande and Cameron


pull_wda_csv <- lapply(1:22, function(x) {
  df <- read_csv(here::here("raw-data", "faethm-jobs-2036", 
                            paste0("job-impact(sorted by all, #FTE) (", x,").csv"))
                 ) %>% 
    clean_names() %>% 
    mutate(
      sheet = 1
    )
  return(df)
})

wda_csvs <- do.call(rbind, pull_wda_csv) %>% 
  left_join(., wda_csv_list, by=c("sheet" = "faethm_csv_num")) %>% 
  group_by(wda_name, job_name) %>% 
  summarise(
    across(c(ft_es, automatable_ft_es, augmentable_ft_es, unimpacted_ft_es), sum, na.rm=T)
  )


#Texas overall
texas_total <- read_csv(here::here("raw-data", "faethm-jobs-2036", "job-impact(sorted by all, #FTE) - Texas.csv")
) %>% 
  clean_names() %>% 
  mutate(
    sheet = 0,
    faethm_wda_name = "Texas"
  )

not_allocated <-read_csv(here::here("raw-data", "faethm-jobs-2036", "job-impact(sorted by all, #FTE) (23) - not allocated.csv")
) %>%
  clean_names() %>%
  mutate(
    sheet = -1,
    faethm_wda_name = "Not Allocated"
  )

#Projections for Northeast, Deep East Texas, Middle Rio Grande --------------------
#Use LMI projections and extrapolate
#Compare combined estimates to "not allocated" estimates from Faethm

lmi_df <- readRDS(here::here("clean-data/lmi-wda-jobs-2028.rds"))


export <- rbind(wda_csvs, texas_total, not_allocated)
write_rds(export, here::here("clean-data", "faethm-jobs-2036.rds"))

