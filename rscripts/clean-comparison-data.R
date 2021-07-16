library(tidyverse)
library(jastyle)
library(leaflet) 
library(leaflet.extras)
library(highcharter)
library(tigris)
library(sf)
library(jastyle)
library(rmapshaper)
library(sparkline)
library(DT)

options(tigris_use_cache = TRUE)
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

###--- Load data -------------------------

# lwh - living wage households
load(here::here("clean-data", "alice_living_wage_hh.RData"))

# waa - working age adults/future workforce
waa <- readRDS(here::here("clean-data", "working-age-pop-2036.rds"))

# idj - indemand jobs
idj_raw <- readRDS(here::here("clean-data", "wda-jobs-proj-with-wages.rds"))
idj_summary <- readRDS(here::here("clean-data", "in-demand-jobs-summary.rds"))
lmi <- readRDS(here::here("clean-data", "lmi-wda-jobs-2028.rds")) # demand and earnings

# lwj - living wage jobs
lw <- readRDS(here::here("clean-data", "twc_living_wage_bands.rds"))
lw_industry <- readRDS(here::here("clean-data", "twc_living_wage_bands_by_industry.rds"))

# aj - attractive jobs 
aj <- readRDS(here::here("clean-data", "brookings-data.rds"))

# edu - education pipeline
edu <- readRDS(here::here("clean-data", "wda_edu_employment.rds")) # census
load(here::here("clean-data", "pseo-data.RData"))                  # pseo

###--- people table ---------------------------------

# workforce total and re breakdown
t1 <- waa %>% 
  filter(year == "2036") %>% 
  select(wda, wda_number, contains("total")) %>% 
  select(wda, wda_number, 
         waa = total, 
         waa_white = nh_white_total,
         waa_black = nh_black_total,
         waa_hispanic = hispanic_total,
         waa_asian = nh_asian_total,
         waa_other = nh_other_total) %>% 
  # mutate(across("Working age adults":"Working age adults: Other",
  #               ~ prettyNum(., big.mark = ","))) %>% 
  #pivot_longer(White:Other, names_to = "race", values_to = "number") %>% 
  mutate(wda = case_when(wda == "State of Texas" ~ "Texas",
                         T ~ wda)) 

t2_wda <- alice_hh_counts %>% 
  ungroup() %>% 
  filter(year == "2018")
t2_texas <- t2_wda %>% 
  ungroup() %>% 
  summarize(year = "2018", wda = "Texas", wda_number = NA, 
            household = sum(household),
            poverty_household = sum(poverty_household),
            alice_household = sum(alice_household),
            above_alice_household = sum(above_alice_household)) %>% 
  mutate(below_poverty_hh_share = 100 * poverty_household / household,
         below_alice_hh_share = 100 * (poverty_household + alice_household) / household,
         above_alice_hh_share = 100 * above_alice_household / household,)
t2 <- rbind(t2_wda, t2_texas) %>% 
  select(wda, wda_number, 
         above_alice_household, 
         above_alice_hh_share) %>% 
  mutate(#"Number of households above ALICE threshold" = prettyNum(`Number of households above ALICE threshold`, big.mark = ","),
    above_alice_hh_share = round(above_alice_hh_share, 1))
  #        "Share of households above ALICE threshold" = as.character(`Share of households above ALICE threshold`))

t3_wda <- edu %>% 
  filter(education == "hs") %>% 
  select(wda, wda_number, wda_number_people, median_income, pct_employed)
t3_texas <- t3_wda %>% 
  summarize(wda = "Texas", wda_number = NA,
            median_income = weighted.mean(median_income, wda_number_people, na.rm = T),
            pct_employed = weighted.mean(pct_employed, wda_number_people, na.rm = T),
            wda_number_people = sum(wda_number_people))
t3 <- rbind(t3_wda, t3_texas) %>% 
  select(wda, wda_number, 
         hs_median_income = median_income, 
         hs_pct_employed = pct_employed) %>% 
  mutate(#"Median income for high school grads" = prettyNum(`Median income for high school grads`, big.mark = ","),
    hs_pct_employed = round(hs_pct_employed, 1))
         #"Employment rate of high school grads" = as.character(`Employment rate of high school grads`))

people <- left_join(t1, t2) %>% 
  left_join(t3) %>% 
  ungroup() %>% 
  select(-wda_number) #%>% 
  #pivot_longer("Working age adults":"Employment rate of high school grads") %>% 
  #pivot_wider(id_cols = name, names_from = wda, values_from = value) %>% 
  #select(Characteristic = name, Texas, everything())

saveRDS(people, here::here("clean-data", "comparison_table_people.rds"))

###--- people table development -------------------
race_spark <- people %>% 
  select(wda, waa_white:waa_other) %>% 
  pivot_longer(waa_white:waa_other) %>% 
  group_by(wda) %>% 
  summarize("Predicted race-ethnicity breakdown of working age adults, 2036" = spk_chr(
    value, 
    type = "pie", 
    height = "100",
    sliceColors = c('#f26852',' #2a366c',' #3ead92',' #5f6fc1',' #f9cd21')
  ))

alice_spark <- people %>% 
  select(wda, above_alice_hh_share) %>%
  mutate(below_alice_hh_share = 100 - above_alice_hh_share) %>% 
  pivot_longer(above_alice_hh_share:below_alice_hh_share) %>% 
  group_by(wda) %>% 
  summarize("Share of households above ALICE threshold" = spk_chr(
    value, 
    type = "pie", 
    height = "100",
    sliceColors = c('#f26852', 'transparent')
  ))

edu_spark <- people %>% 
  select(wda, hs_pct_employed) %>%
  mutate(unemployed = 100 - hs_pct_employed) %>% 
  pivot_longer(hs_pct_employed:unemployed) %>% 
  group_by(wda) %>% 
  summarize("Employment rate of high school graduates" = spk_chr(
    value, 
    type = "pie", 
    height = "100",
    sliceColors = c('#f26852', 'transparent')
  ))

table <- left_join(people, race_spark) %>% 
  left_join(alice_spark) %>% 
  left_join(edu_spark) %>% 
  select(Area = wda, 
         `Predicted number of working age adults, 2036` = waa,
         `Predicted race-ethnicity breakdown of working age adults, 2036`,
         `Number of households above ALICE threshold` = above_alice_household,
         `Share of households above ALICE threshold`,
         `Median income of high school graduates` = hs_median_income,
         `Employment rate of high school graduates`) %>%
  mutate(`Predicted number of working age adults, 2036` = prettyNum(`Predicted number of working age adults, 2036`, big.mark = ","),
         `Number of households above ALICE threshold` = prettyNum(`Number of households above ALICE threshold`, big.mark = ","),
         `Median income of high school graduates` = paste0("$", prettyNum(`Median income of high school graduates`, big.mark = ","))) #%>%
  datatable(., escape = F, filter = "top", 
          options = list(paging = F, fnDrawCallback = htmlwidgets::JS(
            '
            function () {
              HTMLWidgets.staticRender();
            }
            '
          )
          )) %>% 
  spk_add_deps()
table
saveRDS(table, here::here("clean-data", "comparison_table_people_sparkline.rds"))

###--- jobs table ---------------------------------
# idea: just list top 10 in-demand jobs and add a symbol for living wage ($) and/or attractive (*)
# but which data source to use... all brookings? can't join brookings and lmi
demand <- readRDS(here::here("clean-data", "in-demand-jobs-summary.rds")) %>% 
  filter(type == "top")
attractive <- aj %>% 
  select(wda = wfb, job = occupation)