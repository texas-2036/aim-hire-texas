library(tidyverse)
library(tidycensus)
library(janitor)

b19 <- tidycensus::load_variables(2019, "acs5", cache = T)

###--- LOAD AND PROCESS COUNTY LEVEL DATA -------------------------------------------------
# median income by education for tx counties
earnings_edu <- tidycensus::get_acs(
  geography = "county",
  state = "48",          
  year = 2019,
  survey = "acs5",       
  table = "B20004",
  geometry = F,
  keep_geo_vars = F) %>% 
  clean_names() %>% 
  filter(variable %in% c("B20004_001", "B20004_002",
                         "B20004_003", "B20004_004",
                         "B20004_005", "B20004_006")) %>% 
  mutate(variable = case_when(variable == "B20004_001" ~ "all",
                              variable == "B20004_002" ~ "nohs",
                              variable == "B20004_003" ~ "hs",
                              variable == "B20004_004" ~ "somecollege",
                              variable == "B20004_005" ~ "college",
                              variable == "B20004_006" ~ "postgrad")) %>% 
  select(geoid, name, education = variable, median_income = estimate) %>% 
  pivot_wider(id_cols = geoid:name, names_from = education, values_from = median_income) %>% 
  rename(county_medincome = all) %>% 
  pivot_longer(cols = nohs:postgrad, names_to = "education", values_to = "median_income")

# employment status by education for tx counties
employment_edu <- tidycensus::get_acs(
  geography = "county",
  state = "48",          
  year = 2019,
  survey = "acs5",       
  table = "B23006",
  geometry = F,
  keep_geo_vars = F) %>% 
  clean_names() %>% 
  # hierarchical table- use __ to sep education level for pivoting later
  mutate(variable = case_when(variable == "B23006_001" ~ "county_population",
                              variable == "B23006_002" ~ "nohs__total",
                              variable == "B23006_003" ~ "nohs__lf",
                              variable == "B23006_004" ~ "nohs__lf_af",
                              variable == "B23006_005" ~ "nohs__lf_c",
                              variable == "B23006_006" ~ "nohs__lf_c_emp",
                              variable == "B23006_007" ~ "nohs__lf_c_unemp",
                              variable == "B23006_008" ~ "nohs__notlf",
                              variable == "B23006_009" ~ "hs__total",
                              variable == "B23006_010" ~ "hs__lf",
                              variable == "B23006_011" ~ "hs__lf_af",
                              variable == "B23006_012" ~ "hs__lf_c",
                              variable == "B23006_013" ~ "hs__lf_c_emp",
                              variable == "B23006_014" ~ "hs__lf_c_unemp",
                              variable == "B23006_015" ~ "hs__notlf",
                              variable == "B23006_016" ~ "somecollege__total",
                              variable == "B23006_017" ~ "somecollege__lf",
                              variable == "B23006_018" ~ "somecollege__lf_af",
                              variable == "B23006_019" ~ "somecollege__lf_c",
                              variable == "B23006_020" ~ "somecollege__lf_c_emp",
                              variable == "B23006_021" ~ "somecollege__lf_c_unemp",
                              variable == "B23006_022" ~ "somecollege__notlf",
                              variable == "B23006_023" ~ "college__total",
                              variable == "B23006_024" ~ "college__lf",
                              variable == "B23006_025" ~ "college__lf_af",
                              variable == "B23006_026" ~ "college__lf_c",
                              variable == "B23006_027" ~ "college__lf_c_emp",
                              variable == "B23006_028" ~ "college__lf_c_unemp",
                              variable == "B23006_029" ~ "college__notlf")) %>% 
  select(geoid, name, variable, estimate) %>% 
  # pivot wide and long first to separate out population - total county population
  pivot_wider(id_cols = geoid:name, names_from = variable, values_from = estimate) %>% 
  pivot_longer(cols = nohs__total:college__notlf, names_to = "variable", values_to = "estimate") %>% 
  # parse education level - education level will be a column, everything else wide
  separate(variable, into = c("education", "employment"), sep = "__") %>% 
  pivot_wider(id_cols = geoid:education, names_from = employment, values_from = estimate) %>%
  # calculate proportions - be careful with denominator
  # not sure if denominator should be labor force or total in age group- doing both for now
  mutate(pct_in_laborforce = round(100 * lf / total, 1),
         # pooling those in armed forces and employed civilians
         pct_in_laborforce_employed = round(100 * (lf_af + lf_c_emp) / lf, 1),
         pct_total_employed = round(100 * (lf_af + lf_c_emp) / total, 1),
         pct_in_laborforce_unemployed = round(100 * lf_c_unemp / lf, 2),
         pct_total_unemployed = round(100 * lf_c_unemp / total, 2)) %>%
  rename(number_people = total, number_in_laborforce = lf) %>% 
  select(-c(lf_af:notlf)) %>% 
  # join w earnings - right join since earnings has one more edu level we want to keep
  right_join(earnings_edu) %>% 
  mutate(county = gsub(" County, Texas", "", name)) %>% 
  mutate(education = factor(education, levels = c("nohs", "hs", "somecollege", "college", "postgrad"), ordered = T)) %>% 
  # make the county population and income only appear once so we can just add when we aggregate counties to wdas
  mutate(county_population = case_when(education == "nohs" ~ county_population),
         county_medincome = case_when(education == "nohs" ~ county_medincome)) %>% 
  select(geoid, county, 
         county_population, county_medincome, 
         education, number_people, number_in_laborforce, 
         median_income, everything()) %>% 
  select(-name) %>% 
  arrange(county, education)
  
ggplot(employment_edu, aes(x = education, y = median_income)) + geom_boxplot() + theme_bw()
ggplot(employment_edu, aes(x = education, y = pct_total_employed)) + geom_boxplot() + theme_bw()
ggplot(employment_edu, aes(x = education, y = pct_in_laborforce)) + geom_boxplot() + theme_bw()

###-- AGGREGATE TO WORK FORCE BOARD ---------------------------------------
crosswalk <- read.csv(here::here("raw-data", "county_wda_crosswalk.csv"))

edu_wda <- left_join(employment_edu, crosswalk) #%>% 
  group_by(wda_number) %>% 
  summarize(wda = wda[1],
            fips_county = fips_county[1],
            wda_population = sum(county_population, na.rm = T),
            # taking the mean of county medians... is this the best way? correct weight?
            wda_medianincome = weighted.mean(county_medincome, county_population, na.rm = T))
View(edu_wda)
