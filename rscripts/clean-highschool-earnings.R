library(tidyverse)
library(tidycensus)
library(janitor)

b19 <- tidycensus::load_variables(2019, "acs5", cache = T)
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
  mutate(variable = case_when(variable == "B20004_001" ~ "medincome_all",
                              variable == "B20004_002" ~ "medincome_nohs",
                              variable == "B20004_003" ~ "medincome_hs",
                              variable == "B20004_004" ~ "medincome_somecollege",
                              variable == "B20004_005" ~ "medincome_college",
                              variable == "B20004_006" ~ "medincome_postgrad")) %>% 
  select(geoid, name, variable, estimate)
  
  employment_edu <- tidycensus::get_acs(
    geography = "county",
    state = "48",          
    year = 2019,
    survey = "acs5",       
    table = "B23006",
    geometry = F,
    keep_geo_vars = F) %>% 
    clean_names() %>% 
    mutate(variable = case_when(variable == "B23006_001" ~ "population",
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
    pivot_wider(id_cols = geoid:name, names_from = variable, values_from = estimate) %>% 
    pivot_longer(cols = nohs__total:college__notlf, names_to = "variable", values_to = "estimate") %>% 
    separate(variable, into = c("education", "employment"), sep = "__") %>% 
    pivot_wider(id_cols = geoid:education, names_from = employment, values_from = estimate) %>%
    mutate(pct_in_laborforce = round(100 * lf / total, 1),
           # pooling those in armed forces and employed civilians
           pct_in_laborforce_employed = round(100 * (lf_af + lf_c_emp) / lf, 1),
           pct_total_employed = round(100 * (lf_af + lf_c_emp) / total, 1),
           pct_in_laborforce_unemployed = round(100 * lf_c_unemp / lf, 1),
           pct_total_unemployed = round(100 * lf_c_unemp / total, 1)) %>% 
    select(-c(lf:notlf))
    
