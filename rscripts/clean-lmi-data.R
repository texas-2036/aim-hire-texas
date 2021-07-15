library(tidyverse)
library(here)
library(janitor)

#From here: https://www.twc.texas.gov/files/policy_letters/attachments/wd-24-20-att-3.pdf
oes_crosswalk <- readxl::read_excel(here::here("raw-data/oes_2019_hybrid_structure.xlsx"), skip=5) %>% 
  clean_names()

wda_crosswalk <- read_csv(here::here("raw-data", "county_wda_crosswalk.csv")) %>% 
  mutate(
    wda28_number = case_when(
      wda=="DFW" & !(county %in% c("Dallas", "Tarrant")) ~ 4,
      wda=="DFW" & county %in% c("Tarrant") ~ 5,
      wda=="DFW" & county %in% c("Dallas") ~ 6,
      county=="Travis" ~ 14,
      wda=="Greater Austin" & !(county %in% c("Travis")) ~ 15,
      county=="Cameron" ~ 23,
      wda=="Rio Grande Valley" & !(county %in% c("Cameron")) ~ 24,
      TRUE ~ wda_number
    )
  )

wda28_crosswalk <- wda_crosswalk %>% 
  group_by(wda28_number) %>% 
  summarise(
    across(wda:wda_number, first)
  )

#Texas overall -----
tx <- readxl::read_excel(
  here::here("raw-data/TWC WDA Projection Summary/826_826_Tx_Occupations Projections.xlsx"), skip=1
) %>% 
  clean_names() %>% 
  filter(occ_summary_level=="Detail") %>% 
  mutate(
    wda = "Texas", 
    wda_number = 0
  ) %>% 
  mutate(
    x2019_mean_hourly_wage = as.numeric(str_remove(x2019_mean_hourly_wage, "-")),
    x2019_mean_annual_wage = as.numeric(str_remove(x2019_mean_annual_wage, "-"))
  ) 

#By WDA28 ------------
file_list <- list.files(here::here("raw-data/TWC WDA Projection Summary")) %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  filter(str_detect(x, "^WDA"))

pull_job_2028 <- lapply(file_list$x, function(x) {
  df <- readxl::read_excel(
    here::here("raw-data", "TWC WDA Projection Summary", x), skip=1
  ) %>% 
    clean_names() %>% 
    filter(occ_summary_level=="Detail") %>% 
    mutate(file_name = x)
  return(df)
})


wda28_jobs_2028 <- do.call(rbind, pull_job_2028) %>% 
  separate(file_name, into = c("wda28_number", "other"), sep = " ") %>% 
  mutate(
    wda28_number = as.numeric(str_remove(wda28_number, "WDA"))
  ) %>% 
  left_join(., wda28_crosswalk, by=c("wda28_number")) %>% 
  mutate(
    x2019_mean_hourly_wage = as.numeric(str_remove(x2019_mean_hourly_wage, "-")),
    x2019_mean_annual_wage = as.numeric(str_remove(x2019_mean_annual_wage, "-"))
  ) %>% 
  dplyr::select(names(tx)) %>% 
  rbind(., tx) %>% 
  #OES crosswalk
  left_join(., oes_crosswalk, by=c("occ_code" = "x2010_soc_code")) %>% 
  dplyr::select(occ_summary_level:wda_number, oes_2019_estimates_code, oes_2019_estimates_title, x2010_soc_title) %>% 
  mutate(
    oes_2019_estimates_title = case_when(
      occ_code == "51-2098" ~ "Miscellaneous Assemblers and Fabricators",
      occ_code == "51-2028" ~ "Electrical, Electronic, and Electromechanical Assemblers, Except Coil Winders, Tapers, and Finishers",
      occ_code == "53-1048" ~ "First-Line Supervisors of Transportation and Material Moving Workers, Except Aircraft Cargo Handling Supervisors",
      occ_code == "21-1018" ~ "Substance Abuse, Behavioral Disorder, and Mental Health Counselors",
      occ_code == "25-3098" ~ "Substitute Teachers, Short-Term",
      occ_code == "25-3097" ~ "Tutors and Teachers and Instructors, All Other",
      TRUE ~ oes_2019_estimates_title
    ),
    
    oes_2019_estimates_code = case_when(
      occ_code == "51-2098" ~ "51-2090",
      occ_code == "51-2028" ~ "51-2028", #Stays the same
      occ_code == "53-1048" ~ "53-1047",
      occ_code == "21-1018" ~ "21-1018", # Stays the same
      occ_code == "25-3098" ~ "25-3031",
      occ_code == "25-3097" ~ "25-3097", #Stays the same
      TRUE ~ oes_2019_estimates_code
    )
  ) 

#missing <- filter(wda28_jobs_2028, is.na(oes_2019_estimates_code))
#tabyl(missing, occupational_title) #Very few jobs are missing... awesome!
#tabyl(missing, occupational_title, occ_code) #Very few jobs are missing... awesome!



#Projections ------------
wda_jobs_projection <- wda28_jobs_2028 %>% 
  #Projection
  mutate(
    annualized_growth_rate = (log(annual_average_employment_2028)-log(annual_average_employment_2018))/10,
    growth_rate_8yrs = annualized_growth_rate * 8,
    emp_2029 = annual_average_employment_2028 + (annual_average_employment_2028 * annualized_growth_rate),
    emp_2030 = emp_2029 + (emp_2029 * annualized_growth_rate),
    emp_2031 = emp_2030 + (emp_2030 * annualized_growth_rate),
    emp_2032 = emp_2031 + (emp_2031 * annualized_growth_rate),
    emp_2033 = emp_2032 + (emp_2032 * annualized_growth_rate),
    emp_2034 = emp_2033 + (emp_2033 * annualized_growth_rate),
    emp_2035 = emp_2034 + (emp_2034 * annualized_growth_rate),
    emp_2036 = emp_2035 + (emp_2035 * annualized_growth_rate),
    annual_average_employment_2036 = emp_2036
  ) %>% 
  #Summarize by WDA
  group_by(wda, wda_number, oes_2019_estimates_code, oes_2019_estimates_title) %>% 
  summarise(
    # across(c(x2019_mean_hourly_wage, x2019_mean_annual_wage),
    #        weighted.mean, wt=annual_average_employment_2018, na.rm=T),
    
    across(c(annual_average_employment_2018,
             annual_average_employment_2028,
             annual_average_employment_2036), sum, na.rm=T)
  )

#Created by clean-living-wage-bands.R
wages <- readRDS(file=here::here("clean-data", "twc_living_wage_bands.rds"))


df <- left_join(wda_jobs_projection, wages, by=c("wda", "wda_number", "oes_2019_estimates_code"="soc_code",
                                                 "oes_2019_estimates_title"="occupation_title"))


saveRDS(df, here::here("clean-data", "wda-jobs-proj-with-wages.rds"))

###--- Summarize further to only include top 10, bottom 10, top growth ----------------
idj_raw <- readRDS(here::here("clean-data", "wda-jobs-proj-with-wages.rds"))

## Process data to make summary table for speedy loading
top_summary <- idj_raw %>% 
  ungroup() %>% 
  group_by(wda) %>% 
  slice_max(order_by = annual_average_employment_2036, n = 10) %>% 
  select(wda, job = oes_2019_estimates_title, value = annual_average_employment_2036) %>% 
  mutate(type = "top", 
         value = round(value))

bot_summary <- idj_raw %>% 
  ungroup() %>% 
  group_by(wda) %>% 
  slice_min(order_by = annual_average_employment_2036, n = 10) %>% 
  select(wda, job = oes_2019_estimates_title, value = annual_average_employment_2036) %>% 
  mutate(type = "bottom",
         value = round(value))

growth_summary <- idj_raw %>% 
  ungroup() %>% 
  mutate(growth = 100 * (annual_average_employment_2036 - annual_average_employment_2018) / annual_average_employment_2018) %>% 
  group_by(wda) %>% 
  slice_max(order_by = growth, n = 10) %>% 
  select(wda, job = oes_2019_estimates_title, value = growth) %>% 
  mutate(type = "growth", 
         value = round(value))

idj_summary <- rbind(top_summary, bot_summary) %>% 
  rbind(growth_summary) %>% 
  ungroup() %>% 
  group_by(wda, type) %>% 
  mutate(rank = 1:10) %>% 
  ungroup()

saveRDS(idj_summary, here::here("clean-data", "in-demand-jobs-summary.rds"))
