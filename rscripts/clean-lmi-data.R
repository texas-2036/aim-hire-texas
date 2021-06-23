library(tidyverse)
library(here)
library(janitor)

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
  rbind(., tx)


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
  group_by(wda, wda_number, occ_code, occupational_title) %>% 
  summarise(
    across(c(x2019_mean_hourly_wage, x2019_mean_annual_wage),
           weighted.mean, wt=annual_average_employment_2018, na.rm=T),
    
    across(c(annual_average_employment_2018,
             annual_average_employment_2028,
             annual_average_employment_2036), sum, na.rm=T)
  ) %>% 
    mutate(
      x2019_mean_hourly_wage = ifelse(is.nan(x2019_mean_hourly_wage), NA, x2019_mean_hourly_wage),
      x2019_mean_annual_wage = ifelse(is.nan(x2019_mean_annual_wage), NA, x2019_mean_annual_wage),
      annual_average_employment_2036 = round(annual_average_employment_2036)
    ) %>% 
  ##Living wage bands; based on MEAN not MEDIAN -----
  mutate(
    wage_band = case_when(
      x2019_mean_annual_wage>65000 ~ "High Wage",
      x2019_mean_annual_wage>45000 & x2019_mean_annual_wage<=65000 ~ "Mid-High Wage",
      x2019_mean_annual_wage>=25000 & x2019_mean_annual_wage<=45000 ~ "Mid-Low Wage",
      x2019_mean_annual_wage<25000 ~ "Low Wage"
    ),
    wage_band = factor(wage_band, levels = c("Low Wage", "Mid-Low Wage","Mid-High Wage","High Wage"))
  )




saveRDS(wda_jobs_projection, here::here("clean-data", "lmi-wda-jobs-wages-projections.rds"))



