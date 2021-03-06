library(tidyverse)
library(jastyle)
library(leaflet) 
library(leaflet.extras)
library(highcharter)
library(tigris)
library(sf)
library(jastyle)
library(rmapshaper)

options(tigris_use_cache = TRUE)
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# "#2a366c" dark blue
# "#f26852" red
# "#5f6fc1" light blue
# "#3ead92" green

# c("#2a366c", "#f26852", "#5f6fc1", "#3ead92")

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

# shapefiles
wda_sf <- readRDS(here::here("clean-data", "wda_shapefile.rds"))
counties <- tigris::counties(state = "48") %>% 
  dplyr::select(county = NAME, geometry) %>% 
  st_transform(crs = wgs84) %>% 
  ms_simplify(0.05)

###--- Counties list ------------------------------
cos <- crosswalk %>% 
  select(wda, county) %>% 
  group_by(wda) %>% 
  summarize(text = paste(county, collapse = ", "))

saveRDS(cos, here::here("clean-data", "wda_county_list.RDS"))


###--- Landing page map ------------------
pal <- colorFactor(palette = c("#2a366c", "#f26852", "#5f6fc1", "#3ead92"), wda_sf$color_category)
leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 5, maxZoom = 5)) %>%
  setView(-99.9018, 30.9686, zoom = 5) %>% 
  addPolygons(stroke = F,
              fill = T,
              fillOpacity = 0.8,
              fillColor = ~pal(color_category),
              data = wda_sf) %>%
  addPolygons(color = "black",
              stroke = T,
              weight = 1,
              fill = F,
              data = counties) %>%
  addPolygons(stroke = T, 
              weight = 2,
              color = "black",
              opacity = 1,
              fill = T,
              fillOpacity = 0,
              label = wda_sf$wda,
              data = wda_sf) %>% 
  setMapWidgetStyle(list(background= "white")) %>% 
  htmlwidgets::onRender("function(el, x) { 
               map = this
               map.dragging.disable();
               }")

###--- Working age adults ----------------
waa %>% 
  filter(wda == "Gulf Coast") %>% 
  hchart(type = "line", color = "#8da0cb",hcaes(x = year, y = total)) %>% 
  hc_yAxis(title = list(text = "Number of working age adults")) %>% 
  hc_title(text = "Projected Number of Working Age Adults Through 2036")

df <- waa %>% 
  filter(wda == "Alamo") %>% 
  pivot_longer(total:nh_other_female)

waa %>% 
  filter(wda == "Gulf Coast") %>% 
  select(year, nh_white_total, nh_black_total, hispanic_total, nh_asian_total, nh_other_total) %>% 
  pivot_longer(nh_white_total:nh_other_total) %>% 
  hchart(type = "line", hcaes(x = year, y = value, group = name)) %>% 
  hc_yAxis(title = list(text = "Number of working age adults")) %>% 
  hc_title(text = "Projected Number of Working Age Adults Through 2036, by Race-Ethnicity")
waa %>% 
  filter(wda == "Gulf Coast" & year == 2036) %>% 
  select(wda, nh_white_total, nh_black_total, hispanic_total, nh_asian_total, nh_other_total) %>% 
  pivot_longer(nh_white_total:nh_other_total) %>% 
  hchart("pie", hcaes(name, value)) %>% 
  hc_plotOptions(
    series = list(showInLegend = TRUE,
                  dataLabels = F)
    
  ) %>% 
  hc_title(text = "Projected demographic breakdown of workforce")

###--- Attractive jobs --------------------------------
aj %>% 
  filter(wfb == "Alamo") %>% 
  hchart("scatter", hcaes(quality_index, demand_index, group = quality_and_demand_quadrant)) %>% 
  hc_xAxis(title = list(text = "Quality Index")) %>% 
  hc_yAxis(title = list(text = "Demand Index"),
           plotLines = list(list(
             value = 0,
             color = 'black',
             width = 3,
             zIndex = 4,
             label = list(text = "mean",
                          style = list( color = 'black', fontWeight = 'bold'   )
             ))))
  
###--- Living wage jobs -------------------------------

# reactive
df <- lwj_industry %>% 
    filter(wda == "Alamo")

df %>%
  group_by(industry_title, wage_band) %>% 
  summarize(number_jobs = sum(no_of_employed)) %>% 
  hchart(type = "bar", hcaes(x = industry_title, y = number_jobs, group = wage_band)) %>%  
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Number of jobs")) %>%
  hc_plotOptions(bar = list(stacking = "normal")) %>%
  #hc_add_theme(tx2036_hc) %>% 
  hc_title(text = "Share of living wage jobs across industries") %>% 
  hc_colors(c("#f26852", "#5f6fc1", "#2a366c", "#3ead92")) %>% 
  hc_tooltip(formatter = JS("function(){
                                return (this.point.wage_band + ': ' + this.y)}"))

# hchart
df1 <- df %>%
    group_by(industry_title, wage_band) %>% 
    summarize(number_jobs = sum(no_of_employed)) %>% 
  arrange(desc(number_jobs))

highchart() %>% 
  hc_add_series(df1, type = "bar", hcaes(x = industry_title, y = number_jobs, group = wage_band)) %>%  
   hc_xAxis(title = list(text = ""),
            categories = as.list(df1$industry_title)) %>%
    hc_yAxis(title = list(text = "Number of jobs")) %>%
    hc_plotOptions(bar = list(stacking = "normal")) %>%
    #hc_add_theme(tx2036_hc) %>% 
    hc_title(text = "Share of living wage jobs across industries") %>% 
    hc_colors(c("#f26852", "#5f6fc1", "#2a366c", "#3ead92")) %>% 
    hc_tooltip(formatter = JS("function(){
                                return (this.point.wage_band + ': ' + this.y)}"))

###--- Employment by education, post high school ------

# median earnings
pseo_inst_df %>% 
  filter(wda == "Gulf Coast") %>% 
  arrange(-y10_p50_earnings) %>% 
  hchart(type = "bar", hcaes(x = label, y = y10_p50_earnings)) %>% 
  hc_xAxis(title = "") %>% 
  hc_yAxis(title = "") %>% 
  hc_title(text = "Median annual earnings for graduates 10 years after graduation")

# employment rate
c <- pseo_wda_df %>% 
  filter(wda_name == "Alamo") %>% 
  mutate(degree_level = case_when(degree_level == "01" ~ "Certificate < 1 year",
                                  degree_level == "02" ~ "Certificate 1-2 years",
                                  degree_level == "03" ~ "Associates",
                                  degree_level == "04" ~ "Certificate 2-4 years",
                                  degree_level == "05" ~ "Baccalaureate")) %>% 
  mutate(degree_level = factor(degree_level, levels = c("Certificate < 1 year", "Certificate 1-2 years", "Associates",
                                                           "Certificate 2-4 years", "Baccalaureate"),
                                  ordered = T)) #%>% 
  #pivot_longer(y10_p25_earnings:y10_p75_earnings) %>% 
  highchart() %>% 
  hc_add_series(data = c, "scatter", hcaes(y = y10_p50_earnings, x = degree_level),
                radius = 50) %>%
    hc_add_series(data = c, "errorbar", hcaes(x = degree_level, low = y10_p25_earnings, high = y10_p75_earnings),
                  whiskerWidth = 0) %>% 
    hc_plotOptions(series = list(showInLegend = F, radius = 100)) %>%
  #hc_add_theme(tx2036_hc) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_yAxis(title = list(text = "")) %>% 
  # hc_tooltip(formatter = JS("function(){
  #                           return ('Median annual salary: $' + Highcharts.numberFormat(this.y, 0))}")) %>% 
  hc_title(text = "Median and quartile earnings for college graduates")

c


# share of degree type for each institution
pseo_inst_df %>% 
  filter(wda == "Gulf Coast") %>%
  filter(label == "Galveston College") %>% 
  hchart(type = "pie", hcaes(degree_level, y10_grads_earn)) %>% 
  hc_title(text = "Degree types awarded at Galveston College")

# share of in state employment for each institution
pseo_inst_df %>% 
  filter(wda == "Gulf Coast") %>%
  filter(label == "Galveston College") %>%
  select(label, degree_level, y10_grads_emp_instate, y10_grads_emp) %>% 
  group_by(label) %>% 
  summarize(y10_grads_emp_instate = sum(y10_grads_emp_instate, na.rm = T),
            y10_grads_emp = sum(y10_grads_emp, na.rm = T)) %>% 
  mutate(instate_pct = round(100 * y10_grads_emp_instate / y10_grads_emp, 1),
         outstate_pct = 100 - instate_pct) %>% 
  ungroup() %>% 
  select(label, `Employed in Texas` = instate_pct, `Employed outside of Texas` = outstate_pct) %>% 
  pivot_longer(`Employed in Texas`:`Employed outside of Texas`) %>% 
  hchart(type = "pie", hcaes(name, value)) %>% 
  hc_title(text = "Employment location for Galveston College Grads")

# quartile earnings by degree type
pseo_inst_df %>% 
  filter(wda == "Gulf Coast") %>%
  filter(label == "Galveston College") %>% 
  select(degree_level, y10_p25_earnings, y10_p50_earnings, y10_p75_earnings) %>% 
  pivot_longer(y10_p25_earnings : y10_p75_earnings) %>% 
  hchart(type = "scatter", hcaes(degree_level, value), size = 6) %>% 
  hc_xAxis(title = list(text = "Degree type")) %>% 
  hc_yAxis(title = "") %>% 
  hc_title(text = "Median and 25th, 75th percentile earnings for graduates of Galveston College")

state <- pseo_wda_df %>% 
  select(wda_name, y10_grads_emp, y10_grads_emp_instate) %>% 
  mutate(y10_grads_emp_outstate = y10_grads_emp - y10_grads_emp_instate) %>% 
  select(-y10_grads_emp) %>% 
  group_by(wda_name) %>% 
  summarize(instate = sum(y10_grads_emp_instate),
            outstate = sum(y10_grads_emp_outstate)) %>% 
  pivot_longer(instate:outstate) %>% 
  filter(wda_name == "Alamo") %>% 
  hchart("pie", hcaes(name, value)) %>% 
  hc_title(text = "out of state")
state
###--- Employment by education, high school -----------
edu %>% 
  filter(wda == "Gulf Coast") %>% 
  hchart(type = "line", hcaes(x = education, y = median_income)) %>% 
  hc_xAxis(title = list(text = "Education Level")) %>% 
  hc_yAxis(title = list(text = "Median Income")) %>% 
  hc_title(text = "Median income by education")

edu %>% 
  filter(wda == "Gulf Coast") %>% 
  hchart(type = "line", hcaes(x = education, y = pct_employed)) %>% 
  hc_xAxis(title = list(text = "Education Level")) %>% 
  hc_yAxis(title = list(text = "Employment Rate")) %>% 
  hc_title(text = "Employment rate by education")

###--- Living wage --------------------------------------
lw1 <- lw %>% 
  filter(wda == "Gulf Coast") %>% 
  mutate(no_of_employed = as.numeric(na_if(no_of_employed, "N/A"))) %>% 
  filter(soc_code != "00-0000") %>% 
  group_by(wage_band) %>% 
  summarize(no_of_employed = sum(no_of_employed, na.rm = T)) %>% 
  hchart("pie", hcaes(wage_band, no_of_employed)) %>% 
  hc_title(text = "Number of workers employed in each wage bracket")
lw1

lw_industry %>% 
  filter(wda == "Alamo") %>% 
  group_by(wda, industry_title, wage_band) %>% 
  summarize(number_jobs = sum(no_of_employed)) %>% 

  hchart(type = "bar", hcaes(x = industry_title, y = number_jobs, group = wage_band)) %>%  
  hc_plotOptions(column = list(stacking = "normal")) %>% 
  hc_add_theme(tx2036_hc)

###--- living wage households --------------------------
a <- alice_hh_counts %>% 
  filter(wda == "Alamo") %>%
  ggplot(aes(x = year, y = alice_household)) + 
  geom_line()
a
  pivot_longer(poverty_household:above_alice_household) %>% 
  hchart("pie", hcaes(name, value)) %>% 
  hc_plotOptions(series = list(showInLegend = F, dataLabels = F))
  
a <- alice_hh_counts %>%
  filter(wda == "Alamo") %>% 
  mutate(above_poverty_below_alice_hh_share = below_alice_hh_share - below_poverty_hh_share) %>%
  rename("Below poverty" = below_poverty_hh_share,
         "Above poverty, below ALICE" = above_poverty_below_alice_hh_share,
         "Above ALICE" = above_alice_hh_share) %>% 
  pivot_longer(cols = c(`Below poverty`, `Above poverty, below ALICE`, `Above ALICE`)) %>% 
  mutate(value = round(value, 1)) %>% 
  hchart(type = "column", hcaes(x = year, y = value, group = name)) %>% 
  hc_yAxis(min = 0, max = 100, title = list(text = "")) %>% 
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_tooltip(formatter = JS("function(){
                                return (this.point.name + ' ' + this.y + '%')}"))
  #hc_add_theme(tx2036_hc) 
a    

b <- alice_demographics %>% 
  filter(wda == "Alamo") %>% 
  mutate(hh = poverty + alice + above_alice,
         "Below poverty" = 100 * poverty / hh,
         "Above poverty, below ALICE" = 100 * alice / hh,
         "Above ALICE" = 100 * above_alice / hh) %>% 
  mutate(category = case_when(category == "fam_kids" ~ "Families with Children",
                              category == "over_65" ~ "65 and Over",
                              category == "single_cohab" ~ "Single or Cohabiting")) %>% 
  pivot_longer("Below poverty":"Above ALICE") %>% 
  mutate(value = round(value, 1)) %>% 
  hchart(type = "column", hcaes(x = category, y = value, group = name)) %>% 
  hc_yAxis(min = 0, max = 100, title = list(text = "")) %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_plotOptions(column = list(stacking = "normal"),
                 series = list(showInLegend = F)) %>% 
  
  #hc_add_theme(tx2036_hc) %>% 
  hc_title(text = "Share of households in income tiers by family type") %>% 

  hc_colors(c("#3ead92", "#2a366c", "#f26852")) %>% 
  hc_tooltip(formatter = JS("function(){
                                return (this.point.name + ' ' + this.y + '%')}"))
b


  df <- comparison_jobs %>% 
    filter(wda %in% c("Alamo", "Borderplex", "Gulf Coast")) 


purrr::map(unique(df$wda), function(x) {
  df %>% 
    filter(wda == x) %>% 
    filter(type == "demand") %>% 
    hchart("bar", hcaes(y = value, x = job))
}) %>% 
  hw_grid(rowheight = 225, ncol = 1) 
  
  
  
