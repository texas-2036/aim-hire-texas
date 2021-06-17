library(tidyverse)
library(jastyle)
library(leaflet) 
library(leaflet.extras)
library(highcharter)
library(tigris)
library(sf)
library(jastyle)

options(tigris_use_cache = TRUE)
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# "#2a366c" dark blue
# "#f26852" red
# "#5f6fc1" light blue
# "#3ead92" green

# c("#2a366c", "#f26852", "#5f6fc1", "#3ead92")

###--- Load data -------------------------
wda_sf <- readRDS(here::here("clean-data", "wda_shapefile.rds"))
lmi <- readRDS(here::here("clean-data", "lmi-wda-jobs-2028.rds"))
waa <- readRDS(here::here("clean-data", "working-age-pop-2036.rds"))
demand <- readRDS(here::here("clean-data", "faethm-jobs-2036.rds"))
edu <- readRDS(here::here("clean-data", "wda_edu_employment.rds"))
lw <- readRDS(here::here("clean-data", "twc_living_wage_bands.rds"))
load(here::here("clean-data", "pseo-data.RData"))

counties <- tigris::counties(state = "48") %>% 
  dplyr::select(county = NAME, geometry) %>% 
  st_transform(crs = wgs84) %>% 
  ms_simplify(0.05)

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

waa %>% 
  filter(wda == "Gulf Coast") %>% 
  select(year, nh_white_total, nh_black_total, hispanic_total, nh_asian_total, nh_other_total) %>% 
  pivot_longer(nh_white_total:nh_other_total) %>% 
  hchart(type = "line", hcaes(x = year, y = value, group = name)) %>% 
  hc_yAxis(title = list(text = "Number of working age adults")) %>% 
  hc_title(text = "Projected Number of Working Age Adults Through 2036, by Race-Ethnicity")

###--- In demand jobs -------------------------
demand %>% 
  filter(faethm_wda_name == "Alamo") %>% 
  filter(percent_of_total_workforce > 1) %>% 
  arrange(-percent_of_total_workforce) %>% 
  hchart(type = "bar", hcaes(job_name, percent_of_total_workforce), name = "") %>% 
  hc_title(text = "Jobs that will be the most in-demand in 2036") %>% 
  hc_xAxis(title = "") 

###--- Living wage jobs -------------------------------
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
pseo_inst_df %>% 
  filter(wda == "Gulf Coast") %>% 
  mutate(emp_pct = round(100 * y10_grads_emp / (y10_grads_emp + y10_grads_nme), 1)) %>% 
  arrange(-emp_pct) %>% 
  hchart(type = "bar", hcaes(x = label, y = emp_pct)) %>% 
  hc_xAxis(title = "") %>% 
  hc_yAxis(title = "") %>% 
  hc_title(text = "Employment percentage for graduates 10 years after graduation")


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
