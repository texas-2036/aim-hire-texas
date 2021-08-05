library(tidyverse)
library(jastyle)
library(leaflet) 
library(leaflet.extras)
library(highcharter)
library(tigris)
library(sf)
library(jastyle)
library(rmapshaper)
library(scales)

options(tigris_use_cache = TRUE)
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

###--- load data -------------------------
# working age adults, r-e breakdown
waa <- readRDS(here::here("clean-data", "working-age-pop-2036.rds"))
people <- readRDS( here::here("clean-data", "comparison_table_people.rds"))

# living wage households
load(here::here("clean-data", "alice_living_wage_hh.RData"))

# living wage jobs
lwj_industry <- readRDS(here::here("clean-data", "twc_living_wage_bands_by_industry.rds"))
lwj_wages <- readRDS(here::here("clean-data", "wda-jobs-proj-with-wages.rds"))

# education
edu <- readRDS(here::here("clean-data", "wda_edu_employment.rds")) # census
load(here::here("clean-data", "pseo-data.RData"))                  # pseo

# shapefiles
wda_sf <- readRDS(here::here("clean-data", "wda_shapefile.rds"))
counties <- tigris::counties(state = "48") %>% 
  dplyr::select(county = NAME, geometry) %>% 
  st_transform(crs = wgs84) %>% 
  ms_simplify(0.05)

###--- ggtheme ----------------------------------
ggtheme <- function (base_size = 14,
                     base_family = "Montserrat",
                     title_size = 23,
                     subtitle_size = 12,
                     caption_size = 10,
                     ...)
{
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family, ...) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = title_size,
        color = "#FFFFFF",
        family = "Montserrat-ExtraBold"
      ),
      plot.subtitle = ggtext::element_markdown(size = subtitle_size,
                                               family = "Montserrat"),
      plot.caption = ggplot2::element_text(
        family = "Montserrat-Regular",
        color = "#FFFFFF",
        size = caption_size,
        lineheight = 1,
        hjust = 0,
        vjust = -5
      ),
      axis.title.x = ggplot2::element_text(
        family = "Montserrat-Bold",
        size = 8,
        color = "#FFFFFF"
      ),
      axis.title.y = ggplot2::element_text(
        family = "Montserrat-Bold",
        size = 8,
        color = "#FFFFFF"
      ),
      axis.text.x = ggplot2::element_text(
        family = "Montserrat",
        size = 8,
        color = "#FFFFFF"
      ),
      axis.text.y = ggplot2::element_text(
        family = "Montserrat",
        size = 8,
        color = "#FFFFFF"
      ),
      legend.position = "none",
      plot.margin = ggplot2::unit(c(
        t = 1,
        r = 1.5,
        b = 2,
        l = 1
      ), "lines")
    ) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      plot.background = element_rect(fill = "transparent"), # "#2a366c"), # bg of the panel
      panel.grid.major.x = ggplot2::element_line(color = "#D3D3D3",
                                                 size = 0.2),
      panel.grid.minor.x = ggplot2::element_line(
        linetype = 2,
        size = 0,
        color = "#D3D3D3"
      ),
      # panel.grid.major.x = ggplot2::element_blank(),
      # panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(hjust = 1),
      axis.title.y = ggplot2::element_text(hjust = 1),
      axis.ticks.x = ggplot2::element_line(size = 0.5, color = "#FFFFFF"),
      axis.ticks.y = ggplot2::element_line(size = 0.5, color = "#FFFFFF")
    )
}

###--- map -----------------
selected_wda <- wda_sf %>% 
  filter(wda == "Alamo")

leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 6)) %>%
  setView(-99.9018, 30.9686, zoom = 6) %>% 
  addPolygons(stroke = F,
              fill = T,
              fillOpacity = 1,
              fillColor = "#5f6fc1",
              group = "wdas",
              data = wda_sf) %>%
  
  addPolygons(stroke = T, 
              weight = 3,
              color = "black",
              opacity = 1,
              fill = T,
              fillOpacity = 0,
              label = ~wda,
              group = "highlight",
              layerId = ~wda,
              data = wda_sf) %>% 
  addPolygons(stroke = T,
              weight = 3,
              color = "black",
              opacity = 1,
              fill = T,
              fillOpacity = 1,
              fillColor = "#f26852",
              group = "selected_wda",
              data = selected_wda) %>% 
  addPolygons(color = "black",
              stroke = T,
              weight = 0.5,
              fill = F,
              group = "counties",
              data = counties) %>%
  setMapWidgetStyle(list(background= "transparent")) %>% 
  htmlwidgets::onRender("function(el, x) { 
               map = this
               map.dragging.disable();
               }")

###--- plots ---------------
# race-ethnicity pie chart
people %>% 
  filter(wda == "Alamo") %>% 
  select(wda, waa_white:waa_other) %>% 
  pivot_longer(waa_white:waa_other) %>%
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent")) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c('#f26852', '#981E0B', '#3ead92', '#5f6fc1', '#f9cd21'))

# indemand jobs that are living wage
lwj_wages %>%
  filter(wda == "Alamo") %>%
  filter(wage_band == "High Wage" | wage_band == "Mid-High Wage") %>% 
  ungroup() %>% 
  slice_max(order_by = annual_average_employment_2036, n = 10) %>% 
  select(wda, job = oes_2019_estimates_title, value = annual_average_employment_2036) %>% 
  mutate(value = round(value)) %>% 
  mutate(job = str_wrap(job, width = 40)) %>% 
  ggplot(aes(x = value, y = reorder(job, value))) +
  geom_bar(stat = "identity", 
           fill = "#f26852", 
           color = "white",
           size = 0.5,
           width = 0.6) +
  ggtheme() +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = comma)

# share of living wage jobs by industry
lwj_industry %>%
  ungroup() %>%
  filter(wda == "Alamo") %>%
  group_by(wda, industry_title) %>%
  mutate(mid_high = case_when(wage_band %in% c("High Wage", "Mid-High Wage") ~ no_of_employed)) %>%
  mutate(mid_high = sum(mid_high, na.rm = T)) %>%
  mutate(industry_title = str_wrap(industry_title, width = 40)) %>%
  ggplot(aes(x = no_of_employed, y = reorder(industry_title, mid_high), 
             group = wage_band, fill = wage_band)) +
  geom_bar(stat = "identity", 
           position = "stack",
           color = "white", 
           size = 0.5,
           width = 0.6) +
  ggtheme() +
  scale_fill_manual(values = c("#f26852", "#EDB4AB", "#5f6fc1","#2a366c")) +
  labs(x = "NUMBER OF JOBS", y = NULL) +
  scale_x_continuous(labels = comma) 

# median and quartile earnings for college grads
pseo_wda_df %>% 
  filter(wda_name == "Alamo") %>% 
  mutate(degree_level = case_when(degree_level == "01" ~ "Certificate < 1 year",
                                  degree_level == "02" ~ "Certificate 1-2 years",
                                  degree_level == "03" ~ "Associates",
                                  degree_level == "04" ~ "Certificate 2-4 years",
                                  degree_level == "05" ~ "Baccalaureate")) %>% 
  mutate(degree_level = factor(degree_level, levels = c("Certificate < 1 year", "Certificate 1-2 years", "Associates",
                                                        "Certificate 2-4 years", "Baccalaureate"),
                               ordered = T)) %>% 
  ggplot() +
  geom_point(aes(x = degree_level, y = y10_p50_earnings),
             size = 7, color = "#f26852") +
  geom_errorbar(aes(x = degree_level, y = y10_p50_earnings, ymin = y10_p25_earnings, ymax = y10_p75_earnings),
                color = "#f26852", width = 0.2, size = 2) +
  ggtheme() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Earnings") +
  theme(panel.grid.major.y = ggplot2::element_line(color = "#D3D3D3",
                                                   size = 0.2),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(size = 1, color = "#ffffff")
  )

###--- values -------------

# percentage of hh that earn a living wage
# number of households that earn a living wage
alice <- alice_hh_counts %>% 
  ungroup() %>% 
  filter(year == "2018") %>% 
  select(wda, wda_number, above_alice_household, above_alice_hh_share) %>% 
  mutate(above_alice_household = comma(round(above_alice_household, -3)),
         above_alice_hh_share = paste0(round(above_alice_hh_share), "%"))

# workforce
workforce <- waa %>% 
  ungroup() %>% 
  filter(year == "2036") %>% 
  select(wda, wda_number, total, 
         total_female, 
         nh_white_total, nh_black_total, hispanic_total,
         nh_asian_total, nh_other_total) %>% 
  mutate(pct_hispanic = paste0(round(100 * hispanic_total / total), "%"),
         pct_white = paste0(round(100 * nh_white_total / total), "%"),
         pct_black = paste0(round(100 * nh_black_total / total), "%"),
         pct_asian = paste0(round(100 * nh_asian_total / total), "%"),
         pct_other = paste0(round(100 * nh_other_total / total), "%"),
         pct_women = paste0(round(100 * total_female / total), "%"),
         total = comma(round(total, -3))) %>% 
  select(-(total_female:nh_other_total))


# education - census
education1 <- edu %>% 
  filter(education == "hs" | education == "college") %>% 
  select(wda, wda_number, education, median_income) %>% 
  mutate(
    median_income = paste0("$", comma(round(median_income, -2)))
  ) %>% 
  pivot_wider(id_cols = c(wda, wda_number), names_from = education, values_from = median_income) %>% 
  rename(median_income_residents_hs = hs, median_income_residents_college = college)

# education - pseo
wda_numbers <- edu %>% select(wda, wda_number) %>% distinct()
education2 <- pseo_wda_df %>%
  rename(wda = wda_name) %>% 
  left_join(wda_numbers) %>% 
  mutate(wda_number = case_when(wda == "Dallas-Fort Worth" ~ 4,
                                wda == "Heart of Texas" ~ 13,
                                wda == "North East" ~ 7,
                                wda == "South East Texas" ~ 18,
                                wda == "West Central" ~ 9,
                                wda == "Lower Rio Grande" ~ 23,
                                T ~ wda_number)) %>% 
  mutate(degree_level_numeric = as.numeric(gsub("0", "", degree_level))) %>% 
  group_by(wda) %>% 
  slice_max(order_by = degree_level_numeric, n = 1) %>% 
  mutate(emp_rate_wda_ps_grads = paste0(round(100 * y10_grads_emp / (y10_grads_emp + y10_grads_nme)), "%"),
         y10_p50_earnings = paste0("$", comma(round(y10_p50_earnings, -2)))) %>% 
  mutate(wda_ps_grads_degree = case_when(degree_level == "03" ~ "Associates",
                                         degree_level == "05" ~ "Baccalaureate")) %>% 
  ungroup() %>% 
  select(wda_number, wda_ps_grads_degree, median_income_wda_ps_grads = y10_p50_earnings, emp_rate_wda_ps_grads)

pdf_values <- left_join(alice, workforce, by = "wda_number") %>% 
  left_join(education1, by = "wda_number") %>% 
  left_join(education2, by = "wda_number")

write_csv(pdf_values, here::here("clean-data", "pdf_values.csv"))