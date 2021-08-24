#Loops for slides plots

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

aj <- readRDS(here::here("clean-data", "brookings-data.rds"))

# shapefiles
wda_sf <- readRDS(here::here("clean-data", "wda_shapefile.rds"))
counties <- tigris::counties(state = "48") %>% 
  dplyr::select(county = NAME, geometry) %>% 
  st_transform(crs = wgs84) %>% 
  ms_simplify(0.05)

###--- ggtheme ----------------------------------
ggtheme <- function (base_size = 18,
                            base_family = "Montserrat",
                            title_size = 12,
                            subtitle_size = 9,
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
                                               color = "white",
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
        size = 11,
        color = "#FFFFFF"
      ),
      axis.title.y = ggplot2::element_text(
        family = "Montserrat-Bold",
        size = 11,
        color = "#FFFFFF"
      ),
      axis.text.x = ggplot2::element_text(
        family = "Montserrat",
        size = 12,
        color = "#FFFFFF"
      ),
      axis.text.y = ggplot2::element_text(
        family = "Montserrat",
        size = 12,
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
      plot.background = element_rect(fill = "#2a366c"), # bg of the panel
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

###--- plots ---------------

# MAP ------------------------
WDA_NAME <- "Alamo"

for (WDA_NAME in unique(wda_sf$wda)) {
  
  selected_wda <- filter(wda_sf, wda==WDA_NAME)
  
  ggplot() +
    geom_sf(data = wda_sf, fill="#3a4a9f", color = "#1f214d", size=0.75) +
    geom_sf(data = selected_wda, fill="#f26852", color = "#1f214d", size=0.75) +
    geom_sf(data = counties, fill="transparent", color = "#1f214d", size=0.15) +
    theme_void()
  
  filename <- str_remove_all(WDA_NAME, " ")
  ggsave(file=here::here("slides", "maps", paste0(filename, ".png")), width=3, height=3)
  
  if (WDA_NAME == unique(people$wda)[24]) {
    print("Done!")
  }
  
}

#Texas map
ggplot() +
  geom_sf(data = st_union(wda_sf), fill="#3a4a9f", color = "transparent", size=0.75) +
  theme_void()

ggsave(file=here::here("slides", "maps", paste0("Texas", ".png")), width=3, height=3)


# race-ethnicity pie chart ------
for (WDA_NAME in unique(people$wda)) {
  people %>% 
    filter(wda == WDA_NAME) %>% 
    select(wda, "White" = waa_white, "Black" = waa_black, "Hispanic" = waa_hispanic, "Asian" = waa_asian, "Other" = waa_other) %>% 
    pivot_longer(White:Other, names_to = "Race", values_to = "value") %>%
    ggplot(aes(x = "", y = value, fill = Race)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(#plot.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill="#3a4a9f", color=NA),
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "none",
          plot.title = ggtext::element_markdown(
            size = 12,
            color = "#FFFFFF",
            family = "Montserrat-ExtraBold",
            hjust = 0.5)
    ) +
    labs(title = "RACE-ETHNICITY IN 2036") +
    scale_fill_manual(values = c('#F58B7A', '#981E0B', '#3ead92', '#1f214d', '#f9cd21'))
  
  filename <- str_remove_all(WDA_NAME, " ")
  ggsave(file=here::here("slides", "race-pie", paste0(filename, ".png")), width=2.5, height=2.5)
  
}


# indemand jobs that are living wage ---------
for (WDA_NAME in unique(people$wda)) {
  
  lwj_wages %>%
    filter(wda == WDA_NAME) %>%
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
    theme(
      plot.background = element_rect(fill="#3a4a9f", color=NA),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text.y = element_text(size = 7.5),
      axis.text.x = element_text(size = 7.5)
    ) +
    labs(x = NULL, y = NULL,
         title = "TOP 10 IN-DEMAND, LIVING WAGE JOBS IN 2036") +
    scale_x_continuous(labels = comma)
  
  filename <- str_remove_all(WDA_NAME, " ")
  ggsave(file=here::here("slides", "lwj-bar", paste0(filename, ".png")), width=7, height=5)
  
  if (WDA_NAME == unique(people$wda)[25]) {
    print("Done!")
  }
  
}

# share of living wage jobs by industry -----------
for (WDA_NAME in unique(people$wda)) {
  lwj_industry %>%
    ungroup() %>%
    filter(wda == WDA_NAME) %>% 
    group_by(wda, industry_title) %>%
    mutate(mid_high = case_when(wage_band %in% c("High Wage", "Mid-High Wage") ~ no_of_employed)) %>%
    mutate(mid_high = sum(mid_high, na.rm = T)) %>%
    mutate(industry_title = str_wrap(industry_title, width = 35)) %>%
    ggplot(aes(x = no_of_employed, y = reorder(industry_title, mid_high), 
               group = wage_band, fill = wage_band)) +
    geom_bar(stat = "identity", 
             position = "stack",
             color = "white", 
             size = 0.2,
             width = 0.6) +
    ggtheme() +
    theme(
      plot.background = element_rect(fill="#3a4a9f", color=NA),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text.y = element_text(size = 7.5),
      axis.text.x = element_text(size = 7.5)
    ) +
    scale_fill_manual(values = c("#f26852", "#EDB4AB", "#5f6fc1","#2a366c")) +
    labs(x = NULL, y = NULL,
         title = "SHARE OF LIVING WAGE JOBS BY INDUSTRY") +
    scale_x_continuous(labels = comma) 
  
  filename <- str_remove_all(WDA_NAME, " ")
  ggsave(file=here::here("slides", "lwj-industry", paste0(filename, ".png")), width=7, height=5)
  
  if (WDA_NAME == unique(people$wda)[25]) {
    print("Done!")
  }
}

# pseo education lines -----------------
WDA_NAME == "Alamo"
for (WDA_NAME in unique(people$wda)) {
  
  pseo_wda_df %>% 
    filter(wda == WDA_NAME) %>% 
    mutate(degree_level = case_when(degree_level == "01" ~ "Certificate < 1 year",
                                    degree_level == "02" ~ "Certificate 1-2 years",
                                    degree_level == "03" ~ "Associate's",
                                    degree_level == "04" ~ "Certificate 2-4 years",
                                    degree_level == "05" ~ "Bachelor's")) %>% 
    mutate(degree_level = factor(degree_level, levels = c("Certificate < 1 year", "Certificate 1-2 years", "Associate's",
                                                          "Certificate 2-4 years", "Bachelor's"),
                                 ordered = T)) %>% 
    ggplot() +
    geom_point(aes(x = degree_level, y = y10_p50_earnings),
               size = 4, color = "#f26852") +
    geom_errorbar(aes(x = degree_level, y = y10_p50_earnings, ymin = y10_p25_earnings, ymax = y10_p75_earnings),
                  color = "#f26852", width = 0.2, size = 1) +
    ggtheme() +
    scale_y_continuous(labels = dollar) +
    labs(x = NULL, y = NULL,
         title = "MEDIAN, 25TH, AND 75TH PERCENTILE SALARY <br> AMONG AREA GRADUATES BY DEGREE TYPE") +
    theme(
      plot.background = element_rect(fill="#3a4a9f", color=NA),
      panel.grid.major.y = ggplot2::element_line(color = "#D3D3D3",
                                                 size = 0.2),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(size = 1, color = "#ffffff"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 7.5),
      axis.text.y = element_text(size = 7.5)
    ) 
  
  filename <- str_remove_all(WDA_NAME, " ")
  ggsave(file=here::here("slides", "pseo-lines", paste0(filename, ".png")), width=5, height=3.5)
  
  if (WDA_NAME == unique(people$wda)[25]) {
    print("Done!")
  }
}

# attractive jobs -----------------

for (WDA_NAME in unique(people$wda)) {
  aj %>% 
    filter(wda == WDA_NAME) %>% 
    filter(!is.na(occupation)) %>% 
    ggplot() +
    geom_point(aes(x = demand_index, y = quality_index, color = quality_and_demand_quadrant, size = share_of_local_jobs_percent),
               alpha = 0.5) +
    labs(x = "Demand Index",
         y = "Quality Index",
         title = "QUALITY AND DEMAND INDICES",
         subtitle = "Point size is proportional to the number of workers in the occupation. Thresholds<br>show the Quality Index and Demand Index of the average occupation.") +
    geom_hline(aes(yintercept = 0),
               color = "white", size = 1) +
    geom_text(aes(x = -4, y = 0, label = "Quality threshold", 
                  vjust = -1),
              color = "white", size = 3) +
    #geom_text()
    geom_vline(aes(xintercept = 0),
               color = "white", size = 1) +
    geom_text(aes(x = 0, y = -5, label = "Demand threshold",
                  hjust = -0.1), color = "white", size = 3) +
    ggtheme() +
    theme(plot.background = element_rect(fill="#3a4a9f", color=NA),
      panel.grid.major.y = ggplot2::element_line(color = "#D3D3D3",
                                                     size = 0.1),
          axis.text.x = element_text(size = 7.5),
          axis.text.y = element_text(size = 7.5)) +
    xlim(-5, 5) + 
    ylim(-5, 5) +
    scale_color_manual(values = c("#3ead92", "#8490cf", "#080a15", "#f26852"))
  filename <- str_remove_all(WDA_NAME, " ")
  ggsave(file=here::here("slides", "quality", paste0(filename, ".png")), width=6, height=4.5)
  
}


