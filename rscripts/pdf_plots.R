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

###--- GGPLOT THEME ----------------------------------
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
  group_by(industry_title, wage_band) %>% 
  summarize(wda = wda[1],
            number_jobs = sum(no_of_employed)) %>% 
  mutate(industry_title = str_wrap(industry_title, width = 40)) %>% 
  ggplot(aes(x = number_jobs, y = reorder(industry_title, number_jobs), group = wage_band, fill = wage_band)) +
  geom_bar(stat = "identity", 
           position = "stack",
           color = "white", 
           size = 0.5,
           width = 0.6) +
  ggtheme() +
  scale_fill_manual(values = c("#f26852", "#EDB4AB", "#5f6fc1","#2a366c")) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = comma)
    


