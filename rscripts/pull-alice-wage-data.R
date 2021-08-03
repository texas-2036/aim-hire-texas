
library(RSelenium)
library(rvest)
library(tidyverse)
library(janitor)
library(here)
library(xml2)

driver <- rsDriver(browser=c("firefox"), port = 4444L)
rd <- driver[["client"]]
#rd$open()
rd$setTimeout(type = "implicit", milliseconds = 10000)


rd$navigate('https://www.unitedforalice.org/household-budgets/texas')

html <- rd$getPageSource()[[1]]

x <- 2

data <- lapply(1:254, function(x) {
  paths <- read_html(html) %>% 
    html_nodes("path.shape-border") %>% 
    .[[x]] %>% 
    html_attr("data-info") 
  
  table_raw <- read_html(paths) %>% 
    html_table(fill=T) %>% 
    .[[1]]
  
  county_name <- table_raw$X1[1] %>% str_remove("ALICE Survival Budget ") %>% 
    str_remove(" County, Texas")
  
  table <- table_raw[-1,] %>% 
    row_to_names(1) %>% 
    clean_names() %>% 
    mutate(
      county = county_name
    )
  
  return(table)
}) %>% 
  do.call(rbind, data) 

clean_data <- data %>% 
  mutate(
    across(hourly:annually, ~str_remove(.x, "\\$")),
    across(hourly:annually, ~str_remove(.x, ",")),
    across(hourly:annually, as.numeric)
  )

saveRDS(clean_data, here::here("raw-data", "alice-wage-data.rds"))

#Shut down server #######################
rd$close()
driver$server$stop()
