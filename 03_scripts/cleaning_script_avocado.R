library(tidyverse)
library(here)
library(janitor)
library(tmaptools)

data <- read_csv(here::here("02_data", "avocado", "avocado_raw.csv"))

# Function to clean region names
split_camel <- function(string_to_split) {
  paste(unlist(strsplit(string_to_split, "(?<=[a-z])(?=[A-Z])", perl = TRUE)), collapse = " ")
}

# Geocoding
regions_unique <- data$region %>% unique()
geocodes <- regions_unique %>%  geocode_OSM()

regions_unique_clean <- map_chr(regions_unique, split_camel)
geocodes <- regions_unique_clean %>% geocode_OSM()

# Long format
data_long <- data %>% 
  clean_names() %>% 
  select(-c(x1, total_volume, total_bags)) %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, everything()) %>% 
  rename(small = x4046,
         medium = x4225,
         large = x4770) %>% 
  gather(kind, volume, small:x_large_bags) %>% 
  select(id, date, region, type, kind, volume, average_price) %>% 
  mutate(region = map_chr(region, split_camel)) %>% 
  left_join(geocodes[, 1:3], by = c("region" = "query"))

write_csv(data_long, here::here("02_data", "avocado", "avocado_long.csv"), na = "")

# Total sales only
data_total_only <- data %>% 
  clean_names() %>% 
  select(-c(x1, x4046:x_large_bags)) %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, everything())

write_csv(data_total_only, here::here("02_data", "avocado", "avocado_total_only.csv"), na = "")
