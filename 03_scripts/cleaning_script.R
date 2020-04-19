library(tidyverse)
library(readxl)
library(here)
library(conflicted)
library(janitor)
library(countrycode)

conflict_prefer("here", "here")
conflict_prefer("filter", "dplyr")

data <- read_excel(here("02_data", "wdi", "world_development_indicators_raw.xlsx")) %>% 
  clean_names()

indicators <- c("GDP per capita (current US$)",
                "Fertility rate, total (births per woman)",
                "Mortality rate, infant (per 1,000 live births)",
                "Life expectancy at birth, total (years)",
                "Literacy rate, adult total (% of people ages 15 and above)",
                "Population, total",
                "CO2 emissions (metric tons per capita)",
                "Energy use (kg of oil equivalent per capita)",
                "Renewable energy consumption (% of total final energy consumption)",
                "Fossil fuel energy consumption (% of total)",
                "Energy related methane emissions (% of total)")
                
data_clean <- data %>%
  filter(indicator_name %in% indicators) %>%
  select(-c(indicator_code, country_code)) %>% 
  gather(year, value, -c(country_name, indicator_name)) %>% 
  mutate(year = str_remove(year, "x")) %>% 
  spread(indicator_name, value) %>% 
  clean_names() %>% 
  filter(as.numeric(year) <= 2014) %>% 
  rename(gdp_per_capita_current_usd = gdp_per_capita_current_us) %>% 
  mutate(country_name = str_replace(country_name, "Russian Federation", "Russia") %>% 
                                    str_replace("Russian Federation", "Russia") %>% 
                                    str_replace("Korea, Rep.", "South Korea") %>% 
                                    str_replace("Congo, Dem. Rep.", "Republic Congo") %>% 
                                    str_replace("Korea, Dem. People’s Rep.", "North Korea") %>% 
                                    str_extract("[a-zA-Z\\s]+(?=,?)")) %>%
  filter(country_name %in% c(codelist$country.name.en,
                             "Czech Republic",
                             "Myanmar",
                             "North Macedonia",
                             "Syrian Arab Republic",
                             "Kyrgyz Republic",
                             "Bosnia and Herzegovina",
                             "Republic Congo",
                             "South Korea",
                             "North Korea"))  %>% 
  left_join(codelist[,c("country.name.en", "continent")],
             by = c("country_name" = "country.name.en")) %>% 
  mutate(continent = case_when(country_name == "Bosnia and Herzegovina" ~ "Europe",
                               country_name == "Czech Republic" ~ "Europe",
                               country_name == "Kosovo" ~ "Europe",
                               country_name == "Kyrgyz Republic" ~ "Asia",
                               country_name == "Myanmar" ~ "Asia",
                               country_name == "North Macedonia" ~ "Europe",
                               country_name == "Syrian Arab Republic" ~ "Asia",
                               country_name == "Russia" ~ "Asia",
                               country_name == "Republic Congo" ~ "Africa",
                               country_name == "South Korea" ~ "Asia",
                               country_name == "North Korea" ~ "Asia",
                               TRUE ~ continent
                               ))

write_csv(data_clean,
          here("02_data", "wdi", "world_development_indicators_clean.csv"),
          na = "")