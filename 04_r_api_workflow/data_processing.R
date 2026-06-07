# data processing and analysis script loading in API data
# cleaning and transforming data
# calculating and summarising all key values for report


library(tidyverse)
library(httr)
library(jsonlite)
library(furrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("api_functions.R")

nordic_countries <- c("IS","NO","SE","FI","DK")

#sitc_codes <- ons_get_codes("sitc")


# EXTACT DATA: ------------

nordic_data <- map_dfr(
  nordic_countries, 
  ~ ons_get_trade_year(
    2019:2025,
    sitc = "T",
    .x,
    flow = "IM"
  )
)


uk_total <- ons_get_trade_year(
  years = 2019:2025,
  sitc = "T",
  country = "W1",
  flow = "IM"
) 

# CLEAN & TRANSFORM DATA ----------


# join data and clean

data_month <-  nordic_data %>%
  left_join(uk_total %>% select(time, uk_total = value), by = "time") %>%
  mutate(
    value = as.numeric(value),
    uk_total = as.numeric(uk_total),
    year = paste0("20", substr(time, 5, 6)),
    month = substr(time, 1, 3)
  ) %>%
  select(-geography)


data_yr <-  nordic_data %>%
  left_join(uk_total %>% select(time, uk_total = value), by = "time") %>%
  mutate(
    value = as.numeric(value),
    uk_total = as.numeric(uk_total),
    year = paste0("20", substr(time, 5, 6)),
    month = substr(time, 1, 3)
  ) %>%
  group_by(year,country,sitc,flow) %>%
  summarise(
    total_imports = sum(value, rm.na = TRUE),
    total_imports_uk = sum(uk_total, rm.na = TRUE)
  ) %>% 
  arrange(country)



# Calculate shares of monthly data

uk_share_month <- data_month %>%
  group_by(time) %>%
  mutate(
    nordic_total = sum(value, na.rm = TRUE),
    share_of_nordic = value / nordic_total,
    share_of_uk = value / uk_total
  ) %>%
  ungroup() %>%
  group_by(country, year) %>%
  summarise(
    avg_share_uk = mean(share_of_uk, na.rm = TRUE),
    avg_share_nordic = mean(share_of_nordic, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(across(c(avg_share_uk:avg_share_nordic), ~ round(.x, 3)))


# Calculate shares of yearly data

uk_share_yr <- data_yr %>%
  group_by(year) %>%
  mutate(
    nordic_total = sum(total_imports, na.rm = TRUE),
    share_of_nordic = total_imports / nordic_total,
    share_of_uk = total_imports / total_imports_uk
  ) %>%
  ungroup() %>%
  mutate(across(c(share_of_nordic:share_of_uk), ~ round(.x, 3))) %>%
  mutate(nordic_share_uk = nordic_total / total_imports_uk)


# Individual calcs ---------


nordic_import_total.25 <- sum(data_yr$total_imports[data_yr$year==2025])
nordic_import_total.19 <- sum(data_yr$total_imports[data_yr$year==2019])
nordic_change_pc <- round((1-(nordic_import_total.19 / nordic_import_total.25))*100,1)

nordic_import_share_uk.25 <- uk_share_yr %>% filter(year == 2025) %>% select(nordic_share_uk) %>% distinct() %>% pull()
nordic_import_share_uk.25 <- round(nordic_import_share_uk.25*100, 1)

nordic_import_share_uk.19 <- uk_share_yr %>% filter(year == 2019) %>% select(nordic_share_uk) %>% distinct() %>% pull()
nordic_import_share_uk.19 <- round(nordic_import_share_uk.19*100, 1)

nordic_import_share_uk.22 <- uk_share_yr %>% filter(year == 2022) %>% select(nordic_share_uk) %>% distinct() %>% pull()
nordic_import_share_uk.22 <- round(nordic_import_share_uk.22*100, 1)


