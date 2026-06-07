# testing script for calling Trade data API. 

library(tidyverse)
library(httr)
library(jsonlite)
library(furrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("api_functions.R")

nordic_countries <- c("IS","NO","SE","FI","DK")

sitc_codes <- ons_get_codes("sitc")


# EXTACT DATA: ------------

## NOTE: sometimes API when pulling larger scale of data it fails, but when re-run works. 
# strange anomoly which I haven't sought to solve/investigate full reason why
# as this is not the purpose of this work
# Note: running the API call with nordic country list as .x - works fine, jsut re-run the API if it fails. 

data_fi <-
  ons_get_trade_furrr(
    time = c("Jan-22", "Feb-22", "Mar-22"),
    sitc = "T",
    country = "FI",
    flow = "IM"
  )


years = c(2021,2022,2023,2024,2025)

df_fi <- map_dfr(
     years,
  ~ ons_get_trade_year(
    .x,
    sitc = "T",
    country = "FI",
    flow = "IM"
     )
  )


df_se <- map_dfr(
  years,
  ~ ons_get_trade_year(
    .x,
    sitc = "T",
    country = "SE",
    flow = "IM"
  )
)

nordic_data <- map_dfr(
  years,
  ~ ons_get_trade_year(
    .x,
    sitc = "T",
    country = c("SE","FI","NO","DK","IS"),
    flow = "IM"
  )
)

nordic_countries <- c("SE","FI","NO","DK","IS")

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

# create both yearly and monthly files for later use
# calculate overall trade, nordic and uk total share for comparison
# simple tidyverse grouping, aggregations etc - nothing complex. 


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


# PLOT TESTING: ---------------



