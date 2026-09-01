# R practice script for date event data

options(scipen=999) # turn off scientific numerical notation

`%notin%` <- Negate(`%in%`) # custom function for filtering data

# Specify packages
packages <-
  c("tidyverse","janitor","stringr","data.table",# general data wrangling
    "readxl","readr",
    "lubridate"
  ) 

# Install packages if not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
sapply(packages, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read_csv("food_delivery_scooter_events.csv") 

glimpse(data)
# unique list of data to view
unique_vals <- lapply(data, unique)

# Practice question 1

time_taken_df <- data %>%
  filter(event_type %in% c("order_received","delivery_complete")) %>%
  group_by(delivery_id) %>%
  arrange(delivery_id, event_timestamp) %>%
  mutate(time_taken = event_timestamp - lag(event_timestamp))

time_taken_deliv <- time_taken_df %>%
  select(delivery_id, time_taken) %>%
  filter(!is.na(time_taken))
  
time_taken_deliv_all <- time_taken_deliv %>%
  ungroup() %>%
  summarise(time_taken_avg = mean(time_taken)) %>% pull()

time_taken_deliv_store <- time_taken %>%
  group_by(store_name) %>%
  summarise(time_taken_avg = mean(time_taken, na.rm = TRUE))

min(time_taken_deliv_store$time_taken_avg)
max(time_taken_deliv_store$time_taken_avg)


# Practice question 2

