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
#  How long does each stage of the delivery journey take

time_taken_all_stages <- data %>%
  group_by(delivery_id) %>%
  arrange(delivery_id, event_id) %>%
  mutate(
    prev_timestamp = lag(event_timestamp),
    next_timestamp = lead(event_timestamp)
    ) %>%
  mutate(
    stage_time_taken = lead(event_timestamp) - event_timestamp,
    stage_time_taken2 = # diff method, formatted in mins
      as.numeric(difftime(next_timestamp, event_timestamp, units = "mins")
      )
    ) %>%
  ungroup()

time_taken_all_stages2 <- time_taken_all_stages %>%
  group_by(event_type) %>%
  summarise(
    stage_time_taken_all = sum(stage_time_taken, na.rm = TRUE),
    stage_time_taken_avg = round(mean(stage_time_taken, na.rm = TRUE),2)
  ) %>%
  arrange(stage_time_taken_avg) %>%
  filter(!is.na(stage_time_taken_avg))


# Practice question 3:
# Which delivery events or stages are most associated with minor delays, 
# and how long does the next stage take after a delayed event?

# find all duration times between events
# highlight next event time
# filter for delayed event time and calc next stage

time_taken_delayed <- time_taken_all_stages %>%
  group_by(event_type, status) %>%
  summarise(
    n = n(),
    stage_time_taken_avg = round(mean(stage_time_taken, na.rm = TRUE),2)
  ) %>%
  arrange(event_type, desc(status))


time_taken_minor_delay <- time_taken_all_stages %>%
  group_by(delivery_id) %>%
  mutate(
    delay_flag = if_else(any(status == "minor_delay"), TRUE, FALSE)
  ) %>%
  ungroup() %>%
  group_by(delay_flag) %>%
  summarise(stage_time_taken_avg = round(mean(stage_time_taken, na.rm = TRUE),2))


# Practice question 4: After accounting for distance, 
# are delayed deliveries actually slower per kilometre,

#speed_km_per_min = distance_km / stage_time_taken
 
#mins_per_km = stage_time_taken / distance_km

glimpse(data)

time_taken_all_stages3 <- data %>%
  group_by(delivery_id) %>%
  arrange(delivery_id, event_id) %>%
  mutate(
    prev_timestamp = lag(event_timestamp),
    next_timestamp = lead(event_timestamp)
  ) %>%
  mutate(
    stage_time_taken = # diff method, formatted in mins
      as.numeric(difftime(next_timestamp, event_timestamp, units = "mins"))
  ) %>%
  mutate(distance_km = na_if(distance_km, 0)) %>%
  mutate( # distance calcs
    speed_km_per_mind = distance_km / stage_time_taken,
    mins_per_km = stage_time_taken / distance_km
  ) %>%
  ungroup() %>%
  group_by(delivery_id) %>%
  mutate(
    delay_flag = if_else(any(status == "minor_delay"), TRUE, FALSE)
  )


sum = time_taken_all_stages3 %>%
  group_by(delay_flag) %>%
  summarise(
    n = n(), 
    min_per_km_total = sum(mins_per_km, na.rm = TRUE),
    min_per_km_avg = mean(mins_per_km, na.rm = TRUE)
  )


# delivery level

deliv_data <- time_taken_all_stages3 %>%
  group_by(delivery_id, delay_flag) %>%
  summarise(
    deliv_time_taken = sum(stage_time_taken, na.rm = TRUE),
    deliv_distance_km = sum(distance_km,na.rm = TRUE)
  ) %>%
  mutate(deliv_min_per_km = deliv_time_taken / deliv_distance_km) 
  

deliv_data %>%
  group_by(delay_flag) %>%
  summarise(deliv_min_per_km_avg = mean(deliv_min_per_km, na.rm = TRUE))
