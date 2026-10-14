# Delivery dummy data mini project for practice
# Using delivery style dummy data for using R/SQL style coding steps
# TO answer business style questions for practice anf challenge

# Simple script to load in data, transform and summarise outputs to answer
# different business style questions


# Dummy data to practice transaction style data in R

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


# load data

deliv_data <- read_excel("delivery_data.xlsx") 
glimpse(deliv_data)


#' Business framing:
#' Analysing the data to answer:
#' 1. Operational performance: i.e. how many deliveries and wider performance
#' 2. Restaurant performance: which restaurants are performing better than others
#' 3. Customer satisfaction: frequent customers and wider satisfaction 
#' 
#' Combined, this should answer:
#' how many deliveries, from which restaurateurs and what is wider satisfaction
#' Purpose of this is to highlight how well operations is doing. identify where falling short
#' and identify customer patterns for potential marketing opportunities etc. 
#' 
#' 

# 0. Data exploration and cleaning:

# NA check

deliv_data %>%
  filter(if_any(everything(), is.na))

# unique values for group col checks

unique_vals <- lapply(deliv_data, unique)

# data is very normal, no issues besides need to convert dates

deliv_data <- deliv_data %>%
  mutate(
    order_timestamp    = ymd_hms(order_timestamp),
    delivery_timestamp = ymd_hms(delivery_timestamp)
  ) %>%
  filter(!is.na(order_timestamp)) %>%
  filter(total_amount > -100)

# break dates by month

deliv_data <- deliv_data %>% 
  mutate(month = format(order_timestamp, "%Y-%m")) %>%
  mutate(month_name = format(order_timestamp, "%B"))
  
# removed NA, date had 51 instead of likely 31 as d, however, without confirmation removing from analysis
# seperately, total values have negatives - which could be likely offers or refunds
# however, two have minus 999 whicih looks anomolous - so removing until further investigation could occur. 
  
  
# 1. General data exploration for overall deliveries

deliv_complete <- deliv_data %>%
  
# logic is: simple exploration over the groups: customer, restaurant, type, delivery method etc
# to see if any immediate patterns. 
  
group_vars <- c("user_id", "restaurant_id", "cuisine_type", "delivery_method")

deliv_data %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(
    count = n(),
    total_amount       = sum(total_amount, na.rm = TRUE),
    total_items        = sum(number_of_items, na.rm = TRUE),
    avg_amount         = mean(total_amount, na.rm = TRUE),
    .groups = "drop"
  )


summary_group_vals <- map(group_vars, ~ {
 
   deliv_data %>%
    group_by(.data[[.x]]) %>%
    summarise(
      count = n(),
      total_amount       = sum(total_amount, na.rm = TRUE),
      total_items        = sum(number_of_items, na.rm = TRUE),
      avg_amount         = mean(total_amount, na.rm = TRUE),
      .groups = "drop"
    )
})

names(summary_group_vals) <- group_vars

# quick summary of grouped variables:
# few customers who order most - may be worth investigating. 
# few restaurants who have most orders, worth highlighting
# delivery methods are less useful without time. 

# need to total deliviers across months


deliv_data_total <- deliv_data %>%
  group_by(month_name) %>%
  summarise(
    total_deliv = n(),
    total_deliv_items = sum(number_of_items),
    total_deliv_amount = sum(total_amount),
    total_deliv_amount_avg = mean(total_amount)
  )


# next - across what type of food/restaurants 

deliv_data_rest <- deliv_data %>%
   group_by(restaurant_id, cuisine_type) %>%
  summarise(
    total_deliv = n(),
    total_deliv_items = sum(number_of_items),
    total_deliv_amount = sum(total_amount),
    total_deliv_amount_avg = mean(total_amount)
  )


deliv_data_user <- deliv_data %>%
  group_by(user_id) %>%
  summarise(
    total_deliv = n(),
    total_deliv_items = sum(number_of_items),
    total_deliv_amount = sum(total_amount),
    total_deliv_amount_avg = mean(total_amount)
  )


deliv_data_user_cuisine <- deliv_data %>%
  group_by(user_id, cuisine_type) %>%
  summarise(
    total_deliv = n(),
    total_deliv_items = sum(number_of_items),
    total_deliv_amount = sum(total_amount),
    total_deliv_amount_avg = mean(total_amount)
  )

