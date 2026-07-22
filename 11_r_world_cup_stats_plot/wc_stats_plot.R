# World cup stats plotting script

#' This  mini project is essentially
#' small bit of annoying data processing
#' particular with country codes
#' ensuring the matched world cup
#' and econ data is merged properly together
#' to then plot some correlations
#' across a selection of metrics
#' to see if we can find anything interesting
#' 


options(scipen=999) # turn off scientific numerical notation

`%notin%` <- Negate(`%in%`) # custom function for filtering data

# Specify packages
packages <-
  c("tidyverse","janitor","stringr","data.table",# general data wrangling
    "readxl","readr",
    "ggflags", "countrycodes"
  ) 

# Install packages if not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
sapply(packages, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load in imf data --

imf_data <- read_excel("imf_econ_data.xlsx") %>% clean_names()

imf_data <- 
  imf_data %>%
  mutate(iso2 = countrycode(country_id, "iso3c", "iso2c")) %>%
  relocate(iso2, .after = "country_id")

# select IMF indicators

# BCA_NGDPD: Credit account balance pc of GDP
# BCA: Credit account US dollar
# GGX_NGDP: Expenditure, General government, Percent of GDP
# NGDPDPC: Gross domestic product (GDP), Current prices, Per capita, US dollar
# NGDPD:  Gross domestic product (GDP), Current prices, US dollar
# LP: Population, Persons for countries / Index for country groups
# LUR: Unemployment rate

econ_id <- c("GGX_NGDP")

imf_data2 <- imf_data %>%
  filter(indicator_id %in% econ_id) %>%
  select(
    iso2,
    country,
    indicator_id,
    indicator,
    unit, 
    x2025
  )


# wc data processing --

#' for simpler matching and interpretation
#' a team level dataset needs creating
#' team 1 and team 2 needs splitting into two analysing
#' then binidng together and aggregated for a full list
#' and team level wc stats to model against
#' 

wc_data <- read_csv("wc26_results.csv") %>% clean_names()


# match wc data --





