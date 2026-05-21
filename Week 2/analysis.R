
#' General 'policy' question being asked to frame analysis for:
#'  - What does the most recent year of UK preference utilisation look like, 
#'  - how does utilisation vary across partners, 
#'  - how has this changed over time, 
#'  - and which sectors drive high‑utilisation partners?
#'  

# Set up and load data ----------

# I write at the beginning of every script:

# rm(list = ls()) # remove items from global environment

options(scipen=999) # turn off scientific numerical notation

`%notin%` <- Negate(`%in%`) # custom function for filtering data

# Specify packages
packages <-
  c("tidyverse","janitor","stringr","data.table",# general data wrangling
    "readxl","readr","openxlsx", "readODS", # reading/writing excel
    "rvest",# r web-scraping
    "tictoc" # helpful package for timing code speed
  ) 

# Install packages if not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
sapply(packages, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


tic()

# load data and inspect

imp <- read_csv("data/pur_imports_data.csv")
exp <- read_csv("data/pur_exports_data.csv")


# eu27 country list

eu27.iso <-c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI",
             "FR","GR","HR","HU","IE","IT","LT","LU","LV","MT",
             "NL","PL","PT","RO","SE","SI","SK")

# current idea:
# write out generic lines of code you would do to answer question
# do a pur drivers example
# functionise this process to make it mroe interesting


# exploratory analysis first look at high level figures


# note when ranking by pur %
# this dis proportionally benefits smaller trading partners
# countries with larger trade flows
# ordered on largest pref eligible partners are identified
# as a medium sized PUR on a larger amount of pref eligible trade
# is more valuable $$
# than a high PUR on a much smaller amount of trade 


# high level figures and top 10s ----------------------

## imports -------------------

imp.pur <- imp %>%
  filter(year == "2024") %>%
  summarise(
    total_imports = sum(total_imports, na.rm = T),
    pref_elig_imp = sum(pref_eligible_imports, na.rm = T),
    pref_usage = sum(pref_usage_imports, na.rm = T)
  ) %>%
  mutate(pur_pct = round(pref_usage / pref_elig_imp,3)*100) %>%
  select(pur_pct) %>%
  pull()



imp.10.noneu <- imp %>%
  filter(partner_iso %notin% eu27.iso & agreement %notin% c("No FTA", "No Agreement")) %>%
  filter(year == "2024") %>%
  group_by(partner_iso, partner_name) %>%
  summarise(
    total_imports = sum(total_imports, na.rm = T),
    pref_elig_imp = sum(pref_eligible_imports, na.rm = T),
    pref_usage = sum(pref_usage_imports, na.rm = T)
  ) %>%
  mutate(across(c(total_imports:pref_usage), ~ round(.x/1000000,0))) %>%
  mutate(pur_pct = pref_usage / pref_elig_imp) %>%
  ungroup() %>%
  mutate(rank = rank(desc(pref_elig_imp))) %>% 
  filter(rank <= 10) %>%
  arrange(rank)


imp.10.eu <- imp %>%
  filter(partner_iso %in% eu27.iso) %>%
  filter(year == "2024") %>%
  group_by(partner_iso, partner_name) %>%
  summarise(
    total_imports = sum(total_imports, na.rm = T),
    pref_elig_imp = sum(pref_eligible_imports, na.rm = T),
    pref_usage = sum(pref_usage_imports, na.rm = T)
  ) %>%
  mutate(across(c(total_imports:pref_usage), ~ round(.x/1000000,0))) %>%
  mutate(pur_pct = pref_usage / pref_elig_imp) %>%
  ungroup() %>%
  mutate(rank = rank(desc(pref_elig_imp))) %>% 
  filter(rank <= 10) %>%
  arrange(rank)




## exports -------------------------


exp.pur <- exp %>%
  filter(year == "2024") %>%
  summarise(
    total_exports = sum(total_exports, na.rm = T),
    pref_elig_exp = sum(pref_eligible_exports, na.rm = T),
    pref_usage = sum(pref_usage_exports, na.rm = T)
  ) %>%
  mutate(pur_pct = round(pref_usage / pref_elig_exp, 3)*100) %>%
  select(pur_pct) %>%
  pull()


exp.10.eu <- exp %>%
  filter(year == "2024") %>%
  group_by(partner_iso, partner_name) %>%
  summarise(
    total_imports = sum(total_exports, na.rm = T),
    pref_elig_exp = sum(pref_eligible_exports, na.rm = T),
    pref_usage = sum(pref_usage_exports, na.rm = T)
  ) %>%
  #mutate(across(c(total_imports:pref_usage), ~ .x/1000000000))
  mutate(pur_pct = pref_usage / pref_elig_exp) %>%
  ungroup() %>%
  mutate(rank = rank(desc(pref_elig_exp))) %>% 
  filter(rank <= 10)


  
exp.10.noneu <- exp %>%
  filter(partner_iso %notin% eu27.iso) %>%
  filter(year == "2024") %>%
  group_by(partner_iso, partner_name) %>%
  summarise(
    total_imports = sum(total_exports, na.rm = T),
    pref_elig_exp = sum(pref_eligible_exports, na.rm = T),
    pref_usage = sum(pref_usage_exports, na.rm = T)
   ) %>%
  mutate(across(c(total_imports:pref_usage), ~ round(.x/1000000,0))) %>%
  mutate(pur_pct = pref_usage / pref_elig_exp) %>%
  ungroup() %>%
  mutate(rank = rank(desc(pref_elig_exp))) %>% 
  filter(rank <= 10)


exp.10.eu <- exp %>%
  filter(partner_iso %in% eu27.iso) %>%
  filter(year == "2024") %>%
  group_by(partner_iso, partner_name) %>%
  summarise(
    total_imports = sum(total_exports, na.rm = T),
    pref_elig_exp = sum(pref_eligible_exports, na.rm = T),
    pref_usage = sum(pref_usage_exports, na.rm = T)
  ) %>%
  mutate(across(c(total_imports:pref_usage), ~ round(.x/1000000,0))) %>%
  mutate(pur_pct = pref_usage / pref_elig_exp) %>%
  ungroup() %>%
  mutate(rank = rank(desc(pref_elig_exp))) %>% 
  filter(rank <= 10)


# Pur overtime --------------

## imports --------

imp.pur.ts <- imp %>%
  group_by(year) %>%
  summarise(
    total_imports = sum(total_imports, na.rm = T),
    pref_elig_imp = sum(pref_eligible_imports, na.rm = T),
    pref_usage = sum(pref_usage_imports, na.rm = T)
  ) %>%
  mutate(pur_pct = round(pref_usage / pref_elig_imp,3)*100) %>%
  select(year,pur_pct) 


imp.noneu.ts = imp %>%
  group_by(partner_iso, partner_name, year) %>%
  summarise(
    total_imports = sum(total_imports, na.rm = T),
    pref_elig_imp = sum(pref_eligible_imports, na.rm = T),
    pref_usage = sum(pref_usage_imports, na.rm = T)
  ) %>%
  mutate(across(c(total_imports:pref_usage), ~ round(.x/1000000,0))) %>%
  mutate(pur_pct = pref_usage / pref_elig_imp) %>%
  ungroup() %>%
  filter(partner_iso %in% imp.10.noneu$partner_iso) # filter on top countries


imp.eu.ts = imp %>%
  group_by(partner_iso, partner_name, year) %>%
  summarise(
    total_imports = sum(total_imports, na.rm = T),
    pref_elig_imp = sum(pref_eligible_imports, na.rm = T),
    pref_usage = sum(pref_usage_imports, na.rm = T)
  ) %>%
  mutate(across(c(total_imports:pref_usage), ~ round(.x/1000000,0))) %>%
  mutate(pur_pct = pref_usage / pref_elig_imp) %>%
  ungroup() %>%
  filter(partner_iso %in% imp.10.eu$partner_iso) # filter on top countries


## exports ------


exp.pur.ts <- exp %>%
  group_by(year) %>%
  summarise(
    total_exports = sum(total_exports, na.rm = T),
    pref_elig_exp = sum(pref_eligible_exports, na.rm = T),
    pref_usage = sum(pref_usage_exports, na.rm = T)
  ) %>%
  mutate(pur_pct = round(pref_usage / pref_elig_exp,3)*100) %>%
  select(year,pur_pct) 


exp.noneu.ts = exp %>%
  group_by(partner_iso, partner_name, year) %>%
  summarise(
    total_exports = sum(total_exports, na.rm = T),
    pref_elig_exp = sum(pref_eligible_exports, na.rm = T),
    pref_usage = sum(pref_usage_exports, na.rm = T)
  ) %>%
  mutate(across(c(total_exports:pref_usage), ~ round(.x/1000000,1))) %>%
  mutate(pur_pct = pref_usage / pref_elig_exp) %>%
  ungroup() %>%
  filter(partner_iso %in% exp.10.noneu$partner_iso)


exp.eu.ts = exp %>%
  group_by(partner_iso, partner_name, year) %>%
  summarise(
    total_exports = sum(total_exports, na.rm = T),
    pref_elig_exp = sum(pref_eligible_exports, na.rm = T),
    pref_usage = sum(pref_usage_exports, na.rm = T)
  ) %>%
  mutate(across(c(total_exports:pref_usage), ~ round(.x/1000000,1))) %>%
  mutate(pur_pct = pref_usage / pref_elig_exp) %>%
  ungroup() %>%
  filter(partner_iso %in% exp.10.eu$partner_iso)



toc()


# PUR drivers ----------


# What products/sectors drive the Utilisaiton
# i.e. what product has most elig trade
# therefore what high or low sector PUR drives the overall utilisaiton

# method:
#' - find top sectors by pref eligible
#' - group data by sector to calc prop. of each product (hs2) against overall sector
#' - calculate overall proportions each product
#' - filter data on top 3/5 - user choice
#' - so final data is usable in output table
#' - to summarise what products drive overall pref elig/usage
#' - i.e. Product X is 20% of overall pref eligible imports 80% of sector Y
#' - with a very high utilisation of 90% - hence this product drives the overall figure

#' in output table user can
#' - find top sectors
#' - what goods contribute largest pref elig/use %
#' - by their sector and overall
#' i.e. good X makes up 80% of overall sector eligibility 
#' and 10% of overall for all sectors

## imports -----------

# single year/country example

top_hs <- imp %>%
  filter(partner_iso == "TR", year == "2024") %>%
  select(partner_iso, partner_name, year, hs_section, hs_section_description, hs2, total_imports, pref_eligible_imports, pref_usage_imports) %>%
  group_by(hs_section, hs_section_description) %>%
  summarise(
    pref_elig = sum(pref_eligible_imports, na.rm = T),
    pref_use = sum(pref_usage_imports, na.rm = T)
    ) %>%
  mutate(pur_pct = pref_use / pref_elig) %>%
  arrange(desc(pref_elig)) %>%
  head(5)

total_pref_elig = imp %>%
  filter(partner_iso == "TR", year == "2024") %>%
  summarise(pref_elig = sum(pref_eligible_imports, na.rm = T)) %>%
  pull()

total_pref_use = imp %>%
  filter(partner_iso == "TR", year == "2024") %>%
  summarise(pref_use = sum(pref_usage_imports, na.rm = T)) %>%
  pull()


imp.util <- imp %>%
  filter(partner_iso == "TR", year == "2024") %>%
  select(partner_iso, partner_name, year, hs_section, hs_section_description, hs2, total_imports, pref_eligible_imports, pref_usage_imports) %>%
  group_by(hs_section, hs_section_description) %>%
  arrange(hs_section,desc(pref_eligible_imports)) %>%
  mutate(sector_pref_elig = sum(pref_eligible_imports, na.rm = T)) %>%
  mutate(sector_pref_elig_pc = pref_eligible_imports / sector_pref_elig) %>%
  mutate(sector_pref_use = sum(pref_usage_imports, na.rm = T)) %>%
  mutate(sector_pref_use_pc = pref_usage_imports / sector_pref_use) %>%
  ungroup() %>%
  mutate(total_pref_elig_pc = pref_eligible_imports / total_pref_elig) %>%
  mutate(total_pref_use_pc = pref_usage_imports / total_pref_use)
  
  
## exports -----------


top_hs.ex <- exp %>%
  filter(partner_iso == "TR", year == "2024") %>%
  select(partner_iso, partner_name, year, hs_section, hs_section_description, hs2, total_exports, pref_eligible_exports, pref_usage_exports) %>%
  group_by(hs_section, hs_section_description) %>%
  summarise(
    pref_elig = sum(pref_eligible_exports, na.rm = T),
    pref_use = sum(pref_usage_exports, na.rm = T)
  ) %>%
  mutate(pur_pct = pref_use / pref_elig) %>%
  arrange(desc(pref_elig)) %>%
  head(5)

total_pref_elig.ex = exp %>%
  filter(partner_iso == "TR", year == "2024") %>%
  summarise(pref_elig = sum(pref_eligible_exports, na.rm = T)) %>%
  pull()

total_pref_use.ex = exp %>%
  filter(partner_iso == "TR", year == "2024") %>%
  summarise(pref_use = sum(pref_usage_exports, na.rm = T)) %>%
  pull()


exp.util <- exp %>%
  filter(partner_iso == "TR", year == "2024") %>%
  select(partner_iso, partner_name, year, hs_section, hs_section_description, hs2, total_exports, pref_eligible_exports, pref_usage_exports) %>%
  group_by(hs_section, hs_section_description) %>%
  arrange(hs_section,desc(pref_eligible_exports)) %>%
  mutate(sector_pref_elig = sum(pref_eligible_exports, na.rm = T)) %>%
  mutate(sector_pref_elig_pc = pref_eligible_exports / sector_pref_elig) %>%
  mutate(sector_pref_use = sum(pref_usage_exports, na.rm = T)) %>%
  mutate(sector_pref_use_pc = pref_usage_exports / sector_pref_use) %>%
  ungroup() %>%
  mutate(total_pref_elig_pc = pref_eligible_exports / total_pref_elig) %>%
  mutate(total_pref_use_pc = pref_usage_exports / total_pref_use)