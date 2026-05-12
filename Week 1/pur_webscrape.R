
# 0. set up and load data ----------

# I write at the beginning of every script:
rm(list = ls()) # remove items from global environment

options(scipen=999) # turn off scientific numerical notation

`%notin%` <- Negate(`%in%`) # custom function for filtering data

# Specify packages
packages <-
  c("tidyverse","janitor","openxlsx","stringr","data.table",# general data wrangling
    "readxl","readr","openxlsx", # reading/writing excel
    "rvest"# r web-scraping
    ) 

# Install packages if not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
sapply(packages, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#' The initial data processing problem is straight forward
#' Each database file exists on an individual year link
#' which are all stored on a single page
#' The goal of this processing file is using rvest
#' create a single processing script to web-scrape the necessary links
#' to download and save all the database files
#' Seperately these can be processed and cleaned before analysis. 
#' 
#' 

# 1. scrape web-pages ----------

main_url <- "https://www.gov.uk/government/collections/preference-utilisation-of-uk-trade-in-goods"
main_page <- read_html(main_url) # scrape HTML into xml doc 

# web-links live under the tags a and href
# strip out duplicates and identify web-string pattern
year_links <- main_page %>%
  html_nodes("a") %>% 
  html_attr("href") %>%
  unique() %>%
  str_subset("preference-utilisation-of-uk-trade-in-goods-20")

# "/government/statistics/preference-utilisation-of-uk-trade-in-goods-2024"   
# which needs combining with httpps full link:

year_links <- paste0("https://www.gov.uk", year_links)

# Pick ONE year (example: 2022)
year_url <- year_links[str_detect(year_links, "2022")][1]
year_url



# 2. load yearly page

year_page <- read_html(year_url)

# -----------------------------
# 4. Extract dataset file links
# -----------------------------
file_links <- year_page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  unique() %>%
  str_subset(".ods") %>%   # keep only data files
  str_subset("database")  # keep database files only


# Download files
downloaded <- map_chr(file_links, function(link) {
  dest <- file.path("data", basename(link))
  download.file(link, dest, mode = "wb")
  dest
})





