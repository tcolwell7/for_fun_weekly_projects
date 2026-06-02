library(httr)
library(jsonlite)
library(tidyverse)


# ALL DATASETS API ----------

# custom function to call and extract all dataset metadata from api
get_all_datasets <- function() {
  url <- "https://api.beta.ons.gov.uk/v1/datasets"
  offset <- 0
  limit <- 100
  
  all_items <- list()
  
  repeat {
    res <- httr::GET(url, query = list(offset = offset, limit = limit))
    txt <- httr::content(res, "text", encoding = "UTF-8")
    dat <- jsonlite::fromJSON(txt, flatten = TRUE)
    
    # Append the WHOLE data frame, not each row
    all_items <- append(all_items, list(dat$items))
    
    if (offset + limit >= dat$total_count) break
    offset <- offset + limit
  }
  
  # Now bind_rows works because every element is a data frame
  bind_rows(all_items)
}

all_datasets <- get_all_datasets()

all_datasets$id


# WEEKLY DEATH API TEST ----------

# test dataset API and inspection:
url <- "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex"

res <- httr::GET(url)
txt <- httr::content(res, "text")
dat2 <- jsonlite::fromJSON(txt, flatten = TRUE)
names(dat2)

# get's dataset latest version link
dat2$links$latest_version


# dimensions
url <- "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex/editions/2026/versions/16/dimensions"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
dims <- jsonlite::fromJSON(txt, flatten = TRUE)

dims$items

# test dimension tables:
url <- "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex/editions/2026/versions/16/dimensions/agegroups/options"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
age <- jsonlite::fromJSON(txt, flatten = TRUE)

age$items

url <- "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex/editions/2026/versions/16/dimensions/geography/options"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
geo <- jsonlite::fromJSON(txt, flatten = TRUE)

geo$items


url <- "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex/editions/2026/versions/16/dimensions/sex/options"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
age <- jsonlite::fromJSON(txt, flatten = TRUE)

age$items

url <- "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex/editions/2026/versions/16/dimensions/week/options"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
week <- jsonlite::fromJSON(txt, flatten = TRUE)

week$items


# test out API fully:

url <- "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex/editions/2026/versions/16/observations"

res <- GET(url, query = list(
  agegroups = "65-69",
  geography = "E92000001",
  registrationoroccurrence = "occurrence",
  sex = "male",
  time = "2026",
  week = "week-1"
))

txt <- content(res, "text", encoding = "UTF-8")
dat <- fromJSON(txt, flatten = TRUE)

dat


# count how many observations
url <- "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex/editions/2026/versions/16/observations"

res <- GET(url, query = list(limit = 10000))
txt <- content(res, "text", encoding = "UTF-8")
all <- fromJSON(txt, flatten = TRUE)

str(all$observations)




test_results <- map_df(age$items$label, function(a) {
  res <- GET(
    "https://api.beta.ons.gov.uk/v1/datasets/weekly-deaths-age-sex/editions/2026/versions/16/observations",
    query = list(
      agegroups = a,
      geography = "E92000001",
      registrationoroccurrence = "occurrence",
      sex = "male",
      time = "2024",
      week = "week-10"
    )
  )
  
  txt <- content(res, "text", encoding = "UTF-8")
  
  # Try to parse JSON; if it fails, return NA
  out <- tryCatch(fromJSON(txt, flatten = TRUE), error = function(e) NULL)
  
  tibble(
    age = a,
    value = out$observations$observation %||% NA
  )
})

test_results

#' - End: API dataset returned 0 observations so moving onto another ............
#' 
#' 


# TRADE API ---------

url <- "https://api.beta.ons.gov.uk/v1/datasets/trade"
res <- httr::GET(url)
txt <- httr::content(res, "text")
trade <- jsonlite::fromJSON(txt, flatten = TRUE)
names(trade)

# get's dataset latest version link
trade$links$latest_version


# dimensions
url <- "https://api.beta.ons.gov.uk/v1/datasets/trade/editions/time-series/versions/65"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
dims <- jsonlite::fromJSON(txt, flatten = TRUE)

dims$dimensions

# test dimension code list:
url <- "https://api.beta.ons.gov.uk/v1/code-lists/sitc"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
sitc <- jsonlite::fromJSON(txt, flatten = TRUE)
sitc


url <- "https://api.beta.ons.gov.uk/v1/code-lists/sitc/editions"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
sitc_edit <- jsonlite::fromJSON(txt, flatten = TRUE)
sitc_edit

url <- "https://api.beta.ons.gov.uk/v1/code-lists/sitc/editions/one-off/codes?offset=40"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
codes <- jsonlite::fromJSON(txt, flatten = TRUE)
codes

url <- "http://api.beta.ons.gov.uk/v1/code-lists/sitc/editions/one-off/codes/42/datasets"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
datasets <- jsonlite::fromJSON(txt, flatten = TRUE)
datasets


# test geography dim:
url <- "http://api.beta.ons.gov.uk/v1/code-lists/uk-only/editions/one-off/codes/K02000001"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
uk <- jsonlite::fromJSON(txt, flatten = TRUE)
uk

#


# test time:
url <- "http://api.beta.ons.gov.uk/v1/code-lists/mmm-yy/editions/one-off/codes"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
time <- jsonlite::fromJSON(txt, flatten = TRUE)
time


# test country dim:

url <- "http://api.beta.ons.gov.uk/v1/code-lists/countries-and-territories/editions/one-off/codes"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
countries <- jsonlite::fromJSON(txt, flatten = TRUE)
countries$items$code


# test flow dim:
url <- "https://api.beta.ons.gov.uk/v1/code-lists/trade-direction/editions/one-off/codes"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
flow <- jsonlite::fromJSON(txt, flatten = TRUE)
flow




url <- "https://api.beta.ons.gov.uk/v1/datasets/trade/editions/time-series/versions/65/observations"

res <- httr::GET(url, query = list(
  time = "Jan-22",
  geography = "K02000001",
  standardindustrialtradeclassification = "T",
  countriesandterritories = "W1",
  direction = "EX"
))

txt <- httr::content(res, "text", encoding = "UTF-8")
obs <- jsonlite::fromJSON(txt, flatten = TRUE)
obs$observations

End -- we have a working API and not an empty one like the weekly deaths!