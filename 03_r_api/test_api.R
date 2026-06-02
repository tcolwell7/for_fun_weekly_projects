
library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)


# testing UK prison data APIs to scrape data. 
# basic use of APIs to obtain data for wider analysis
# API testing for fun..



# 1. Derbyshire police force neighborhood codes

url <- "https://data.police.uk/api/derbyshire/neighbourhoods"
res <- GET(url)
raw_json <- content(res, "text")
dat <- fromJSON(raw_json, flatten = TRUE)
str(dat)


# 2. Test boundary coordinate API
url <- "https://data.police.uk/api/derbyshire/ND06/boundary"
res <- GET(url)
raw_json <- content(res, "text")
dat2 <- fromJSON(raw_json, flatten = TRUE)


# 3. Test police data for coordinates
url <- "https://data.police.uk/api/crimes-at-location?date=2024-04&lat=53.23634647&lng=-1.746974588"
res <- GET(url)
raw_json <- content(res, "text")
dat3 <- fromJSON(raw_json, flatten = TRUE)



# Functionised way to iterate through data
# to extract full areas crime data


# 1. Get all neighbourhood codes for Derbyshire
get_neighbourhood_codes <- function(force = "derbyshire") {
  url <- paste0("https://data.police.uk/api/", force, "/neighbourhoods")
  res <- GET(url)
  txt <- content(res, "text", encoding = "UTF-8")
  fromJSON(txt, flatten = TRUE)
}

neigh_codes <- get_neighbourhood_codes("derbyshire")
neigh_codes
 

# 2. Get boundary polygon for a single neighbourhood
get_boundary <- function(force = "derbyshire", neigh_id) {
  url <- paste0("https://data.police.uk/api/", force, "/", neigh_id, "/boundary")
  res <- GET(url)
  txt <- content(res, "text", encoding = "UTF-8")
  fromJSON(txt, flatten = TRUE)
}

# Get boundaries for ALL neighbourhoods
all_boundaries <- map(neigh_codes$id, ~ get_boundary("derbyshire", .x))

# Name the list for clarity
names(all_boundaries) <- neigh_codes$id


valid_idx <- sapply(all_boundaries, function(x) is.data.frame(x))

valid_boundaries <- all_boundaries[valid_idx]
valid_names      <- names(all_boundaries)[valid_idx]

coords_df <- bind_rows(
  map2(valid_boundaries, valid_names, ~ mutate(.x, neighbourhood = .y))
) %>%
  left_join(neigh_codes, by = c("neighbourhood" =  "id"))


# 3. Get crimes at a single coordinate
get_crimes_at_coord <- function(lat, lng, date = "2024-04") {
  url <- "https://data.police.uk/api/crimes-at-location"
  res <- GET(url, query = list(lat = lat, lng = lng, date = date))
  
  txt <- content(res, "text", encoding = "UTF-8")
  if (txt == "" || txt == "[]") return(NULL)
  
  fromJSON(txt, flatten = TRUE)
}

# Loop over ALL coordinates
crime_list <- pmap(
  coords_df[, c("latitude", "longitude")],
  ~ get_crimes_at_coord(..1, ..2, date = "2024-04")
)


#' After some testing...
#' I realise this approach is wrong and 
#' The API isn't designed for this type of request
#' So re-writing the functions to be
#' driven by one area at a time
#' Then test to see if you can easily attract a year of data
#' 
#' 



get_boundary <- function(force = "derbyshire", neigh_id) {
  
  # Build the URL for the boundary endpoint
  url <- paste0("https://data.police.uk/api/", force, "/", neigh_id, "/boundary")
  
  # Send the GET request
  res <- GET(url)
  
  # Extract the raw JSON text
  txt <- content(res, "text", encoding = "UTF-8")
  
  # Convert JSON → R data frame
  fromJSON(txt, flatten = TRUE)
}

# Fetch the boundary for ND04
boundary <- get_boundary("derbyshire", "SN02")

# Inspect the first few rows
head(boundary)


# Convert latitude/longitude to numeric and compute the centroid
lat <- mean(as.numeric(boundary$latitude))
lng <- mean(as.numeric(boundary$longitude))

lat
lng


# FUNCTION: get_crimes_month()
# Purpose: Fetch crime data for a single month at a coordinate
# Inputs:
#   - lat, lng: coordinate inside the neighbourhood
#   - date: "YYYY-MM"
# Output:
#   - A data frame of crimes for that month (or NULL if empty)
# --

get_crimes_month <- function(lat, lng, date) {
  
  # API endpoint for crimes at a location
  url <- "https://data.police.uk/api/crimes-at-location"
  
  # Send GET request with query parameters
  res <- GET(url, query = list(lat = lat, lng = lng, date = date))
  
  # Extract JSON text
  txt <- content(res, "text", encoding = "UTF-8")
  
  # If API returns empty string or empty array, return NULL
  if (txt == "" || txt == "[]") return(NULL)
  
  # Convert JSON → R data frame
  dat <- fromJSON(txt, flatten = TRUE)
  
  # Add the month as a column (useful later)
  dat$month <- date
  
  dat
}


# Create a vector of month strings: "2024-01", "2024-02", ..., "2024-12"
dates <- sprintf("2024-%02d", 1:6)

dates


# -- --
# Loop over each month and fetch crime data
# map() applies the function to each element of 'dates'
# -

crime_list <- map(dates, ~ get_crimes_month(lat, lng, .x))


# Combine all monthly data frames into one
crime_df <- bind_rows(crime_list)

# Remove duplicate crimes (same crime ID)
crime_df <- distinct(crime_df, id, .keep_all = TRUE)

# Inspect the final dataset
crime_df


# Lesson learnt: API not good for larger data requests

# End. 
