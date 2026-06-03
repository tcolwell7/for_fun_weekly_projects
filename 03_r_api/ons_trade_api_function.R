
library(httr)
library(jsonlite)
library(tidyverse)


#' get ONS trade data API script
#' functionised way to call ONS API
#' to pull in trade data based on user request
#' basic functionisation
#' however with a practical use
#' if this work was being developed to create
#' bacn-end functions for an application/tool
#' 


# TESTING APIs -----------

# re-testing APIs, reading outputs and figuring out how to approach this

# First call: check API dimensions - to then check value inputs. 

# dimensions
url <- "https://api.beta.ons.gov.uk/v1/datasets/trade/editions/time-series/versions/65"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
dims <- jsonlite::fromJSON(txt, flatten = TRUE)

dims$dimensions

# test dimension code list:
## SITC code inputs

url <- "https://api.beta.ons.gov.uk/v1/code-lists/sitc/editions/one-off/codes"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
codes <- jsonlite::fromJSON(txt, flatten = TRUE)
codes

# output: code list of letters and values ranging from 0-99, 


# test geography dim:
url <- "http://api.beta.ons.gov.uk/v1/code-lists/uk-only/editions/one-off/codes/K02000001"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
uk <- jsonlite::fromJSON(txt, flatten = TRUE)
uk

#output: is single England/UK code k02...


# test time:
url <- "http://api.beta.ons.gov.uk/v1/code-lists/mmm-yy/editions/one-off/codes"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
time <- jsonlite::fromJSON(txt, flatten = TRUE)
time

# output: large time-series dating back to Dec-49... ending Dec-25. 

# test country dim: 

url <- "http://api.beta.ons.gov.uk/v1/code-lists/countries-and-territories/editions/one-off/codes"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
countries <- jsonlite::fromJSON(txt, flatten = TRUE)
countries$items$code

# output: list of value codes to use in API, however provides words such as "africa" etc which don't return data. 

# test flow dim:
url <- "https://api.beta.ons.gov.uk/v1/code-lists/trade-direction/editions/one-off/codes"
res <- httr::GET(url)
txt <- httr::content(res, "text", encoding = "UTF-8")
flow <- jsonlite::fromJSON(txt, flatten = TRUE)
flow

# output: flow = import or export denoted by IM or EX. 

url <- "https://api.beta.ons.gov.uk/v1/datasets/trade/editions/time-series/versions/65/observations"

res <- httr::GET(url, query = list(
  time = "Jan-23",
  geography = "K02000001",
  standardindustrialtradeclassification = "T",
  countriesandterritories = "FR",
  direction = "EX"
))

obs

txt <- httr::content(res, "text", encoding = "UTF-8")
obs <- jsonlite::fromJSON(txt, flatten = TRUE)
obs$observations


# BUILD API FUNCTIONS: ---------

## GET DIMENSIONS

# dynamically call API to brining in metadata/dim values for futurue use


# Function: ons_get_codes()
# Purpose:  Fetch valid codes for any ONS API dimension
# Inputs:   dimension_id (string) - e.g. "sitc", "mmm-yy"
# Output:   A tidy tibble of codes and labels
# 

ons_get_codes <- function(dimension_id) {
  
  # Base URL for all code lists
  base_url <- "https://api.beta.ons.gov.uk/v1/code-lists"
  
  # Construct the URL for the first page
  url <- paste0(base_url, "/", dimension_id, "/editions/one-off/codes")
  
  # First request (to get limit + total_count)
  res <- httr::GET(url)
  
  if (httr::status_code(res) != 200) {
    stop(paste0("API request failed. Check dimension_id: '", dimension_id, "'"))
  }
  
  # Parse JSON
  txt <- httr::content(res, "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(txt, flatten = TRUE)
  
  # Extract pagination info
  total <- json$total_count
  limit <- json$limit
  
  # Calculate all offsets needed
  offsets <- seq(0, total - 1, by = limit)
  
  # Function to fetch a single page
  fetch_page <- function(off) {
    page_url <- paste0(url, "?offset=", off)
    res <- httr::GET(page_url)
    txt <- httr::content(res, "text", encoding = "UTF-8")
    jsonlite::fromJSON(txt, flatten = TRUE)$items
  }
  
  # Fetch all pages and combine
  all_items <- purrr::map_df(offsets, fetch_page)
  
  # Return clean tibble
  clean <- all_items %>%
    dplyr::select(code, label)
  
  return(clean)
}


#explore dim dfs
dim_sitc = ons_get_codes("sitc")
dim_flow = ons_get_codes("trade-direction")
dim_time = ons_get_codes("mmm-yy") # note dates are from 00s to 99 which are future dates. 
dim_geo = ons_get_codes("uk-only")
dim_country = ons_get_codes("countries-and-territories")


# CREATE CALL FUNCTION: --------


# Function: ons_get_trade()
# Purpose:  Retrieve ONS trade observations for any combination
# Inputs:   time, geography, sitc, country, flow (all strings or vectors)
# Output:   A tidy tibble of all observations
# Notes:    Includes progress bar + timing


ons_get_trade <- function(time,
                          geography = "K02000001",
                          sitc,
                          country,
                          flow) {
  
  # Load required packages
  require(httr)
  require(jsonlite)
  require(tidyverse)
  
  # Start timer
  start_time <- Sys.time()
  
  # Base URL for observations
  base_url <- "https://api.beta.ons.gov.uk/v1/datasets/trade/editions/time-series/versions/65/observations"
  
  # Create all combinations of inputs
  # Creates df from all vector/character inputs
  
  combos <- expand.grid(
    time = time,
    geography = geography,
    sitc = sitc,
    country = country,
    flow = flow,
    stringsAsFactors = FALSE
  )
  
  # Number of API calls required
  n_calls <- nrow(combos)
  
  # Progress bar
  pb <- txtProgressBar(min = 0, max = n_calls, style = 3)
  
  # Storage for results
  results <- list()
  
  # Loop through each combination
  for (i in seq_len(n_calls)) {
    
    # Extract row i
    row <- combos[i, ]
    
    # Build query list
    query_list <- list(
      time = row$time,
      geography = row$geography,
      standardindustrialtradeclassification = row$sitc,
      countriesandterritories = row$country,
      direction = row$flow
    )
    
    # Make API request
    res <- httr::GET(base_url, query = query_list)
    
    # Parse JSON
    txt <- httr::content(res, "text", encoding = "UTF-8")
    json <- jsonlite::fromJSON(txt, flatten = TRUE)
    
    # Extract observation value
    value <- json$observations$observation
    
    # Store tidy row
    results[[i]] <- tibble(
      time = row$time,
      geography = row$geography,
      sitc = row$sitc,
      country = row$country,
      flow = row$flow,
      value = value
    )
    
    # Update progress bar
    setTxtProgressBar(pb, i)
  }
  
  # Close progress bar
  close(pb)
  
  # End timer
  end_time <- Sys.time()
  duration <- round(as.numeric(end_time - start_time, units = "secs"), 2)
  
  message("Completed ", n_calls, " API calls in ", duration, " seconds.")
  
  # Combine all results into one tibble
  final <- bind_rows(results)
  
  return(final)
}

# testing function: 
ons_get_trade(
  time = "Jan-22",
  sitc = c("T","1"),
  country = "FR",
  flow = "EX"
)

ons_get_trade(
  time = c("Jan-22", "Feb-22", "Mar-22"),
  sitc = "T",
  country = "FR",
  flow = "EX"
)

ons_get_trade(
  time = c("Jan-22", "Feb-22", "Mar-22", "Apr-22"),
  sitc = "T",
  country = "W1",
  flow = "IM"
)

# ADDITIONAL FUNCTIONS: ----------

# 
# Function: ons_get_trade_year()
# Purpose:  Retrieve a full year (or multiple years) of monthly
#           ONS trade data using ons_get_trade()
# Inputs:   years (numeric or vector), sitc, country, flow
# Output:   A tidy tibble of all months for all years
# Notes:    Includes progress bar + timing
# -


ons_get_trade_year <- function(years,
                               sitc,
                               country,
                               flow,
                               geography = "K02000001") {
  
  # Load required packages
  require(tidyverse)
  
  # Start timer
  start_time <- Sys.time()
  
  # Convert numeric years into monthly codes like "Jan-22"
  month_codes <- function(y) {
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    paste0(months, "-", substr(y, 3, 4))
  }
  
  # Generate all month codes for all years
  all_months <- unlist(lapply(years, month_codes))
  
  message("Fetching ", length(all_months), " months across ", length(years), " year(s).")
  
  # Call your existing ons_get_trade() function
  results <- ons_get_trade(
    time = all_months,
    geography = geography,
    sitc = sitc,
    country = country,
    flow = flow
  )
  
  # End timer
  end_time <- Sys.time()
  duration <- round(as.numeric(end_time - start_time, units = "secs"), 2)
  
  message("Completed yearly retrieval in ", duration, " seconds.")
  
  return(results)
  
}


# test function:
ons_get_trade_year(
  years = 2022,
  sitc = "T",
  country = "W1",
  flow = "IM"
)


ons_get_trade_year(
  years = 2019:2022,
  sitc = "T",
  country = "FI",
  flow = "IM"
)



# -
# Function: ons_get_country_sitc_year()
# Purpose:  Retrieve ALL SITC codes for ALL months in a year
#           for a given country + flow.
# Inputs:   years (numeric or vector), country, flow, geography
# Output:   A tidy tibble of SITC × month × year
# Notes:    Uses ons_get_trade() internally
# --

ons_get_country_sitc_year <- function(years,
                                      country,
                                      flow,
                                      geography = "K02000001") {
  
  require(tidyverse)
  
  # Start timer
  start_time <- Sys.time()
  
  # 1. Get ALL SITC codes from your dimension function
  sitc_codes <- ons_get_codes("sitc")$code
  
  # 2. Helper to convert a year into 12 monthly codes
  month_codes <- function(y) {
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    paste0(months, "-", substr(y, 3, 4))
  }
  
  # 3. Generate all months for all years
  all_months <- unlist(lapply(years, month_codes))
  
  message("Preparing to fetch ",
          length(all_months), " months × ",
          length(sitc_codes), " SITC codes = ",
          length(all_months) * length(sitc_codes),
          " API calls.")
  
  # 4. Call existing engine
  results <- ons_get_trade(
    time = all_months,
    geography = geography,
    sitc = sitc_codes,
    country = country,
    flow = flow
  )
  
  # 5. Add year column for convenience
  results <- results %>%
    mutate(year = paste0("20", substr(time, 5, 6)))
  
  # End timer
  end_time <- Sys.time()
  duration <- round(as.numeric(end_time - start_time, units = "mins"), 2)
  
  message("Completed full SITC-year retrieval in ", duration, " minutes.")
  
  return(results)
  
}

fi <- ons_get_country_sitc_year(
  years = 2022,
  country = "FI",
  flow = "EX"
)

# TAKES ALONG TIME TO RUN.... so need to re-think

