
#' script to store API call functions of ONS trade data
#' 

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


# Function: ons_get_trade_furrr()
# Purpose:  Retrieve ONS trade observations for any combination - fast processing
# Inputs:   time, geography, sitc, country, flow (all strings or vectors)
# Output:   A tidy tibble of all observations
# Notes:    Includes progress bar + timing

ons_get_trade_furrr <- function(time,
                                geography = "K02000001",
                                sitc,
                                country,
                                flow,
                                workers = parallel::detectCores() - 1) {
  
  require(httr)
  require(jsonlite)
  require(tidyverse)
  require(furrr)
  
  # Start timer
  start_time <- Sys.time()
  
  # Create all combinations
  combos <- expand.grid(
    time = time,
    geography = geography,
    sitc = sitc,
    country = country,
    flow = flow,
    stringsAsFactors = FALSE
  )
  
  n_calls <- nrow(combos)
  message("Preparing ", n_calls, " API calls using ", workers, " workers.")
  
  # Set up parallel plan
  plan(multisession, workers = workers)
  
  # Parallel map with progress bar
  results <- furrr::future_pmap_dfr(
    combos,
    function(time, geography, sitc, country, flow) {
      
      query_list <- list(
        time = time,
        geography = geography,
        standardindustrialtradeclassification = sitc,
        countriesandterritories = country,
        direction = flow
      )
      
      res <- httr::GET(
        "https://api.beta.ons.gov.uk/v1/datasets/trade/editions/time-series/versions/65/observations",
        query = query_list
      )
      
      txt <- httr::content(res, "text", encoding = "UTF-8")
      json <- jsonlite::fromJSON(txt, flatten = TRUE)
      
      tibble(
        time = time,
        geography = geography,
        sitc = sitc,
        country = country,
        flow = flow,
        value = json$observations$observation
      )
    },
    .progress = TRUE
  )
  
  # End timer
  duration <- round(as.numeric(Sys.time() - start_time, units = "secs"), 2)
  message("Completed ", n_calls, " parallel API calls in ", duration, " seconds.")
  
  return(results)
}


# Function: ons_get_trade_year()
# Purpose:  Retrieve a full year (or multiple years) of monthly
#           ONS trade data using ons_get_trade()
# Inputs:   years (numeric or vector), sitc, country, flow
# Output:   A tidy tibble of all months for all years
# Notes:    Includes progress bar + timing


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
  results <- ons_get_trade_furrr(
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
