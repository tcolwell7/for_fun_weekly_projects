
#' Continuing on from my previous trade API script
#' This script is exploring creating 
#' new functions using parralel running of APIs 
#' to speed up calling the API to get required data
#' 



# ONS TRADE API TEST: ----------------


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

ons_get_trade(
  time = c("Jan-22", "Feb-22", "Mar-22", "Apr-22","May-22","Jun-22","Jul-22","Aug-22","Sep-22","Oct-22","Nov-22","Dec-22"),
  sitc = c("T"),
  country = "SE",
  flow = "EX"
)



# ---------------------------------------------------------------
# Parallel version of ons_get_trade() using future.apply
# ---------------------------------------------------------------

ons_get_trade_parallel <- function(time,
                                   geography = "K02000001",
                                   sitc,
                                   country,
                                   flow,
                                   workers = parallel::detectCores() - 1) {
  
  require(httr)
  require(jsonlite)
  require(tidyverse)
  require(future.apply)
  
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
  future::plan(multisession, workers = workers)
  
  # Parallel loop
  results <- future.apply::future_lapply(seq_len(n_calls), function(i) {
    
    row <- combos[i, ]
    
    query_list <- list(
      time = row$time,
      geography = row$geography,
      standardindustrialtradeclassification = row$sitc,
      countriesandterritories = row$country,
      direction = row$flow
    )
    
    res <- httr::GET(
      "https://api.beta.ons.gov.uk/v1/datasets/trade/editions/time-series/versions/65/observations",
      query = query_list
    )
    
    txt <- httr::content(res, "text", encoding = "UTF-8")
    json <- jsonlite::fromJSON(txt, flatten = TRUE)
    
    tibble(
      time = row$time,
      geography = row$geography,
      sitc = row$sitc,
      country = row$country,
      flow = row$flow,
      value = json$observations$observation
    )
  })
  
  # Combine results
  final <- bind_rows(results)
  
  # End timer
  duration <- round(as.numeric(Sys.time() - start_time, units = "secs"), 2)
  message("Completed ", n_calls, " parallel API calls in ", duration, " seconds.")
  
  return(final)
}


ons_get_trade_parallel(
  time = c("Jan-22", "Feb-22", "Mar-22", "Apr-22","May-22","Jun-22","Jul-22","Aug-22","Sep-22","Oct-22","Nov-22","Dec-22"),
  sitc = c("T"),
  country = "SE",
  flow = "EX"
)


# ---------------------------------------------------------------
# Parallel version of ons_get_trade() using furrr
# ---------------------------------------------------------------

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



ons_get_trade_furrr(
  time = c("Jan-22", "Feb-22", "Mar-22", "Apr-22","May-22","Jun-22","Jul-22","Aug-22","Sep-22","Oct-22","Nov-22","Dec-22"),
  sitc = c("T"),
  country = "SE",
  flow = "EX"
)

#' - furrr is the quickest, looks better and doens't cause an issue with my laptop asking for permissions...
#' 


# TEST PARRALEL RUNNING MULTI YEARS: -----------------



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


ons_get_trade_year(
  years = 2019:2022,
  sitc = "T",
  country = "SE",
  flow = "EX"
)

fi = ons_get_trade_year(
  years = 2023:2025,
  sitc = "T",
  country = "FI",
  flow = "IM"
)

ons_get_trade_year(
  years = 2025,
  sitc = "T",
  country = "FI",
  flow = "IM"
)
ons_get_trade_year(
  years = 2023,
  sitc = "T",
  country = "FI",
  flow = "IM"
)


years <- 2020:2024

finland <- purrr::map_dfr(years, ~ ons_get_trade_year(
  years = .x,
  sitc = "T",
  country = "FI",
  flow = "IM"
))



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
  results <- ons_get_trade_furrr(
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


ons_get_country_sitc_year(
  years = 2022,
  country = "FI",
  flow = "EX"
)


# End.