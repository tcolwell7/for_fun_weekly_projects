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


#' data processing script to create individual daatsets
#' for imports and exports data
#' to be later used for analysis 
#' 
#' general step is automate uploading data
#' into environment
#' basic checks, cleaning and transformations to combine 
#' all year data together
#' 
#' Slight amendments to data columns due to naming convetions
#' and formats which required uniformity in order to create single df
#' 
#' Rather than update raw files manually
#' I would prefer to have this automated so 
#' when data is updated, they can just be saved, and code runs automatically
#' 
#' 


# load data -----

tic()

data_path <- "data_raw/" 
import_files <- list.files(data_path, pattern = "import", full.names = TRUE, ignore.case = TRUE)
export_files <- list.files(data_path, pattern = "export", full.names = TRUE, ignore.case = TRUE)


#' read_ods_clean function
#' reads in raw data, compiles, cleans col names, data types
#' auto checks for chnages in expected column names
#' incase of new data fields/changes to naming conventions


read_ods_clean <- function(file_path) {
  
  # Step 1: Read the entire sheet without skipping rows
  raw <- readODS::read_ods(
    path  = file_path,
    sheet = "Table_1"
  )
  
  # Step 2: Find the first row that contains real data
  # We define "real data" as: at least one non-NA value
  
  # Step 2: Remove rows that are completely NA
  # This handles files with blank rows above the header
  raw <- raw %>%
    filter(!if_all(everything(), is.na))
  
  header_row <- raw %>%
    filter(!if_all(everything(), is.na)) %>%   # filter if all rows = NA
    mutate(row_id = row_number()) %>%                     # keep row numbers
    filter(if_any(everything(), ~ . == "Year")) %>%       # find row containing "Year"
    slice(1) %>%                                           # take the first match
    pull(row_id)
  
  # Step 3: Slice the data from that row onward
  df <- raw %>%
    slice(header_row:n()) %>%                  # keep only real data rows
    janitor::row_to_names(1) %>%
    janitor::clean_names() %>%   
    rename_with(
   
      ~ str_replace(
        .,  
        # REGEX pattern:
        # (gsp|dcts|dtcs)   → capture the scheme prefix (group 1)
        # .*?               → allow ANY junk in the middle (non-greedy)
        # (zero|non_zero)   → capture the zero flag (group 2)
        # _imports          → literal suffix
        "(gsp|dcts).*?(zero|non_zero)_imports",
        
        # Replacement pattern:
        # gsp_              → force scheme prefix to "gsp"
        # \\2               → insert the captured zero flag (zero / non_zero)
        # _imports          → keep the suffix
        "gsp_\\2_imports"
      ),
      
      # This selects WHICH columns to rename:
      # any column whose name contains:
      #   - gsp OR dcts OR dtcs
      #   - followed somewhere by zero_imports or non_zero_imports
      matches("(gsp|dcts).*?(zero|non_zero)_imports", ignore.case = TRUE)
    ) %>%
    #check_schema(expected_cols, file_path) %>%
    mutate(
      source_file = basename(file_path)           # store filename
    )
  

  
  # Detect file type for colnames for testing
  file_type <- if (str_detect(tolower(file_path), "import")) {
    "import"
  } else if (str_detect(tolower(file_path), "export")) {
    "export"
  } else {
    stop("Cannot determine file type (import/export) from filename: ", file_path)
  }
  
  # Define schemas
  expected_import_cols <- c("partner_iso","partner_name",                      
                            "agreement","year",                                 
                            "hs2","hs2_description",                     
                            "hs_section","hs_section_description",             
                            "agri_flag","total_imports",                   
                            "total_imports_excl_special_processing" ,
                            "mfn_zero_imports",                   
                            "mfn_non_zero_imports",                 
                            "gsp_zero_imports",                   
                            "gsp_non_zero_imports" ,                 
                            "fta_zero_imports",                   
                            "fta_non_zero_imports" ,                 
                            "special_processing_imports"  ,      
                            "unknown_imports"  ,  "pref_eligible_imports" ,              
                            "pref_usage_imports"  , "pur_percent" ,                      
                            "tariff_free_percent")
  
  expected_export_cols <- c("partner_iso" ,"partner_name",                         
                            "agreement" , "year" ,                                
                            "hs2" , "hs2_description" ,                     
                            "hs_section","hs_section_description"  ,             
                            "agri_flag" , "total_exports" ,                     
                            "total_exports_excl_special_processing", "mfn_zero_exports" ,                    
                            "mfn_non_zero_exports", "fta_zero_exports" ,  
                            "fta_non_zero_exports"  , "special_processing_exports" ,    
                            "unknown_exports" , "pref_eligible_exports" ,           
                            "pref_usage_exports"  , "pur_percent" ,                       
                            "tariff_free_percent")
  
  # Select correct schema
  expected_cols <- if (file_type == "import") {
    expected_import_cols
  } else {
    expected_export_cols
  }
  
  # Identify missing and extra columns
  actual_cols <- names(df)
  missing_cols <- setdiff(expected_cols, actual_cols)
  extra_cols   <- setdiff(actual_cols, expected_cols)
  
  # warn about missing columns
  if (length(missing_cols) > 0) {
    warning(
      "Missing expected columns in ", basename(file_path), ": ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Add missing columns as NA
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  # Optional: warn about extra columns
  if (length(extra_cols) > 0) {
    warning(
      "Unexpected columns in ", basename(file_path), ": ",
      paste(extra_cols, collapse = ", ")
    )
  }
  
  # Reorder columns to match expected schema
  df <- df[, expected_cols]
  
  
  return(df)
    
}

# apply function to raw data files
# cleans column names so uniform to combine into single df
# export to excel for wider use

imports_data <- 
  map_df(import_files, read_ods_clean) %>%
  mutate(across(c(total_imports:tariff_free_percent), ~ as.numeric(.x))) %>%
  # mutate(across(matches("imports|exports|percent"), as.numeric)) %>%     # alt line if col ordering changes
  readr::write_csv("data_processed/pur_imports_data.csv")



exports_data <- 
  map_df(export_files, read_ods_clean) %>%
  mutate(across(c(total_exports:tariff_free_percent), ~ as.numeric(.x))) %>%
  readr::write_csv("data_processed/pur_exports_data.csv")



# high level sense checks:

import_year_summary <- imports_data %>%
  group_by(year) %>%
  summarise(
    rows = n(),                                   # number of rows for that year
    cols = ncol(imports_data),                    # total number of columns (same for all years)
    total_imports_sum = sum(total_imports, na.rm = TRUE),   # total imports for that year
    total_pref_elig_sum = sum(pref_eligible_imports, na.rm = TRUE), # sum of tariff-free %
    unique_partners = n_distinct(partner_iso),    # number of partner countries
    #missing_values = sum(is.na(across(everything()))), # total missing values in that year
    zero_import_rows = sum(total_imports == 0, na.rm = TRUE) # rows with zero imports
  ) %>%
  arrange(year)



export_year_summary <- exports_data %>%
  group_by(year) %>%
  summarise(
    rows = n(),                                   # number of rows for that year
    cols = ncol(exports_data),                    # total number of columns
    total_exports_sum = sum(total_exports, na.rm = TRUE),   # total exports for that year
    total_pref_elig_sum = sum(pref_eligible_exports, na.rm = TRUE),
    unique_partners = n_distinct(partner_iso),
    unique_hs2 = n_distinct(hs2),
    missing_values = sum(is.na(across(everything()))),
    zero_export_rows = sum(total_exports == 0, na.rm = TRUE)
  ) %>%
  arrange(year)


toc()

# End

