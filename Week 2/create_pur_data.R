# create PUR data helper script
#' simple helper script to load in, transform and disseminate PUR data
#' User inputs their country, year and top ranking to 
#' produce series of output tables
#' formatted to enable quick and easier
#' analysis for wider report writing, presentations etc. 
#' 
#' 


# LOAD IN DATA & FUNCTIONS

source("functions.R")


# LOAD IN HIGH LEVEL PUR OUTPUTS (select year and top ranking countries)

pur_data_list = get_pur_high_level(.yr = 2023, .rank= 10)


# OPTIONAL: LOAD PUR DRIVERS (Country specific)

pur_drivers_list <- get_pur_drivers(.yr = 2023, .rank = 5, .partner_iso = "CA", .flow = "ex")


# COMBINE DATA:

all_data <- c(pur_data_list, pur_drivers_list)

# MAP DATA AND CREATE EXCEL:

purrr::imap(
  all_data,
  ~ create_excel_output(
    wb = wb,
    .data = .x,
    .name = .y
  )
)

saveWorkbook(wb, file = "outputs/excel_function_output.xlsx", overwrite = TRUE)
