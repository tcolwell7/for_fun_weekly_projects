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

wb <- createWorkbook()

# function script
# functionise analysis script to make more reproduciable and reduce the need to write out multiple years. 

get_pur_high_level <- function(.yr, .rank){
  
  # two inuputs: year and rank of countries i.e. top 5, 10, 20 etc. 
  
  .yr = as.character(.yr) 
  
  imp.pur <- imp %>%
    filter(year == .yr) %>%
    summarise(
      total_imports = sum(total_imports, na.rm = T),
      pref_elig_imp = sum(pref_eligible_imports, na.rm = T),
      pref_usage = sum(pref_usage_imports, na.rm = T)
    ) %>%
    mutate(pur_pct = round(pref_usage / pref_elig_imp,3)*100) %>%
    select(pur_pct) %>%
    pull()
  
  
  
  imp.top.noneu <- imp %>%
    filter(partner_iso %notin% eu27.iso & agreement %notin% c("No FTA", "No Agreement")) %>%
    filter(year == .yr) %>%
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
    filter(rank <= .rank) %>%
    arrange(rank)
  
  
  imp.top.eu <- imp %>%
    filter(partner_iso %in% eu27.iso) %>%
    filter(year == .yr) %>%
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
    filter(rank <= .rank) %>%
    arrange(rank)
  
  
  
  
  ## exports ---
  
  
  exp.pur <- exp %>%
    filter(year == .yr) %>%
    summarise(
      total_exports = sum(total_exports, na.rm = T),
      pref_elig_exp = sum(pref_eligible_exports, na.rm = T),
      pref_usage = sum(pref_usage_exports, na.rm = T)
    ) %>%
    mutate(pur_pct = pref_usage / pref_elig_exp) %>%
    select(pur_pct) %>%
    pull()
  
  
 
  
  
  exp.top.noneu <- exp %>%
    filter(partner_iso %notin% eu27.iso) %>%
    filter(year == as.character(.yr)) %>%
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
    filter(rank <= .rank) %>%
    arrange(rank)
  
  
  exp.top.eu <- exp %>%
    filter(partner_iso %in% eu27.iso) %>%
    filter(year == .yr) %>%
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
    filter(rank <= .rank) %>%
    arrange(rank)
  
  
  # Pur overtime --
  ## imports --
  
  imp.pur.ts <- imp %>%
    group_by(year) %>%
    summarise(
      total_imports = sum(total_imports, na.rm = T),
      pref_elig_imp = sum(pref_eligible_imports, na.rm = T),
      pref_usage = sum(pref_usage_imports, na.rm = T)
    ) %>%
    mutate(pur_pct = pref_usage / pref_elig_imp) %>%
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
    filter(partner_iso %in% imp.top.noneu$partner_iso) # filter on top countries
  
  
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
    filter(partner_iso %in% imp.top.eu$partner_iso) # filter on top countries
  
  
  ## exports --
  
  
  exp.pur.ts <- exp %>%
    group_by(year) %>%
    summarise(
      total_exports = sum(total_exports, na.rm = T),
      pref_elig_exp = sum(pref_eligible_exports, na.rm = T),
      pref_usage = sum(pref_usage_exports, na.rm = T)
    ) %>%
    mutate(pur_pct = pref_usage / pref_elig_exp) %>%
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
    filter(partner_iso %in% exp.top.noneu$partner_iso)
  
  
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
    filter(partner_iso %in% exp.top.eu$partner_iso)
  
  # return a list of all dfs
  # renamed for wider use
  
  output_data <- list(
    
    #imp_pur = imp.pur,
    imp_top_eu = imp.top.eu,
    imp_top_noneu = imp.top.noneu,
    imp_pur_ts = imp.pur.ts,
    imp_top_eu_ts = imp.eu.ts,
    imp_top_noneu_ts = imp.noneu.ts,
    
    #exp_pur = exp.pur,
    exp_top_eu = exp.top.eu,
    exp_top_noneu = exp.top.noneu,
    exp_pur_ts = exp.pur.ts,
    exp_top_eu_ts = exp.eu.ts,
    exp_top_noneu_ts = exp.noneu.ts
    
  )
  
  
  
 
  
  
  return(output_data)
  
}



get_pur_drivers <- function(.yr, .rank, .partner_iso, .flow = NULL){
  
  .yr = as.character(.yr)
  
  if(is.null(.flow)){
  
  imp_filt = imp %>% filter(partner_iso == .partner_iso, year == .yr)
  
  top_hs <- imp_filt %>%
    select(partner_iso, partner_name, year, hs_section, hs_section_description, hs2, total_imports, pref_eligible_imports, pref_usage_imports) %>%
    group_by(hs_section, hs_section_description) %>%
    summarise(
      pref_elig = sum(pref_eligible_imports, na.rm = T),
      pref_use = sum(pref_usage_imports, na.rm = T)
    ) %>%
    mutate(pur_pct = pref_use / pref_elig) %>%
    arrange(desc(pref_elig)) %>%
    head(.rank)
  
  # create ordering for final df
  ordered_sections <- top_hs$hs_section
  
  
  total_pref_elig = imp_filt %>%
    summarise(pref_elig = sum(pref_eligible_imports, na.rm = T)) %>%
    pull()
  
  total_pref_use = imp_filt %>%
    summarise(pref_use = sum(pref_usage_imports, na.rm = T)) %>%
    pull()
  
  
  imp_filt <- imp_filt %>%
    select(partner_iso, partner_name, year, hs_section, hs_section_description, hs2, total_imports, pref_eligible_imports, pref_usage_imports) %>%
    filter(hs_section%in% top_hs$hs_section) %>%
    group_by(hs_section, hs_section_description) %>%
    arrange(hs_section,desc(pref_eligible_imports)) %>%
    mutate(sector_pref_elig = sum(pref_eligible_imports, na.rm = T)) %>%
    mutate(sector_pref_elig_pct = pref_eligible_imports / sector_pref_elig) %>%
    mutate(sector_pref_use = sum(pref_usage_imports, na.rm = T)) %>%
    mutate(sector_pref_use_pct = pref_usage_imports / sector_pref_use) %>%
    ungroup() %>%
    mutate(total_pref_elig_pct = pref_eligible_imports / total_pref_elig) %>%
    mutate(total_pref_use_pct = pref_usage_imports / total_pref_use) %>%
    mutate(hs_section = factor(hs_section, levels = ordered_sections)) %>%
    arrange(hs_section)
  
  output_data <- list(
    
    pur_drivers_hs2    = imp_filt,
    pur_drivers_top_hs = top_hs
    
  )
  
  
  } else{
    
    
    exp_filt = exp %>% filter(partner_iso == .partner_iso, year == .yr)
    
    top_hs <- exp_filt %>%
      select(partner_iso, partner_name, year, hs_section, hs_section_description, hs2, total_exports, pref_eligible_exports, pref_usage_exports) %>%
      group_by(hs_section, hs_section_description) %>%
      summarise(
        pref_elig = sum(pref_eligible_exports, na.rm = T),
        pref_use = sum(pref_usage_exports, na.rm = T)
      ) %>%
      mutate(pur_pct = pref_use / pref_elig) %>%
      arrange(desc(pref_elig)) %>%
      head(.rank)
    
    # create ordering for final df
    ordered_sections <- top_hs$hs_section
    
    
    total_pref_elig = exp_filt %>%
      summarise(pref_elig = sum(pref_eligible_exports, na.rm = T)) %>%
      pull()
    
    total_pref_use = exp_filt %>%
      summarise(pref_use = sum(pref_usage_exports, na.rm = T)) %>%
      pull()
    
    
    exp_filt <- exp_filt %>%
      select(partner_iso, partner_name, year, hs_section, hs_section_description, hs2, total_exports, pref_eligible_exports, pref_usage_exports) %>%
      filter(hs_section%in% top_hs$hs_section) %>%
      group_by(hs_section, hs_section_description) %>%
      arrange(hs_section,desc(pref_eligible_exports)) %>%
      mutate(sector_pref_elig = sum(pref_eligible_exports, na.rm = T)) %>%
      mutate(sector_pref_elig_pct = pref_eligible_exports / sector_pref_elig) %>%
      mutate(sector_pref_use = sum(pref_usage_exports, na.rm = T)) %>%
      mutate(sector_pref_use_pct = pref_usage_exports / sector_pref_use) %>%
      ungroup() %>%
      mutate(total_pref_elig_pct = pref_eligible_exports / total_pref_elig) %>%
      mutate(total_pref_use_pct = pref_usage_exports / total_pref_use) %>%
      mutate(hs_section = factor(hs_section, levels = ordered_sections)) %>%
      arrange(hs_section)
    
    output_data <- list(
      
      pur_drivers_hs2    = exp_filt,
      pur_drivers_top_hs = top_hs
      
    )
    
    
    
  }
  
  return(output_data)
   
}


#xx = get_pur_drivers(2021, 5, "DE", .flow = "Ex")

#wb <- createWorkbook()
create_excel_output <- function(wb, .data, .name, .title = NULL, .source_dsc = NULL){
  
  
  #wb <- createWorkbook()
  
  addWorksheet(wb, .name)
  
  #.data = exp.eu.ts
  
  rowNo <- nrow(.data)
  colNo <- ncol(.data)
  
  writeData(
    wb, 
    sheet = .name, 
    .data, 
    withFilter = TRUE,
    startRow = 2, 
    startCol = 1
  ) # set row to 2 to insert merged cell in row 1 for header title. 
  
  # 0. set column widths:
  setColWidths(wb, sheet = .name, cols = 1:colNo, width = 15)
  
  # 1. create border style:
  borderStyle_gov <- createStyle(
    border = "Bottom",
    borderColour = "#505A5F"
  )
  
  # 2. create headerStyle:
  headerStyle_gov <- createStyle(
    fontSize = 12,
    fontColour = "#0B0C0C",      # GOV.UK black
    fgFill = "#F3F2F1",          # GOV.UK light grey
    halign = "center",
    textDecoration = "bold",
    border = "Bottom",
    borderColour = "#FFDD00",    # GOV.UK yellow underline
    borderStyle = "thick",
    wrapText = TRUE
  )
  
  # 3. Add merged cell header: (one row merged across all columns 1:7). 
  # first write header title
  
  headerTitle <- paste0("Preference Utilisation data: ", .name)
  
  writeData(
    wb, 
    .name,
    headerTitle, 
    startCol = 1, 
    startRow = 1, 
    borders="surrounding", 
    borderColour = "black"
  )
  
  mergeCells(wb, .name, cols = 1:colNo, rows = 1)
  
  titleStyle_gov <- createStyle(
    fgFill = "#0B0C0C",     # GOV.UK black
    fontColour = "#FFFFFF",
    fontSize = 14,
    textDecoration = "bold",
    halign = "left"
  )
  
  
  numStyle_gov <- createStyle(
    numFmt = "#,##0",
    border = "Bottom",
    borderColour = "#505A5F"
  )
  
  
  # auto detect numeric cols for numStyle
  num_cols <- tidyselect::eval_select(rlang::expr(where(is.numeric)), .data)
  
  #  freezepane:
  
  freezePane(wb, .name, firstActiveRow = 3, firstActiveCol = 1)
  
  # Add styles:
  addStyle(wb, sheet = .name, borderStyle_gov, rows = 3:(rowNo+2), cols = 1:colNo, gridExpand = T)
  addStyle(wb, sheet = .name, headerStyle_gov, rows = 2, cols = 1:colNo)
  addStyle(wb, sheet = .name, numStyle_gov, rows = 3:(rowNo+2), cols = num_cols, gridExpand = T)
  addStyle(wb, sheet = .name, titleStyle_gov, rows = 1, cols = 1:colNo, gridExpand = T)
  
  # create and add percentage style
  
  pct_col <- tidyselect::eval_select(rlang::expr(contains("pct")), .data)
  
  pctStyle_gov <- createStyle(
    numFmt = "0.0%",
    border = "Bottom",
    borderColour = "#505A5F"
  )
  
  addStyle(
    wb,
    sheet = .name,
    style = pctStyle_gov,
    rows = 3:(rowNo+2),
    cols = pct_col,
    gridExpand = TRUE
  )
  
  
  # fix year col which gets converted if numeric format:
  yr_col <- tidyselect::eval_select(rlang::expr(contains("year")), .data)
  
  addStyle(
    wb,
    sheet = .name,
    style = borderStyle_gov,
    rows = 3:(rowNo+2),
    cols = yr_col,
    gridExpand = TRUE
  )
  
  
  # add source:
  
  # Determine where to place notes
  note_row <- rowNo + 3   # 2 blank rows after table
  
  writeData(
    wb, 
    sheet = .name,
    x = paste0("Source: ", headerTitle),
    startRow = note_row,
    startCol = 1
  )
  
  writeData(
    wb, 
    sheet = .name,
    x = "Note 2: [text to be confirmed]",
    startRow = note_row + 1,
    startCol = 1
  )
  
  writeData(
    wb, 
    sheet = .name,
    x = "Note 3: [text to be confirmed]",
    startRow = note_row + 2,
    startCol = 1
  )
  
  sourceStyle <- createStyle(
    fontSize = 9,
    fontColour = "#505A5F",
    halign = "left"
  )
  
  addStyle(
    wb, 
    sheet = .name,
    style = sourceStyle,
    rows = note_row:(note_row + 2),
    cols = 1,
    gridExpand = TRUE
  )
  
  
  
  #saveWorkbook(wb, file = "outputs/excel_function_output.xlsx", overwrite = TRUE)
  
  
  
}





# create_excel_output(.data = imp, .name = "data")
# 
# purrr::imap(
#   x,
#   ~ create_excel_output(
#     wb = wb,
#     .name = .y,
#     .data = .x
#     #title_desc = title_desc[[.y]],
#     #note_desc  = note_desc[[.y]]
#   )
# )