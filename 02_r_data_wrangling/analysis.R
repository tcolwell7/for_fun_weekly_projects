
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



imp.top.noneu <- imp %>%
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


imp.top.eu <- imp %>%
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


exp.top.eu <- exp %>%
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


  
exp.top.noneu <- exp %>%
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


exp.top.eu <- exp %>%
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

tic()
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

toc()

# Excel output ---------

## style 1 ----------


wb2 <- createWorkbook()


  addWorksheet(wb2, "name")
  
  data = exp
  
  rowNo <- nrow(data)
  colNo <- ncol(data)
  
  writeData(
    wb2, 
    sheet = "name", 
    data, 
    withFilter = TRUE,
    startRow = 2, 
    startCol = 1
    ) # set row to 2 to insert merged cell in row 1 for header title. 
  
  # 0. set column widths:
  setColWidths(wb2, sheet = "name", cols = 1:colNo, width = 15)
  
  # 1. create border style:
  borderStyle <- createStyle(border = "TopBottom", borderColour = "#4F81BD")
  
  # 2. create headerStyle:
  headerStyle <- 
    createStyle(
      fontSize = 12, 
      fontColour = "#FFFFFF", 
      halign = "center",
      fgFill = "#4F81BD", 
      border="TopBottom", 
      borderColour = "#4F81BD", 
      wrapText = TRUE, 
      textDecoration = "bold"
      )
  
  # 3. Add merged cell header: (one row merged across all columns 1:7). 
  # first write header title
  
  headerTitle <- paste0("Preference Utilisation data: ", "name")
  
  writeData(
    wb2, 
    "name",
    headerTitle, 
    startCol = 1, 
    startRow = 1, 
    borders="surrounding", 
    borderColour = "black"
    )
  
  mergeCells(wb2, "name", cols = 1:colNo, rows = 1)
  
  firstRow <- 
    createStyle(
      fontSize = 14, 
      halign = "center", 
      border = "TopBottomLeftRight", 
      textDecoration = "bold", 
      borderStyle = "thick"
      )
  
  numStyle <- 
    createStyle(
      numFmt = "#,##0",
      border = "TopBottom",
      borderColour = "#4F81BD"
      )
  
 
  
  #  freezepane:
  
  freezePane(wb2, "name", firstActiveRow = 3, firstActiveCol = 1)
  
  # Add styles:
  addStyle(wb2, sheet = "name", borderStyle, rows = 3:rowNo, cols = 1:colNo, gridExpand = T)
  addStyle(wb2, sheet = "name", headerStyle, rows = 2, cols = 1:colNo)
  addStyle(wb2, sheet = "name", numStyle, rows = 3:rowNo, cols = 6, gridExpand = T)
  addStyle(wb2, sheet = "name", firstRow, rows = 1, cols = 1:colNo, gridExpand = T)

  
  
  # add source:
  
  # Determine where to place notes
  note_row <- rowNo + 4   # 2 blank rows after table
  
  writeData(
    wb2, 
    sheet = "name",
    x = paste0("Source: ", headerTitle),
    startRow = note_row,
    startCol = 1
  )
  
  writeData(
    wb2, 
    sheet = "name",
    x = "Note 2: [text to be confirmed]",
    startRow = note_row + 1,
    startCol = 1
  )
  
  writeData(
    wb2, 
    sheet = "name",
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
    wb2, 
    sheet = "name",
    style = sourceStyle,
    rows = note_row:(note_row + 2),
    cols = 1,
    gridExpand = TRUE
  )
  


saveWorkbook(wb2, file = "outputs/test_excel.xlsx", overwrite = TRUE)


## style 2 ---------

wb2 <- createWorkbook()

addWorksheet(wb2, "name")

data = exp.eu.ts

rowNo <- nrow(data)
colNo <- ncol(data)

writeData(
  wb2, 
  sheet = "name", 
  data, 
  withFilter = TRUE,
  startRow = 2, 
  startCol = 1
) # set row to 2 to insert merged cell in row 1 for header title. 

# 0. set column widths:
setColWidths(wb2, sheet = "name", cols = 1:colNo, width = 15)

# 1. create border style:
borderStyle_minimal <- createStyle(
  border = "Bottom",
  borderColour = "#D9D9D9"
)

# 2. create headerStyle:
headerStyle_minimal <- createStyle(
  fontSize = 11,
  fontColour = "#000000",
  fgFill = "#E6E6E6",
  halign = "center",
  textDecoration = "bold",
  border = "Bottom",
  borderColour = "#BFBFBF"
)



# 3. Add merged cell header: (one row merged across all columns 1:7). 
# first write header title

headerTitle <- paste0("Preference Utilisation data: ", "name")

writeData(
  wb2, 
  "name",
  headerTitle, 
  startCol = 1, 
  startRow = 1, 
  borders="surrounding", 
  borderColour = "black"
)

mergeCells(wb2, "name", cols = 1:colNo, rows = 1)

firstRow <- 
  createStyle(
    fontSize = 14, 
    halign = "center", 
    border = "TopBottomLeftRight", 
    textDecoration = "bold", 
    borderStyle = "thick"
  )

numStyle_minimal <- createStyle(
  numFmt = "#,##0",
  border = "Bottom",
  borderColour = "#D9D9D9"
)



#  freezepane:

freezePane(wb2, "name", firstActiveRow = 3, firstActiveCol = 1)

# Add styles:
addStyle(wb2, sheet = "name", borderStyle_minimal, rows = 3:rowNo, cols = 1:colNo, gridExpand = T)
addStyle(wb2, sheet = "name", headerStyle_minimal, rows = 2, cols = 1:colNo)
addStyle(wb2, sheet = "name", numStyle_minimal, rows = 3:rowNo, cols = 3:colNo, gridExpand = T)
addStyle(wb2, sheet = "name", firstRow, rows = 1, cols = 1:colNo, gridExpand = T)



# add source:

# Determine where to place notes
note_row <- rowNo + 3   # 2 blank rows after table

writeData(
  wb2, 
  sheet = "name",
  x = paste0("Source: ", headerTitle),
  startRow = note_row,
  startCol = 1
)

writeData(
  wb2, 
  sheet = "name",
  x = "Note 2: [text to be confirmed]",
  startRow = note_row + 1,
  startCol = 1
)

writeData(
  wb2, 
  sheet = "name",
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
  wb2, 
  sheet = "name",
  style = sourceStyle,
  rows = note_row:(note_row + 2),
  cols = 1,
  gridExpand = TRUE
)



saveWorkbook(wb2, file = "outputs/test_excel2.xlsx", overwrite = TRUE)






## style 3 -------------


wb2 <- createWorkbook()

addWorksheet(wb2, "name")

data = exp.eu.ts

rowNo <- nrow(data)
colNo <- ncol(data)

writeData(
  wb2, 
  sheet = "name", 
  data, 
  withFilter = TRUE,
  startRow = 2, 
  startCol = 1
) # set row to 2 to insert merged cell in row 1 for header title. 

# 0. set column widths:
setColWidths(wb2, sheet = "name", cols = 1:colNo, width = 15)

# 1. create border style:
borderStyle_blue <- createStyle(
  border = "Bottom",
  borderColour = "#1F4E79"
)

# 2. create headerStyle:
headerStyle_blue <- createStyle(
  fontSize = 12,
  fontColour = "#FFFFFF",
  fgFill = "#1F4E79",
  halign = "center",
  textDecoration = "bold",
  border = "Bottom",
  borderColour = "#163758"
)


# 3. Add merged cell header: (one row merged across all columns 1:7). 
# first write header title

headerTitle <- paste0("Preference Utilisation data: ", "name")

writeData(
  wb2, 
  "name",
  headerTitle, 
  startCol = 1, 
  startRow = 1, 
  borders="surrounding", 
  borderColour = "black"
)

mergeCells(wb2, "name", cols = 1:colNo, rows = 1)

titleStyle <- createStyle(
  fontSize = 14,
  textDecoration = "bold",
  halign = "left",
  fgFill = "#F3F2F1",   # light grey
  border = "Bottom",
  borderColour = "#BFBFBF"
)


numStyle_blue <- createStyle(
  numFmt = "#,##0",
  border = "Bottom",
  borderColour = "#1F4E79"
)



#  freezepane:

freezePane(wb2, "name", firstActiveRow = 3, firstActiveCol = 1)

# Add styles:
addStyle(wb2, sheet = "name", borderStyle_blue, rows = 3:rowNo, cols = 1:colNo, gridExpand = T)
addStyle(wb2, sheet = "name", headerStyle_blue, rows = 2, cols = 1:colNo)
addStyle(wb2, sheet = "name", numStyle_blue, rows = 3:rowNo, cols = 3:colNo, gridExpand = T)
addStyle(wb2, sheet = "name", titleStyle, rows = 1, cols = 1:colNo, gridExpand = T)



# add source:

# Determine where to place notes
note_row <- rowNo + 3   # 2 blank rows after table

writeData(
  wb2, 
  sheet = "name",
  x = paste0("Source: ", headerTitle),
  startRow = note_row,
  startCol = 1
)

writeData(
  wb2, 
  sheet = "name",
  x = "Note 2: [text to be confirmed]",
  startRow = note_row + 1,
  startCol = 1
)

writeData(
  wb2, 
  sheet = "name",
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
  wb2, 
  sheet = "name",
  style = sourceStyle,
  rows = note_row:(note_row + 2),
  cols = 1,
  gridExpand = TRUE
)



saveWorkbook(wb2, file = "outputs/test_excel3.xlsx", overwrite = TRUE)


## style 4 -----------


wb2 <- createWorkbook()

addWorksheet(wb2, "name")

data = exp.eu.ts

rowNo <- nrow(data)
colNo <- ncol(data)

writeData(
  wb2, 
  sheet = "name", 
  data, 
  withFilter = TRUE,
  startRow = 2, 
  startCol = 1
) # set row to 2 to insert merged cell in row 1 for header title. 

# 0. set column widths:
setColWidths(wb2, sheet = "name", cols = 1:colNo, width = 15)

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

headerTitle <- paste0("Preference Utilisation data: ", "name")

writeData(
  wb2, 
  "name",
  headerTitle, 
  startCol = 1, 
  startRow = 1, 
  borders="surrounding", 
  borderColour = "black"
)

mergeCells(wb2, "name", cols = 1:colNo, rows = 1)

titleStyle_gov <- createStyle(
  fontSize = 13,
  textDecoration = "bold",
  halign = "left",
  fgFill = NULL,              # no background
  border = "Bottom",
  borderColour = "#D0D0D0",   # soft grey underline
  borderStyle = "thin"
)


numStyle_gov <- createStyle(
  numFmt = "#,##0",
  border = "Bottom",
  borderColour = "#505A5F"
)



#  freezepane:

freezePane(wb2, "name", firstActiveRow = 3, firstActiveCol = 1)

# Add styles:
addStyle(wb2, sheet = "name", borderStyle_gov, rows = 3:(rowNo+2), cols = 1:colNo, gridExpand = T)
addStyle(wb2, sheet = "name", headerStyle_gov, rows = 2, cols = 1:colNo)
addStyle(wb2, sheet = "name", numStyle_gov, rows = 3:(rowNo+2), cols = 4:(colNo-1), gridExpand = T)
addStyle(wb2, sheet = "name", titleStyle_gov, rows = 1, cols = 1:colNo, gridExpand = T)



# add source:

# Determine where to place notes
note_row <- rowNo + 3   # 2 blank rows after table

writeData(
  wb2, 
  sheet = "name",
  x = paste0("Source: ", headerTitle),
  startRow = note_row,
  startCol = 1
)

writeData(
  wb2, 
  sheet = "name",
  x = "Note 2: [text to be confirmed]",
  startRow = note_row + 1,
  startCol = 1
)

writeData(
  wb2, 
  sheet = "name",
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
  wb2, 
  sheet = "name",
  style = sourceStyle,
  rows = note_row:(note_row + 2),
  cols = 1,
  gridExpand = TRUE
)



saveWorkbook(wb2, file = "outputs/test_excel4.xlsx", overwrite = TRUE)



## final style -------


wb2 <- createWorkbook()

addWorksheet(wb2, "name")

data = exp.eu.ts

rowNo <- nrow(data)
colNo <- ncol(data)

writeData(
  wb2, 
  sheet = "name", 
  data, 
  withFilter = TRUE,
  startRow = 2, 
  startCol = 1
) # set row to 2 to insert merged cell in row 1 for header title. 

# 0. set column widths:
setColWidths(wb2, sheet = "name", cols = 1:colNo, width = 15)

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

headerTitle <- paste0("Preference Utilisation data: ", "name")

writeData(
  wb2, 
  "name",
  headerTitle, 
  startCol = 1, 
  startRow = 1, 
  borders="surrounding", 
  borderColour = "black"
)

mergeCells(wb2, "name", cols = 1:colNo, rows = 1)

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
num_cols <- tidyselect::eval_select(rlang::expr(where(is.numeric)), data)

#  freezepane:

freezePane(wb2, "name", firstActiveRow = 3, firstActiveCol = 1)

# Add styles:
addStyle(wb2, sheet = "name", borderStyle_gov, rows = 3:(rowNo+2), cols = 1:colNo, gridExpand = T)
addStyle(wb2, sheet = "name", headerStyle_gov, rows = 2, cols = 1:colNo)
addStyle(wb2, sheet = "name", numStyle_gov, rows = 3:(rowNo+2), cols = num_cols, gridExpand = T)
addStyle(wb2, sheet = "name", titleStyle_gov, rows = 1, cols = 1:colNo, gridExpand = T)

# create and add percentage style

pct_col <- tidyselect::eval_select(rlang::expr(contains("pct")), data)

pctStyle_gov <- createStyle(
  numFmt = "0.0%",
  border = "Bottom",
  borderColour = "#505A5F"
)

addStyle(
  wb2,
  sheet = "name",
  style = pctStyle_gov,
  rows = 3:(rowNo+2),
  cols = pct_col,
  gridExpand = TRUE
)


# fix year col which gets converted if numeric format:
yr_col <- tidyselect::eval_select(rlang::expr(contains("year")), data)

addStyle(
  wb2,
  sheet = "name",
  style = borderStyle_gov,
  rows = 3:rowNo,
  cols = yr_col,
  gridExpand = TRUE
)


# add source:

# Determine where to place notes
note_row <- rowNo + 3   # 2 blank rows after table

writeData(
  wb2, 
  sheet = "name",
  x = paste0("Source: ", headerTitle),
  startRow = note_row,
  startCol = 1
)

writeData(
  wb2, 
  sheet = "name",
  x = "Note 2: [text to be confirmed]",
  startRow = note_row + 1,
  startCol = 1
)

writeData(
  wb2, 
  sheet = "name",
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
  wb2, 
  sheet = "name",
  style = sourceStyle,
  rows = note_row:(note_row + 2),
  cols = 1,
  gridExpand = TRUE
)



saveWorkbook(wb2, file = "outputs/test_excel_final.xlsx", overwrite = TRUE)


# end
