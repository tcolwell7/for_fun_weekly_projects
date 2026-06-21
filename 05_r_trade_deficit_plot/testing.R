# Basic r testing script for porcessing

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



data <- read_excel("tradequarterlyq425seasonallyadjusted.xlsx", sheet = 3, skip = 3) %>% clean_names()

imp_row = data %>%
  mutate(row_id = row_number()) %>%
  filter(is.na(country)) %>%
  pull(row_id)

exp = data %>%
  slice(1:(imp_row-1)) %>%
  mutate(flow = "Exports")


imp = data %>%
  slice((imp_row+2):nrow(data)) %>%
  mutate(flow = "Imports")

total = bind_rows(exp, imp)


eu27 <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
  "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
  "PL","PT","RO","SK","SI","ES","SE"
)


eu <- total %>% 
  filter(country_code %in% eu27) %>%
  arrange(country_code)


# solution 1

eu.td = eu %>%
  mutate(across(starts_with("x"), as.numeric)) %>%
  group_by(country_code, country) %>%
  summarise(
    across(starts_with("x"), ~ first(.x) - last(.x)),
    .groups = "drop"
  )



# solution 2





# select data and test plot

df <- eu.td %>% 
  select(
    country_code,
    country,
    x2016:x2025
  ) %>%
  pivot_longer(
    cols = x2016:x2025,
    names_to = "year",
    values_to = "trade_def"
  ) %>%
  mutate(year = str_remove(year,"x"))




# plot testing -------

custom_palette <- c( "#12436D" , "#0965A0" , "#2493D9" , "#7BC7F8", "#bdd7e7","#777d96")


# DBT theme (your defaults)

font <- "sans"

grid_y <- element_line(color = "grey90", linewidth = 0.2)
grid_x <- element_blank()

update_geom_defaults("line", list(linewidth = 0.8))
update_geom_defaults("bar", list(fill = "#00285f"))

theme_default <-
  theme_minimal() +
  theme(
    plot.margin = margin(t = 15, r = 5.5, b = 5.5, l = 5.5, "pt"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    plot.title = element_text(
      family = font, size = 14, face = "bold",
      hjust = 0, vjust = 5
    ),
    plot.subtitle = element_text(
      family = font, size = 12,
      hjust = 0, vjust = 6
    ),
    plot.caption = element_text(
      family = font, size = 8, hjust = 0
    ),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    
    panel.grid.major.y = grid_y,
    panel.grid.major.x = grid_x,
    panel.grid.minor = element_blank(),
    
    axis.ticks = element_blank(),
    
    axis.title = element_text(family = font, size = 12),
    axis.text = element_text(family = font, size = 10),
    
    axis.text.x = element_text(margin = margin(b = 6)),
    axis.text.y = element_text(margin = margin(l = 6))
  )



df %>%
  filter(country_code == "SE") %>%
  mutate(td = if_else(trade_def >= 0, "Surplus", "Deficit")) %>%
  ggplot(
    aes(
      x = year,
      y = trade_def,
      fill = td
    )
  )+
  geom_col(color="black", alpha=0.7)+
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 1) +
  scale_fill_manual(
    values = c(
      Surplus = flag_cols[["SE"]][1],
      Deficit = flag_cols[["SE"]][2]
    )
  )+
  #geom_bar(stat="identity", fill = "#005293", color = "#FECB00") +
  theme_default+
  labs(
    y="",
    x="",
    title = paste0("UK-Sweden trade balance (£m)"))+
  scale_y_continuous(labels = scales::label_dollar(prefix = "£", big.mark = ","))

flag_cols[["SE"]][1]

flag_cols <- list(
  SE = c("#005293", "#FECB00"),                     # Sweden
  FR = c("#0055A4", "#FFFFFF", "#EF4135"),          # France
  FI = c("#FFFFFF", "#003580"),                     # Finland
  DE = c("#000000", "#DD0000", "#FFCE00"),          # Germany
  NL = c("#AE1C28", "#21468B", "#FFFFFF"),          # Netherlands
  BE = c("#000000", "#FFD90C", "#EF3340"),          # Belgium
  IT = c("#009246", "#FFFFFF", "#CE2B37"),          # Italy
  ES = c("#AA151B", "#F1BF00"),                     # Spain
  PT = c("#006600", "#FF0000", "#FFCC00"),          # Portugal
  DK = c("#C60C30", "#FFFFFF"),                     # Denmark
  AT = c("#ED2939", "#FFFFFF"),                     # Austria
  PL = c("#FFFFFF", "#DC143C"),                     # Poland
  CZ = c("#FFFFFF", "#D7141A", "#11457E"),          # Czechia
  EE = c("#0072CE", "#FFFFFF", "#000000"),          # Estonia
  LV = c("#9E3039", "#FFFFFF"),                     # Latvia
  LT = c("#FDB913", "#006A44", "#C1272D"),          # Lithuania
  IE = c("#169B62", "#FFFFFF", "#FF883E"),          # Ireland
  HU = c("#CE2939", "#FFFFFF", "#477050"),          # Hungary
  SK = c("#FFFFFF", "#0B4EA2", "#EE1C25"),          # Slovakia
  SI = c("#FFFFFF", "#005DA4", "#ED1C24"),          # Slovenia
  RO = c("#002B7F", "#FCD116", "#CE1126"),          # Romania
  BG = c("#FFFFFF", "#00966E", "#D62612"),          # Bulgaria
  HR = c("#FF0000", "#FFFFFF", "#171796"),          # Croatia
  CY = c("#FFFFFF", "#D57800", "#5A8E3E"),          # Cyprus
  LU = c("#EF3340", "#FFFFFF", "#00A3E0"),          # Luxembourg
  MT = c("#FFFFFF", "#CF142B", "#BEBEBE"),          # Malta
  GR = c("#0D5EAF", "#FFFFFF")                      # Greece
)




# dynamic example: -----------


cc <- "DK"   # change this to any EU country code

cols <- flag_cols[[cc]]

country_name <- df %>% 
  filter(country_code == cc) %>% 
  distinct(country) %>% 
  pull()

plot <- df %>%
  filter(country_code == cc) %>%
  mutate(td = if_else(trade_def >= 0, "Surplus", "Deficit")) %>%
  ggplot(aes(x = year, y = trade_def, fill = td)) +
  geom_col(color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 1) +
  scale_fill_manual(
    values = c(
      Surplus = cols[1],
      Deficit = cols[2]
    )
  ) +
  theme_default +
  labs(
    y = "",
    x = "",
    title = paste0("UK–", country_name, " trade balance (£m)")
  ) +
  scale_y_continuous(labels = scales::label_dollar(prefix = "£", big.mark = ","))



# diff background colours:

plot +
 theme(
   plot.background = element_rect(fill = "#F7F7F7", colour = NA),
   panel.background = element_rect(fill = "#F7F7F7", colour = NA)
   )


