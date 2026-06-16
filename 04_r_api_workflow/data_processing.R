# data processing and analysis script loading in API data
# cleaning and transforming data
# calculating and summarising all key values for report


library(tidyverse)
library(httr)
library(jsonlite)
library(furrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("api_functions.R")

nordic_countries <- c("IS","NO","SE","FI","DK")

#sitc_codes <- ons_get_codes("sitc")


# GGPLOT THEME -----------

# custom formatting
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
    
    panel.grid.major.y = grid_y,
    panel.grid.major.x = grid_x,
    panel.grid.minor = element_blank(),
    
    axis.ticks = element_blank(),
    
    axis.title = element_text(family = font, size = 12),
    axis.text = element_text(family = font, size = 10),
    
    axis.text.x = element_text(margin = margin(b = 6)),
    axis.text.y = element_text(margin = margin(l = 6))
  )



# EXTACT DATA: ------------

nordic_data <- map_dfr(
  nordic_countries, 
  ~ ons_get_trade_year(
    2019:2025,
    sitc = "T",
    .x,
    flow = "IM"
  )
)


uk_total <- ons_get_trade_year(
  years = 2019:2025,
  sitc = "T",
  country = "W1",
  flow = "IM"
) 

#save(nordic_data, uk_total, file = "data.RData")

# CLEAN & TRANSFORM DATA ----------


# join data and clean

data_month <-  nordic_data %>%
  left_join(uk_total %>% select(time, uk_total = value), by = "time") %>%
  mutate(
    value = as.numeric(value),
    uk_total = as.numeric(uk_total),
    year = paste0("20", substr(time, 5, 6)),
    month = substr(time, 1, 3)
  ) %>%
  select(-geography)


data_yr <-  nordic_data %>%
  left_join(uk_total %>% select(time, uk_total = value), by = "time") %>%
  mutate(
    value = as.numeric(value),
    uk_total = as.numeric(uk_total),
    year = paste0("20", substr(time, 5, 6)),
    month = substr(time, 1, 3)
  ) %>%
  group_by(year,country,sitc,flow) %>%
  summarise(
    total_imports = sum(value, rm.na = TRUE),
    total_imports_uk = sum(uk_total, rm.na = TRUE)
  ) %>% 
  arrange(country)



# Calculate shares of monthly data

uk_share_month <- data_month %>%
  group_by(time) %>%
  mutate(
    nordic_total = sum(value, na.rm = TRUE),
    share_of_nordic = value / nordic_total,
    share_of_uk = value / uk_total
  ) %>%
  ungroup() %>%
  group_by(country, year) %>%
  summarise(
    avg_share_uk = mean(share_of_uk, na.rm = TRUE),
    avg_share_nordic = mean(share_of_nordic, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(across(c(avg_share_uk:avg_share_nordic), ~ round(.x, 3)))


# Calculate shares of yearly data

uk_share_yr <- data_yr %>%
  group_by(year) %>%
  mutate(
    nordic_total = sum(total_imports, na.rm = TRUE),
    share_of_nordic = total_imports / nordic_total,
    share_of_uk = total_imports / total_imports_uk
  ) %>%
  ungroup() %>%
  mutate(across(c(share_of_nordic:share_of_uk), ~ round(.x, 3))) %>%
  mutate(nordic_share_uk = nordic_total / total_imports_uk)


# Individual tables: -------


uk_totals <- uk_share_yr %>% select(year, total_imports_uk) %>% distinct() %>% tibble::deframe()


nordic_import_totals <- uk_share_yr %>% 
  group_by(year) %>% 
  summarise(total_imports = sum(total_imports, na.rm = T)) %>%
  tibble::deframe()


nordic_share_uk <- uk_share_yr %>%
  select(year, nordic_share_uk) %>%
  mutate(nordic_share_uk = round(nordic_share_uk*100,1)) %>%
  distinct() %>%
  tibble::deframe()


nordic_value_list <- uk_share_yr %>%
  split(.$country) %>%        # DK, FI, NO, SE, IS...
  lapply(as.list)



uk_total_imports.25 <- uk_share_yr %>% filter(year == 2025) %>% select(total_imports_uk) %>% distinct() %>% pull() 


# Individual Nordic values ---------



nordic_import_total.25 <- sum(data_yr$total_imports[data_yr$year==2025])
nordic_import_total.19 <- sum(data_yr$total_imports[data_yr$year==2019])
nordic_change_pc <- round((1-(nordic_import_total.19 / nordic_import_total.25))*100,1)

nordic_import_share_uk.25 <- uk_share_yr %>% filter(year == 2025) %>% select(nordic_share_uk) %>% distinct() %>% pull()
nordic_import_share_uk.25 <- round(nordic_import_share_uk.25*100, 1)

nordic_import_share_uk.19 <- uk_share_yr %>% filter(year == 2019) %>% select(nordic_share_uk) %>% distinct() %>% pull()
nordic_import_share_uk.19 <- round(nordic_import_share_uk.19*100, 1)

nordic_import_share_uk.22 <- uk_share_yr %>% filter(year == 2022) %>% select(nordic_share_uk) %>% distinct() %>% pull()
nordic_import_share_uk.22 <- round(nordic_import_share_uk.22*100, 1)



# Norway values --------

no_import_total.25 <- nordic_value_list$NO$total_imports[nordic_value_list$NO$year == "2025"]
no_import_total.19 <- nordic_value_list$NO$total_imports[nordic_value_list$NO$year == "2025"]

no_import_totals <- setNames(
  nordic_value_list$NO$total_imports,
  nordic_value_list$NO$year
)



# Nordic shares -------

no_nordic_share <- setNames(
  nordic_value_list$NO$share_of_nordic,
  nordic_value_list$NO$year
)

no_nordic_share <- no_nordic_share * 100

se_nordic_share <- setNames(
  nordic_value_list$SE$share_of_nordic,
  nordic_value_list$SE$year
)

se_nordic_share <- se_nordic_share * 100

fi_nordic_share <- setNames(
  nordic_value_list$FI$share_of_nordic,
  nordic_value_list$FI$year
)

fi_nordic_share <- fi_nordic_share * 100


is_nordic_share <- setNames(
  nordic_value_list$IS$share_of_nordic,
  nordic_value_list$IS$year
)

is_nordic_share <- is_nordic_share * 100

dk_nordic_share <- setNames(
  nordic_value_list$DK$share_of_nordic,
  nordic_value_list$DK$year
)

dk_nordic_share <- dk_nordic_share * 100