# testing script for calling Trade data API. 

library(tidyverse)
library(httr)
library(jsonlite)
library(furrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("api_functions.R")

nordic_countries <- c("IS","NO","SE","FI","DK")

sitc_codes <- ons_get_codes("sitc")


# EXTACT DATA: ------------

## NOTE: sometimes API when pulling larger scale of data it fails, but when re-run works. 
# strange anomoly which I haven't sought to solve/investigate full reason why
# as this is not the purpose of this work
# Note: running the API call with nordic country list as .x - works fine, jsut re-run the API if it fails. 

data_fi <-
  ons_get_trade_furrr(
    time = c("Jan-22", "Feb-22", "Mar-22"),
    sitc = "T",
    country = "FI",
    flow = "IM"
  )


years = c(2021,2022,2023,2024,2025)

df_fi <- map_dfr(
     years,
  ~ ons_get_trade_year(
    .x,
    sitc = "T",
    country = "FI",
    flow = "IM"
     )
  )


df_se <- map_dfr(
  years,
  ~ ons_get_trade_year(
    .x,
    sitc = "T",
    country = "SE",
    flow = "IM"
  )
)

nordic_data <- map_dfr(
  years,
  ~ ons_get_trade_year(
    .x,
    sitc = "T",
    country = c("SE","FI","NO","DK","IS"),
    flow = "IM"
  )
)

nordic_countries <- c("SE","FI","NO","DK","IS")

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

# CLEAN & TRANSFORM DATA ----------

# create both yearly and monthly files for later use
# calculate overall trade, nordic and uk total share for comparison
# simple tidyverse grouping, aggregations etc - nothing complex. 


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


# PLOT TESTING: ---------------



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


# custom factor for plot:


df <-  uk_share_yr %>% ungroup()

ordered_levels <- df %>%
  arrange(-share_of_nordic) %>%
  pull(country) %>%
  unique()

df$country <- factor(df$country, levels = ordered_levels)




# stacked area plot for nordic share:

plot <- df %>%
  ggplot(
    aes(
      x=year, 
      y=share_of_nordic, 
      group = country,
      fill = country
     )
    )+
  geom_area(alpha = 0.5) + 
  theme_default +
  scale_fill_manual(
    values = c( "#12436D" , "#0965A0" , "#2493D9" , "#7BC7F8" , "#777d96"),
    #values = c( "#00285f" , "#a90083" , "#12436D" , "#777d96" , "#a3abcc"), #"#d9ddea"),
    #values = c( "#00285f" , "#a90083" , "#2493D9" , "#777d96" , "#d9ddea"), #"#d9ddea"),
    labels = c('Norway','Sweden','Denmark', 'Finland','Iceland')
  ) +
  labs(x="",  y="")+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.title = element_blank())



df %>%
  ggplot(
    aes(
      x=year, 
      y=total_imports, 
      group = country,
      color = country
    )
  )+
  geom_line(alpha = 0.8) + 
  theme_default +
  scale_color_manual(
    #values = c( "#12436D" , "#0965A0" , "#2493D9" , "#7BC7F8" , "#777d96"),
    #values = c( "#00285f" , "#a90083" , "#12436D" , "#777d96" , "#a3abcc"), #"#d9ddea"),
    values = c( "#00285f" , "#a90083" , "#2493D9" , "#777d96" , "#d9ddea"), #"#d9ddea"),
    labels = c('Norway','Sweden','Denmark', 'Finland','Iceland')
  ) +
  labs(x="",  y="")+
  #scale_y_continuous(labels = scales::percent)+
  theme(legend.title = element_blank())

# due to import size different plotting on same plot looks off
# so testing a facet plot:

uk_share_yr %>%
  mutate(year = as.integer(year)) %>%
  ggplot(aes(year, total_imports)) +
  geom_line(colour = "#1f78b4", linewidth = 1) +
  #facet_wrap(~ country, scales = "free_y") +
  scale_x_continuous(breaks = 2019:2025) +
  facet_wrap(~ country, scales = "free")+
  theme_default+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.3),
    )
  

