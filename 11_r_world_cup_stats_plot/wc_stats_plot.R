# World cup stats plotting script

#' This  mini project is essentially
#' small bit of annoying data processing
#' particular with country codes
#' ensuring the matched world cup
#' and econ data is merged properly together
#' to then plot some correlations
#' across a selection of metrics
#' to see if we can find anything interesting
#' 


options(scipen=999) # turn off scientific numerical notation

`%notin%` <- Negate(`%in%`) # custom function for filtering data

# Specify packages
packages <-
  c("tidyverse","janitor","stringr","data.table",# general data wrangling
    "readxl","readr",
    "ggflags", "countrycode"
  ) 

# Install packages if not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
sapply(packages, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load in imf data --

imf_data <- read_excel("imf_econ_data.xlsx") %>% clean_names()

imf_data <- 
  imf_data %>%
  mutate(iso2 = countrycode(country_id, "iso3c", "iso2c")) %>%
  relocate(iso2, .after = "country_id")

# select IMF indicators

# BCA_NGDPD: Credit account balance pc of GDP
# BCA: Credit account US dollar
# BCA_NGDPD: Current account balance (credit less debit), Percent of GDP
# GGX_NGDP: Expenditure, General government, Percent of GDP
# NGDPDPC: Gross domestic product (GDP), Current prices, Per capita, US dollar
# NGDPD:  Gross domestic product (GDP), Current prices, US dollar,
# NGDPRPPPPC: Gross domestic product (GDP), Constant prices, Per capita, purchasing power parity (PPP) international dollar, ICP benchmark 2021
# LP: Population, Persons for countries / Index for country groups
# LUR: Unemployment rate
# LE: Employed persons, Persons for countries / Index for country groups


econ_id <- 
  c("BCA", "BCA_NGDPD","GGX_NGDP",
    "LP","LUR", "LE"
    ,
    "NGDPDPC","NGDPD", "NGDPRPPPPC"
    )

imf_data2 <- imf_data %>%
  filter(indicator_id %in% econ_id) %>%
  select(
    iso2,
    country,
    indicator_id,
    indicator,
    unit, 
    x2025
  )


# wc data processing --

#' for simpler matching and interpretation
#' a team level dataset needs creating
#' team 1 and team 2 needs splitting into two analysing
#' then binidng together and aggregated for a full list
#' and team level wc stats to model against
#' 

wc_data <- read_csv("wc26_results.csv") %>% 
  clean_names() %>%
  mutate(
    team1_res = ifelse(goals1 > goals2, "W",
                       ifelse(goals1 < goals2, "L",
                              ifelse(goals1 == goals2, "D","Error")
                  )
               ),
    team2_res = ifelse(goals2 > goals1, "W",
                       ifelse(goals2 < goals1, "L",
                              ifelse(goals2 == goals1, "D","Error")
                       )
                  )
    ) %>%
  mutate(
    points1 = ifelse(team1_res == "W", 3,
                     ifelse(team1_res == "D", 1,
                            ifelse(team1_res == "L", 0, 0))),
    points2 = ifelse(team2_res == "W", 3,
                     ifelse(team2_res == "D", 1,
                            ifelse(team2_res == "L", 0, 0)))
  )
  
  




wc1 <- wc_data %>%
  select(
    iso = iso1,
    team = team1,
    goals = goals1,
    result = team1_res,
    points = points1
  )


wc2 <- wc_data %>%
  select(
    iso = iso2,
    team = team2,
    goals = goals2,
    result = team2_res,
    points = points2
  )
    

wc = bind_rows(wc1, wc2) %>%
  group_by(iso,team) %>%
  summarise(
    matches = n(),
    goals_scored = sum(goals, na.rm = TRUE),
    won = sum(result == "W", na.rm = TRUE),
    draw = sum(result == "D", na.rm = TRUE),
    loss = sum(result == "L", na.rm = TRUE),
    points = sum(points, na.rm = TRUE)
  ) %>%
  mutate(win_ratio = won / matches)


  

# match wc data --



df <- wc %>%
  left_join(
    imf_data2[,c("iso2","indicator_id","indicator","x2025")], 
    by = c("iso"= "iso2")
    ) %>%
  filter(!is.na(x2025))

# r2 value per indc
r2 <- df %>%
  group_by(indicator_id, indicator_short) %>%
  summarise(
    model = list(lm(x2025 ~ goals_scored, data = cur_data())),
    r2 = summary(model[[1]])$r.squared,
    .groups = "drop"
  )

# checking diff values:
r2_df <- df %>%
  group_by(indicator) %>%
  summarise(
    model = list(lm(x2025 ~ win_ratio)),
    r2 = summary(model[[1]])$r.squared
  )


r2_df <- df %>%
  group_by(indicator) %>%
  summarise(
    model = list(lm(x2025 ~ points)),
    r2 = summary(model[[1]])$r.squared
  )

# plot indicator short names:

indicator_lookup <- tibble::tribble(
  ~indicator_id,   ~indicator_short,
  "NGDPD",         "GDP (current USD)",
  "NGDPRPPPPC",    "GDP pc (PPP)",
  "NGDPDPC",       "GDP pc (USD)",
  "LUR",           "Unemployment",
  "LP",            "Population",
  "GGX_NGDP",      "Govt exp (% GDP)",
  "BCA",           "CA balance (USD)",
  "BCA_NGDPD",     "CA balance (% GDP)",
  "LE",            "Employment"
)

df <- df %>%
  left_join(indicator_lookup, by = "indicator_id") %>%
  mutate(indicator_short = factor(indicator_short))

# plot test ----

fc_plot <- 
  ggplot(df,
       aes(
         x = goals_scored,
         y = x2025
       )
    )+
  geom_point() +
  facet_wrap(~ indicator_short, scales = "free_y", ncol = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(base_size = 12) +
  labs(y="")


fc_plot <- 
  ggplot(df,
       aes(
         x = points,
         y = x2025
       )
)+
  geom_point() +
  facet_wrap(~ indicator_id, scales = "free_y", ncol = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(base_size = 12)




fc_plot <- 
  ggplot(df,
       aes(
         x = win_ratio,
         y = x2025
       )
)+
  geom_point() +
  facet_wrap(~ indicator_id, scales = "free_y", ncol = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(base_size = 12)


ggsave("fc_plot.png", fc_plot, width = 8, height = 12, dpi = 300)



# log scale check ----

# quick check on transformed gdp/pop values
# so countries are more comparable given large difference in pop sizes


