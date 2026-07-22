
#' test script
#' to create initial World Cup
#' For fun plot idea
#' Plotting countries flags, results and 
#' some Economic stats in a custom
#' R layered visual for fun
#' To make something interesting
#' 

# Set up

options(scipen=999) # turn off scientific numerical notation

`%notin%` <- Negate(`%in%`) # custom function for filtering data

# Specify packages
packages <-
  c("tidyverse","janitor","stringr","data.table",# general data wrangling
    "readxl","readr",
    "ggflags"
  ) 

# Install packages if not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
sapply(packages, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load and match data ----

imf_data <- read_excel("imf_econ_data.xlsx") %>% clean_names()
wc_data <- read_csv("wc26_r16_results.csv") %>% clean_names()


# select/transform imf data for econ metric

econ_ind <- c("NGDPRPPPPC")

imf_data2 <- imf_data %>%
  filter(indicator_id %in% econ_ind) %>%
  select(
    iso,
    country,
    x2026
  )


plot_data <- wc_data %>%
  left_join(
    imf_data2,
    by = c("iso1" = "iso")
  ) %>%
  left_join(
    imf_data2,
    by = c("iso2" = "iso")
  ) %>%
  rename(
    team1_val = x2026.x,
    team2_val = x2026.y
  ) %>%
  mutate( # for chart + flag match in lower case
    match_id = row_number(),
    iso1 = tolower(iso1),
    iso2 = tolower(iso2),
    goals1 = as.character(goals1),
    goals2 = as.character(goals2)
  )

# convert any econ indicator if neccessary


plot_data <- plot_data %>%
  mutate(
    team1_val = 
      paste0(
        "$", 
        format(
          round(team1_val,0), 
          big.mark = ",", 
          trim = TRUE
          )
        ),
    team2_val = 
      paste0(
        "$", 
        format(
          round(team2_val,0), 
          big.mark = ",", 
          trim = TRUE
        )
      )
    )




# Plot theme ----

theme_wc_light <- theme_minimal(base_family = "sans") +
  theme(
    plot.background  = element_rect(fill = "#f6f2ea", colour = NA),
    panel.background = element_rect(fill = "#f6f2ea", colour = NA),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    
    plot.title = element_text(
      colour = "#1f1f1f", size = 20, face = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      colour = "#5e5a54", size = 12,
      margin = margin(b = 14)
    ),
    plot.margin = margin(20, 30, 20, 30)
  )

# wc_plot_light <- wc_plot_base +
#   theme_wc_light +
#   theme(
#     text = element_text(colour = "#1f1f1f")
#   )








theme_wc_dark <- theme_minimal(base_family = "sans") +
  theme(
    plot.background  = element_rect(fill = "#0e0e0e", colour = NA),
    panel.background = element_rect(fill = "#0e0e0e", colour = NA),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    
    plot.title = element_text(
      colour = "#f5f3ef", size = 20, face = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      colour = "#d6d1c9", size = 11,
      margin = margin(b = 14)
    ),
    plot.margin = margin(20, 30, 20, 30)
  )


# wc_plot_dark <- wc_plot_base +
#   theme_wc_dark +
#   theme(
#     text = element_text(colour = "#f5f3ef")
#   )



# Plot testing  (4 teams) ----

  
x_flag_left   <- -1.5
x_score_left  <- -1
x_dash        <-  0.00
x_score_right <-  1
x_flag_right  <-  1.5

y_flag_offset <-  0.5
y_econ_offset <- 0


plot <- ggplot(tail(plot_data,4)) +
  # left flag
  ggflags::geom_flag(
    aes(x = x_flag_left, y = match_id + y_flag_offset, country = iso1),
    size = 27
  ) +
  
  # right flag
  ggflags::geom_flag(
    aes(x = x_flag_right, y = match_id + y_flag_offset, country = iso2),
    size = 27
  )


plot <- plot +
  # left score
  geom_text(
    aes(x = x_score_left, y = match_id+y_flag_offset, label = goals1),
    size = 20,
    fontface = "bold",
    family = "sans"
  ) +
  
  # dash separator
  geom_text(
    aes(x = x_dash, y = match_id+y_flag_offset, label = "—"),
    size = 19,
    fontface = "plain",
    family = "sans"
  ) +
  
  # right score
  geom_text(
    aes(x = x_score_right, y = match_id+y_flag_offset, label = goals2),
    size = 19,
    fontface = "bold",
    family = "sans"
  ) +
  
  # left econ block
  geom_text(
    aes(x = x_flag_left, y = match_id + y_econ_offset, label = team1_val),
    size = 4,
    lineheight = 0.95,
    family = "sans",
    vjust = 0.7,
    fontface = "bold"
  ) +
  # right econ block
  geom_text(
    aes(x = x_flag_right, y = match_id + y_econ_offset, label = team2_val),
    size = 4,
    lineheight = 0.95,
    family = "sans",
    vjust = 0.7,
    fontface = "bold"
  )


# fix scale:
plot <- plot +
  
  scale_x_continuous(
    limits = c(-2, 2),
    breaks = NULL
  ) +
  scale_y_continuous(
    limits = c(5, 8.8),
    breaks = NULL
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "2026 World Cup Round of 16",
    subtitle = "Flags, scorelines, and GDP per capita (2026 $ prices)",
    x = "",
    y=""
  ) +
  theme_wc_light


ggsave("img/r16_test_plot_4.png", plot)
       



# Plot testing  (8 teams) ----


x_flag_left   <- -1.5
x_score_left  <- -1
x_dash        <-  0.00
x_score_right <-  1
x_flag_right  <-  1.5

y_flag_offset <-  0.5
y_econ_offset <- 0


plot <- ggplot(plot_data) +
  # left flag
  ggflags::geom_flag(
    aes(x = x_flag_left, y = match_id + y_flag_offset, country = iso1),
    size = 27
  ) +
  
  # right flag
  ggflags::geom_flag(
    aes(x = x_flag_right, y = match_id + y_flag_offset, country = iso2),
    size = 27
  )


plot <- plot +
  # left score
  geom_text(
    aes(x = x_score_left, y = match_id+y_flag_offset, label = goals1),
    size = 20,
    fontface = "bold",
    family = "sans"
  ) +
  
  # dash separator
  geom_text(
    aes(x = x_dash, y = match_id+y_flag_offset, label = "—"),
    size = 18,
    fontface = "plain",
    family = "sans"
  ) +
  
  # right score
  geom_text(
    aes(x = x_score_right, y = match_id+y_flag_offset, label = goals2),
    size = 19,
    fontface = "bold",
    family = "sans"
  ) +
  
  # left econ block
  geom_text(
    aes(x = x_flag_left, y = match_id + y_econ_offset, label = team1_val),
    size = 4,
    lineheight = 0.95,
    family = "sans",
    vjust = 0.7,
    fontface = "bold"
  ) +
  # right econ block
  geom_text(
    aes(x = x_flag_right, y = match_id + y_econ_offset, label = team2_val),
    size = 4,
    lineheight = 0.95,
    family = "sans",
    vjust = 0.7,
    fontface = "bold"
  )


# fix scale:
plot <- plot +
  
  scale_x_continuous(
    limits = c(-2, 2),
    breaks = NULL
  ) +
  scale_y_continuous(
    limits = c(1, 9),
    breaks = NULL
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "2026 World Cup Round of 16",
    subtitle = "Flags, scorelines, and GDP per capita (2026 $ prices)",
    x = "",
    y=""
  ) +
  theme_wc_light


ggsave("img/r16_test_plot_8.png", plot, width = 8, height = 12, dpi = 300)

# plot height works, however title text needs increasing etc. 
