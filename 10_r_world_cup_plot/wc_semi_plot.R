

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
wc_data <- read_csv("wc26_knockout_results.csv") %>% clean_names()


# select/transform imf data for econ metric

econ_ind <- c("NGDPD" , "NGDPRPPPPC")
# LP = population figures


imf_data <- imf_data %>%
  filter(indicator_id %in% econ_ind) %>%
  select(
    iso,
    country,
    indicator_id,
    x2026
  )

imf_data2 <- imf_data %>%
  pivot_wider(
    names_from = indicator_id,
    values_from = x2026
  ) %>%
  rename(
    !!!setNames(econ_ind, paste0("val_col", seq_along(econ_ind)))
  )

# change ind wider-col names to 1 and 2 for simplicity


plot_data <- wc_data %>%
  filter(stage == "SF") %>%
  left_join(
    imf_data2[,c("iso","val_col1","val_col2")],
    by = c("iso1" = "iso")
  ) %>%
  left_join(
    imf_data2[,c("iso","val_col1","val_col2")],
    by = c("iso2" = "iso")
  ) %>%
  rename(
    team1_val1 = val_col1.x,
    team2_val1 = val_col1.y,
    team1_val2 = val_col2.x,
    team2_val2 = val_col2.y
  ) %>%
  mutate( # for chart + flag match in lower case
    match_id = row_number(),
    iso1 = tolower(iso1),
    iso2 = tolower(iso2),
    goals1 = as.character(goals1),
    goals2 = as.character(goals2)
  )

# convert any econ indicator if neccessary


plot_data <- plot_data %>% # format GDP figures to Bn/Tn:
  mutate(
    team1_val1 = ifelse(
      team1_val1 < 1000,
      paste0("$",round(team1_val1, 1), "Bn"),
      paste0("$",round(team1_val1 / 1000, 1), "Tn")
    ),
    team2_val1 = ifelse(
      team2_val1 < 1000,
      paste0("$",round(team2_val1, 1), "Bn"),
      paste0("$",round(team2_val1 / 1000, 1), "Tn")
     )
  ) %>%
  mutate(
    team1_val2 = 
      paste0(
        "$", 
        format(
          round(team1_val2,0), 
          big.mark = ",", 
          trim = TRUE
        )
      ),
    team2_val2 = 
      paste0(
        "$", 
        format(
          round(team2_val2,0), 
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
      colour = "#1f1f1f", size = 24, face = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      colour = "#5e5a54", size = 16,
      margin = margin(b = 14)
    ),
    plot.margin = margin(20, 30, 20, 30)
  )



# Plot value pos --------

x_flag_left    <- -1.5
x_score_left   <- -0.9
x_dash         <-  0.00
x_score_right  <-  0.9
x_flag_right   <-  1.5
x_econ_offset1 <- -1.47
x_econ_offset2 <- 1.52

y_flag_offset  <-  0.6
y_econ_offset  <- 0.3
y_econ_offset2 <- 0.18


# plot flags: 

plot <- ggplot(plot_data) +
  # left flag
  ggflags::geom_flag(
    aes(x = x_flag_left, y = match_id + y_flag_offset, country = iso1),
    size = 35
  ) +
  
  # right flag
  ggflags::geom_flag(
    aes(x = x_flag_right, y = match_id + y_flag_offset, country = iso2),
    size = 35
  )

# plot scores:

plot <- plot +
  # left score
  geom_text(
    aes(x = x_score_left, y = match_id+y_flag_offset, label = goals1),
    size = 22,
    fontface = "bold",
    family = "sans",
    color = "#2a2725"
  ) +
  
  # dash separator
  geom_text(
    aes(x = x_dash, y = match_id+y_flag_offset, label = "—"),
    size = 20,
    fontface = "plain",
    family = "sans",
    color = "#2a2725"
  ) +
  
  # right score
  geom_text(
    aes(x = x_score_right, y = match_id+y_flag_offset, label = goals2),
    size = 22,
    fontface = "bold",
    family = "sans",
    color = "#2a2725"
  )

# note good color to use also: #3a3734

# Econ GDP:
plot <- plot +
  
  # left econ block
  geom_text(
    aes(x = x_flag_left, y = match_id + y_econ_offset, label = team1_val1),
    size = 8,
    lineheight = 0.95,
    family = "sans",
    vjust = 0.7,
    fontface = "bold",
    color = "#2e2c29"
  ) +
  # right econ block
  geom_text(
    aes(x = x_flag_right, y = match_id + y_econ_offset, label = team2_val1),
    size = 8,
    lineheight = 0.95,
    family = "sans",
    vjust = 0.7,
    fontface = "bold",
    color = "#2e2c29"
  )


# Econ GDP per cap:
plot <- plot +
  
  # left econ block
  geom_text(
    aes(x = x_econ_offset1, y = match_id + y_econ_offset2, label = team1_val2),
    size = 6,
    lineheight = 0.95,
    family = "sans",
    vjust = 0.7,
    color = "#2a2725"
  ) +
  # right econ block
  geom_text(
    aes(x = x_econ_offset2, y = match_id + y_econ_offset2, label = team2_val2),
    size = 6,
    lineheight = 0.95,
    family = "sans",
    vjust = 0.7,
    color = "#2a2725"
  )


# fix scale:
plot <- plot +
  
  scale_x_continuous(
    limits = c(-2, 2),
    breaks = NULL
  ) +
  scale_y_continuous(
    limits = c(1, 2.9),
    breaks = NULL
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "2026 World Cup Semi Final Results",
    subtitle = "Economic indicators: GDPand GDP per capita (2026 $ prices).",
    x = "",
    y="",
    caption = "Please note the GB flag is a placeholder for England."
  ) +
  theme_wc_light

plot

ggsave("img/wc_26_sf_plot.png", plot)