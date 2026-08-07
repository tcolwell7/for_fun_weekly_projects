
#' R script to
#' create some custom ggplots
#' for top prem scorers
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


# Load data

data <- read_csv("premier_league_top_scorers_full.csv") %>% clean_names()
glimpse(data)

unique(data$club)
unique(data$nationality)

# stats and plots of interest:
# 1. Goals scored per season
# 2. Cumulative goals over time (doens't work - not plotting in the end)
# 3. Total goals scored by club/how many entries
# 4. Goals scored by non-eng players


# Goals scored per season ------

## simple bar chart

# first agg data to season level 

df <- data %>%
  group_by(season) %>%
  summarise(
    top_scorer = max(goals),
    total_top_10 = sum(goals),
    avg_top_10 = mean(goals)
  ) %>%
  mutate(season_start = as.integer(substr(season, 1, 4)))



plot <- ggplot(df, aes(x = season, y = top_scorer)) +
  geom_vline(
    xintercept = seq_along(df$season),
    colour = "grey80",
    linewidth = 0.9
    )+ 
  geom_vline(
    xintercept = 4,
    linetype = "dashed",
    colour = "#4A4E69",
    linewidth = 0.9
  )+
  geom_col(
    fill = "#577590", 
    width = 0.75, 
    alpha = 0.95,
    color = "#6B6B6B",
    linewidth = 0.3) +
  labs(
    title = "Premier League Top Scorer Goals by Season",
    subtitle = "Please note 42 game season between years 1992-1995",
    x = "",
    y = "",
    caption = "Quiter blue colour theme"
  ) +
  scale_y_continuous(breaks = seq(0, 40, by = 5))+
  theme_minimal(base_family = "sans")

plot <- plot + 
  theme(
    plot.background  = element_rect(fill = "#f6f2ea", colour = NA),
    panel.background = element_rect(fill = "#f6f2ea", colour = NA),
    panel.grid       = element_blank(),
    plot.title = element_text(
      colour = "#1B263B", size = 18, face = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      colour = "#4A4E69", size = 12,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      color = "#1B263B",
      size = 10,
      margin = margin(b = 10, t =-6)
    )
    ,
    plot.margin = margin(10, 20, 0, 5),
    axis.text.x = 
      element_text(
        size=11, 
        angle = 45,
        hjust = 0.85
    ),
    plot.title.position = "plot"
  )

plot


# hex colours to toggle:
# "#2E4057"
# "#577590"
# "#4D908E"
# "#277DA1"
# "#1B263B"
# "#5E4B56"
# "#6A5D7B"
# "#8D6A9F"
# "#4A4E69"
# "#2C2C34"



## Top scorer 2 --------



plot2 <- ggplot(df, aes(x = season, y = avg_top_10)) +
  geom_vline(
    xintercept = seq_along(df$season),
    colour = "grey80",
    linewidth = 0.9
  )+ 
  geom_vline(
    xintercept = 4,
    linetype = "dashed",
    colour = "#4A4E69",
    linewidth = 0.9
  )+
  geom_col(
    fill = "#2E4057", 
    width = 0.75, 
    alpha = 0.95,
    color = "#4D4D4D",
    linewidth = 0.3) +
  coord_flip()+
  labs(
    title = "Premier League top 10 scorer average per season.",
    subtitle = "Please note 42 game season between years 1992-1995",
    x = "",
    y = "",
    caption = "Steel blue colour theme"
  ) +
  scale_y_continuous(breaks = seq(0, 40, by = 5))+
  theme_minimal(base_family = "sans")

plot2 <- plot2 + 
  theme(
    plot.background  = element_rect(fill = "#f6f2ea", colour = NA),
    panel.background = element_rect(fill = "#f6f2ea", colour = NA),
    panel.grid       = element_blank(),
    plot.title = element_text(
      colour = "#3A3734", size = 18, face = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      colour = "#5C5854", size = 12,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      color = "#1B263B",
      size = 10,
      margin = margin(b = 10, t =-6)
    )
    ,
    plot.margin = margin(10, 20, 0, 5),
    axis.text.x = 
      element_text(
        size=11, 
        #angle = 45,
        hjust = 0.85
      ),
    plot.title.position = "plot"
  )

plot2


## Top scorer 3 -------



plot3 <- ggplot(df, aes(x = season, y = total_top_10)) +
 geom_segment(
    x = seq_along(df$season),
    xend = seq_along(df$season),
    y = 0,
    yend = max(df$total_top_10)+20,
    colour = "#CC6F4A",
    linewidth = 0.8,
    alpha = 0.6
  )+
  geom_vline(
    xintercept = 4,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.8
  )+
  geom_col(
    fill = "#1D3557", 
    width = 0.75, 
    color = "#4A4A4A",
    linewidth = 0.3) +
  labs(
    title = "Premier League top 10 scorer total goals scored per season.",
    subtitle = "Please note 42 game season between years 1992-1995",
    x = "",
    y = "",
    caption = "Navy rust colour theme"
  ) +
  scale_y_continuous(breaks = seq(0, (max(df$total_top_10)+10), by = 10))+
  theme_minimal(base_family = "sans")

plot3 <- plot3 + 
  theme(
    plot.background  = element_rect(fill = "#f6f2ea", colour = NA),
    panel.background = element_rect(fill = "#f6f2ea", colour = NA),
    panel.grid       = element_blank(),
    plot.title = element_text(
      colour = "#2A2A2A", size = 18, face = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      colour = "#5E5A54", size = 12,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      color = "#5C5C5C",
      size = 10,
      margin = margin(b = 10, t =-6)
    )
    ,
    plot.margin = margin(10, 20, 0, 5),
    axis.text.x = 
      element_text(
        size=11, 
        angle = 45,
        hjust = 0.85
      ),
    plot.title.position = "plot"
  )

plot3


## patchwork ------

library(patchwork)

pw_plot <- plot / plot2 / plot3


ggsave("img/top_scorers_patchwork_plot.png", pw_plot, width = 8, height = 16, dpi = 300)


## Cumulative top scorer chart ------

# reorder data, so low scoers at the start of the dataset per group (season)

df3 <- data %>%
  group_by(season) %>%
  arrange(goals, .by_group = TRUE) %>%
  ungroup() %>%
  mutate(cum_goals = cumsum(goals)) %>%
  mutate(row_id = row_number())

season_positions <- df3 %>%
  group_by(season) %>%
  summarise(pos = min(row_id))


df3_agg <- data %>%
  group_by(season) %>%
  summarise(total_goals = sum(goals)) %>%
  ungroup() %>%
  mutate(cum_goals = cumsum(total_goals)) %>%
  mutate(season_index = row_number())




ggplot(df3_agg, aes(x = season_index, y = cum_goals)) +
  geom_area(fill = "#1D3557", alpha = 0.8) +
  geom_line(colour = "#CC6F4A", linewidth = 0.8) +
  scale_x_continuous(
    breaks = df3_agg$season_index,
    labels = df3_agg$season
  ) +
  labs(
    title = "Cumulative Premier League Goals",
    subtitle = "Season-level cumulative sum",
    x = "Season",
    y = "Cumulative goals"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# hmm after consideration cumsum plot looks rubbish and adds nothing so moving on. 

# end


