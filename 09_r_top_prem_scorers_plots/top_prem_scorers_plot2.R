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
    "ggflags", "patchwork"
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


# Goals scored by club ------ 


df_top <- data %>% 
  group_by(club) %>%
  summarise(
    top_scorer = max(goals),
    total_top_10 = sum(goals),
    avg_top_10 = mean(goals),
    totalscorers = n()
  ) %>%
  arrange(desc(total_top_10))

# highlght how many times each club has a top 10 entry
# suprisingly manc or arsenal arne't in every season.. (had to manualyl check! but its correct)
df_seas <- data %>%
  group_by(season, club) %>%
  tally() %>%
  group_by(club) %>%
  tally()


df <- df_top %>%
  left_join(df_seas, by = "club")

# Test bar plot -----



plot <- 
 ggplot()+
   geom_col(
     data = df,
     aes(
       x = reorder(club, total_top_10),
       y = total_top_10
       ),
     color = "#1E2A35",
     fill = "#5A6A73",
     width = 0.7,
     linewidth = 0.2
   )+
  geom_text(
    data = df,
    aes(
      x = reorder(club, total_top_10),
      y = total_top_10,
      label = totalscorers
    ),
    hjust = -0.45,        # pushes text slightly to the right of the bar
    colour = "#1E2A35",  # matches your outline colour
    size = 3
  )+
   labs(
     x = "",
     y = "",
     title = "Premier league total goals by top ten scorers by club",
     subtitle = "Covering all premier league seasons (1992/93 - 2025/26)",
     caption = "Numeric text is for number of players entries per club in top 10 scorers list."
   )+
   coord_flip()+
   scale_y_continuous(
     limits = c(0, 750),
     breaks = seq(0, 700, by = 100)
   )

plot <- plot + theme(
  plot.background  = element_rect(fill = "#f6f2ea", colour = NA),
  panel.background = element_rect(fill = "#f6f2ea", colour = NA),
  panel.grid.major.x = element_line(colour = "#D8D3C8", size = 0.3),
  panel.grid       = element_blank(),
  axis.text = element_text(colour = "#3A3A3A", size = 11),
  plot.title = element_text(colour = "#1F1F1F", size = 15, face = "bold",margin = margin(b = 6)),
  plot.subtitle = element_text(colour = "#4A5568", size = 12,margin = margin(b = 12)),
  plot.caption = element_text(color = "#1B263B",size = 9,margin = margin(b = 10, t =-3), hjust = 0),
  plot.margin = margin(10, 20, 0, 5),
  axis.text.x = element_text(size=10),
  axis.text.y = element_text(size=10),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  axis.ticks.x = element_blank()
)

plot

# "#2C3E50"
# "#1E2A35"
# "#5A6A73"
# "#3C464C"
# 
# 
# "#1B263B"
# "#4A5568"
# "#2F3A45"
# 
# # text color:
# "#3A3A3A"
# "#4D4D4D"
# "#1F1F1F"
# 
# "#D8D3C8"
# "#AFA89C"


# average goals plot -----


plot_avg <- 
  ggplot()+
  geom_col(
    data = df,
    aes(
      x = reorder(club, avg_top_10),
      y = avg_top_10
    ),
    color = "#1E2A35",
    fill = "#5A6A73",
    width = 0.7,
    linewidth = 0.2
  )+
  labs(
    x = "",
    y = "",
    title = "Premier league average goals per season by top ten scorers by club",
    subtitle = "Covering all premier league seasons (1992/93 - 2025/26).",
    caption = "Note some teams have only 1-3 seasons which skews their figures"
  )+
  coord_flip()+
  scale_y_continuous(
    limits = c(0, 22),
    breaks = seq(0, 22, by = 5)
  )


plot_avg <- plot_avg + theme(
  plot.background  = element_rect(fill = "#f6f2ea", colour = NA),
  panel.background = element_rect(fill = "#f6f2ea", colour = NA),
  panel.grid.major.x = element_line(colour = "#D8D3C8", size = 0.3),
  panel.grid       = element_blank(),
  axis.text = element_text(colour = "#3A3A3A", size = 11),
  plot.title = element_text(colour = "#1F1F1F", size = 15, face = "bold",margin = margin(b = 6)),
  plot.subtitle = element_text(colour = "#4A5568", size = 12,margin = margin(b = 12)),
  plot.caption = element_text(color = "#1B263B",size = 9,margin = margin(b = 10, t =-3), hjust = 0),
  plot.margin = margin(10, 20, 0, 5),
  axis.text.x = element_text(size=10),
  axis.text.y = element_text(size=10),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  axis.ticks.x = element_blank()
)
# )+
#   theme(
#     plot.caption.position = "plot",
#     plot.caption = element_text(
#       hjust = 0,
#       margin = margin(t = 10),
#       colour = "#4D4D4D",
#       size = 9
#     ),
#     plot.margin = margin(20, 20, 20, 20)
#   )


plot_avg



# patch work

pw = plot + plot_avg
pw


ggsave("img/top_scorers_patchwork_plot2.png", pw, width = 15, height = 10, dpi = 300)
