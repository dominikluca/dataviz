library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Description of Code

# Sunshine vs. trips bar chart:
# - Filter the dataset to high quality subset: years 2014–2022 and exclude station matching "arnulf"
# - Convert sunshine hours and total counts to numeric
# - Bin daily sunshine into 2-hour groups (0–2, 2–4, …, 14–16)
# - Compute the average daily trips per sunshine group and counting station
# - Reorder stations by overall volume to improve readability
# - Plot a horizontal grouped bar chart with a high contrast color palette and cleaned axis formatting for good readability


# Data Preparation
subset_df <- df %>%
  filter(
    !is.na(datum),
    between(year(datum), 2014, 2022),
    !str_detect(str_to_lower(zaehlstelle), "arnulf")
  )

df_sun_eng <- subset_df %>%
  mutate(
    sun_hours_num = as.numeric(sonnenstunden),
    total_count_num = as.numeric(gesamt),
    # Creating group labels
    sun_group = cut(sun_hours_num, 
                    breaks = seq(0, 16, by = 2), 
                    include.lowest = TRUE, 
                    right = FALSE,
                    labels = c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-14", "14-16"))
  ) %>%
  filter(!is.na(sun_group)) %>%
  group_by(sun_group, zaehlstelle) %>%
  summarise(avg_daily_trips = mean(total_count_num, na.rm = TRUE), .groups = "drop")

# Reordering counting stations by volume for better visualization
df_sun_eng <- df_sun_eng %>%
  group_by(zaehlstelle) %>%
  mutate(total_vol = sum(avg_daily_trips)) %>%
  ungroup() %>%
  mutate(zaehlstelle = reorder(zaehlstelle, total_vol))

# Horizontal Bar Chart
ggplot(df_sun_eng, aes(y = sun_group, x = avg_daily_trips, fill = zaehlstelle)) +
  
  geom_col(color = "white", linewidth = 0.3, width = 0.8) +
  
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "Station") +
  
  scale_x_continuous(labels = function(x) paste0(x / 1000)) +
  
  labs(
    title = "Daily Sunshine vs. Average Daily Trips",
    subtitle = NULL,
    x = "Average Daily Trips (in thousands)",
    y = "Daily Sunshine (in hours)",
    caption = NULL
  ) +
  
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    
    axis.title.x = element_text(margin = margin(t = 10), face = "bold", color = "#34495e"),
    axis.title.y = element_text(margin = margin(r = 10), face = "bold", color = "#34495e"),
    axis.text.y = element_text(face = "bold", color = "#2c3e50"),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linetype = "dashed")
  )