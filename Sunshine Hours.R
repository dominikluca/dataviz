library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# 1. Data Preparation
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

# 2. Plotting
ggplot(df_sun_eng, aes(y = sun_group, x = avg_daily_trips, fill = zaehlstelle)) +
  # Bars with slight transparency and white borders
  geom_col(color = "white", linewidth = 0.3, width = 0.8) +
  
  # Professional color palette (Viridis)
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "Station") +
  
  # Formatting X-axis to show 5k, 10k, etc.
  scale_x_continuous(labels = function(x) paste0(x / 1000)) +
  
  # Labels and removing title/caption as requested
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Average Daily Trips (in thousands)",
    y = "Daily Sunshine (in hours)",
    caption = NULL
  ) +
  
  # Minimal theme customization
  theme_minimal(base_size = 12) + 
  theme(
    # Move legend to top
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    
    # Axis styling
    axis.title.x = element_text(margin = margin(t = 10), face = "bold", color = "#34495e"),
    axis.title.y = element_text(margin = margin(r = 10), face = "bold", color = "#34495e"),
    axis.text.y = element_text(face = "bold", color = "#2c3e50"),
    
    # Cleaning up grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linetype = "dashed")
  )