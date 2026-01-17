library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci) # For the high-quality NPG color palette

# 1. Data Preparation
df_sun_eng <- df %>%
  mutate(
    sun_hours_num = as.numeric(sonnenstunden),
    total_count_num = as.numeric(gesamt),
    # Grouping into 2-hour intervals
    sun_group = cut(sun_hours_num, 
                    breaks = seq(0, 16, by = 2), 
                    include.lowest = TRUE, 
                    right = FALSE,
                    labels = c("0-2h", "2-4h", "4-6h", "6-8h", "8-10h", "10-12h", "12-14h", "14-16h"))
  ) %>%
  filter(!is.na(sun_group)) %>%
  group_by(sun_group, zaehlstelle) %>%
  summarise(avg_daily_trips = mean(total_count_num, na.rm = TRUE), .groups = "drop")

# 2. Plot: Horizontal Stacked Bar Chart
ggplot(df_sun_eng, aes(y = sun_group, x = avg_daily_trips, fill = zaehlstelle)) +
  # Create stacked bars with a thin white border for better separation
  geom_col(color = "white", linewidth = 0.2) +
  # Using the NPG color scale
  scale_fill_npg(name = "Station") + 
  labs(
    title = "Sunshine as a Driver for Cycling",
    subtitle = "Average daily bicycle trips based on sunshine duration per day",
    x = "Average Daily Trips (Cumulative across all stations)",
    y = "Hours of Sunshine"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(), # Removes horizontal lines for better focus
    legend.position = "bottom",
    axis.text.y = element_text(face = "bold", size = 11)
  )
