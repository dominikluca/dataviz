library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# 1. Daten vorbereiten (wie gehabt)
subset_df <- df %>%
  filter(
    !is.na(datum),
    between(year(datum), 2014, 2023),
    !str_detect(str_to_lower(zaehlstelle), "arnulf")
  )

df_sun_eng <- subset_df %>%
  mutate(
    sun_hours_num = as.numeric(sonnenstunden),
    total_count_num = as.numeric(gesamt),
    sun_group = cut(sun_hours_num, 
                    breaks = seq(0, 16, by = 2), 
                    include.lowest = TRUE, 
                    right = FALSE,
                    labels = c("0-2h", "2-4h", "4-6h", "6-8h", "8-10h", "10-12h", "12-14h", "14-16h"))
  ) %>%
  filter(!is.na(sun_group)) %>%
  group_by(sun_group, zaehlstelle) %>%
  summarise(avg_daily_trips = mean(total_count_num, na.rm = TRUE), .groups = "drop")

# 2. Plot: Horizontal Stacked Bar Chart mit Kontrast-Palette
ggplot(df_sun_eng, aes(y = sun_group, x = avg_daily_trips, fill = zaehlstelle)) +
  geom_col(color = "white", linewidth = 0.2) +
  
  # DIE PALETTE: Sonnig, aber stark kontrastierend
  scale_fill_manual(
    name = "Station",
    values = c(
      "#FFD700", # Gold/Sonne
      "#FF6B6B", # Korallenrot
      "#4ECDC4", # Türkis/Teal (starker Kontrast zu Orange)
      "#FF8C00", # Dunkelorange
      "#1A535C", # Dunkles Petrol
      "#F7FFF7", # Ganz helles Grün/Weiß
      "#FFE66D", # Pastellgelb
      "#6B4226"  # Erdbraun (für den Kontrast am Boden)
    )
  ) + 
  labs(
    title = NULL, 
    subtitle = NULL, 
    x = "Average Daily Trips (Cumulative)",
    y = "Hours of Sunshine"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(face = "bold", size = 11)
  )