library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# 1. Daten vorbereiten
subset_df <- df %>%
  filter(
    !is.na(datum),
    between(year(datum), 2014, 2023),
    !str_detect(str_to_lower(zaehlstelle), "arnulf")
  )

df_story_combined <- subset_df %>%
  mutate(
    year = year(as.Date(datum)),
    regen_mm = as.numeric(niederschlag),
    sun_hours = as.numeric(sonnenstunden),
    gesamt_num = as.numeric(gesamt)
  ) %>%
  group_by(year) %>%
  summarise(
    total_bikes = sum(gesamt_num, na.rm = TRUE),
    avg_rainy = mean(gesamt_num[regen_mm > 2], na.rm = TRUE),
    avg_sunny = mean(gesamt_num[sun_hours > 6 & regen_mm == 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(resilience_index = avg_rainy / avg_sunny)

# Durchschnitt berechnen
mean_resilience <- mean(df_story_combined$resilience_index, na.rm = TRUE)

# Hilfsvariable für die Farben der Punkte
df_story_combined <- df_story_combined %>%
  mutate(status = ifelse(resilience_index >= mean_resilience, "Above Average", "Below Average"))

# 2. Transformation für 40-75% Fokus
y_max <- max(df_story_combined$total_bikes, na.rm = TRUE)
rescale_index_40_75 <- function(x) { (x - 0.40) / (0.75 - 0.40) * y_max }

# 3. Plot
ggplot(df_story_combined, aes(x = year)) +
  # BARS: Gesamtvolumen
  geom_col(aes(y = total_bikes, fill = "Total Annual Volume"), alpha = 0.5) +
  
  # AVERAGE LINE (Mittellinie)
  geom_hline(aes(yintercept = rescale_index_40_75(mean_resilience), linetype = "Average Resilience"), 
             color = "black", linewidth = 0.8, alpha = 0.6) +
  
  # LINE CHART: Die Verbindungslinie bleibt grau oder dezent
  geom_line(aes(y = rescale_index_40_75(resilience_index)), color = "grey40", linewidth = 1, alpha = 0.5) +
  
  # POINTS: Blau wenn drüber, Gelb wenn drunter
  geom_point(aes(y = rescale_index_40_75(resilience_index), color = status), size = 4) +
  
  # Achsen-Konfiguration
  scale_y_continuous(
    name = "Total Annual Bicycle Trips",
    labels = scales::comma,
    limits = c(0, y_max), 
    sec.axis = sec_axis(
      ~ . / y_max * (0.75 - 0.40) + 0.40, 
      name = "Resilience Index (Rainy/Sunny Ratio)",
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0.40, 0.75, by = 0.05)
    )
  ) +
  scale_x_continuous(breaks = seq(2014, 2023, 1)) +
  
  # Manuelle Farbwahl für die Punkte (Blau und Gelb)
  scale_color_manual(values = c("Above Average" = "#0077b6", "Below Average" = "#ffb703")) +
  scale_fill_manual(values = c("Total Annual Volume" = "#bdc3c7"), name = "") +
  scale_linetype_manual(values = c("Average Resilience" = "dashed"), name = "") +
  
  labs(title = NULL, subtitle = NULL, x = "Year") + # Titel und Untertitel entfernt
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )