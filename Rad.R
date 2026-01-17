library(ggplot2)
library(dplyr)
library(patchwork)

# 1. Daten einlesen
data <- read.csv("rad_2025.csv")

# 2. WICHTIG: Datum umwandeln (Sonst versteht R die Zeitachse nicht)
data$datum <- as.Date(data$datum, format = "%Y.%m.%d")

# 3. Filter für Arnulfstraße
data_arnulf <- data %>% 
  filter(zaehlstelle == "Arnulf")

# 4. Plot 1: Radnutzung
# group = 1 stellt sicher, dass alle Punkte mit einer Linie verbunden werden
p1 <- ggplot(data_arnulf, aes(x = datum, y = gesamt, group = 1)) +
  geom_line(color = "#0072B2", linewidth = 1) +
  labs(
    title = "Analyse Zählstelle Arnulfstraße",
    y = "Anzahl Radfahrende",
    x = NULL
  ) +
  theme_minimal()

# 5. Plot 2: Niederschlag
p2 <- ggplot(data_arnulf, aes(x = datum, y = niederschlag)) +
  geom_col(fill = "#56B4E9") + 
  labs(
    y = "Niederschlag (mm)",
    x = "Datum"
  ) +
  theme_minimal()

# 6. Kombinieren
p1 / p2