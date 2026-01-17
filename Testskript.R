# Bibliothek laden (falls du read_csv nutzen willst)
# Falls du readr noch nicht hast: install.packages("readr")
library(readr)

# 1. Daten einlesen (NUR der Dateiname, da sie im Projektordner liegt!)
daten <- read_csv("minuten_werte_2024_korr2.csv")

# 2. Kurzer Check: Hat es geklappt? (Zeigt die ersten 6 Zeilen)
head(daten)

# 3. Eine einfache Operation: Zusammenfassung der Daten
summary(daten)



