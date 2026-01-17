library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# 1. Daten radikal säubern
df_clean_heat <- df %>%
  mutate(
    # Sicherstellen, dass es ein Datum ist
    datum_dt = as.Date(datum),
    # Monat extrahieren
    monat_datum = floor_date(datum_dt, "month"),
    # Leerzeichen aus Stationsnamen entfernen
    zaehlstelle = trimws(as.character(zaehlstelle)),
    # Wert sicher als Zahl
    gesamt_num = as.numeric(gesamt)
  ) %>%
  # Monatssumme berechnen
  group_by(monat_datum, zaehlstelle) %>%
  summarise(monats_summe = sum(gesamt_num, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(monat_datum))

# 2. Die lückenlose Matrix erstellen (Soll-Zustand)
alle_monate <- seq(min(df_clean_heat$monat_datum), max(df_clean_heat$monat_datum), by = "month")
alle_stationen <- unique(df_clean_heat$zaehlstelle)
matrix_soll <- expand.grid(monat_datum = alle_monate, zaehlstelle = alle_stationen)

# 3. Zusammenführen und Status prüfen
plot_data <- matrix_soll %>%
  left_join(df_clean_heat, by = c("monat_datum", "zaehlstelle")) %>%
  mutate(
    # Wenn monats_summe > 0, dann sind Daten da (TRUE)
    ist_da = ifelse(!is.na(monats_summe) & monats_summe > 0, TRUE, FALSE)
  )

# 4. Plot (Farben getauscht für bessere Sichtbarkeit)
ggplot(plot_data, aes(x = monat_datum, y = zaehlstelle)) +
  geom_tile(aes(fill = ist_da), color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"), 
    labels = c("Ausfall / Keine Daten", "Daten vorhanden"),
    name = "Status"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal() +
  labs(
    title = "Check: Wo liegen die Datenlücken?",
    subtitle = "Grün = Daten OK | Rot = Lücke (0 oder NA)",
    x = "Jahr", y = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))