library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# --- 1. Data Cleaning & Aggregation ---
df_clean_heat <- df %>%
  mutate(
    datum_dt = as.Date(datum),
    monat_datum = floor_date(datum_dt, "month"),
    zaehlstelle = trimws(as.character(zaehlstelle)),
    gesamt_num = as.numeric(gesamt)
  ) %>%
  group_by(monat_datum, zaehlstelle) %>%
  summarise(monats_summe = sum(gesamt_num, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(monat_datum))

# --- 2. Gap Analysis (Matrix Creation) ---
alle_monate <- seq(min(df_clean_heat$monat_datum), max(df_clean_heat$monat_datum), by = "month")
alle_stationen <- unique(df_clean_heat$zaehlstelle)
matrix_soll <- expand.grid(monat_datum = alle_monate, zaehlstelle = alle_stationen)

# --- 3. Heatmap Visualization ---
plot_data <- matrix_soll %>%
  left_join(df_clean_heat, by = c("monat_datum", "zaehlstelle")) %>%
  mutate(
    ist_da = ifelse(!is.na(monats_summe) & monats_summe > 0, "Working", "Out of Order")
  )

ggplot(plot_data, aes(x = monat_datum, y = zaehlstelle)) +
  geom_tile(aes(fill = ist_da), color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c("Working" = "#2ecc71", "Out of Order" = "#e74c3c"), 
    name = "Status"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal() +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Year",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    # Hier werden die vertikalen Linien für die Jahre definiert:
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )
