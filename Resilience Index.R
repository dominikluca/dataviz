library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(scales)

# Description of Code

# Cycling resilience plot:
# - Filter the dataset to high quality subset: years 2014–2022 and exclude station matching "arnulf"
# - Convert rainfall, sunshine hours, and total counts to numeric and extract the year
# - Aggregate by year to compute total annual trips and average trips on rainy vs. sunny days
# - Calculate a yearly resilience index (avg trips on rainy days / avg trips on sunny days)
# - Compute the overall mean resilience and label each year as above/below average
# - Plot total annual volume as background bars and overlay the resilience index as a line + points
# - Use a dual y-axis with a custom transformation to align index values to the volume scale


# Data Preparation
subset_df <- df %>%
  filter(
    !is.na(datum),
    between(year(datum), 2014, 2022),
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

mean_resilience <- mean(df_story_combined$resilience_index, na.rm = TRUE)


df_story_combined <- df_story_combined %>%
  mutate(status = ifelse(resilience_index >= mean_resilience, "Above Average", "Below Average"))

# Dual Axis - Dynamic Scaling
idx_min <- 0.35  
idx_max <- 0.80  
y_max_vol <- max(df_story_combined$total_bikes, na.rm = TRUE) * 1.10 

scale_idx_to_vol <- function(x) { 
  (x - idx_min) / (idx_max - idx_min) * y_max_vol 
}

scale_vol_to_idx <- function(y) { 
  (y / y_max_vol) * (idx_max - idx_min) + idx_min 
}

# Bar Chart and Line + Point Chart
ggplot(df_story_combined, aes(x = year)) +
  
  geom_col(aes(y = total_bikes, fill = "Total Volume"), width = 0.6, alpha = 0.3) +
  
  geom_hline(yintercept = scale_idx_to_vol(mean_resilience), 
             color = "grey50", linetype = "dashed", linewidth = 0.5) +
  
  geom_line(aes(y = scale_idx_to_vol(resilience_index)), 
            color = "grey60", linewidth = 0.8, alpha = 0.6) +
  
  geom_point(aes(y = scale_idx_to_vol(resilience_index), color = status), 
             size = 5, stroke = 1.5, fill = "white", shape = 21) + 
  
  scale_y_continuous(
    name = "Total Annual Trips (Millions)",
    labels = label_number(scale = 1e-6, suffix = "M"), 
    limits = c(0, y_max_vol),
    expand = c(0, 0),
    
    sec.axis = sec_axis(
      trans = ~ scale_vol_to_idx(.), 
      name = "Resilience Index (Rain / Sun Ratio)",
      labels = scales::percent_format(accuracy = 1)
    )
  ) +
  scale_x_continuous(breaks = 2014:2022) +
  
  scale_color_manual(values = c("Above Average" = "#0077b6", "Below Average" = "#ffb703"), 
                     name = "Resilience Status") +
  scale_fill_manual(values = c("Total Volume" = "#7f8c8d"), name = "") +
  
  labs(
    title = "Cycling Resilience: Annual Volume vs. Weather Impact",
    x = "Year"
  ) +

  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#2c3e50"),
    plot.subtitle = element_text(color = "grey50", size = 11, margin = margin(b = 20)),
    
    legend.position = "top",
    legend.justification = "left",
    legend.box.margin = margin(l = -10),
    
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y.left = element_text(color = "#2c3e50", face = "bold", margin = margin(r = 10)),
    axis.text.y.left = element_text(color = "#2c3e50"),
    
    axis.title.y.right = element_text(color = "#2c3e50", face = "bold", margin = margin(l = 10)),
    axis.text.y.right = element_text(color = "#2c3e50"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )