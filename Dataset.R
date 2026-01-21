library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)

# Description of Code

# Merge + clean yearly CSV datasets:
# - Load all CSVs from "Yearly Datasets" as character data to avoid type conflicts
# - Standardize column names (lowercase, '-' -> '.') and trim whitespace
# - Parse the 'datum' column across multiple date formats (YYYY-MM-DD, DD.MM.YYYY, etc.)
# - Normalize and parse start/end times, and convert selected columns to numeric
# - Combine all files into one dataframe (df) and keep the origin in 'source_file'


files <- list.files("Yearly Datasets", pattern="\\.csv$", full.names = TRUE)

parse_datum_multi <- function(x) {
  x <- str_trim(x)
  x <- na_if(x, "")
  x2 <- x %>%
    str_replace_all("/", ".") %>%
    str_replace_all("-", ".")
  out <- case_when(
    str_detect(x,  "^\\d{4}-\\d{2}-\\d{2}$") ~ as.Date(x,  format = "%Y-%m-%d"), # 2020-01-31
    str_detect(x2, "^\\d{2}\\.\\d{2}\\.\\d{4}$") ~ as.Date(x2, format = "%d.%m.%Y"), # 31.01.2020
    str_detect(x2, "^\\d{4}\\.\\d{2}\\.\\d{2}$") ~ as.Date(x2, format = "%Y.%m.%d"), # 2020.01.31
    TRUE ~ as.Date(parse_date_time(x, orders = c("ymd", "dmy", "Ymd", "dmY"), tz = "UTC"))
  )
  out
}

df <- files %>%
  set_names(basename(.)) %>%
  map(~ read_csv(.x,
                 col_types = cols(.default = col_character()),
                 show_col_types = FALSE)) %>%
  map(~ .x %>%
        rename_with(tolower) %>%
        rename_with(~ str_replace_all(.x, "-", ".")) %>%
        mutate(across(everything(), ~ ifelse(is.na(.x), NA, str_trim(.x)))) %>%
        mutate(
          datum = parse_datum_multi(datum)
        )
  ) %>%
  bind_rows(.id = "source_file")


df <- df %>%
  mutate(
    uhrzeit_start = str_replace(uhrzeit_start, "^(\\d{1,2})\\.(\\d{2})$", "\\1:\\2"),
    uhrzeit_ende  = str_replace(uhrzeit_ende,  "^(\\d{1,2})\\.(\\d{2})$", "\\1:\\2"),
    uhrzeit_start = str_replace(uhrzeit_start, "^(\\d):", "0\\1:"),
    uhrzeit_ende  = str_replace(uhrzeit_ende,  "^(\\d):", "0\\1:"),
    uhrzeit_start = parse_time(uhrzeit_start),
    uhrzeit_ende  = parse_time(uhrzeit_ende),
    
    kommentar = na_if(kommentar, ""),
    kommentar = as.character(kommentar),
    
    across(c(richtung_1, richtung_2, gesamt, `min.temp`, `max.temp`, niederschlag),
           ~ parse_number(.x, locale = locale(decimal_mark = ".")))
  )


view(df)

