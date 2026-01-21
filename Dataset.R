library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)

files <- list.files("Yearly Datasets", pattern="\\.csv$", full.names = TRUE)

# Merging all CSV files together


# Enabling different date formats as date format changes from 2023 ongoing to avoid NA values when merging CSVs
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

# Parsing of Times and Numbers
df <- df %>%
  mutate(
    # Uhrzeiten: "00.00" -> "00:00"
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


View(df)

