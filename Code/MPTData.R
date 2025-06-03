library(readxl)
library(tidyverse)


mpd <- read_xlsx("Data/Market Probability Tracker Data.xlsx", sheet = 3)
write_csv(mpd, "Data/Market Probability Tracker Data.csv")


mpd$field |> unique()

mpd_a <-
  mpd |>
  mutate(date = as_date(date), reference_start = as_date(reference_start), value = as.numeric(value))



mpd_a |>
  filter(field == "Rate: mean", date >= as_date("2025-01-01")) |>
  ggplot(aes(x = reference_start, y = value, group = date, color = date)) +
  geom_line()
