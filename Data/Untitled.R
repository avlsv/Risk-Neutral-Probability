library(tidyverse)
library(quantmod)
library(tsibble)
library(fable)

getSymbols("ZQU24.CBT") #Sep 24
getSymbols("ZQV24.CBT") #Oct 24
getSymbols("ZQX24.CBT") #Nov 24
getSymbols("ZQZ24.CBT") #Dec 24
getSymbols("ZQF25.CBT") #Jan 24
getSymbols("ZQF26.CBT") #Jan 24



#
# ZQU24.CBT	Thirty-Day Fed Fund Futures,Sep
# ZQV24.CBT	Thirty-Day Fed Fund Futures,Oct
# ZQX24.CBT	Thirty-Day Fed Fund Futures,Nov
# ZQZ24.CBT	Thirty-Day Fed Fund Futures,Dec
# ZQF25.CBT	Thirty-Day Fed Fund Futures,Jan

ZQU24_tbl <-
  ZQU24.CBT |>
  fortify.zoo() |>
  as_tibble() |>
  pivot_longer(!Index) |>
  mutate(term = yearmonth("2024-09-01"))


ZQV24_tbl <-
  ZQV24.CBT |>
  fortify.zoo() |>
  as_tibble() |>
  pivot_longer(!Index) |>
  mutate(term = yearmonth("2024-10-01"))


ZQX24_tbl <-
  ZQX24.CBT |>
  fortify.zoo() |>
  as_tibble() |>
  pivot_longer(!Index) |>
  mutate(term = yearmonth("2024-11-01"))



ZQZ24_tbl <-
  ZQZ24.CBT |>
  fortify.zoo() |>
  as_tibble() |>
  pivot_longer(!Index) |>
  mutate(term = yearmonth("2024-12-01"))

ZQF25_tbl <-
  ZQZ24.CBT |>
  fortify.zoo() |>
  as_tibble() |>
  pivot_longer(!Index) |>
  mutate(term = yearmonth("2025-01-01"))


Sep_t_Jan <- bind_rows(ZQU24_tbl, ZQV24_tbl, ZQX24_tbl, ZQZ24_tbl, ZQF25_tbl)

write_csv(Sep_t_Jan, file = "Sep_t_Jan.csv")

read_csv("Sep_t_Jan.csv")



3#select(ends_with("Adjusted")|ends_with("Volume")) |>
#rename(price_nov_24 = ZQX24.CBT.Adjusted) |>
#mutate(return_nov_24 = (1/(price_nov_24/100)-1)*100,
#      return_nov_24_bad = 100-price_nov_24)

ZQU24_tbl <-
  ZQU24.CBT |>
  fortify.zoo() |>
  as_tibble() |>
  as_tsibble() |>
  select(ends_with("Adjusted") | ends_with("Volume")) |>
  rename(price_nov_24 = ZQU24.CBT.Adjusted) |>
  mutate(return_nov_24 = (1 / (price_nov_24 / 100) - 1) * 100,
         return_nov_24_bad = 100 - price_nov_24)
