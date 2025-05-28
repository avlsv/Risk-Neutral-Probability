library(tidyverse)
library(quantmod)
library(tsibble)
library(fable)
library(broom)
library(broom.mixed)
library(rstan)

options(mc.cores =  parallel::detectCores())


dataset <- read_csv("Job a186978 options.csv")

num_of_states = 15

dataset_1 <- dataset |> select(expiration, strike, call_put, bid, ask)


dataset_2 <-
  dataset_1 |>
  mutate(option_price = (bid + ask) / 2) |>
  select(-bid, -ask)

expirations <- unique(dataset_2$expiration)


results <- vector("list", length(expirations))


for (j in seq(1:length(expirations))) {
  print(expirations[j])

  dataset_2.5 <-
    dataset_2 |>
    filter(expiration == expirations[j])



  states <-
    tibble(state = seq(
      min(dataset_2$strike) - 10,
      max(dataset_2$strike) + 10,
      length.out = num_of_states
    ))

  
  dataset_3 <-
    cross_join(states, dataset_2.5) |>
    mutate(
      payoff =
        as.numeric(call_put == "Call") * pmax(state - strike, 0) +
          as.numeric(call_put == "Put") * pmax(strike - state, 0)
    ) |>
    select(-expiration)

  dataset_4 <-
    dataset_3 |>
    pivot_wider(names_from = state, values_from = payoff) |>
    select(-strike, -call_put)


  X_tbl <- dataset_4 |> select(-option_price)
  y_tbl <- dataset_4 |> select(option_price)


  stan_data_aapl <- list(
    n = nrow(X_tbl),
    k = ncol(X_tbl),
    X = X_tbl |> as.data.frame(),
    y = as.vector(y_tbl$option_price)
  )

  stan_model_aapl <-
    stan(
      "simplex.stan",
      data = stan_data_aapl,
      iter = 4000,
      chains = 4
    )

  coefs <- stan_model_aapl |>
    tidy(conf.int = T, conf.level = 0.95, conf.method = "HPDinterval")

  betas <- coefs |>
    filter(startsWith(term, "b")) |>
    mutate(state = states$state)

  plot <- ggplot(betas, aes(x = state, y = estimate)) +
    geom_col() +
    geom_errorbar(aes(max = conf.high, min = conf.low))


  results[[j]] <- list(stan_model_aapl, coefs, betas, plot)
  results
}



results[[1]][[2]]
results[[2]][[2]]
results[[3]][[2]]



