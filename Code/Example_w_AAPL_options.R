library(tidyverse)
library(quantmod)
library(broom)
library(broom.mixed)
library(rstan)
library(scales)
library(patchwork)


options(mc.cores = parallel::detectCores())



estimation_procedure <- function(dataset, model = "simplex.stan", states = seq(120 - 20, 260 + 20, by = 10), iter = 4000,
                                 chains = 4) {
  print(dataset$date[1])

  dataset_1 <- dataset |> select(expiration, strike, call_put, bid, ask)




  dataset_2 <-
    dataset_1 |>
    mutate(option_price = (bid + ask) / 2) |>
    select(-bid, -ask)




  expirations <- unique(dataset_2$expiration)

  current_prices <- aapl |>
    filter(date == dataset$date[1]) |>
    select(-date) |>
    mutate(count = (length(expirations))) |>
    uncount(count) |>
    mutate(call_put = "Call", strike = 0, expiration = expirations, option_price = price) |>
    select(-price)



  results <- vector("list", length(expirations))

  dataset_2.1 <- bind_rows(dataset_2, current_prices)


  for (j in seq(1:length(expirations))) {
    print(expirations[j])

    dataset_2.5 <-
      dataset_2 |>
      filter(expiration == expirations[j])


    states_vec <- as.vector(states)

    states_tbl <-
      tibble(state = states_vec)


    dataset_3 <-
      cross_join(states_tbl, dataset_2.5) |>
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
        as.character(model),
        data = stan_data_aapl,
        iter = iter,
        chains = chains
      )

    coefs_90 <- stan_model_aapl |>
      tidy(conf.int = T, conf.level = 0.90, conf.method = "HPDinterval")


    coefs_95 <- stan_model_aapl |>
      tidy(conf.int = T, conf.level = 0.95, conf.method = "HPDinterval") |>
      rename(
        conf.low.hpd.0.95 = conf.low,
        conf.high.hpd.0.95 = conf.high
      )

    coefs_50 <- stan_model_aapl |>
      tidy(conf.int = T, conf.level = 0.50, conf.method = "HPDinterval") |>
      rename(
        conf.low.hpd.50 = conf.low,
        conf.high.hpd.50 = conf.high
      )



    coefs <- coefs_90 |>
      left_join(coefs_95) |>
      left_join(coefs_50)



    betas <- coefs |>
      filter(startsWith(term, "b")) |>
      mutate(state = states_tbl$state)

    plot <- ggplot(betas, aes(x = state, y = estimate)) +
      geom_col(color = "black", fill = "gray", alpha = 0.8) +
      geom_errorbar(aes(max = conf.high, min = conf.low), width = 4) +
      scale_y_continuous("", breaks = extended_breaks(n = 6)) +
      scale_x_continuous("State", breaks = extended_breaks(n = round(length(states) / 2) + 1)) +
      theme_light()


    results[[j]] <- list(stan_model_aapl, coefs, betas, plot)
    results
  }
  return(results)
}

state_space <- seq(100, 290, by = 10)
length(state_space)


aapl <- getSymbols("AAPL", src = "yahoo", auto.assign = FALSE) |>
  fortify.zoo(z, name = "date") |>
  as_tibble() |>
  select(date, AAPL.Close) |>
  rename(price = AAPL.Close) |>
  mutate(date = as_date(date)) |>
  arrange(date)

<<<<<<< Updated upstream
iter=6000
chains=6
=======
iter <- 10000
chains <- 8
>>>>>>> Stashed changes

results_25 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-03-25.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

saveRDS(results_25, file = "Data/Results/results_25.RData")


results_26 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-03-26.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

saveRDS(results_26, file = "Data/Results/results_26.RData")


results_27 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-03-27.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

saveRDS(results_27, file = "Data/Results/results_27.RData")



results_28 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-03-28.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

saveRDS(results_28, file = "Data/Results/results_28.RData")



results_31 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-03-31.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

saveRDS(results_31, file = "Data/Results/results_31.RData")





results_01 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-01.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

saveRDS(results_01, file = "Data/Results/results_01.RData")




# readRDS("data/results/results_01.RData")

# results_01_alt <-
#   estimation_procedure(
#     dataset = read_csv("data/AAPL options 2025-04-01.csv", show_col_types = F),
#     states = state_space,
#     model = "simplex_alternative.stan"
#   )

# results_02_alt <-
#   estimation_procedure(
#     dataset = read_csv("data/AAPL options 2025-04-02.csv", show_col_types = F),
#     states =  seq(100, 290, by = 20),
#     model = "simplex_alternative.stan"
#   )



results_02 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-02.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

saveRDS(results_02, file = "Data/Results/results_02.RData")


results_03 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-03.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

saveRDS(results_03, file = "data/results/results_03.RData")


results_04 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-04.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

saveRDS(results_04, file = "data/results/results_04.RData")

readRDS("data/results/results_04.RData")

results_07 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-07.csv", show_col_types = F),
    states = state_space,
    iter = iter,
    chains = chains
  )

results_07_15 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-07.csv", show_col_types = F),
    states = seq(100, 295, by = 15)
  )


saveRDS(results_07, file = "data/results/results_07.RData")

results_08 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-08.csv", show_col_types = F),
    states = state_space
  )
saveRDS(results_08, file = "data/results/results_08.RData")


results_09 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-09.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_09, file = "data/results/results_09.RData")


results_10 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-10.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_10, file = "data/results/results_10.RData")


results_11 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-11.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_11, file = "data/results/results_11.RData")


results_14 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-14.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_14, file = "data/results/results_14.RData")



results_15 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-15.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_15, file = "data/results/results_15.RData")


results_16 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-16.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_16, file = "data/results/results_16.RData")

results_17 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-17.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_17, file = "data/results/results_17.RData")







# alternative specification
# results_23_1 <-
#   estimation_procedure(
#     dataset = read_csv("data/AAPL options 2025-05-23.csv", show_col_types = F),
#     states = state_space,
#     model = "simplex_alternative.stan"
#   )
