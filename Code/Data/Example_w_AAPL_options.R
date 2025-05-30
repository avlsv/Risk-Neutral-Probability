library(tidyverse)
library(quantmod)
library(tsibble)
library(fable)
library(broom)
library(broom.mixed)
library(rstan)
library(scales)

options(mc.cores = parallel::detectCores())



estimation_procedure <- function(dataset, states= seq(120 - 20,260 + 20,  by = 10)) {
  print(dataset$date[1])
  
  dataset_1 <- dataset |> select(expiration, strike, call_put, bid, ask)

  nn <-
    dataset_1 |> mutate(spread = (ask - bid) / bid * 100)


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
        "simplex.stan",
        data = stan_data_aapl,
        iter = 4000,
        chains = 4
      )

    coefs <- stan_model_aapl |>
      tidy(conf.int = T, conf.level = 0.95, conf.method = "HPDinterval")

    betas <- coefs |>
      filter(startsWith(term, "b")) |>
      mutate(state = states_tbl$state)

    plot <- ggplot(betas, aes(x = state, y = estimate)) +
      geom_col() +
      geom_errorbar(aes(max = conf.high, min = conf.low)) +
      scale_y_continuous("Probability", breaks = extended_breaks(n = 6))+
      scale_x_continuous("State", breaks = extended_breaks(n = round(length(states)/2)+1)) 


    results[[j]] <- list(stan_model_aapl, coefs, betas, plot)
    results
  }
  return(results)
}


results_23 <-
  estimation_procedure(read_csv("AAPL options 2025-05-23.csv"), 
                       seq(110, 300, by = 5))#length=20


results_27 <-
  estimation_procedure(read_csv("AAPL options 2025-05-27.csv"), 
                       seq(110, 300, by = 10))


beta_coefs <-
  bind_rows(
    results_23[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-05-23"),
    results_23[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-05-23"),
    results_23[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-05-23"),
    results_27[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-05-27"),
    results_27[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-05-27"),
    results_27[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-05-27"),
  )



# Function to calculate discrete quantiles (e.g., 5% and 95%)
get_discrete_quantiles <- function(prob_vec, bins, probs) {
  cdf <- cumsum(prob_vec)
  sapply(probs, function(p) bins[which(cdf >= p)[1]])
}





# Calculate quantiles for each group
summaries <- beta_coefs |>
  group_by(expiration, date) |>
  summarise(
    q5 = get_discrete_quantiles(estimate, state, 0.05),
    q25 = get_discrete_quantiles(estimate, state, 0.05),
    q50 = get_discrete_quantiles(estimate, state, 0.95),
    q75 = get_discrete_quantiles(estimate, state, 0.95),
    q95 = get_discrete_quantiles(estimate, state, 0.95),
    mean = sum(estimate * state)
  )|> ungroup()|> relocate(date)|> arrange(date)



ggplot(beta_coefs, aes(x = state, y = estimate, group = expiration)) +
  geom_col() +
  facet_grid(date~expiration) +
  geom_vline(data = summaries, aes(xintercept = q5), linetype = "dashed", linewidth = 0.3) +
  geom_vline(data = summaries, aes(xintercept = q95), linetype = "dashed", linewidth = 0.3) +
  geom_vline(data = summaries, aes(xintercept = mean), linetype = "solid", linewidth = 0.3)
  scale_x_continuous("State", breaks = extended_breaks(n = 10)) +
  scale_y_continuous("Probability")


alphas_23 <-
  bind_rows(
    results_23[[1]][[2]] |>
      filter(term == "alpha"),
    results_23[[2]][[2]] |>
      filter(term == "alpha"),
    results_23[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = rep("2025-05-23")))


alphas_27 <-
  bind_rows(
    results_27[[1]][[2]] |>
      filter(term == "alpha"),
    results_27[[2]][[2]] |>
      filter(term == "alpha"),
    results_27[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = rep("2025-05-27")))

alphas <- bind_rows(alphas_23, alphas_27)



extract(results_27[[3]][[1]])$lambda|> hist()

library(patchwork)


results_23[[3]][[4]]

a+b
