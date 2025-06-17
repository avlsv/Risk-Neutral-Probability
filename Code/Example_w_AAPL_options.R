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

    coefs <- stan_model_aapl |>
      tidy(conf.int = T, conf.level = 0.90, conf.method = "HPDinterval")

    betas <- coefs |>
      filter(startsWith(term, "b")) |>
      mutate(state = states_tbl$state)

    plot <- ggplot(betas, aes(x = state, y = estimate)) +
      geom_col(color = "black", fill = "gray", alpha = 0.8) +
      geom_errorbar(aes(max = conf.high, min = conf.low), width = 4) +
      scale_y_continuous("Probability", breaks = extended_breaks(n = 6)) +
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


results_25 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-03-25.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_25, file = "Data/Results/results_25.RData")


results_27 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-03-27.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_27, file = "Data/Results/results_27.RData")



results_28 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-03-28.csv", show_col_types = F),
    states = state_space, iter = 6000
  )

saveRDS(results_28, file = "Data/Results/results_28.RData")



results_31 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-03-31.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_31, file = "Data/Results/results_31.RData")





results_01 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-01.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_01, file = "Data/Results/results_01.RData")


results_01_15 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-01.csv", show_col_types = F),
    states = seq(100, 295, by = 15)
  )


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
    states = state_space
  )

saveRDS(results_02, file = "Data/Results/results_02.RData")

readRDS("Data/Results/results_02.RData")

results_03 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-03.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_03, file = "data/results/results_03.RData")


results_04 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-04.csv", show_col_types = F),
    states = state_space
  )

saveRDS(results_04, file = "data/results/results_04.RData")

readRDS("data/results/results_04.RData")

results_07 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-04-07.csv", show_col_types = F),
    states = state_space
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




expirations <-
  read_csv("data/AAPL options 2025-04-01.csv",
    show_col_types = F
  )$expiration |>
  unique()



beta_coefs_full <-
  bind_rows(
    results_25[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-03-25"),
    results_25[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-03-25"),
    results_25[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-03-25"),
    results_27[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-03-27"),
    results_27[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-03-27"),
    results_27[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-03-27"),
    results_28[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-03-28"),
    results_28[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-03-28"),
    results_28[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-03-28"),
    results_31[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-03-31"),
    results_31[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-03-31"),
    results_31[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-03-31"),
    results_01[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-01"),
    results_01[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-01"),
    results_01[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-01"),
    results_02[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-02"),
    results_02[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-02"),
    results_02[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-02"),
    results_04[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-04"),
    results_04[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-04"),
    results_04[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-04"),
    results_07[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-07"),
    results_07[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-07"),
    results_07[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-07"),
    results_08[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-08"),
    results_08[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-08"),
    results_08[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-08"),
    results_09[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-09"),
    results_09[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-09"),
    results_09[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-09"),
    results_10[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-10"),
    results_10[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-10"),
    results_10[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-10"),
    results_11[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-11"),
    results_11[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-11"),
    results_11[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-11"),
    results_14[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-14"),
    results_14[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-14"),
    results_14[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-14"),
    results_15[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-15"),
    results_15[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-15"),
    results_15[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-15"),
    results_16[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-16"),
    results_16[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-16"),
    results_16[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-16"),
    results_17[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-17"),
    results_17[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-17"),
    results_17[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-17")
  ) |> mutate(date = as_date(date))

beta_coefs <- beta_coefs_full |> filter(date %in% c("2025-04-01", "2025-04-04"))


# Function to calculate discrete quantiles (e.g., 5% and 95%)
get_discrete_quantiles <- function(prob_vec, bins, probs) {
  cdf <- cumsum(prob_vec)
  sapply(probs, function(p) bins[which(cdf >= p)[1]])
}


price_01 <- filter(aapl, date == "2025-04-01")$price
price_04 <- filter(aapl, date == "2025-04-04")$price


# Calculate quantiles for each group




summaries_full <- beta_coefs_full |>
  group_by(expiration, date) |>
  summarise(
    q5 = get_discrete_quantiles(estimate, state, 0.05),
    q25 = get_discrete_quantiles(estimate, state, 0.25),
    q50 = get_discrete_quantiles(estimate, state, 0.50),
    q75 = get_discrete_quantiles(estimate, state, 0.75),
    q95 = get_discrete_quantiles(estimate, state, 0.95),
    iqr = (q75 - q25) / 1.35,
    mean = sum(estimate * state)
  ) |>
  ungroup() |>
  relocate(date) |>
  arrange(date)


summaries <- summaries_full |>
  filter(date %in% c("2025-04-01", "2025-04-04")) |>
  mutate(
    price =
      c(
        rep(price_01, length(expirations)),
        rep(price_04, length(expirations))
      )
  )


beta_coefs_plot <-
  ggplot(beta_coefs, aes(x = state, y = estimate, group = expiration)) +
  geom_col(color = "black", fill = "gray", alpha = 0.8) +
  facet_grid(date ~ as_date(expiration)) +
  geom_vline(data = summaries, aes(xintercept = q5), linetype = "dashed", linewidth = 0.3) +
  geom_vline(data = summaries, aes(xintercept = q95), linetype = "dashed", linewidth = 0.3) +
  geom_vline(data = summaries, aes(xintercept = mean), linetype = "solid", linewidth = 0.3) +
  scale_x_continuous("State", breaks = extended_breaks(n = 10)) +
  scale_y_continuous("Probability") +
  theme_light()


beta_coefs_plot


ggsave("betas.pdf",
  beta_coefs_plot,
  path = "~/Documents/Risk-Neutral-Probability/Figures/",
  width = 297 / 1.3,
  height = 210 / 1.3,
  units = "mm"
)







alphas_01 <-
  bind_rows(
    results_01[[1]][[2]] |>
      filter(term == "alpha"),
    results_01[[2]][[2]] |>
      filter(term == "alpha"),
    results_01[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-01")))



alphas_02 <-
  bind_rows(
    results_02[[1]][[2]] |>
      filter(term == "alpha"),
    results_02[[2]][[2]] |>
      filter(term == "alpha"),
    results_02[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-02")))


alphas_03 <-
  bind_rows(
    results_03[[1]][[2]] |>
      filter(term == "alpha"),
    results_03[[2]][[2]] |>
      filter(term == "alpha"),
    results_03[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-03")))




alphas_04 <-
  bind_rows(
    results_04[[1]][[2]] |>
      filter(term == "alpha"),
    results_04[[2]][[2]] |>
      filter(term == "alpha"),
    results_04[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-04")))

alphas_07 <-
  bind_rows(
    results_07[[1]][[2]] |>
      filter(term == "alpha"),
    results_07[[2]][[2]] |>
      filter(term == "alpha"),
    results_07[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-07")))




alphas_08 <-
  bind_rows(
    results_08[[1]][[2]] |>
      filter(term == "alpha"),
    results_08[[2]][[2]] |>
      filter(term == "alpha"),
    results_08[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-08")))


alphas_09 <-
  bind_rows(
    results_09[[1]][[2]] |>
      filter(term == "alpha"),
    results_09[[2]][[2]] |>
      filter(term == "alpha"),
    results_09[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-09")))



alphas_10 <-
  bind_rows(
    results_10[[1]][[2]] |>
      filter(term == "alpha"),
    results_10[[2]][[2]] |>
      filter(term == "alpha"),
    results_10[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-10")))


alphas_11 <-
  bind_rows(
    results_11[[1]][[2]] |>
      filter(term == "alpha"),
    results_11[[2]][[2]] |>
      filter(term == "alpha"),
    results_11[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-11")))


alphas_14 <-
  bind_rows(
    results_14[[1]][[2]] |>
      filter(term == "alpha"),
    results_14[[2]][[2]] |>
      filter(term == "alpha"),
    results_14[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-14")))


alphas_15 <-
  bind_rows(
    results_15[[1]][[2]] |>
      filter(term == "alpha"),
    results_15[[2]][[2]] |>
      filter(term == "alpha"),
    results_15[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-15")))



alphas_16 <-
  bind_rows(
    results_16[[1]][[2]] |>
      filter(term == "alpha"),
    results_16[[2]][[2]] |>
      filter(term == "alpha"),
    results_16[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-16")))


alphas_17 <-
  bind_rows(
    results_17[[1]][[2]] |>
      filter(term == "alpha"),
    results_17[[2]][[2]] |>
      filter(term == "alpha"),
    results_17[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-04-17")))





alphas_28 <-
  bind_rows(
    results_28[[1]][[2]] |>
      filter(term == "alpha"),
    results_28[[2]][[2]] |>
      filter(term == "alpha"),
    results_28[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-03-28")))


alphas_31 <-
  bind_rows(
    results_31[[1]][[2]] |>
      filter(term == "alpha"),
    results_31[[2]][[2]] |>
      filter(term == "alpha"),
    results_31[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = as_date("2025-03-31")))


alphas_27 <-
  bind_rows(
    results_27[[1]][[2]] |>
      filter(term == "alpha"),
    results_27[[2]][[2]] |>
      filter(term == "alpha"),
    results_27[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(
    expiration = expirations,
    date = as_date("2025-03-27")
  ))


alphas_25 <-
  bind_rows(
    results_25[[1]][[2]] |>
      filter(term == "alpha"),
    results_25[[2]][[2]] |>
      filter(term == "alpha"),
    results_25[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(
    expiration = expirations,
    date = as_date("2025-03-25")
  ))



alphas <-
  bind_rows(
    alphas_25,
    alphas_27,
    alphas_28,
    alphas_31,
    alphas_01,
    alphas_02,
    alphas_03,
    alphas_04,
    alphas_07,
    alphas_08,
    alphas_09,
    alphas_10, 
    alphas_11, 
    alphas_12, 
    alphas_14, 
    alphas_16, alphas_17
  )




two_alphas_plot <-
  ggplot(
    alphas |> filter(date %in% c(as_date("2025-04-01"), as_date("2025-04-04"))),
    aes(x = as_factor(as.character(expiration)), y = estimate, group = date, color = as_factor(as.character(date)))
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(min = conf.low, max = conf.high), width = .3, position = position_dodge(width = 0.5)) +
  scale_x_discrete("Expiration Date") +
  scale_y_continuous(expression(alpha ~ "Estimate"), breaks = extended_breaks(n = 6)) +
  theme_minimal() +
  labs(color = "Date") +
  theme(legend.position = "bottom")
two_alphas_plot

ggsave("two_alphas_plot.pdf",
  two_alphas_plot,
  path = "~/Documents/Risk-Neutral-Probability/Figures/",
  width = 297 / (1.6 * 1.2),
  height = 210 / 1.6,
  units = "mm"
)

all_alphas_plot <-
  ggplot(alphas, aes(x = date, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray50", alpha = 0.3) +
  geom_vline(xintercept = as_date("2025-04-02"), color = "darkred") +
  geom_line() +
  geom_point() +
  scale_x_date("") +
  scale_y_continuous(expression(alpha ~ "Estimate"), breaks = extended_breaks(n = 6)) +
  facet_wrap(~expiration, nrow = 3, scales = "free_y") +
  labs(color = "Date") +
  theme_light() +
  theme(legend.position = "bottom")
all_alphas_plot

ggsave("all_alphas_plot.pdf",
  all_alphas_plot,
  path = "~/Documents/Risk-Neutral-Probability/Figures/",
  width = 297 / 1.6 ,
  height = 210 / 1.6,
  units = "mm"
)


for (i in seq(1, length(expirations))) {
  ggsave(paste("betas_01_", as.character(i), ".pdf", sep = ""),
    results_01[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )
  ggsave(paste("betas_04_", as.character(i), ".pdf", sep = ""),
    results_04[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )

  # ggsave(paste("betas_24_", as.character(i), ".pdf", sep = ""),
  #        results_24[[i]][[4]],
  #        path = "~/Documents/Risk-Neutral-Probability/Figures/",
  #        width = 297 / 1.6,
  #        height = 210 / 1.6,
  #        units = "mm"
  # )

  ggsave(paste("betas_25_", as.character(i), ".pdf", sep = ""),
    results_25[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )

  ggsave(paste("betas_27_", as.character(i), ".pdf", sep = ""),
    results_27[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )

  ggsave(paste("betas_28_", as.character(i), ".pdf", sep = ""),
    results_28[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )

  ggsave(paste("betas_31_", as.character(i), ".pdf", sep = ""),
    results_31[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )

  ggsave(paste("betas_02_", as.character(i), ".pdf", sep = ""),
    results_02[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )

  ggsave(paste("betas_03_", as.character(i), ".pdf", sep = ""),
    results_03[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )

  ggsave(paste("betas_07_", as.character(i), ".pdf", sep = ""),
    results_07[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )

  ggsave(paste("betas_08_", as.character(i), ".pdf", sep = ""),
    results_08[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )

  ggsave(paste("betas_09_", as.character(i), ".pdf", sep = ""),
    results_09[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )
  ggsave(paste("betas_10_", as.character(i), ".pdf", sep = ""),
         results_10[[i]][[4]],
         path = "~/Documents/Risk-Neutral-Probability/Figures/",
         width = 297 / 1.6,
         height = 210 / 1.6,
         units = "mm"
  )
  
  ggsave(paste("betas_11_", as.character(i), ".pdf", sep = ""),
         results_11[[i]][[4]],
         path = "~/Documents/Risk-Neutral-Probability/Figures/",
         width = 297 / 1.6,
         height = 210 / 1.6,
         units = "mm"
  )
  
  

  
  ggsave(paste("betas_14_", as.character(i), ".pdf", sep = ""),
         results_14[[i]][[4]],
         path = "~/Documents/Risk-Neutral-Probability/Figures/",
         width = 297 / 1.6,
         height = 210 / 1.6,
         units = "mm"
  )
  
  ggsave(paste("betas_15_", as.character(i), ".pdf", sep = ""),
         results_15[[i]][[4]],
         path = "~/Documents/Risk-Neutral-Probability/Figures/",
         width = 297 / 1.6,
         height = 210 / 1.6,
         units = "mm"
  )
  
  
  ggsave(paste("betas_16_", as.character(i), ".pdf", sep = ""),
         results_16[[i]][[4]],
         path = "~/Documents/Risk-Neutral-Probability/Figures/",
         width = 297 / 1.6,
         height = 210 / 1.6,
         units = "mm"
  )
  
  
  ggsave(paste("betas_17_", as.character(i), ".pdf", sep = ""),
         results_17[[i]][[4]],
         path = "~/Documents/Risk-Neutral-Probability/Figures/",
         width = 297 / 1.6,
         height = 210 / 1.6,
         units = "mm"
  )
  
  
  
  
}








alpha_histogram_1 <-
  ggplot(as_tibble(extract(results_01[[1]][[1]])$alpha), aes(x = value)) +
  geom_histogram(aes(y = ..density..), alpha = 0.15, color = "black", fill = "#F8766D") +
  geom_histogram(
    data = as_tibble(extract(results_04[[1]][[1]])$alpha),
    aes(y = ..density..),
    alpha = 0.15, color = "black", fill = "#00BFC4"
  ) +
  labs(x = "Alpha", y = "") +
  theme_light()

alpha_histogram_2 <-
  ggplot(as_tibble(extract(results_01[[2]][[1]])$alpha), aes(x = value)) +
  geom_histogram(aes(y = ..density..), alpha = 0.15, color = "black", fill = "#F8766D") +
  geom_histogram(
    data = as_tibble(extract(results_04[[2]][[1]])$alpha),
    aes(y = ..density..),
    alpha = 0.15, color = "black", fill = "#00BFC4"
  ) +
  labs(x = "Alpha", y = "") +
  theme_light()

alpha_histogram_3 <-
  ggplot(as_tibble(extract(results_01[[3]][[1]])$alpha), aes(x = value)) +
  geom_histogram(aes(y = ..density..), alpha = 0.15, color = "black", fill = "#F8766D") +
  geom_histogram(
    data = as_tibble(extract(results_04[[3]][[1]])$alpha),
    aes(y = ..density..),
    alpha = 0.15, color = "black", fill = "#00BFC4"
  ) +
  labs(x = "Alpha", y = "") +
  theme_light()


alpha_histogram <- alpha_histogram_1

alpha_histogram

ggsave("alpha_histogram.pdf",
  alpha_histogram,
  path = "~/Documents/Risk-Neutral-Probability/Figures/",
  width = 297 / 1.6,
  height = 210 / 1.6,
  units = "mm"
)


# two-sample Wilcoxon (Mann-Whitney) tests

wilcox.test(
  extract(results_04[[1]][[1]])$alpha,
  extract(results_07[[1]][[1]])$alpha,
  paired = F
)




summaries_plot <-
  ggplot(summaries, aes(x = as_factor(as.character(expiration)), y = mean, group = date, color = as.character(date))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(min = q5, max = q95), width = .5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(min = q25, max = q75), width = .2, position = position_dodge(width = 0.5)) +
  geom_line(aes(y = price), position = position_dodge(width = 0.5), linetype = "dashed") +
  scale_x_discrete("Expiration Date") +
  scale_y_continuous("Price", breaks = extended_breaks(n = 6)) +
  labs(color = "Date") +
  theme_light() +
  theme(legend.position = "bottom")
summaries_plot

ggsave("summaries_plot.pdf",
  summaries_plot,
  path = "~/Documents/Risk-Neutral-Probability/Figures/",
  width = 297 / 1.6,
  height = 210 / 1.6,
  units = "mm"
)


summaries_plot_full <-
  ggplot(summaries_full, aes(x = date, y = mean, group = expiration)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = "gray50", alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "gray50", alpha = 0.3) +
  geom_vline(xintercept = as_date("2025-04-02"), color = "darkred") +
  geom_point() +
  geom_line() +
  facet_wrap(~expiration, nrow = 3, scales = "free_y") +
  scale_x_date("") +
  scale_y_continuous("Price", breaks = extended_breaks(n = 6)) +
  labs(color = "Date") +
  theme_light()
summaries_plot_full

ggsave("summaries_plot_full.pdf",
       summaries_plot_full,
  path = "~/Documents/Risk-Neutral-Probability/Figures/",
  width = 297 / 1.6  ,
  height = 210 / 1.6,
  units = "mm"
)






ggplot(results_01[[1]][[3]], aes(x = state, y = estimate)) +
  geom_col() +
  geom_errorbar(aes(max = conf.high, min = conf.low), width = 4) +
  scale_y_continuous("Probability", breaks = extended_breaks(n = 6)) +
  scale_x_continuous("State") +
  theme_light()
