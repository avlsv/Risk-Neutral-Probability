library(tidyverse)
library(quantmod)
library(broom)
library(broom.mixed)
library(rstan)
library(scales)
library(patchwork)


options(mc.cores = parallel::detectCores())



estimation_procedure <- function(dataset, model = "simplex.stan", states = seq(120 - 20, 260 + 20, by = 10)) {
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
        iter = 4000,
        chains = 4
      )

    coefs <- stan_model_aapl |>
      tidy(conf.int = T, conf.level = 0.90, conf.method = "HPDinterval")

    betas <- coefs |>
      filter(startsWith(term, "b")) |>
      mutate(state = states_tbl$state)

    plot <- ggplot(betas, aes(x = state, y = estimate)) +
      geom_col() +
      geom_errorbar(aes(max = conf.high, min = conf.low), width = 4) +
      scale_y_continuous("Probability", breaks = extended_breaks(n = 6)) +
      scale_x_continuous("State", breaks = extended_breaks(n = round(length(states) / 2) + 1)) +
      theme_light()


    results[[j]] <- list(stan_model_aapl, coefs, betas, plot)
    results
  }
  return(results)
}

state_space <- seq(120, 280, by = 10)
length(state_space)


aapl <- getSymbols("AAPL", src = "yahoo", auto.assign = FALSE) |>
  fortify.zoo(z, name = "date") |>
  as_tibble() |>
  select(date, AAPL.Close) |>
  rename(price = AAPL.Close) |>
  mutate(date = as_date(date)) |>
  arrange(date)


results_23 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-05-23.csv", show_col_types = F),
    states = state_space
  )




results_27 <-
  estimation_procedure(
    dataset = read_csv("data/AAPL options 2025-05-27.csv", show_col_types = F),
    states = state_space
  )


# alternative specification
# results_23_1 <-
#   estimation_procedure(
#     dataset = read_csv("data/AAPL options 2025-05-23.csv", show_col_types = F),
#     states = state_space,
#     model = "simplex_alternative.stan"
#   )




expirations <-
  read_csv("data/AAPL options 2025-05-27.csv",
    show_col_types = F
  )$expiration |>
  unique()



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



price_23 <- filter(aapl, date == "2025-05-23")$price
price_27 <- filter(aapl, date == "2025-05-27")$price

# Calculate quantiles for each group
summaries <- beta_coefs |>
  group_by(expiration, date) |>
  summarise(
    q5 = get_discrete_quantiles(estimate, state, 0.05),
    q25 = get_discrete_quantiles(estimate, state, 0.25),
    q50 = get_discrete_quantiles(estimate, state, 0.50),
    q75 = get_discrete_quantiles(estimate, state, 0.75),
    q95 = get_discrete_quantiles(estimate, state, 0.95),
    mean = sum(estimate * state)
  ) |>
  ungroup() |>
  relocate(date) |>
  arrange(date) |>
  mutate(
    price =
      c(
        rep(price_23, length(expirations)),
        rep(price_27, length(expirations))
      )
  )



beta_coefs_plot <-
  ggplot(beta_coefs, aes(x = state, y = estimate, group = expiration)) +
  geom_col() +
  facet_grid(date ~ as_date(expiration)) +
  geom_vline(data = summaries, aes(xintercept = q5), linetype = "dashed", linewidth = 0.3) +
  geom_vline(data = summaries, aes(xintercept = q95), linetype = "dashed", linewidth = 0.3) +
  geom_vline(data = summaries, aes(xintercept = mean), linetype = "solid", linewidth = 0.3) +
  scale_x_continuous("State", breaks = extended_breaks(n = 10)) +
  scale_y_continuous("Probability") +
  theme_light()

ggsave("betas.pdf",
  beta_coefs_plot,
  path = "~/Documents/Risk-Neutral-Probability/Figures/",
  width = 297 / 1.3,
  height = 210 / 1.3,
  units = "mm"
)



alphas_23 <-
  bind_rows(
    results_23[[1]][[2]] |>
      filter(term == "alpha"),
    results_23[[2]][[2]] |>
      filter(term == "alpha"),
    results_23[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = rep(as_date("2025-05-23"))))


alphas_27 <-
  bind_rows(
    results_27[[1]][[2]] |>
      filter(term == "alpha"),
    results_27[[2]][[2]] |>
      filter(term == "alpha"),
    results_27[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(expiration = expirations, date = rep(as_date("2025-05-27"))))

alphas <- bind_rows(alphas_23, alphas_27)



alphas


alphas_plot <-
  ggplot(alphas, aes(x = as_factor(as.character(expiration)), y = estimate, group = date, color = as_factor(as.character(date)))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(min = conf.low, max = conf.high), width = .3, position = position_dodge(width = 0.5)) +
  scale_x_discrete("Expiration Date") +
  scale_y_continuous(expression(alpha ~ "Estimate"), breaks = extended_breaks(n = 6)) +
  theme_minimal() +
  labs(color = "Date") +
  theme(legend.position = "bottom")
alphas_plot

ggsave("alphas.pdf",
  alphas_plot,
  path = "~/Documents/Risk-Neutral-Probability/Figures/",
  width = 297 / (1.6 * 1.2),
  height = 210 / 1.6,
  units = "mm"
)

for (i in seq(1, length(expirations))) {
  ggsave(paste("betas_23_", as.character(i), ".pdf", sep = ""),
    results_23[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )
}


for (i in seq(1, length(expirations))) {
  ggsave(paste("betas_27_", as.character(i), ".pdf", sep = ""),
    results_27[[i]][[4]],
    path = "~/Documents/Risk-Neutral-Probability/Figures/",
    width = 297 / 1.6,
    height = 210 / 1.6,
    units = "mm"
  )
}



alpha_histogram <-
  ggplot(as_tibble(extract(results_23[[3]][[1]])$alpha), aes(x = value)) +
  geom_histogram(alpha = 0.2, color = "black", fill = "#F8766D") +
  geom_histogram(
    data = as_tibble(extract(results_27[[3]][[1]])$alpha),
    alpha = 0.2, color = "black", fill = "#00BFC4"
  ) +
  labs(x = "Alpha", y = "Frequency") +
  theme_light()
alpha_histogram



ggsave("alpha_histogram.pdf",
  alpha_histogram,
  path = "~/Documents/Risk-Neutral-Probability/Figures/",
  width = 297 / 1.6,
  height = 210 / 1.6,
  units = "mm"
)


summaries_plot <-
  ggplot(summaries, aes(x = as_factor(as.character(expiration)), y = mean, group = date, color = date)) +
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

