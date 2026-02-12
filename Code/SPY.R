library(renv)
renv::restore()
library(pacman)

p_load(quantmod, broom, broom.mixed, rstan, scales, patchwork, readr, readxl, scales, tictoc, systemfonts, ragg, styler, tidyverse)

# renv::snapshot()


options(mc.cores = parallel::detectCores())

simplex_model <- "simplex.stan" |> stan_model()

estimation_procedure <- function(dataset,
                                 model = simplex_model,
                                 states = seq(120 - 20, 260 + 20, by = 10),
                                 iter = 1000,
                                 chains = 4) {
  print(dataset$date[1])

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
      sampling(
        model,
        data = stan_data_aapl,
        iter = iter,
        chains = chains
      )

    coefs_90 <- stan_model_aapl |>
      tidy(
        conf.int = T,
        conf.level = 0.90,
        robust = T,
        conf.method = "HPDinterval"
      )


    coefs_95 <- stan_model_aapl |>
      tidy(
        conf.int = T,
        conf.level = 0.95,
        robust = T,
        conf.method = "HPDinterval"
      ) |>
      rename(
        conf.low.hpd.0.95 = conf.low,
        conf.high.hpd.0.95 = conf.high
      )

    coefs_90_q <- stan_model_aapl |>
      tidy(
        conf.int = T,
        conf.level = 0.90,
        robust = T,
        conf.method = "quantile"
      ) |>
      rename(conf.low.q.90 = conf.low, conf.high.q.90 = conf.high)

    coefs_90_m <- stan_model_aapl |>
      tidy(
        conf.int = T,
        conf.level = 0.90,
        robust = T,
        conf.method = "quantile"
      ) |>
      rename(
        conf.low.q.90 = conf.low, conf.high.q.90 = conf.high,
        estimate_mean = estimate
      )


    coefs <- coefs_90 |>
      full_join(coefs_95) |>
      full_join(coefs_90_q) |>
      full_join(coefs_90_m)


    betas <- coefs |>
      filter(startsWith(term, "b")) |>
      mutate(
        state = states_tbl$state,
        expiration = expirations[j]
      )

    plot <- ggplot(betas, aes(x = state, y = estimate)) +
      geom_col(
        color = "black",
        fill = "gray",
        alpha = 0.8
      ) +
      geom_errorbar(aes(max = conf.high, min = conf.low), width = 4) +
      scale_y_continuous("", breaks = extended_breaks(n = 6)) +
      scale_x_continuous("State", breaks = extended_breaks(n = round(length(states) / 2) + 1)) +
      theme_light()


    results[[j]] <- list("model" = stan_model_aapl, "coefs" = coefs, "betas" = betas, "plot" = plot)
    results
  }
  return(results)
}



getSymbols("SPY", from = "2026-01-30", to = "2026-02-05", auto.assign = T)

last_price <- last(as_tibble(SPY, rownames = "Date"))$SPY.Close
old_price <- first(as_tibble(SPY, rownames = "Date"))$SPY.Close

state_space_normalized <- seq(-30, 20, by = 5)


state_space <- (state_space_normalized / 100 + 1) * last_price
state_space_old <- (state_space_normalized / 100 + 1) * old_price


iter <- 5000
chains <- 4

{
  tic("estimation")

  results <-
    estimation_procedure(
      dataset = read_csv("Data/SPY Options Feb 5 2026.csv", show_col_types = F),
      states = state_space,
      iter = iter,
      chains = chains
    )


  results_old <-
    estimation_procedure(
      dataset = read_csv("Data/SPY Options Jan 30 2026.csv", show_col_types = F),
      states = state_space_old,
      iter = iter,
      chains = chains
    )

  # saveRDS(results_25, file = "Data/Results/results_25.RData")

  toc()
}


# renv::snapshot()

{
  betas <- tibble()
  for (i in 1:length(results)) {
    betas <- bind_rows(betas, results[[i]]$betas)
  }
}

betas <- betas |> mutate(state_percent = state / last_price - 1)


{
  betas_old <- tibble()
  for (i in 1:length(results)) {
    betas_old <- bind_rows(betas_old, results_old[[i]]$betas)
  }
}


betas_compare <-
  results_old[[4]]$betas |>
  mutate(date = "2026-01-30") |>
  mutate(state_percent = state / old_price - 1) |>
  bind_rows(results[[4]]$betas |> mutate(date = "2026-02-05") |>
    mutate(state_percent = state / last_price - 1))


betas_plot <-
  ggplot(
    betas_compare,
    aes(x = state_percent, y = estimate)
  ) +
  geom_col(
    color = "#2F2926",
    fill =  "#6E8C2F"
  ) +
  geom_errorbar(aes(max = conf.high.hpd.0.95, min = conf.low.hpd.0.95), width = .01, alpha = 0.5) +
  scale_y_continuous(NULL) +
  scale_x_continuous("%", labels = \(x){
    100 * x
  }, breaks = extended_breaks(n = 6)) +
  theme_light() +
  facet_wrap(~date) +
  labs(title = "Имплицитные вероятности изменения цен S&P500\nк 2026-03-31", caption = "95% highest posterior density interval") +
  theme(
    plot.title = element_text(size = 11, color = "#2F2926"),
    text = element_text(size = 12, color = "#2F2926", family = "SB Sans Interface"),
    plot.subtitle = element_text(size = 12, color = "#2F2926"),
    plot.caption = element_text(size = 6, color = "#737373"),
    plot.background = element_rect(fill = "#FDF5E6", color = NA),
    panel.background = element_rect(fill = "#FDF5E6", color = NA),
    legend.position = "top",
    # legend.direction = "horizontal",
    # legend.direction = "vertical",
    legend.box.margin = margin(b = -5, l = -4, t = -5),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box = "horizontal",
    legend.justification = "left",
    panel.border = element_blank(),
    axis.line = element_line(colour = "#2F2926"),
    axis.text = element_text(colour = "#2F2926"),
    axis.ticks = element_line(colour = "#2F2926"),
    axis.title = element_text(color = "#2F2926"),
    legend.text = element_text(color = "#2F2926"),
    legend.title = element_text(color = "#2F2926"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"),
    # panel.grid.major = element_line(color = NA),
    # panel.grid.minor = element_line(color = NA),
    panel.grid.major.y = element_line(color = alpha("#2F2926", 0.1)),
    panel.grid.minor.y = element_line(color = alpha("#2F2926", 0.1)),
    panel.grid.major.x = element_line(color = alpha("#2F2926", 0.1)),
    panel.grid.minor.x = element_line(color = alpha("#2F2926", 0.1)),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = "#2F2926")
  )

ggsave("~/Library/CloudStorage/OneDrive-Personal/MACRO/export/us_S&P_implied_probability_density.png", betas_plot,
  width = 9 / 1.6, height = 9 / 1.6
)

xx <-
  results[[4]]$coefs |> filter(term == "alpha")
