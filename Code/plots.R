library(tidyverse)
library(quantmod)
library(broom)
library(broom.mixed)
library(rstan)
library(scales)
library(patchwork)



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
    results_26[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-03-26"),
    results_26[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-03-26"),
    results_26[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-03-26"),
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
    results_03[[1]][[3]] |> mutate(expiration = expirations[1], date = "2025-04-03"),
    results_03[[2]][[3]] |> mutate(expiration = expirations[2], date = "2025-04-03"),
    results_03[[3]][[3]] |> mutate(expiration = expirations[3], date = "2025-04-03"),
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



# Function to calculate discrete quantiles (e.g., 5% and 95%)
get_discrete_quantiles <- function(prob_vec, bins, probs) {
  cdf <- cumsum(prob_vec)
  sapply(probs, function(p) bins[which(cdf >= p)[1]])
}


price_01 <- filter(aapl, date == "2025-04-01")$price
price_03 <- filter(aapl, date == "2025-04-03")$price
price_04 <- filter(aapl, date == "2025-04-04")$price
price_09 <- filter(aapl, date == "2025-04-09")$price

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

beta_coefs <-
  beta_coefs_full |> filter(date %in% c("2025-04-01", "2025-04-03"))

summaries <- summaries_full |>
  filter(date %in% c("2025-04-01", "2025-04-03")) |>
  mutate(
    price =
      c(
        rep(price_01, length(expirations)),
        rep(price_03, length(expirations))
      )
  )


beta_coefs_plot <-
  ggplot(beta_coefs, aes(x = state, y = estimate, group = expiration)) +
  geom_col(color = "black", fill = "gray", alpha = 0.8) +
  geom_errorbar(aes(max = conf.high, min = conf.low), width = 3, color = "gray60") +
  facet_grid(date ~ as_date(expiration)) +
  geom_vline(data = summaries, aes(xintercept = q5), linetype = "dashed", linewidth = 0.3) +
  geom_vline(data = summaries, aes(xintercept = q95), linetype = "dashed", linewidth = 0.3) +
  geom_vline(data = summaries, aes(xintercept = mean), linetype = "solid", linewidth = 0.3) +
  scale_x_continuous("State", breaks = extended_breaks(n = 10)) +
  scale_y_continuous("") +
  theme_light()


beta_coefs_plot


ggsave("betas.pdf",
       beta_coefs_plot,
       path = "~/Documents/Risk-Neutral-Probability/Figures/",
       width = 297 / 1.3,
       height = 210 / 1.3,
       units = "mm"
)



summaries_plot <-
  ggplot(summaries, aes(x = as_factor(as.character(expiration)), y = mean, group = date, color = as.character(date))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(min = q5, max = q95), width = .3, position = position_dodge(width = 0.5)) +
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
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = "black", alpha = 0.15) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "black", alpha = 0.2) +
  geom_vline(xintercept = as_date("2025-04-02"), color = "darkred") +
  geom_vline(xintercept = as_date("2025-04-09"), color = "gray") +
  geom_point() +
  geom_line() +
  facet_wrap(~expiration, nrow = 3) +
  scale_x_date("", date_breaks = "3 days", date_minor_breaks = "1 day", date_labels = "%b %d") +
  scale_y_continuous("Price", breaks = extended_breaks(n = 6)) +
  labs(color = "Date") +
  theme_light()
summaries_plot_full

ggsave("summaries_plot_full.pdf",
       summaries_plot_full,
       path = "~/Documents/Risk-Neutral-Probability/Figures/",
       width = 297 / 1.6,
       height = 210 / 1.6,
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


alphas_26 <-
  bind_rows(
    results_26[[1]][[2]] |>
      filter(term == "alpha"),
    results_26[[2]][[2]] |>
      filter(term == "alpha"),
    results_26[[3]][[2]] |>
      filter(term == "alpha")
  ) |> bind_cols(tibble(
    expiration = expirations,
    date = as_date("2025-03-26")
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
    alphas_26,
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
    alphas_15,
    alphas_16, alphas_17
  )




two_alphas_plot <-
  ggplot(
    alphas |> filter(date %in% c(as_date("2025-04-01"), as_date("2025-04-03"))),
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
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "black", alpha = 0.2) +
  geom_vline(xintercept = as_date("2025-04-02"), color = "darkred") +
  geom_vline(xintercept = as_date("2025-04-09"), color = "gray") +
  geom_line() +
  geom_point() +
  scale_x_date("", date_breaks = "3 days", date_minor_breaks = "1 day", date_labels = "%b %d") +
  scale_y_continuous(expression(alpha ~ "Estimate"), breaks = extended_breaks(n = 6)) +
  facet_wrap(~expiration, nrow = 3, scales = "free_y") +
  theme_light() +
  theme(legend.position = "bottom")
all_alphas_plot


ggsave("all_alphas_plot.pdf",
       all_alphas_plot,
       path = "~/Documents/Risk-Neutral-Probability/Figures/",
       width = 297 / 1.6,
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
