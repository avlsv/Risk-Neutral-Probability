library(tidyverse)
library(LaplacesDemon)


# Set parameters
n_samples <- 10000
alpha_1 <- 0.2
alpha_2 <- 0.4
alpha_3 <- 0.8

samples_1 <- rdirichlet(n_samples, rep(alpha_1, length(state_space)))
samples_2 <- rdirichlet(n_samples, rep(alpha_2, length(state_space)))
samples_3 <- rdirichlet(n_samples, rep(alpha_3, length(state_space)))

# Sort each sample's values in decreasing order
sorted_samples_1 <- t(apply(samples_1, 1, sort, decreasing = TRUE))
sorted_samples_2 <- t(apply(samples_2, 1, sort, decreasing = TRUE))
sorted_samples_3 <- t(apply(samples_3, 1, sort, decreasing = TRUE))


# Convert to tidy format for plotting
df_1 <- as_tibble(sorted_samples_1) |> mutate(SampleID = 1:n_samples)
df_2 <- as_tibble(sorted_samples_2) |> mutate(SampleID = 1:n_samples)
df_3 <- as_tibble(sorted_samples_3) |> mutate(SampleID = 1:n_samples)


tidy_df_1 <- df_1 |>
  pivot_longer(cols = -SampleID, names_to = "Rank", values_to = "Probability") %>%
  mutate(Rank = as.integer(gsub("V", "", Rank))) |>
  mutate(rank_mod = (1 - Rank %% 2) * (Rank / 2 + 10) + (Rank %% 2) * (10 - (Rank - 1) / 2)) |>
  mutate(alpha = alpha_1)

tidy_df_2 <- df_2 |>
  pivot_longer(cols = -SampleID, names_to = "Rank", values_to = "Probability") %>%
  mutate(Rank = as.integer(gsub("V", "", Rank))) |>
  mutate(rank_mod = (1 - Rank %% 2) * (Rank / 2 + 10) + (Rank %% 2) * (10 - (Rank - 1) / 2)) |>
  mutate(alpha = alpha_2)
tidy_df_3 <- df_3 |>
  pivot_longer(cols = -SampleID, names_to = "Rank", values_to = "Probability") %>%
  mutate(Rank = as.integer(gsub("V", "", Rank))) |>
  mutate(rank_mod = (1 - Rank %% 2) * (Rank / 2 + 10) + (Rank %% 2) * (10 - (Rank - 1) / 2)) |>
  mutate(alpha = alpha_3)


tidy_df <- bind_rows(tidy_df_1, tidy_df_2, tidy_df_3)


# Compute mean and error bars for each rank
summary_df <- tidy_df %>%
  group_by(rank_mod, alpha) %>%
  summarise(
    Mean = mean(Probability),
    SD = sd(Probability),
    .groups = "drop"
  )

# Plot: Histogram-like barplot of sorted Dirichlet values

dirichlet_histogram <-
  ggplot(summary_df, aes(x = rank_mod, y = Mean)) +
  facet_wrap(~alpha) +
  geom_col(color = "black", alpha = 0.2) +
  labs(y = "Mean Probability", x = "") +
  theme_light()



ggsave("dirichlet_histogram.pdf",
       dirichlet_histogram,
       width = 297 / 1.6,
       height = 210 / (1.6 * 1.4),
       units = "mm",
       path = "~/Documents/Risk-Neutral-Probability/Figures/"
)
