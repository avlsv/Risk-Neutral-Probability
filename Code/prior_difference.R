library(tidyverse)

x_vals <- seq(0, 3, length.out = 1000)

# Create data frame with both densities
df <- tibble(
  x = rep(x_vals, 2),
  density = c(dweibull(x_vals, shape = 1.1, scale = 1),
              dlnorm(x_vals, meanlog = 0, sdlog = 1)),
  dist = rep(c("Weibull(1.1, 1)", "Lognormal(0, 1)"), each = length(x_vals))
)




# Plot
prior_difference_plot <-
  ggplot(df, aes(x = x, y = density, color = dist)) +
  geom_line() +
  geom_vline(aes(xintercept = 1), alpha = 0.5) +
  labs(
    x = "x",
    y = "",
    color = "Distribution",
    fill = "Distribution"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


ggsave("prior_difference.pdf",
       prior_difference_plot,
       path = "~/Documents/Risk-Neutral-Probability/Figures/",
       width = 297 / 1.8,
       height = 210 / 1.8,
       units = "mm"
)
