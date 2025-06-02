library(tidyverse)

x_vals <- seq(0, 5, length.out = 1000)

# Create data frame with both densities
df <- data.frame(
  x = rep(x_vals, 2),
  density = c(dweibull(x_vals, shape = 1.1, scale = 1),
              dlnorm(x_vals, meanlog = 0, sdlog = 1)),
  dist = rep(c("Weibull(1.1, 1)", "Lognormal(0, 1)"), each = length(x_vals))
)



# Plot
ggplot(df, aes(x = x, y = density, color = dist)) +
  geom_line() +
  labs(
    x = "x",
    y="",
    color = "Distribution",
    fill = "Distribution"
  ) + theme_minimal() +theme(legend.position = "bottom")


ggsave("prior_difference.pdf",
       path = "~/Documents/Risk-Neutral-Probability/Figures/",
       width = 297 / 1.6,
       height = 210 / 1.6,
       units = "mm"
