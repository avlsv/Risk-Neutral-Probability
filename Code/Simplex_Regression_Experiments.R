library(tidyverse)
library(rstan)

options(mc.cores =  parallel::detectCores())

n <- 50
X <- cbind(
  x1 = rnorm(n , 10, 2),
  x2 = runif(n, 0, 8),
  x3 = rnorm(n, 15, 3),
  x4 = runif(n, 0, 20),
  x5 = rnorm(n, 3, 1.5),
  x6 = rnorm(n, 8, 1)
)

true_coefs_unnorm <- c(0.3, 0.3, 0.3, 0.03, 0.035, 0.035)
true_coefs <- true_coefs_unnorm / sum(true_coefs_unnorm)

y <- 1 / (1 + 0.05) * X %*% true_coefs + rnorm(n, 0, 5)


stan_data <- list(
  n = nrow(X),
  k = ncol(X),
  X = X,
  y = as.vector(y)
)

stan_model <- 
  stan(
  "simplex.stan",
  data = stan_data,
  iter = 4000,
  chains = 8
)


coefs5 <- apply(extract(mod5, "b")$b, 2, mean)

summary(mod5)[1] |>  as_tibble()

coefs_tbl <- rstan::extract(mod5, "b")$b |> as_tibble() |> summarize_all(mean)


coefs <- get_posterior_mean(mod5) |> rowMeans()

distr <- extract(mod5)

hist_b1 <- distr$b[,1]|> hist(plot=F)

hist(distr$b[,5],plot=F)$mids[which.max(hist(distr$b[,5],plot=F)$counts)]


 (round(coefs_tbl, digits = 3) - round(true_coefs, digits = 3)) / round(true_coefs, digits =
                                                                         3)
